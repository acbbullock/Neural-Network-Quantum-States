module nnqs
	!-------------------------------------------------------------------------------------------------------------------
	!! This module contains an implementation of the stochastic optimization algorithm for learning the ground state
	!! of the Ising spin model by representing the wave-functions ψ(s,α) as a type RestrictedBoltzmannMachine.
	!-------------------------------------------------------------------------------------------------------------------
	use, intrinsic :: iso_fortran_env, only: rk=>real32, ik=>int8, int64, compiler_version, compiler_options
	use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
	use io_fortran_lib, only: echo, LF, str, to_file                                     !! I/O procedures and constants
	use lapack95, only: posvx
	implicit none (type,external)                                                     !! No implicit types or interfaces
	private                             !! All objects in scope are inaccessible outside of scope unless declared public

	!! Public API list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	public :: RestrictedBoltzmannMachine

	!! Definitions and Interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	type RestrictedBoltzmannMachine
		private
		integer   :: v_units     = 0                                                          !! Number of visible units
		integer   :: h_units     = 0                                                           !! Number of hidden units
		character :: alignment   = 'N'                                                    !! For tracking spin alignment
		logical   :: initialized = .false.                                                      !! Initialization status
		real(rk),    allocatable, dimension(:)   :: a, p_a, r_a                    !! Visible layer biases & ADAM arrays
		complex(rk), allocatable, dimension(:)   :: b, p_b, r_b                     !! Hidden layer biases & ADAM arrays
		complex(rk), allocatable, dimension(:,:) :: w, p_w, r_w                                 !! Weights & ADAM arrays
		contains
			private
			procedure, pass(self), public :: stochastic_optimization                          !! Public training routine
			procedure, pass(self)         :: init                                              !! Initialization routine
			procedure, pass(self)         :: sample_distribution                      !! MCMC routine for sampling |ψ|^2
			procedure, pass(self)         :: prob_ratio                           !! Probability ratio |ψ(s_2)/ψ(s_1)|^2
			procedure, pass(self)         :: ising_energy                                          !! Ising local energy
			procedure, pass(self)         :: propagate                        !! Routine for updating weights and biases
	end type RestrictedBoltzmannMachine

	interface RestrictedBoltzmannMachine
		procedure :: new_rbm                                              !! Constructor function with same name as type
	end interface

	contains !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	pure recursive type(RestrictedBoltzmannMachine) function new_rbm(v_units, h_units) result(new)
		!---------------------------------------------------------------------------------------------------------------
		!! Function for constructing a RestrictedBoltzmannMachine
		!---------------------------------------------------------------------------------------------------------------
		integer, intent(in) :: v_units, h_units                       ! Number of visible and hidden units to initialize
		new%v_units = v_units                       !! Set number of visible units (always equal to the number of spins)
		new%h_units = h_units                    !! Set number of hidden units (chosen arbitrarily to optimize learning)
	end function new_rbm

	impure recursive subroutine stochastic_optimization(self, ising_params)
		!---------------------------------------------------------------------------------------------------------------
		!! Public routine for training RBM
		!---------------------------------------------------------------------------------------------------------------
		class(RestrictedBoltzmannMachine), intent(inout) :: self                                    !! Boltzmann machine
		real(rk), contiguous, dimension(:), intent(in)   :: ising_params                             !! Ising parameters

		integer(ik), allocatable, dimension(:)   :: start_sample                         !! Start sample for Monte Carlo
		integer(ik), allocatable, dimension(:,:) :: samples                                      !! Sample storage array

		real(rk),    allocatable, dimension(:)   :: e_loc, corrs                  !! Local energies, sample correlations
		real(rk),    allocatable, dimension(:,:) :: energies, correlations             !! Energy and correlation storage
		complex(rk), allocatable, dimension(:)   :: theta                                         !! Cache of θ = b + ws

		real(rk),    allocatable, dimension(:,:)   :: O_a, S_a           !! Log derivatives with respect to a, SR matrix
		complex(rk), allocatable, dimension(:,:)   :: O_b, S_b           !! Log derivatives with respect to b, SR matrix
		complex(rk), allocatable, dimension(:,:,:) :: O_w, S_w           !! Log derivatives with respect to w, SR matrix

		real(rk),    allocatable, dimension(:)   :: F_a, x_a                                  !! Forces for a, solutions
		complex(rk), allocatable, dimension(:)   :: F_b, x_b                                  !! Forces for b, solutions
		complex(rk), allocatable, dimension(:,:) :: F_w, x_w                                  !! Forces for w, solutions

		character(len=:), allocatable :: logfile, logmsg                                          !! Recording variables
		character(len=10)             :: date, time                                                     !! Date and time
		character(len=1000)           :: errmsg                                                         !! Error message

		integer(int64) :: t1, t2                                                                      !! Clock variables
		real(rk)       :: rate, wall_time                                                             !! Clock variables

		real(rk) :: energy, sqerr, stderr, tau, acc                                               !! Recording variables
		integer  :: epoch, max_epochs, num_samples, n, m, stat                    !! Size and loop variables, error stat

		if ( this_image() == 1 ) then                                     !! Check validity of Ising strength parameters
			if ( size(ising_params) /= 2 ) then
				error stop  LF//'FATAL: Invalid size for ising_params... size must be (2).'// &
							LF//'USAGE: ising_params=[J,B] where J is the neighbor coupling strength and B '// &
								'is the transverse field strength.'//LF
			end if

			if ( abs(ising_params(2)) >= 1.0_rk ) then
				error stop LF//'FATAL: Invalid field strength parameter... try again with |B| < 1.'//LF
			end if
			sync images (*)                                                                   !! Respond to other images
		else
			sync images (1)                                                            !! Wait for response from image 1
		end if

		if ( ising_params(1) < 0.0_rk ) then                                          !! Check sign of coupling strength
			self%alignment = 'A'                                                          !! Anti-ferromagnetic if J < 0
		else
			self%alignment = 'F'                                                               !! Ferromagnetic if J > 0
		end if

		n = self%v_units                                                                          !! Get number of spins
		m = self%h_units                                                                   !! Get number of hidden units
		max_epochs = 1000                                                                          !! Set maximum epochs
		num_samples = 16                                                             !! Set number of samples to produce
		errmsg = ''                                                                          !! Initialize error message
		stat = 0                                                                                !! Initialize error stat

		call self%init(n=n, m=m, stat=stat, errmsg=errmsg)                               !! Initialize Boltzmann machine

		!! Allocate work arrays across all images:
		allocate(start_sample(n), samples(num_samples,n), source=0_ik, stat=stat, errmsg=errmsg)
		if (stat /= 0) error stop LF//errmsg                                                         !! Check allocation

		allocate(e_loc(num_samples), corrs(n), source=0.0_rk, stat=stat, errmsg=errmsg)
		if (stat /= 0) error stop LF//errmsg                                                         !! Check allocation

		allocate(O_a(num_samples,n), S_a(n,n), F_a(n), x_a(n), source=0.0_rk, stat=stat, errmsg=errmsg)
		if (stat /= 0) error stop LF//errmsg                                                         !! Check allocation

		allocate(O_b(num_samples,m), S_b(m,m), F_b(m), x_b(m), source=(0.0_rk,0.0_rk), stat=stat, errmsg=errmsg)
		if (stat /= 0) error stop LF//errmsg                                                         !! Check allocation

		allocate(O_w(num_samples,m,n), S_w(m,m,n), F_w(m,n), x_w(m,n), source=(0.0_rk,0.0_rk), stat=stat, errmsg=errmsg)
		if (stat /= 0) error stop LF//errmsg                                                         !! Check allocation

		call random_sample(n, start_sample)                                             !! Randomize the starting sample
		theta = conjg(self%b) + matmul(self%w, start_sample)                                            !! Get initial θ

		!$omp target data map(to:samples,e_loc,self%w,self%b) map(from:F_a,F_b,F_w,S_a,S_b,S_w) map(alloc:O_a,O_b,O_w)

		if ( this_image() == 1 ) then                                                       !! Do preparation on image 1
			!! Allocate storage arrays on image 1:
			allocate(energies(max_epochs,2), correlations(n,max_epochs), source=0.0_rk, stat=stat, errmsg=errmsg)
			if (stat /= 0) error stop LF//errmsg                                                     !! Check allocation

			logfile = 'optimization_results.log'                                                         !! Set log file
			call date_and_time(date=date, time=time)                                                !! Get date and time

			logmsg = 'Stochastic Optimization - date: '//trim(adjustl(date))//' | time: '//time//& !! Training log title
					 LF//repeat('-', ncopies=59)//LF

			call echo(logmsg, file_name=logfile)                                                         !! Echo to file
			write(unit=*, fmt='(a)') logmsg                                                         !! Print log message

			call system_clock(t1)                                                                         !! Start timer
			sync images (*)                                                                   !! Respond to other images
		else
			sync images (1)                                                            !! Wait for response from image 1
		end if

		learning: do epoch = 1, max_epochs                                                             !! Begin learning
			call co_sum(self%w); self%w = self%w/num_images()                           !! Average weights across images

			call self%sample_distribution(n=n, m=m, epoch=epoch, ising_params=ising_params, &                  !! Inputs
										  start_sample=start_sample, theta=theta, &                     !! Input/outputs
										  samples=samples, e_loc=e_loc, corrs=corrs, &                  !! Array outputs
										  energy=energy, sqerr=sqerr, &                                !! Scalar outputs
										  stat=stat, errmsg=errmsg)                  !! Error vars for local allocations

			call co_sum(energy); energy = energy/num_images()                            !! Average energy across images
			call co_sum(sqerr);  stderr = sqrt(sqerr)/num_images()                        !! Average error across images
			call co_sum(corrs);  corrs = corrs/num_images()                        !! Average correlations across images

			if ( stderr < 0.01_rk ) then
				!! Add Gaussian perturbation:
				self%w = self%w + cmplx( gauss(mu=0.01_rk, sig=1e-4_rk), gauss(mu=-0.005_rk, sig=1e-5_rk), kind=rk )
			end if

			if ( this_image() == 1 ) then                                                !! Do data recording on image 1
				energies(epoch,:) = [energy, stderr]                               !! Record energy and error to storage
				correlations(:,epoch) = corrs                                          !! Record correlations to storage

				!! Write progress report ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
				tau = (epoch-1)*0.01_rk                                                            !! Get imaginary time

				logmsg = '    Epoch '//str(epoch)//': E[ψ(α(τ='//str(tau, fmt='f', decimals=3)//'))] = '// &
						 str(energy, fmt='f', decimals=3)//' ± '//str(stderr, fmt='f', decimals=3)

				call echo(logmsg, logfile)                                                         !! Record log message
				write(unit=*, fmt='(a)') logmsg                                                     !! Print log message

				if ( ieee_is_nan(energy) ) error stop LF//'FATAL: Numerical instability.'           !! Error termination
				sync images (*)                                                               !! Respond to other images
			else
				sync images (1)                                                        !! Wait for response from image 1
			end if

			acc = 1.0_rk - real(count(samples == 0_ik), kind=rk)/size(samples)              !! Get ground state accuracy
			call co_sum(acc); acc = acc/num_images()                                   !! Average accuracy across images
			if ( (acc > 0.9999_rk) .or. (epoch == max_epochs) ) exit learning                         !! Exit conditions

			e_loc = e_loc - sum(e_loc)/num_samples                                          !! Center the local energies

			call self%propagate(n=n, m=m, epoch=epoch, e_loc=e_loc, samples=samples, &              !! Update parameters
								O_a=O_a, O_b=O_b, O_w=O_w, &
								S_a=S_a, S_b=S_b, S_w=S_w, &
								F_a=F_a, F_b=F_b, F_w=F_w, &
								x_a=x_a, x_b=x_b, x_w=x_w)
		end do learning

		!$omp end target data

		if ( this_image() == 1 ) then                                              !! Do finalization and I/O on image 1
			call system_clock(t2, count_rate=rate); wall_time = real(t2-t1, kind=rk)/rate               !! Get time in s

			logmsg = LF//'    Optimization time: '//str(wall_time, fmt='f', decimals=3)//' seconds for n = '// &
						 str(n)//' spins.'// &
					 LF//'    Ground state energy: E[ψ(α(τ → ∞))] = '//str(energy, fmt='f', decimals=3)// &
						 ' ± '//str(stderr, fmt='f', decimals=3)//' for J = '// &
						 str(ising_params(1), fmt='f', decimals=1)//' and B = '// &
						 str(ising_params(2), fmt='f', decimals=1)// &
					 LF//'    Ground state accuracy: '//str(acc, fmt='f', decimals=6)//LF// &
					 LF//'    This program was built and run with compiler "'//compiler_version()//'" '// &
						 'using compiler options "'//compiler_options()//'".'//LF

			call echo(logmsg, logfile)                                                             !! Record log message
			write(unit=*, fmt='(a)') logmsg                                                         !! Print log message

			!! Write data to files:
			call to_file(energies(:epoch,:), './data/energies_'//self%alignment//'.csv', header=['Energy', 'Error '] )
			call to_file(correlations(:,:epoch), './data/correlations_'//self%alignment//'.csv', header=['Epoch'] )
		end if
	end subroutine stochastic_optimization

	impure recursive subroutine init(self, n, m, stat, errmsg)
		!---------------------------------------------------------------------------------------------------------------
		!! Procedure for initialization
		!---------------------------------------------------------------------------------------------------------------
		class(RestrictedBoltzmannMachine), intent(inout) :: self                                    !! Boltzmann machine
		integer,                           intent(in)    :: n, m                               !! Spins and hidden units
		integer,                           intent(inout) :: stat                                           !! Error stat
		character(len=1000),               intent(inout) :: errmsg                                      !! Error message

		integer :: i, j                                                                                !! Loop variables

		if ( this_image() == 1 ) then
			if ( (n < 1) .or. (m < 1) ) then
				error stop LF//'FATAL: Structure has not been instantiated or has invalid number of units.'
			end if
			sync images (*)                                                                   !! Respond to other images
		else
			sync images (1)                                                            !! Wait for response from image 1
		end if

		if ( self%initialized ) then
			deallocate(self%a, self%p_a, self%r_a, self%b, self%p_b, self%r_b, self%w, self%p_w, self%r_w, &
					   stat=stat, errmsg=errmsg)
			if (stat /= 0) error stop LF//errmsg                                                     !! Check allocation
			self%initialized = .false.
		end if

		!! Allocate class components:
		allocate(self%a(n), self%p_a(n), self%r_a(n), source=0.0_rk, stat=stat, errmsg=errmsg)
		if (stat /= 0) error stop LF//errmsg                                                         !! Check allocation

		allocate(self%b(m), self%p_b(m), self%r_b(m), source=(0.0_rk,0.0_rk), stat=stat, errmsg=errmsg)
		if (stat /= 0) error stop LF//errmsg                                                         !! Check allocation

		allocate(self%w(m,n), self%p_w(m,n), self%r_w(m,n), source=(0.0_rk,0.0_rk), stat=stat, errmsg=errmsg)
		if (stat /= 0) error stop LF//errmsg                                                         !! Check allocation

		do j = 1, n
			do i = 1, m
				self%w(i,j) = cmplx( gauss(mu=0.01_rk, sig=1e-4_rk), gauss(mu=-0.005_rk, sig=1e-5_rk), kind=rk )
			end do
		end do

		self%initialized = .true.
	end subroutine init

	impure recursive subroutine sample_distribution(self, n, m, epoch, ising_params, start_sample, theta, samples, &
													e_loc, corrs, energy, sqerr, stat, errmsg)
		!---------------------------------------------------------------------------------------------------------------
		!! Markov Chain Monte Carlo procedure for sampling |ψ|^2 with Metropolis-Hastings algorithm
		!---------------------------------------------------------------------------------------------------------------
		class(RestrictedBoltzmannMachine),       intent(in)    :: self                     !! Distribution to be sampled
		integer,                                 intent(in)    :: n, m, epoch      !! Spins, hidden units, current epoch
		real(rk),    contiguous, dimension(:),   intent(in)    :: ising_params                       !! Ising parameters
		integer(ik), contiguous, dimension(:),   intent(inout) :: start_sample         !! Sample to begin thermalization
		complex(rk), contiguous, dimension(:),   intent(inout) :: theta                   !! θ = b + ws for start sample
		integer(ik), contiguous, dimension(:,:), intent(out)   :: samples                              !! Output samples
		real(rk),    contiguous, dimension(:),   intent(out)   :: e_loc, corrs    !! Local energies, sample correlations
		real(rk),                                intent(out)   :: energy, sqerr          !! Energy average, square error
		integer,                                 intent(inout) :: stat                                     !! Error stat
		character(len=1000),                     intent(inout) :: errmsg                                !! Error message

		real(rk),    allocatable, dimension(:) :: s_map                                                 !! Storage array
		integer(ik), allocatable, dimension(:) :: s_prop                                        !! Sample storage arrays
		complex(rk), allocatable, dimension(:) :: theta2, arg_theta                                    !! Storage arrays

		real(rk) :: acc_prob, r                                                            !! M-H acceptance probability
		integer  :: num_samples, max_thermal_time, passes, k, pass, rind        !! Size and loop variables, random index

		num_samples = size(samples, dim=1)                                               !! Number of samples to produce

		!! Allocate local work arrays:
		allocate(s_map(n), source=0.0_rk, stat=stat, errmsg=errmsg)
		if (stat /= 0) error stop LF//errmsg                                                         !! Check allocation

		allocate(s_prop(n), source=0_ik, stat=stat, errmsg=errmsg)
		if (stat /= 0) error stop LF//errmsg                                                         !! Check allocation

		allocate(theta2(m), arg_theta(m), source=(0.0_rk,0.0_rk), stat=stat, errmsg=errmsg)
		if (stat /= 0) error stop LF//errmsg                                                         !! Check allocation

		!! Thermalization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		max_thermal_time = 2001 - 2*epoch                                           !! Set time limit for thermalization
		s_prop(:) = start_sample                                                                    !! Copy start sample

		thermalize: do k = 1, max_thermal_time
			call random_number(r); rind = floor(n*r) + 1                                        !! Generate random index
			s_prop(rind) = 1_ik - s_prop(rind)                                              !! Flip spin at random index
			call self%prob_ratio(ind=rind, s1=start_sample, s2=s_prop, theta1=theta, theta2=theta2, p=acc_prob) !! Acc p

			call random_number(r)                                           !! Sample from uniform distribution on [0,1)
			if ( r < acc_prob ) then                                                         !! M-H acceptance criterion
				theta = theta + self%w(:,rind)*real(s_prop(rind) - start_sample(rind), kind=rk)              !! Update θ
				start_sample(rind) = s_prop(rind)                                                       !! Update sample
			else
				s_prop(rind) = 1_ik - s_prop(rind)                                                       !! Reverse flip
			end if
		end do thermalize
		!! End thermalization

		passes = 2*n - min(2*epoch, 2*n-1)                               !! Number of passes to make on the start sample

		!! Stationary sampling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		stationary_sampling: do k = 1, num_samples
			s_prop(:) = start_sample                                                                 !! Copy temp sample

			metropolis_hastings: do pass = 1, passes
				call random_number(r); rind = floor(n*r) + 1                                    !! Generate random index
				s_prop(rind) = 1_ik - s_prop(rind)                                          !! Flip spin at random index
				call self%prob_ratio(ind=rind,s1=start_sample,s2=s_prop,theta1=theta,theta2=theta2,p=acc_prob)  !! Acc p

				call random_number(r)                                       !! Sample from uniform distribution on [0,1)
				if ( r < acc_prob ) then                                                     !! M-H acceptance criterion
					theta(:) = theta(:) + self%w(:,rind)*real(s_prop(rind) - start_sample(rind), kind=rk)    !! Update θ
					start_sample(rind) = s_prop(rind)                                                   !! Update sample
				else
					s_prop(rind) = 1_ik - s_prop(rind)                                                   !! Reverse flip
				end if
			end do metropolis_hastings

			samples(k,:) = start_sample                                                     !! Transfer sample to output
			call self%ising_energy( n=n, s=start_sample, theta=theta, ising_params=ising_params, &  !! Local energy of s
									s_map=s_map, arg_theta=arg_theta, energy=e_loc(k) )
		end do stationary_sampling
		!! End stationary sampling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		call get_correlations(n, samples, corrs, self%alignment)                     !! Get spin correlations of samples
		energy = sum(e_loc)/num_samples                                                           !! Average of energies
		sqerr = var(e_loc)/num_samples                                                       !! Square error of energies
	end subroutine sample_distribution

	pure recursive subroutine prob_ratio(self, ind, s1, s2, theta1, theta2, p)
		!---------------------------------------------------------------------------------------------------------------
		!! Function for computing the ratio of probabilities |ψ(s_2)/ψ(s_1)|^2 for two given configurations
		!---------------------------------------------------------------------------------------------------------------
		class(RestrictedBoltzmannMachine),     intent(in)    :: self                                !! Boltzmann machine
		integer,                               intent(in)    :: ind                                   !! Spin flip index
		integer(ik), contiguous, dimension(:), intent(in)    :: s1, s2                                 !! Configurations
		complex(rk), contiguous, dimension(:), intent(in)    :: theta1                       !! Cached value of b + ws_1
		complex(rk), contiguous, dimension(:), intent(inout) :: theta2                              !! Value of b + ws_2
		real(rk),                              intent(out)   :: p                                  !! Probability ration

		complex(rk) :: prob_amplitude_ratio                                                             !! ψ(s_2)/ψ(s_1)
		real(rk)    :: s                                                                                           !! ±1

		s = real(s2(ind) - s1(ind), kind=rk)
		theta2 = theta1 + self%w(:,ind)*s                                                                !! Get b + ws_2
		prob_amplitude_ratio = exp(self%a(ind)*s + sum(log(1.0_rk+exp(theta2)) - log(1.0_rk+exp(theta1))))    !! ψ ratio
		p = real(conjg(prob_amplitude_ratio)*prob_amplitude_ratio, kind=rk)                         !! |ψ(s_2)/ψ(s_1)|^2
	end subroutine prob_ratio

	pure recursive subroutine ising_energy(self, n, s, theta, ising_params, s_map, arg_theta, energy)
		!---------------------------------------------------------------------------------------------------------------
		!! Function for calculating local energy of configuration s in Ising model
		!---------------------------------------------------------------------------------------------------------------
		class(RestrictedBoltzmannMachine),     intent(in)    :: self                                !! Boltzmann machine
		integer,                               intent(in)    :: n                                     !! Number of spins
		integer(ik), contiguous, dimension(:), intent(in)    :: s                                 !! Configuration input
		complex(rk), contiguous, dimension(:), intent(in)    :: theta                          !! Cached value of b + ws
		real(rk),    contiguous, dimension(:), intent(in)    :: ising_params                         !! Ising parameters
		real(rk),    contiguous, dimension(:), intent(inout) :: s_map                                         !! s -> ±1
		complex(rk), contiguous, dimension(:), intent(inout) :: arg_theta                                  !! 1 + exp(θ)
		real(rk),                              intent(out)   :: energy                                   !! Ising energy

		real(rk) :: J_str, B_str, field_couplings, e_coupling, e_transverse   !! Ising params, field couplings, energies
		integer  :: j                                                                                   !! Loop variable

		J_str = abs(ising_params(1)); B_str = abs(ising_params(2))           !! Set coupling strength and field strength
		e_coupling = 0.0_rk; e_transverse = 0.0_rk                                                         !! Initialize

		s_map = -2.0_rk*s + 1.0_rk                                                              !! Map {0,1} -> {1.,-1.}
		arg_theta = 1.0_rk + exp(theta)                                                        !! 1 + exp(θ) for input s

		field_couplings = 0.0_rk
		do j = 1, n
			field_couplings = field_couplings + exp( self%a(j)*s_map(j) + &                     !! ψ(s')/ψ(s) for all s'
			sum(log(1.0_rk + exp(theta + self%w(:,j)*s_map(j))) - log(arg_theta)) )             !! Forget imaginary part
		end do

		e_coupling = -J_str*sum(s_map(1:n-1)*s_map(2:n))                    !! Local energy due to neighbor interactions
		e_transverse = -B_str*field_couplings                                    !! Local energy due to transverse field
		energy = e_coupling + e_transverse              !! Local energy is sum of coupling and transverse field energies
	end subroutine ising_energy

	impure recursive subroutine propagate(self, n, m, epoch, e_loc, samples, O_a, O_b, O_w, S_a, S_b, S_w, &
										  F_a, F_b, F_w, x_a, x_b, x_w)
		!---------------------------------------------------------------------------------------------------------------
		!! Procedure for updating parameters according to stochastic optimization update rule
		!---------------------------------------------------------------------------------------------------------------
		class(RestrictedBoltzmannMachine),         intent(inout) :: self                            !! Boltzmann machine
		integer,                                   intent(in)    :: n, m, epoch    !! Spins, hidden units, current epoch
		real(rk),    contiguous, dimension(:),     intent(in)    :: e_loc                              !! Local energies
		integer(ik), contiguous, dimension(:,:),   intent(in)    :: samples                            !! Network inputs
		real(rk),    contiguous, dimension(:,:),   intent(inout) :: O_a, S_a         !! Log derivatives for a, SR matrix
		complex(rk), contiguous, dimension(:,:),   intent(inout) :: O_b, S_b         !! Log derivatives for b, SR matrix
		complex(rk), contiguous, dimension(:,:,:), intent(inout) :: O_w, S_w         !! Log derivatives for w, SR matrix
		real(rk),    contiguous, dimension(:),     intent(inout) :: F_a, x_a                  !! Forces for a, solutions
		complex(rk), contiguous, dimension(:),     intent(inout) :: F_b, x_b                  !! Forces for b, solutions
		complex(rk), contiguous, dimension(:,:),   intent(inout) :: F_w, x_w                  !! Forces for w, solutions

		real(rk) :: covar_norm, delta, beta_1, beta_2, epsilon, dtau         !! Normalization, regularization, ADAM vars
		integer  :: num_samples, i, ii, j, jj, k                                        !! Size and loop variables

		real(rk)    :: tmpr                                                                                 !! Temp real
		complex(rk) :: tmpc                                                                              !! Temp complex

		num_samples = size(samples, dim=1)                                                      !! Get number of samples
		covar_norm  = 1.0_rk/(num_samples - 1)                                    !! Set sample covariance normalization

		delta   = 1e-3_rk                                                                !! Set regularization parameter
		beta_1  = 0.99_rk                                                                 !! Decay rate for first moment
		beta_2  = 0.999_rk                                                               !! Decay rate for second moment
		epsilon = 1e-8_rk                                                       !! Parameter to prevent division by zero
		dtau    = 0.01_rk                                                                                   !! Time step

		!$omp target update to(samples, e_loc, self%w, self%b)

		!$omp target teams distribute
		do j = 1, n
			!$omp parallel do
			do k = 1, num_samples
				O_a(k,j) = real(samples(k,j), kind=rk)                            !! O_a(k,j) = 𝜕/𝜕a_j ln ψ(s^k) = s_j^k
			end do
			!$omp end parallel do
		end do
		!$omp end target teams distribute

		!$omp target teams distribute reduction(+: tmpc) map(tmpc)
		do i = 1, m
			!$omp parallel do reduction(+: tmpc)
			do k = 1, num_samples
				tmpc = (0.0_rk,0.0_rk)
				do j = 1, n
					tmpc = tmpc + O_a(k,j)*self%w(i,j)
				end do
				O_b(k,i) = tmpc                                                                !! ws^k for all samples k
			end do
			!$omp end parallel do
		end do
		!$omp end target teams distribute

		!$omp target teams distribute
		do i = 1, m
			!$omp parallel do private(tmpc)
			do k = 1, num_samples
				tmpc = exp(conjg(self%b(i)) + O_b(k,i))      !! exp(θ_i^k) = exp(b_i + Σ_j w_ij*s_j^k) for all samples k
				O_b(k,i) = tmpc/(1.0_rk + tmpc)             !! O_b(k,i) = 𝜕/𝜕b_i ln ψ(s^k) = exp(θ_i^k)/(1 + exp(θ_i^k))
			end do
			!$omp end parallel do
		end do
		!$omp end target teams distribute

		!$omp target teams distribute
		do j = 1, n
			!$omp parallel do collapse(2)
			do i = 1, m
				do k = 1, num_samples
					O_w(k,i,j) = O_a(k,j)*O_b(k,i) !! O_w(k,i,j) = 𝜕/𝜕w_ij ln ψ(s^k) = s_j^k exp(θ_i^k)/(1 + exp(θ_i^k))
				end do
			end do
			!$omp end parallel do
		end do
		!$omp end target teams distribute

		!$omp target teams distribute parallel do reduction(+: tmpr)
		do j = 1, n
			tmpr = 0.0_rk
			do k = 1, num_samples
				tmpr = tmpr + O_a(k,j)
			end do
			tmpr = tmpr/num_samples
			do k = 1, num_samples
				O_a(k,j) = O_a(k,j) - tmpr                                          !! Center each column about its mean
			end do
		end do
		!$omp end target teams distribute parallel do

		!$omp target teams distribute parallel do reduction(+: tmpc)
		do i = 1, m
			tmpc = (0.0_rk,0.0_rk)
			do k = 1, num_samples
				tmpc = tmpc + O_b(k,i)
			end do
			tmpc = tmpc/num_samples
			do k = 1, num_samples
				O_b(k,i) = O_b(k,i) - tmpc                                           !! Center each column about its mean
			end do
		end do
		!$omp end target teams distribute parallel do

		!$omp target teams distribute reduction(+: tmpc) map(tmpc)
		do j = 1, n
			!$omp parallel do reduction(+: tmpc)
			do i = 1, m
				tmpc = (0.0_rk,0.0_rk)
				do k = 1, num_samples
					tmpc = tmpc + O_w(k,i,j)
				end do
				tmpc = tmpc/num_samples
				do k = 1, num_samples
					O_w(k,i,j) = O_w(k,i,j) - tmpc                                  !! Center each column about its mean
				end do
			end do
			!$omp end parallel do
		end do
		!$omp end target teams distribute

		!$omp target teams distribute parallel do reduction(+: tmpr)
		do j = 1, n
			tmpr = 0.0_rk
			do k = 1, num_samples
				tmpr = tmpr + O_a(k,j)*e_loc(k)
			end do
			F_a(j) = tmpr*covar_norm                                                           !! F(j) = ⟨Δ∂_{a_j}^† ΔH⟩
		end do
		!$omp end target teams distribute parallel do

		!$omp target teams distribute parallel do reduction(+: tmpc)
		do i = 1, m
			tmpc = (0.0_rk,0.0_rk)
			do k = 1, num_samples
				tmpc = tmpc + conjg(O_b(k,i))*e_loc(k)
			end do
			F_b(i) = tmpc*covar_norm                                                           !! F(i) = ⟨Δ∂_{b_i}^† ΔH⟩
		end do
		!$omp end target teams distribute parallel do

		!$omp target teams distribute reduction(+: tmpc) map(tmpc)
		do j = 1, n
			!$omp parallel do reduction(+: tmpc)
			do i = 1, m
				tmpc = (0.0_rk,0.0_rk)
				do k = 1, num_samples
					tmpc = tmpc + conjg(O_w(k,i,j))*e_loc(k)
				end do
				F_w(i,j) = tmpc*covar_norm                                                !! F(i,j) = ⟨Δ∂_{w_{ij}}^† ΔH⟩
			end do
			!$omp end parallel do
		end do
		!$omp end target teams distribute

		!$omp target teams distribute reduction(+: tmpr) map(tmpr)
		do jj = 1, n
			!$omp parallel do reduction(+: tmpr)
			do j = 1, n
				if (j < jj) cycle
				tmpr = 0.0_rk
				do k = 1, num_samples
					tmpr = tmpr + O_a(k,j)*O_a(k,jj)
				end do
				S_a(j,jj) = tmpr*covar_norm                                                               !! Covariance
				if (j == jj) S_a(j,jj) = S_a(j,jj) + delta                           !! Add regularization to diagonals
			end do
			!$omp end parallel do
		end do
		!$omp end target teams distribute

		!$omp target teams distribute reduction(+: tmpc) map(tmpc)
		do ii = 1, m
			!$omp parallel do reduction(+: tmpc)
			do i = 1, m
				if (i < ii) cycle
				tmpc = (0.0_rk,0.0_rk)
				do k = 1, num_samples
					tmpc = tmpc + conjg(O_b(k,i))*O_b(k,ii)
				end do
				S_b(i,ii) = tmpc*covar_norm                                                               !! Covariance
				if (i == ii) S_b(i,ii) = S_b(i,ii)%re + delta                        !! Add regularization to diagonals
			end do
			!$omp end parallel do
		end do
		!$omp end target teams distribute

		!$omp target teams distribute reduction(+: tmpc) map(tmpc)
		do j = 1, n
			!$omp parallel do reduction(+: tmpc) collapse(2)
			do ii = 1, m
				do i = 1, m
					if (i < ii) cycle
					tmpc = (0.0_rk,0.0_rk)
					do k = 1, num_samples
						tmpc = tmpc + conjg(O_w(k,i,j))*O_w(k,ii,j)
					end do
					S_w(i,ii,j) = tmpc*covar_norm                                                           !! Covariance
					if (i == ii) S_w(i,ii,j) = S_w(i,ii,j)%re + delta                  !! Add regularization to diagonals
				end do
			end do
			!$omp end parallel do
		end do
		!$omp end target teams distribute

		!$omp target update from(S_a, S_b, S_w, F_a, F_b, F_w)

		call posvx(A=S_a, b=F_a, x=x_a, uplo='L', fact='E')
		call posvx(A=S_b, b=F_b, x=x_b, uplo='L', fact='E')
		do j = 1, n
			call posvx(A=S_w(:,:,j), b=F_w(:,j), x=x_w(:,j), uplo='L', fact='E')
		end do

		tmpr = sqrt(1.0_rk - beta_2**epoch)/(1.0_rk - beta_1**epoch)

		self%p_a(:) = beta_1*self%p_a + (1.0_rk - beta_1)*x_a                            !! Biased first moment estimate
		self%r_a(:) = beta_2*self%r_a + (1.0_rk - beta_2)*(x_a**2)                  !! Biased second raw moment estimate
		x_a(:) = tmpr*self%p_a/(sqrt(self%r_a) + epsilon)                                                        !! ADAM
		self%a(:) = self%a - dtau*x_a                                                           !! Update visible biases

		self%p_b(:) = beta_1*self%p_b + (1.0_rk - beta_1)*x_b                            !! Biased first moment estimate
		self%r_b(:) = beta_2*self%r_b + (1.0_rk - beta_2)*(x_b**2)                  !! Biased second raw moment estimate
		x_b(:) = tmpr*self%p_b/(sqrt(self%r_b) + epsilon)                                                        !! ADAM
		self%b(:) = self%b - dtau*x_b                                                            !! Update hidden biases

		self%p_w(:,:) = beta_1*self%p_w + (1.0_rk - beta_1)*x_w                          !! Biased first moment estimate
		self%r_w(:,:) = beta_2*self%r_w + (1.0_rk - beta_2)*(x_w**2)                !! Biased second raw moment estimate
		x_w(:,:) = tmpr*self%p_w/(sqrt(self%r_w) + epsilon)                                                      !! ADAM
		self%w(:,:) = self%w - dtau*x_w                                                                !! Update weights
	end subroutine propagate

	impure recursive subroutine random_sample(n, s)
		!---------------------------------------------------------------------------------------------------------------
		!! Subroutine for generating a random sample
		!---------------------------------------------------------------------------------------------------------------
		integer,                   intent(in)    :: n
		integer(ik), dimension(n), intent(inout) :: s

		real    :: r
		integer :: j

		do j = 1, n
			call random_number(r)
			s(j) = nint(r, kind=ik)
		end do
	end subroutine random_sample

	pure recursive real(rk) function var(x) result(variance)
		!---------------------------------------------------------------------------------------------------------------
		!! Function for calculating sample variance of a real vector using canonical two-pass algorithm
		!---------------------------------------------------------------------------------------------------------------
		real(rk), contiguous, dimension(:), intent(in) :: x

		variance = sum( (x - sum(x)/size(x))**2 )/(size(x)-1)
	end function var

	pure recursive subroutine get_correlations(n, samples, corrs, alignment)
		!---------------------------------------------------------------------------------------------------------------
		!! Function for calculating spin-spin correlations of sampled configurations given alignment 'F' or 'A'
		!---------------------------------------------------------------------------------------------------------------
		integer,                                         intent(in)    :: n                           !! Number of spins
		integer(ik), contiguous, dimension(:,:), target, intent(inout) :: samples                       !! Input samples
		real(rk),    contiguous, dimension(:),           intent(out)   :: corrs                   !! Output correlations
		character(len=1),                                intent(in)    :: alignment                    !! Spin alignment

		integer(ik), pointer, dimension(:) :: ref_spin, current_spin                              !! Sample row pointers
		integer :: j, num_samples, agrees, disagrees                                  !! Loop, size, and count variables

		nullify(ref_spin); nullify(current_spin)                                                  !! Initialize pointers
		num_samples = size(samples, dim=1)                                                      !! Get number of samples
		ref_spin => samples(:,n/2+1)                                                        !! Set an odd reference spin
		corrs(n/2+1) = 1.0_rk                                      !! Reference spin is perfectly correlated with itself

		do j = 1, n; if (j == (n/2+1)) cycle
			current_spin => samples(:,j)                                                                    !! j-th spin

			if ( (alignment == 'A') .and. (mod(j,2) == 0) ) then                      !! Count agreements with reference
				agrees = count( (1_ik - current_spin) == ref_spin )
			else
				agrees = count( current_spin == ref_spin )
			end if

			disagrees = num_samples - agrees                                       !! Count disagreements with reference
			corrs(j) = real(agrees - disagrees, kind=rk)/num_samples                           !! Proportional agreement
		end do

		nullify(ref_spin); nullify(current_spin)                                                     !! Nullify pointers
	end subroutine get_correlations

	impure recursive real(rk) function gauss(mu, sig) result(gauss_res)
		!---------------------------------------------------------------------------------------------------------------
		!! Samples random numbers from the standard Normal (Gaussian) Distribution with the given mean and sigma.
		!! Uses the Acceptance-complement ratio from W. Hoermann and G. Derflinger.
		!! This is one of the fastest existing methods for generating normal random variables.
		!!
		!! REFERENCE:  - W. Hoermann and G. Derflinger (1990):
		!!               The ACR Method for generating normal random variables,
		!!               OR Spektrum 12 (1990), 181-185.
		!!
		!! Implementation taken from <https://root.cern.ch/doc/master/TRandom_8cxx_source.html#l00274>
		!! UNURAN (c) 2000  W. Hoermann & J. Leydold, Institut f. Statistik, WU Wien
		!---------------------------------------------------------------------------------------------------------------
		real(rk), intent(in) :: mu, sig

		real(rk) :: kC1, kC2, kC3, kD1, kD2, kD3, kHm, kZm, kHp, kZp, kPhln, kHm1
		real(rk) :: kHp1, kHzm, kHzmp, kAs, kBs, kCs, kB, kX0, kYm, kS, kT
		real(rk) :: rn, x, y, z, res

		kC1   = 1.448242853_rk
		kC2   = 3.307147487_rk
		kC3   = 1.46754004_rk
		kD1   = 1.036467755_rk
		kD2   = 5.295844968_rk
		kD3   = 3.631288474_rk
		kHm   = 0.483941449_rk
		kZm   = 0.107981933_rk
		kHp   = 4.132731354_rk
		kZp   = 18.52161694_rk
		kPhln = 0.4515827053_rk
		kHm1  = 0.516058551_rk
		kHp1  = 3.132731354_rk
		kHzm  = 0.375959516_rk
		kHzmp = 0.591923442_rk

		kAs = 0.8853395638_rk
		kBs = 0.2452635696_rk
		kCs = 0.2770276848_rk
		kB  = 0.5029324303_rk
		kX0 = 0.4571828819_rk
		kYm = 0.187308492_rk
		kS  = 0.7270572718_rk
		kT  = 0.03895759111_rk

		outer: do
			call random_number(y)

			if ( y > kHm1 ) then
				res = kHp*y - kHp1; exit outer
			else if ( y < kZm ) then
				rn = kZp*y - 1.0_rk

				if ( rn > 0.0_rk ) then
					res = 1.0_rk + rn; exit outer
				else
					res = -1.0_rk + rn; exit outer
				end if
			else if ( y < kHm ) then
				call random_number(rn)
				rn = rn - 1.0_rk + rn

				if ( rn > 0.0_rk ) then
					z = 2.0_rk - rn
				else
					z = -2.0_rk - rn
				end if

				if ( (kC1-y)*(kC3+abs(z)) < kC2 ) then
					res = z; exit outer
				else
					x = rn*rn
					if ( (y+kD1)*(kD3+x) < kD2 ) then
						res = rn; exit outer
					else if ( kHzmp-y < exp(-(z*z+kPhln)/2.0_rk) ) then
						res = z; exit outer
					else if ( y+kHzm < exp(-(x+kPhln)/2.0_rk) ) then
						res = rn; exit outer
					end if
				end if
			end if

			inner: do
				call random_number(x); call random_number(y)
				y = kYm*y
				z = kX0 - kS*x - y

				if ( z > 0.0_rk ) then
					rn = 2.0_rk + y/x
				else
					x = 1.0_rk - x
					y = kYm - y
					rn = -( 2.0_rk + y/x )
				end if

				if ( (y-kAs+x)*(kCs+x)+kBs < 0.0_rk ) then
					res = rn; exit inner
				else if ( y < x+kT ) then
					if ( rn*rn < 4.0_rk*(kB-log(x)) ) then
						res = rn; exit inner
					end if
				end if
			end do inner

			exit outer
		end do outer

		gauss_res = res*sig + mu
	end function gauss

end module nnqs
