module nnqs
	!------------------------------------------------------------------------------------------------------------------
	!! This module contains an implementation of the stochastic optimization algorithm for learning the ground state
	!! of the Ising spin model by representing the wave-functions 𝜓(s,α) as a type RestrictedBoltzmannMachine.
	!------------------------------------------------------------------------------------------------------------------
	use, intrinsic :: iso_fortran_env, only: rk=>real64, ik=>int8, int64, compiler_version, compiler_options
	use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
	use io_fortran_lib, only: echo, LF, str, to_file									!! I/O procedures and constants
	use lapack95, only: ppsvx								  !! Routine for solving linear systems with packed storage
	implicit none (type,external)													 !! No implicit types or interfaces
	private							   !! All objects in scope are inaccessible outside of scope unless declared public

	!! Public API list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	public :: RestrictedBoltzmannMachine

	!! Definitions and Interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	type RestrictedBoltzmannMachine
		private
		integer :: v_units = 0								                                  !! Number of visble units
		integer :: h_units = 0								                    	          !! Number of hidden units
		real(rk), allocatable, dimension(:) :: a, p_a, r_a						  !! Visible layer biases & ADAM arrays
		complex(rk), allocatable, dimension(:) :: b, p_b, r_b					   !! Hidden layer biases & ADAM arrays
		complex(rk), allocatable, dimension(:,:) :: w, p_w, r_w								   !! Weights & ADAM arrays
		character(len=1) :: alignment = 'N'												 !! For tracking spin alignment
		contains
			private
			procedure, pass(self), public :: stochastic_optimization						 !! Public training routine
			procedure, pass(self) :: init													  !! Initialization routine
			procedure, pass(self) :: sample_distribution							 !! MCMC routine for sampling |𝜓|^2
			procedure, pass(self) :: prob_ratio									 !! Probability ratio |𝜓(s_2)/𝜓(s_1)|^2
			procedure, pass(self) :: ising_energy												  !! Ising local energy
			procedure, pass(self) :: propagate								 !! Routine for updating weights and biases
	end type RestrictedBoltzmannMachine

	generic :: RestrictedBoltzmannMachine => new_rbm

	contains
	pure type(RestrictedBoltzmannMachine) function new_rbm(v_units, h_units) result(new)
		!--------------------------------------------------------------------------------------------------------------
		!! Function for constructing a RestrictedBoltzmannMachine
		!--------------------------------------------------------------------------------------------------------------
		integer, intent(in) :: v_units, h_units						!! Number of visible and hidden units to initialize
		new%v_units = v_units					   !! Set number of visible units (always equal to the number of spins)
		new%h_units = h_units					!! Set number of hidden units (chosen arbitrarily to optimize learning)
	end function new_rbm

	impure subroutine stochastic_optimization(self, ising_strengths)
		!--------------------------------------------------------------------------------------------------------------
		!! Public routine for training RBM
		!--------------------------------------------------------------------------------------------------------------
		class(RestrictedBoltzmannMachine), intent(inout) :: self								   !! Boltzmann machine
		real(rk), contiguous, dimension(:), intent(in) :: ising_strengths							!! Ising parameters

		real(rk), allocatable, dimension(:,:) :: energies, correlations					   !! Energies and correlations
		integer(ik), allocatable, dimension(:) :: start_sample							!! Start sample for Monte Carlo
		integer(ik), allocatable, dimension(:,:) :: samples										!! Sample storage array
		real(rk), allocatable, dimension(:) :: e_local, corrs					 !! Local energies, sample correlations
		real(rk) :: energy, sqerr, stderr, tau, acc												 !! Recording variables
		integer :: epoch, max_epochs, num_samples, n			 !! Loop variable, max epochs, number of samples, spins

		character(len=:), allocatable :: logfile, logmsg										 !! Recording variables
		character(len=10) :: date, time																   !! Date and time
		integer(int64) :: t1, t2																	 !! Clock variables
		real(rk) :: rate, wall_time																	 !! Clock variables

		if ( this_image() == 1 ) then									 !! Check validity of Ising strength parameters
			if ( size(ising_strengths) /= 2 ) then
				error stop	LF//'FATAL: Invalid size for ising_strengths... size must be (2).'// &
							LF//'USAGE: ising_strengths = [J,B] where J is the neighbor coupling strength and B '// &
								'is the transverse field strength.'//LF
			end if
			sync images (*)																	 !! Respond to other images
		else
			sync images (1)															  !! Wait for response from image 1
		end if

		call self%init()																!! Initialize Boltzmann machine

		if ( ising_strengths(1) < 0.0_rk ) then										 !! Check sign of coupling strength
			self%alignment = 'A'														 !! Anti-ferromagnetic if J < 0
		else
			self%alignment = 'F'															  !! Ferromagnetic if J > 0
		end if

		max_epochs = 1000																		  !! Set maximum epochs
		num_samples = 15															!! Set number of samples to produce
		n = self%v_units																		 !! Get number of spins
		start_sample = random_sample(n)														!! Initialize random sample
		allocate( samples(n, num_samples), e_local(num_samples), corrs(n) )						!! Allocate work arrays

		if ( this_image() == 1 ) then													   !! Do preparation on image 1
			allocate( energies(max_epochs, 2), correlations(n, max_epochs) )				 !! Allocate storage arrays

			logfile = 'optimization_results.log'														!! Set log file
			call date_and_time(date=date, time=time)											   !! Get date and time

			logmsg = 'Stochastic Optimization - date: '//trim(adjustl(date))//' | time: '//time	  !! Training log title
			call echo(logmsg//LF//repeat('-', ncopies=len(logmsg))//LF, file_name=logfile)				!! Echo to file

			call system_clock(t1)																		 !! Start timer
		end if

		learning: do epoch = 1, max_epochs															  !! Begin learning
			call co_sum(self%w); self%w = self%w/num_images()						   !! Average weights across images

			call self%sample_distribution( epoch=epoch, ising_strengths=ising_strengths, start_sample=start_sample, &
										   samples=samples, e_local=e_local, corrs=corrs, &			   !! Array outputs
										   energy=energy, sqerr=sqerr )								  !! Scalar outputs

			call co_sum(energy); energy = energy/num_images()							!! Average energy across images
			call co_sum(sqerr); stderr = sqrt(sqerr)/num_images()						 !! Average error across images
			call co_sum(corrs); corrs = corrs/num_images()						  !! Average correlations across images

			if ( this_image() == 1 ) then												!! Do data recording on image 1
				energies(epoch,:) = [energy, stderr]							  !! Record energy and error to storage
				correlations(:,epoch) = corrs										  !! Record correlations to storage

				!! Write progress report ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
				tau = (epoch-1)*merge(1.0_rk/n, 10.0_rk/n, mask=(n < 100))							  !! Imaginary time
				logmsg = '    Epoch '//str(epoch)//': E[ψ(α(τ='//str(tau, fmt='f', decimals=3)//'))] = '// &
							  str(energy, fmt='f', decimals=3)//' ± '//str(stderr, fmt='f', decimals=3)
				call echo(logmsg, logfile)

				if ( ieee_is_nan(energy) ) error stop LF//'FATAL: Numerical instability.'		   !! Error termination
				sync images (*)																 !! Respond to other images
			else
				sync images (1)														  !! Wait for response from image 1
			end if

			if ( all(abs(corrs) > 0.99_rk) .or. (epoch == max_epochs) ) exit learning				 !! Exit conditions

			call self%propagate(epoch=epoch, e_local=e_local, samples=samples)					   !! Update parameters
		end do learning

		if ( this_image() == 1 ) then											  !! Do finalization and I/O on image 1
			call system_clock(t2, count_rate=rate); wall_time = real(t2-t1, kind=rk)/rate			   !! Get time in s

			acc = 1.0_rk - real(count(samples == 0_ik), kind=rk)/size(samples)			   !! Get ground state accuracy

			logmsg =	LF//'    Optimization time: '//str(wall_time, fmt='f', decimals=3)//' seconds for n = '// & 
								str(n)//' spins.'// &
						LF//'    Ground state energy: E[ψ(α(τ → ∞))] = '//str(energy, fmt='f', decimals=3)// &
								' ± '//str(stderr, fmt='f', decimals=3)//' for J = '// &
								str(ising_strengths(1), fmt='f', decimals=1)//' and B = '// &
								str(ising_strengths(2), fmt='f', decimals=1)// &
						LF//'    Ground state accuracy: '//str(acc, fmt='f', decimals=6)//LF// &
						LF//'    This program was built and run with compiler "'//compiler_version()//'" '// &
								'using compiler options "'//compiler_options()//'".'//LF

			call echo(logmsg, logfile)
			call to_file(energies(:epoch,:), './data/energies_'//self%alignment//'.csv', header=['Energy', 'Error'] )
			call to_file(correlations(:,:epoch), './data/correlations_'//self%alignment//'.csv', header=['Epoch'] )
		end if
	end subroutine stochastic_optimization

	impure subroutine init(self)
		!--------------------------------------------------------------------------------------------------------------
		!! Procedure for initialization
		!--------------------------------------------------------------------------------------------------------------
		class(RestrictedBoltzmannMachine), intent(inout) :: self								   !! Boltzmann machine
		integer :: n, m, i, j																!! Sizes and loop variables

		n = self%v_units																		 !! Get number of spins
		m = self%h_units																  !! Get number of hidden units

		if ( this_image() == 1 ) then
			if ( (n < 1) .or. (m < 1) ) then
				error stop LF//'FATAL: Structure has not been declared or has invalid number of units.'
			end if
			sync images(*)																	 !! Respond to other images
		else
			sync images (1)															  !! Wait for response from image 1
		end if
		
		if ( allocated(self%a) ) deallocate(	self%a, self%p_a, self%r_a, &
												self%b, self%p_b, self%r_b, &
												self%w, self%p_w, self%r_w	)			  !! Reset components if needed

		allocate( self%a(n), self%p_a(n), self%r_a(n), source=0.0_rk )			  !! Allocate visible layer bias arrays
		allocate( self%b(m), self%p_b(m), self%r_b(m), source=(0.0_rk, 0.0_rk) )   !! Allocate hidden layer bias arrays
		allocate( self%w(m,n), self%p_w(m,n), self%r_w(m,n), source=(0.0_rk, 0.0_rk) )		  !! Allocate weight arrays

		do j = 1, n
			do i = 1, m
				self%w(i,j) = cmplx( gauss(mu=0.01_rk, sig=1e-4_rk), gauss(mu=-0.005_rk, sig=1e-5_rk), kind=rk )
			end do
		end do
	end subroutine init

	impure subroutine sample_distribution(self, epoch,ising_strengths,start_sample,samples,e_local,corrs,energy,sqerr)
		!--------------------------------------------------------------------------------------------------------------
		!! Markov Chain Monte Carlo procedure for sampling |𝜓|^2 with Metropolis-Hastings algorithm
		!--------------------------------------------------------------------------------------------------------------
		class(RestrictedBoltzmannMachine), intent(in) :: self							  !! Distribution to be sampled
		integer, intent(in) :: epoch																   !! Current epoch
		real(rk), contiguous, dimension(:), intent(in) :: ising_strengths							!! Ising parameters
		integer(ik), contiguous, dimension(:), intent(inout) :: start_sample		  !! Sample to begin thermalization
		integer(ik), contiguous, dimension(:,:), intent(out) :: samples								  !! Output samples
		real(rk), contiguous, dimension(:), intent(out) :: e_local, corrs		 !! Local energies, sample correlations
		real(rk), intent(out) :: energy, sqerr											!! Energy average, square error
		
		complex(rk), dimension(self%h_units) :: theta													  !! 𝜃 = b + ws
		integer :: n, passes, num_samples											!! Number of spins, passes, samples

		n = self%v_units																		 !! Get number of spins
		num_samples = size(samples, dim=2)												!! Number of samples to produce
		theta = conjg(self%b) + matmul(self%w, start_sample)										   !! Get initial 𝜃

		thermalization: block
			integer(ik), dimension(n) :: new_proposal, s_prop										!! Proposal samples
			real :: acc_prob, prob, r													!! M-H acceptance probabilities
			integer :: k, j, max_thermal_time														  !! Loop variables

			max_thermal_time = 2001 - 2*epoch									   !! Set time limit for thermalization

			thermalize: do k = 1, max_thermal_time
				s_prop = start_sample													 !! Transfer sample to proposal
				s_prop(1) = 1_ik - s_prop(1)													 !! Flip the first spin
				acc_prob = self%prob_ratio(s1=start_sample, s2=s_prop, theta_1=theta)		  !! Acceptance probability

				get_best_proposal: do j = 2, n					   !! Find proposal with largest acceptance probability
					new_proposal = s_prop														   !! Transfer proposal
					new_proposal(j) = 1_ik - new_proposal(j)										  !! Flip j-th spin
					prob = self%prob_ratio(s1=start_sample, s2=new_proposal, theta_1=theta)			 !! New probability
					if ( prob > acc_prob ) then											!! If new probability is better
						s_prop = new_proposal											 !! Update with better proposal
						acc_prob = prob										  !! Update with new acceptance probability
					end if
				end do get_best_proposal

				call random_number(r)									   !! Sample from uniform distribution on [0,1)
				if ( r < acc_prob ) then													!! M-H acceptance criterion
					start_sample = s_prop															   !! Update sample
					theta = conjg(self%b) + matmul(self%w, start_sample)									!! Update 𝜃
				else
					exit thermalization											  !! Sample is sufficiently thermalized
				end if
			end do thermalize
		end block thermalization

		passes = 2*n - min(2*epoch, 2*n-1)								!! Number of passes to make on the start sample

		stationary_sampling: block
			integer(ik), dimension(n) :: this_sample, s_prop								   !! Sample storage arrays
			integer, dimension(passes, num_samples) :: rind											  !! Random indices
			real, dimension(passes, num_samples) :: r												  !! Random numbers
			real :: acc_prob															  !! M-H acceptance probability
			integer :: k, pass																		  !! Loop variables

			call random_number(r)																!! Get randoms on [0,1)
			rind = floor(n*r) + 1													!! Generate random indices in [1,n]
			call random_number(r)																  !! Repopulate randoms

			do concurrent (k = 1:num_samples)
				this_sample = start_sample													   !! Transfer start sample

				metropolis_hastings: do pass = 1, passes
					s_prop = this_sample												 !! Transfer sample to proposal
					s_prop(rind(pass, k)) = 1_ik - s_prop(rind(pass, k))				   !! Flip spin at random index
					acc_prob = self%prob_ratio(s1=this_sample, s2=s_prop, theta_1=theta)	  !! Acceptance probability

					if ( r(pass, k) < acc_prob ) then										!! M-H acceptance criterion
						theta = theta + self%w(:,rind(pass,k))*(-2.0_rk*this_sample(rind(pass,k)) + 1.0_rk)	!! Update 𝜃
						this_sample = s_prop														   !! Update sample
					end if
				end do metropolis_hastings

				samples(:,k) = this_sample												   !! Transfer sample to output
				e_local(k) = self%ising_energy(s=this_sample, theta=theta, ising_strengths=ising_strengths)	!! E_loc(s)
			end do
		end block stationary_sampling

		corrs = corr(samples, alignment=self%alignment)							   !! Spin correlations given alignment
		energy = sum(e_local)/num_samples														 !! Average of energies
		sqerr = var(e_local)/num_samples													!! Square error of energies
		start_sample = samples(:,num_samples)				!! Record stationary sample to begin next round of sampling
	end subroutine sample_distribution

	pure real function prob_ratio(self, s1, s2, theta_1) result(p)
		!--------------------------------------------------------------------------------------------------------------
		!! Function for computing the ratio of probabilities |𝜓(s_2)/𝜓(s_1)|^2 for two given configurations
		!--------------------------------------------------------------------------------------------------------------
		class(RestrictedBoltzmannMachine), intent(in) :: self									   !! Boltzmann machine
		integer(ik), contiguous, dimension(:), intent(in) :: s1, s2									  !! Configurations
		complex(rk), contiguous, dimension(:), intent(in) :: theta_1						!! Cached value of b + ws_1
		
		complex(rk) :: sum_as, amplitude_prob_ratio										  !! ∑_j a_j*s_j, 𝜓(s_2)/𝜓(s_1)
		complex(rk), dimension(self%h_units) :: theta_2														!! b + ws_2
		integer(ik), dimension(self%v_units) :: s												   !! Sample difference
		integer, dimension(self%v_units) :: indices														 !! Index array
		integer, allocatable, dimension(:) :: contributors										 !! Contributor indices
		integer :: i, n																		 !! Loop and size variables

		n = self%v_units																		 !! Get number of spins

		s = s2-s1																  !! Difference of input configurations

		indices = merge(0, [(i, i=1,n)], mask=(s==0_ik))						 !! Indices {1,...,n} -> 0 where s == 0
		contributors = pack(indices, mask=(indices/=0))						  !! Pack nonzero indices into contributors

		theta_2 = theta_1 + matmul(self%w(:, contributors), s(contributors)) !! Matmul over vector subscript where s/=0

		sum_as = sum( self%a(contributors)*s(contributors) )											 !! ∑_j a_j s_j

		amplitude_prob_ratio = exp(sum_as + sum(log(1.0_rk+exp(theta_2)) - log(1.0_rk+exp(theta_1))))  !! 𝜓(s_2)/𝜓(s_1)

		p = real(conjg(amplitude_prob_ratio)*amplitude_prob_ratio)								   !! |𝜓(s_2)/𝜓(s_1)|^2
	end function prob_ratio

	pure real(rk) function ising_energy(self, s, theta, ising_strengths) result(energy)
		!--------------------------------------------------------------------------------------------------------------
		!! Function for calculating local energy of configuration s in Ising model
		!--------------------------------------------------------------------------------------------------------------
		class(RestrictedBoltzmannMachine), intent(in) :: self									   !! Boltzmann machine
		integer(ik), contiguous, dimension(:), intent(in) :: s									 !! Configuration input
		complex(rk), contiguous, dimension(:), intent(in) :: theta							  !! Cached value of b + ws
		real(rk), contiguous, dimension(:), intent(in) :: ising_strengths							!! Ising parameters
		
		real(rk), dimension(self%v_units) :: s_unit, field_couplings						!! s -> ±1, field couplings
		real(rk), dimension(self%v_units - 1) :: neighbor_couplings								  !! Neighbor couplings
		complex(rk), dimension(self%h_units) :: arg_theta												  !! 1 + exp(𝜃)
		real(rk) :: J_str, B_str, e_interaction, e_transverse						!! Ising model parameters, energies
		integer :: j, n																  !! Loop variable, number of spins

		n = self%v_units																		 !! Get number of spins
		J_str = abs(ising_strengths(1))														   !! Set coupling strength
		B_str = abs(ising_strengths(2))															  !! Set field strength

		s_unit = -2.0_rk*s + 1.0_rk															   !! Map {0,1} -> {1.,-1.}
		neighbor_couplings = s_unit(1:n-1)*s_unit(2:n)									  !! Nearest neighbor couplings

		arg_theta = 1.0_rk + exp(theta)													 !! 1 + exp(b + ws) for input s

		get_field_couplings: do concurrent (j = 1:n)
			field_couplings(j) = exp( self%a(j)*s_unit(j) + &								   !! 𝜓(s')/𝜓(s) for all s'
			sum(log(1.0_rk + exp(theta + self%w(:,j)*s_unit(j))) - log(arg_theta)) )   !! Forgets im part on assignment
		end do get_field_couplings

		e_interaction = -J_str*sum(neighbor_couplings)					   !! Local energy due to neighbor interactions
		e_transverse = -B_str*sum(field_couplings)								!! Local energy due to transverse field

		energy = e_interaction + e_transverse		!! Local energy is sum of interaction and transverse field energies
	end function ising_energy

	pure subroutine propagate(self, epoch, e_local, samples)
		!--------------------------------------------------------------------------------------------------------------
		!! Procedure for updating parameters according to stochastic optimization update rule
		!--------------------------------------------------------------------------------------------------------------
		class(RestrictedBoltzmannMachine), intent(inout) :: self								   !! Boltzmann machine
		integer, intent(in) :: epoch																   !! Current epoch
		real(rk), contiguous, dimension(:), intent(in) :: e_local									  !! Local energies
		integer(ik), contiguous, dimension(:,:), intent(in) :: samples								  !! Network inputs
		
		real(rk), allocatable, dimension(:) :: e_local_cent									 !! Centered local energies
		real(rk), allocatable, dimension(:,:) :: dlna												 !! Log derivatives
		complex(rk), allocatable, dimension(:,:) :: dlnb											 !! Log derivatives
		complex(rk), allocatable, dimension(:,:,:) :: dlnw											 !! Log derivatives
		real(rk) :: covar_norm, delta, beta_1, beta_2, epsilon, dtau		!! Normalization, regularization, ADAM vars
		integer :: n, m, num_samples, i, ii, j, jj, k, ind									 !! Size and loop variables

		n = self%v_units																		 !! Get number of spins
		m = self%h_units																  !! Get number of hidden units
		num_samples = size(samples, dim=2)													   !! Get number of samples
		covar_norm = 1.0_rk/(num_samples - 1)									 !! Set sample covariance normalization
		delta = 1e-5_rk																	!! Set regularization parameter
		beta_1 = 0.99_rk																 !! Decay rate for first moment
		beta_2 = 0.999_rk																!! Decay rate for second moment
		epsilon = 1e-8_rk													   !! Parameter to prevent division by zero

		if ( n < 100 ) then
			dtau = 1.0_rk/n																			   !! Set time step
		else
			dtau = 10.0_rk/n																		   !! Set time step
		end if

		e_local_cent = e_local - sum(e_local)/num_samples								   !! Center the local energies

		dlna = transpose(samples)							   !! O_a(k,j) = 𝜕/𝜕a_j ln 𝜓(s^k) = s_kj, k=1,…,K , j=1,…,n

		update_a: block !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			real(rk), dimension(num_samples, n) :: dlna_cent										   !! Centered data
			real(rk), dimension(n) :: forces, x											   !! Forces and solution array
			real(rk), dimension((n*(n+1))/2) :: sr_matrix												   !! SR matrix

			grad: do concurrent (j = 1:n)														  !! Generalized forces
				dlna_cent(:,j) = dlna(:,j) - sum(dlna(:,j))/num_samples			   !! Center each column about its mean
				forces(j) = sum(dlna_cent(:,j)*e_local_cent)*covar_norm						  !! F(j) = ⟨Δ∂_{a_j}^† ΔH⟩
			end do grad

			cov_mat: do concurrent (jj = 1:n, j = 1:n, j>=jj)
				ind = n*(jj-1) - ((jj-2)*(jj-1))/2 + (j-jj) + 1									!! Packed index mapping
				sr_matrix(ind) = sum(dlna_cent(:,j)*dlna_cent(:,jj))*covar_norm							  !! Covariance
				if (j == jj) sr_matrix(ind) = sr_matrix(ind) + delta				 !! Add regularization to diagonals
			end do cov_mat

			call ppsvx(AP=sr_matrix, b=forces, x=x, uplo='L', fact='E')		  !! Stochastic reconfiguration x = S^{-1}F

			self%p_a = beta_1*self%p_a + (1.0_rk - beta_1)*x							!! Biased first moment estimate
			self%r_a = beta_2*self%r_a + (1.0_rk - beta_2)*(x**2)				   !! Biased second raw moment estimate

			!! Modify gradient with bias-corrected moments:
			x = ( self%p_a/(1.0_rk - beta_1**epoch) )/sqrt( (self%r_a/(1.0_rk - beta_2**epoch)) + epsilon )

			self%a = self%a - dtau*x														   !! Update visible biases
		end block update_a !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		dlnb = matmul(self%w, samples)																	  !! 𝜃 - b = ws

		get_effective_angles: do concurrent (k = 1:num_samples)
			dlnb(:,k) = exp(conjg(self%b) + dlnb(:,k))							!! exp(𝜃) = exp(b + ws) for each sample
		end do get_effective_angles

		dlnb = transpose(dlnb/(1.0_rk + dlnb))						  !! O_b(k,i) = 𝜕/𝜕b_i ln 𝜓(s^k), k=1,…,K , i=1,…,m

		update_b: block !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			complex(rk), dimension(num_samples, m) :: dlnb_cent, dlnb_cent_conj						   !! Centered data
			complex(rk), dimension(m) :: forces, x										   !! Forces and solution array
			complex(rk), dimension((m*(m+1))/2) :: sr_matrix											   !! SR matrix

			grad: do concurrent (i = 1:m)														  !! Generalized forces
				dlnb_cent(:,i) = dlnb(:,i) - sum(dlnb(:,i))/num_samples			   !! Center each column about its mean
				dlnb_cent_conj(:,i) = conjg(dlnb_cent(:,i))						   !! Cache conjugates of centered data
				forces(i) = sum(dlnb_cent_conj(:,i)*e_local_cent)*covar_norm				  !! F(i) = ⟨Δ∂_{b_i}^† ΔH⟩
			end do grad

			cov_mat: do concurrent (ii = 1:m, i = 1:m, i>=ii)
				ind = m*(ii-1) - ((ii-2)*(ii-1))/2 + (i-ii) + 1									!! Packed index mapping
				sr_matrix(ind) = sum(dlnb_cent_conj(:,i)*dlnb_cent(:,ii))*covar_norm					  !! Covariance
				if (i == ii) sr_matrix(ind) = sr_matrix(ind)%re + delta				 !! Add regularization to diagonals
			end do cov_mat

			call ppsvx(AP=sr_matrix, b=forces, x=x, uplo='L', fact='E')		  !! Stochastic reconfiguration x = S^{-1}F

			self%p_b = beta_1*self%p_b + (1.0_rk - beta_1)*x							!! Biased first moment estimate
			self%r_b = beta_2*self%r_b + (1.0_rk - beta_2)*(x**2)				   !! Biased second raw moment estimate

			!! Modify gradient with bias-corrected moments:
			x = ( self%p_b/(1.0_rk - beta_1**epoch) )/sqrt( (self%r_b/(1.0_rk - beta_2**epoch)) + epsilon )

			self%b = self%b - dtau*x															!! Update hidden biases
		end block update_b !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		allocate( dlnw(num_samples, m, n) )												 !! k=1,…,K , i=1,…,m , j=1,…,n

		do concurrent (j = 1:n, i = 1:m)
			dlnw(:,i,j) = dlna(:,j)*dlnb(:,i)	 !! O_w(k,i,j) = 𝜕/𝜕w_ij ln 𝜓(s^k) = s_kj exp(𝜃(k,i))/(1 + exp(𝜃(k,i)))
		end do

		deallocate(dlna,dlnb)

		update_w: block !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			complex(rk), dimension(num_samples, m, n) :: dlnw_cent, dlnw_cent_conj					   !! Centered data
			complex(rk), dimension(m, n) :: forces, x									   !! Forces and solution array
			complex(rk), dimension((m*(m+1))/2, n) :: sr_matrix											   !! SR matrix

			grad: do concurrent (j = 1:n, i = 1:m)												  !! Generalized forces
				dlnw_cent(:,i,j) = dlnw(:,i,j) - sum(dlnw(:,i,j))/num_samples	   !! Center each column about its mean
				dlnw_cent_conj(:,i,j) = conjg(dlnw_cent(:,i,j))					   !! Cache conjugates of centered data
				forces(i,j) = sum(dlnw_cent_conj(:,i,j)*e_local_cent)*covar_norm		 !! F(i,j) = ⟨Δ∂_{w_{ij}}^† ΔH⟩
			end do grad

			cov_mat: do concurrent (j = 1:n, ii = 1:m, i = 1:m, i>=ii)
				ind = m*(ii-1) - ((ii-2)*(ii-1))/2 + (i-ii) + 1									!! Packed index mapping
				sr_matrix(ind,j) = sum(dlnw_cent_conj(:,i,j)*dlnw_cent(:,ii,j))*covar_norm				  !! Covariance
				if (i == ii) sr_matrix(ind,j) = sr_matrix(ind,j)%re + delta			 !! Add regularization to diagonals
			end do cov_mat

			stochastic_reconfiguration: do concurrent (j = 1:n)
				call ppsvx(AP=sr_matrix(:,j), b=forces(:,j), x=x(:,j), uplo='L', fact='E')	!! Sto reconfig x = S^{-1}F
			end do stochastic_reconfiguration

			self%p_w = beta_1*self%p_w + (1.0_rk - beta_1)*x							!! Biased first moment estimate
			self%r_w = beta_2*self%r_w + (1.0_rk - beta_2)*(x**2)				   !! Biased second raw moment estimate

			!! Modify gradient with bias-corrected moments:
			x = ( self%p_w/(1.0_rk - beta_1**epoch) )/sqrt( (self%r_w/(1.0_rk - beta_2**epoch)) + epsilon )

			self%w = self%w - dtau*x																  !! Update weights
		end block update_w !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	end subroutine propagate

	impure function random_sample(n) result(s_rand)
		!--------------------------------------------------------------------------------------------------------------
		!! Function for generating a random spin configuration
		!--------------------------------------------------------------------------------------------------------------
		integer, intent(in) :: n												 !! Length of configuration to generate
		integer(ik), dimension(n) :: s_rand														  !! Sample to generate
		
		real, dimension(n) :: r																	 !! Random number array

		call random_number(r)							  !! Generate random numbers from uniform distribution on [0,1)
		s_rand = nint(r, kind=ik)													!! Quantize to nearest whole number
	end function random_sample

	pure real(rk) function var(x) result(variance)
		!--------------------------------------------------------------------------------------------------------------
		!! Function for calculating sample variance of a real vector using canonical two-pass algorithm
		!--------------------------------------------------------------------------------------------------------------
		real(rk), contiguous, dimension(:), intent(in) :: x
		
		real(rk), dimension(size(x)) :: x_cent

		x_cent = x - sum(x)/size(x)
		variance = sum(x_cent**2)/(size(x)-1)
	end function var

	pure function corr(samples, alignment) result(correlations)
		!--------------------------------------------------------------------------------------------------------------
		!! Function for calculating spin-spin correlations of sampled configurations given alignment 'F' or 'A'
		!--------------------------------------------------------------------------------------------------------------
		integer(ik), contiguous, dimension(:,:), intent(in) :: samples								   !! Input samples
		character(len=1), intent(in) :: alignment										   !! Spin alignment 'F' or 'A'
		real(rk), allocatable, dimension(:) :: correlations										 !! Output correlations
		
		integer(ik), allocatable, dimension(:,:) :: S											   !! Transformed spins
		integer(ik), allocatable, dimension(:) :: ref_spin											  !! Reference spin
		integer :: n, num_samples, j, agrees, disagrees					!! Size and loop variables, agreement variables

		n = size(samples, dim=1)																 !! Get number of spins
		num_samples = size(samples, dim=2)													   !! Get number of samples
		S = transpose(samples)																  !! Lay spins down columns
		ref_spin = S(:,n/2+1)															!! Set middle spin as reference

		allocate( correlations(n), source=1.0_rk )

		get_correlations: do concurrent (j = 1:n, j /= n/2+1)
			if ( alignment == 'A' ) then
				if ( mod(j,2) == 0 ) then
					S(:,j) = 1_ik - S(:,j)											  !! Conform spins to 'A' alignment
				end if
			end if
			agrees = count( S(:,j) == ref_spin )											!! Get number of agreements
			disagrees = num_samples - agrees											 !! Get number of disagreements
			correlations(j) = real(agrees - disagrees, kind=rk)/num_samples					!! Mean agreement on [-1,1]
		end do get_correlations
	end function corr

	impure real(rk) function gauss(mu, sig) result(gauss_res)
		!!-------------------------------------------------------------------------------------------------------------
		!! Samples random numbers from the standard Normal (Gaussian) Distribution with the given mean and sigma.
		!! Uses the Acceptance-complement ratio from W. Hoermann and G. Derflinger.
		!! This is one of the fastest existing methods for generating normal random variables.
		!!
		!! REFERENCE:  - W. Hoermann and G. Derflinger (1990):
		!!              The ACR Method for generating normal random variables,
		!!              OR Spektrum 12 (1990), 181-185.
		!!
		!! Implementation taken from <https://root.cern.ch/doc/master/TRandom_8cxx_source.html#l00274>
		!! UNURAN (c) 2000  W. Hoermann & J. Leydold, Institut f. Statistik, WU Wien
		!!-------------------------------------------------------------------------------------------------------------
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
