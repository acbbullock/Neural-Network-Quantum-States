module ising_ml
    use, intrinsic :: iso_fortran_env, only: rk=>real64, ik=>int8                              !! Import standard kinds
	implicit none (type,external)                                                    !! No implicit types or interfaces
	private                            !! All objects in scope are inaccessible outside of scope unless declared public

	!! Public APIs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	public :: RestrictedBoltzmannMachine

	!! Class Definitions and Interfaces ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	type RestrictedBoltzmannMachine                     !! Custom class for implementing a Restricted Boltzmann Machine
		private
		integer, allocatable :: v_units, h_units                                             !! Number of layer neurons
		complex(rk), allocatable, dimension(:) :: a, b                                                !! Network biases
        complex(rk), allocatable, dimension(:,:) :: w                                                !! Network weights
		complex(rk), allocatable, dimension(:) :: p_a, r_a, p_b, r_b                                     !! ADAM arrays
		complex(rk), allocatable, dimension(:,:) :: p_w, r_w                                             !! ADAM arrays
		contains                                                                     !! Type-bound procedures (methods)
			private
			procedure, pass, public :: train                                !! Procedure for training Boltzmann machine
            procedure, pass :: init                                     !! Procedure for initializing Boltzmann machine
			procedure, pass :: metropolis_hastings             !! Markov Chain Monte Carlo procedure for sampling |𝜓|^2
            procedure, pass :: sq_psi_ratio                   !! Computes |𝜓(s_2)/𝜓(s_1)|^2 for configurations s_1, s_2
            procedure, pass :: ising_energy                    !! Computes Ising local energy for given configuration s
            procedure, pass :: gradient_descent                            !! Procedure for updating weights and biases
	end type RestrictedBoltzmannMachine

	interface RestrictedBoltzmannMachine                                         !! Interface for structure constructor
		procedure :: new
	end interface

    interface cas_sum                                                          !! Interface for cascading sum functions
        procedure :: cas_sum_real, cas_sum_complex
    end interface

	contains  !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    !! Initialization Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	pure type(RestrictedBoltzmannMachine) function new(v_units, h_units) result(self)
		!! Function for constructing RestrictedBoltzmannMachine
		integer, intent(in) :: v_units, h_units                     !! Number of visible and hidden units to initialize

		self%v_units = v_units                     !! Set number of visible units (always equal to the number of spins)
		self%h_units = h_units                  !! Set number of hidden units (chosen arbitrarily to optimize learning)
	end function new

    impure subroutine init(self)
		!! Procedure for Xavier Initialization
		class(RestrictedBoltzmannMachine), intent(inout) :: self                                   !! Boltzmann machine

		real(rk), allocatable, dimension(:,:,:) :: w                              !! Local variable for setting weights
		integer :: n, m                                                                     !! Visible and hidden units

		if ( this_image() == 1 ) then
			if (.not. allocated(self%v_units)) error stop 'Structure not declared... terminating'  !! Error termination
			sync images(*)                                                                   !! Respond to other images
		end if

		if ( this_image() /= 1 ) sync images (1)                                      !! Wait for response from image 1
		
		if ( allocated(self%a) ) deallocate( self%p_a, self%r_a, self%p_b, self%r_b, self%p_w, self%r_w, &
                                             self%a, self%b, self%w )                     !! Reset components if needed

		n = self%v_units                                                                         !! Get number of spins
		m = self%h_units                                                                  !! Get number of hidden units

		allocate( self%p_a(n), self%r_a(n), self%p_b(m), self%r_b(m), self%p_w(m,n), self%r_w(m,n), &    !! ADAM arrays
                  self%a(n), self%b(m), self%w(m,n), source=(0.0_rk, 0.0_rk) )                    !! Biases and weights

		self%w%re = gauss_matrix(dims=shape(self%w), mu=0.0_rk, sig=sqrt(1.0_rk/n))                        !! Real part
		self%w%im = gauss_matrix(dims=shape(self%w), mu=0.0_rk, sig=sqrt(1.0_rk/n))                   !! Imaginary part
	end subroutine init

	!! Ising Model Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    pure complex(rk) function ising_energy(self, s, params) result(energy)
		!! Function for calculating local energy of configuration s in Ising model
		class(RestrictedBoltzmannMachine), intent(in) :: self                                      !! Boltzmann machine
		integer(ik), contiguous, dimension(:), intent(in) :: s                                   !! Configuration input
        real(rk), dimension(2), intent(in) :: params                  !! Specifies coupling strength and field strength

        real(rk), allocatable, dimension(:) :: s_unit, neighbor_couplings                !! s -> ±1, neighbor couplings
		complex(rk), allocatable, dimension(:) :: b_ws, sech_b_ws                               !! b + ws, sech(b + ws)
		complex(rk), allocatable, dimension(:) :: field_couplings                                    !! Field couplings
		complex(rk), allocatable :: e_interaction, e_transverse          !! Interaction energy, transverse field energy
        real(rk), allocatable :: J_str, B_str                       !! Ising model coupling strength and field strength
		integer :: j, n, m                                                            !! Loop variable, number of spins

		n = self%v_units                                                                         !! Get number of spins
		m = self%h_units                                                                  !! Get number of hidden units
        J_str = params(1)                                                                      !! Set coupling strength
        B_str = params(2)                                                                         !! Set field strength

        s_unit = -2.0_rk*s + 1.0_rk                                                            !! Map {0,1} -> {1.,-1.}
        neighbor_couplings = s_unit(1:n-1)*s_unit(2:n)                                    !! Nearest neighbor couplings

		b_ws = self%b + matmul(self%w, s)                                                         !! b + ws for input s
        sech_b_ws = sech(b_ws)                                                            !! f^{-1} = cosh^{-1}(b + ws)

        allocate( field_couplings(n) )                                                          !! Spin-field couplings

		get_field_couplings: do concurrent (j = 1:n)
			field_couplings(j) = exp( conjg(self%a(j))*s_unit(j) + &                           !! 𝜓(s')/𝜓(s) for all s'
            cas_sum(log(cosh(b_ws + self%w(:,j)*s_unit(j))*sech_b_ws)) )            !! ∑ ln(cosh(b + ws')*sech(b + ws))
		end do get_field_couplings

		e_interaction = J_str*cas_sum(neighbor_couplings)                  !! Local energy due to neighbor interactions
		e_transverse = B_str*cas_sum(field_couplings)                           !! Local energy due to transverse field

		energy = e_interaction + e_transverse       !! Local energy is sum of interaction and transverse field energies
	end function ising_energy

	!!  Metropolis-Hastings Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	impure subroutine metropolis_hastings(self, num_samples, ising_parameters, markov_chain, e_local, energy, sqerr)
		!! Procedure for generating Markov Chain of samples from distribution |𝜓|^2
		class(RestrictedBoltzmannMachine), intent(in) :: self                             !! Distribution to be sampled
		integer, intent(in) :: num_samples                                              !! Number of samples to produce
        real(rk), dimension(2), intent(in) :: ising_parameters        !! Specifies coupling strength and field strength
		integer(ik), allocatable, dimension(:,:), intent(out) :: markov_chain                !! Output chain of samples
		complex(rk), allocatable, dimension(:), intent(out) :: e_local                         !! Output local energies
		real(rk), allocatable, intent(out) :: energy, sqerr                             !! Energy average, square error

		integer(ik), allocatable, dimension(:) :: start_sample               !! Start sample of stationary distribution
        complex(rk), allocatable, dimension(:) :: theta                                                       !! b + ws
		integer :: n                                                                                 !! Number of spins

		n = self%v_units                                                                         !! Get number of spins

		start_sample = random_sample(n)                                                     !! Initialize random sample
        theta = self%b + matmul(self%w, start_sample)                                              !! Get initial theta

		thermalization: block
			integer(ik), allocatable, dimension(:) :: new_proposal, s_prop                          !! Proposal samples
			real, allocatable, dimension(:) :: rands                                                  !! Random numbers
			real :: acc_prob, prob                                                      !! M-H acceptance probabilities
			integer :: k, j, thermal_length                                                           !! Loop variables

			thermal_length = 25 + n/100                                         !! Number of samples for thermalization

			allocate( rands(thermal_length) )
			call random_number(rands)                                                           !! Get randoms on [0,1)

			thermalize: do k = 1, thermal_length
				s_prop = start_sample                                                    !! Transfer sample to proposal
				s_prop(1) = 1_ik - s_prop(1)                                                     !! Flip the first spin
                call self%sq_psi_ratio(s1=start_sample, s2=s_prop, theta=theta, prob_ratio=acc_prob)
				get_best_proposal: do j = 2, n                     !! Find proposal with largest acceptance probability
					new_proposal = s_prop                                                          !! Transfer proposal
					new_proposal(j) = 1_ik - new_proposal(j)                                          !! Flip j-th spin
                    call self%sq_psi_ratio(s1=start_sample, s2=new_proposal, theta=theta, prob_ratio=prob)
					if ( prob > acc_prob ) then                                         !! If new probability is better
						s_prop = new_proposal                                            !! Update with better proposal
						acc_prob = prob                                       !! Update with new acceptance probability
					end if
				end do get_best_proposal
				if ( rands(k) < acc_prob ) then
                    start_sample = s_prop                                   !! M-H acceptance criterion - update sample
                    theta = self%b + matmul(self%w, start_sample)                                       !! Update theta
                end if
			end do thermalize
		end block thermalization

		allocate( markov_chain(n, num_samples), e_local(num_samples) )                        !! Allocate output arrays

		stationary_sampling: block
			integer(ik), allocatable, dimension(:) :: this_sample, s_prop                      !! Sample storage arrays
			integer, allocatable, dimension(:,:) :: rand_index                                        !! Random indices
			real, allocatable, dimension(:,:) :: rands                                                !! Random numbers
			real :: acc_prob                                                              !! M-H acceptance probability
			integer :: k, pass, passes                                                                !! Loop variables

			passes = max(4, 2*n/5)                                      !! Number of passes to make on the start sample

			allocate( rands(passes, num_samples) )

			call random_number(rands)                                                           !! Get randoms on [0,1)
			rand_index = floor(n*rands) + 1                                         !! Generate random indices in [1,n]
			call random_number(rands)                                                             !! Repopulate randoms

			do concurrent (k = 1:num_samples) local(theta, this_sample, s_prop, acc_prob) shared(markov_chain, e_local)
				this_sample = start_sample                                                     !! Transfer start sample
				do pass = 1, passes
					s_prop = this_sample                                                 !! Transfer sample to proposal
					s_prop(rand_index(pass, k)) = 1_ik - s_prop(rand_index(pass, k))       !! Flip spin at random index
                    call self%sq_psi_ratio(s1=this_sample, s2=s_prop, theta=theta, prob_ratio=acc_prob)
					if ( rands(pass, k) < acc_prob ) then                            !! Roll to update sample and theta
                        theta = theta + self%w(:,rand_index(pass,k))*(-2.0_rk*this_sample(rand_index(pass,k)) + 1.0_rk)
                        this_sample = s_prop                                !! M-H acceptance criterion - update sample
                    end if
				end do
				markov_chain(:,k) = this_sample                                         !! Final sample to output chain
				e_local(k) = self%ising_energy(s=this_sample, params=ising_parameters)        !! Local energy to output
			end do
		end block stationary_sampling

		energy = sum(e_local%re)/num_samples                                               !! Average of final energies
		sqerr = var(e_local)/num_samples                                                    !! Square error of energies
	end subroutine metropolis_hastings

    pure subroutine sq_psi_ratio(self, s1, s2, theta, prob_ratio)
		!! Function for computing the ratio of probabilities |𝜓(s_2)/𝜓(s_1)|^2 for two given configurations
		class(RestrictedBoltzmannMachine), intent(in) :: self                                      !! Boltzmann machine
		integer(ik), contiguous, dimension(:), intent(in) :: s1, s2                                   !! Configurations
        complex(rk), contiguous, dimension(:), intent(in) :: theta                          !! Cached value of b + ws_1
        real, intent(out) :: prob_ratio                                                       !! Ratio of probabilities

		complex(rk), allocatable :: sum_as, amplitude_prob_ratio                          !! ∑_j a_j*s_j, 𝜓(s_2)/𝜓(s_1)
		complex(rk), allocatable, dimension(:) :: thetap                                                     !! b + ws'
		integer(ik), allocatable, dimension(:) :: s                                                !! Sample difference
		integer, allocatable, dimension(:) :: indices, contributors                              !! For sorting indices
		integer :: j, n, m                                                                   !! Loop and size variables

		n = self%v_units                                                                         !! Get number of spins
		m = self%h_units                                                                  !! Get number of hidden units

		s = s2-s1                                                                 !! Difference of input configurations

		indices = merge(0, [(j, j=1,n)], mask=(s==0_ik))                         !! Indices {1,...,n} -> 0 where s == 0
		contributors = pack(indices, mask=(indices/=0))                       !! Pack nonzero indices into contributors
        deallocate(indices)

        thetap = theta + matmul(self%w(:, contributors), s(contributors))    !! Matmul over vector subscript where s/=0

        sum_as = sum( conjg(self%a(contributors))*s(contributors) )                                      !! ∑_j a_j*s_j

		amplitude_prob_ratio = exp(sum_as + sum(log(cosh(thetap)*sech(theta))))                        !! 𝜓(s_2)/𝜓(s_1)
		prob_ratio = real(conjg(amplitude_prob_ratio)*amplitude_prob_ratio)                        !! |𝜓(s_2)/𝜓(s_1)|^2
	end subroutine sq_psi_ratio

	impure function random_sample(n) result(s_random)
		!! Function for generating a random spin configuration
		integer, intent(in) :: n                                                 !! Length of configuration to generate
		integer(ik), allocatable :: s_random(:)                                                     !! Generated sample

		real, allocatable :: r(:)                                                                !! Random number array

		allocate( r(n) )                                                                            !! Allocate randoms
		call random_number(r)                             !! Generate random numbers from uniform distribution on [0,1)
		s_random = nint(r, kind=ik)                                                 !! Quantize to nearest whole number
	end function random_sample

	!! Training Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	impure subroutine train(self, sample_size, ising_parameters, energies, correlations)
		!! Procedure for training Boltzmann machine
		use, intrinsic :: ieee_arithmetic, only: ieee_is_nan                                   !! IEEE inquiry function
		class(RestrictedBoltzmannMachine), intent(inout) :: self                                   !! Boltzmann machine
		integer, intent(in) :: sample_size                                                               !! Sample size
        real(rk), dimension(2), intent(in) :: ising_parameters        !! Specifies coupling strength and field strength
		real(rk), allocatable, dimension(:,:), intent(out) :: energies, correlations       !! Energies and correlations

        integer(ik), allocatable, dimension(:,:) :: chain                                 !! Markov chain storage array
		complex(rk), allocatable, dimension(:) :: e_local                                 !! Local energy storage array
		real(rk), allocatable, dimension(:) :: cor                                   !! Spin correlations storage array
		real(rk), allocatable :: energy, sqerr, stderr                                           !! Recording variables
		integer :: epoch, max_epochs                                                       !! Loop variable, max epochs

		call self%init()                                                                !! Initialize Boltzmann machine

        max_epochs = 1000                                                                         !! Set maximum epochs

		allocate( energies(max_epochs, 2), correlations(self%v_units, max_epochs) )          !! Allocate storage arrays

		learning: do epoch = 1, max_epochs                                                            !! Begin learning
            call co_sum(self%w); self%w = self%w/num_images()                          !! Average weights across images
			call self%metropolis_hastings(  num_samples=sample_size, ising_parameters=ising_parameters, &     !! Inputs
                                            markov_chain=chain, e_local=e_local, energy=energy, sqerr=sqerr ) !! Output

			cor = corr(chain)                                                            !! Get local spin correlations
			call co_sum(energy); energy = energy/num_images()                           !! Average energy across images
			call co_sum(sqerr); stderr = sqrt(sqerr)/num_images()                       !! Standard error across images
			call co_sum(cor); cor = cor/num_images()                  !! Average correlations elementally across images

			energies(epoch,:) = [energy, stderr]                                             !! Record energy and error
			correlations(:,epoch) = cor                                                          !! Record correlations

            if ( this_image() == 1 ) then
                print*, 'E = ', energy, 'pm', stderr, 'on epoch', epoch                               !! Print progress
				if ( ieee_is_nan(energy) ) error stop 'Numerical instability... terminating'       !! Error termination
				sync images (*)                                                              !! Respond to other images
			end if

			if ( this_image() /= 1 ) sync images (1)                                  !! Wait for response from image 1

            if ( all(abs(cor) > 0.98_rk) .or. (epoch == max_epochs) ) exit learning                   !! Exit condition

			call self%gradient_descent(markov_chain=chain, e_local=e_local, epoch=epoch)              !! Update network
		end do learning

        if ( this_image() == 1 ) then
            energies = energies(1:epoch,:)                                         !! Dynamic reallocation - truncation
            correlations = correlations(:,1:epoch)                                 !! Dynamic reallocation - truncation
        end if
	end subroutine train

	pure subroutine gradient_descent(self, markov_chain, e_local, epoch)
        !! Procedure for updating weights and biases
		use lapack95, only: ppsvx                             !! Routine for solving linear systems with packed storage
		class(RestrictedBoltzmannMachine), intent(inout) :: self                                   !! Boltzmann machine
		integer(ik), contiguous, dimension(:,:), intent(in) :: markov_chain                  !! Markov chain of samples
		complex(rk), contiguous, dimension(:), intent(in) :: e_local                                  !! Local energies
        integer, intent(in) :: epoch                                                                   !! Current epoch

        real(rk), allocatable, dimension(:,:) :: dlna                                                !! Log derivatives
        complex(rk), allocatable, dimension(:,:) :: dlnb                                             !! Log derivatives
		complex(rk), allocatable, dimension(:,:,:) :: dlnw                                           !! Log derivatives
        complex(rk), allocatable, dimension(:) :: e_local_cent                               !! Centered local energies
		real(rk) :: delta, denom                                     !! Regularization, covariance normalization factor
		integer :: n, m, num_samples, i, ii, j, jj, k, ind                                   !! Size and loop variables

		n = self%v_units                                                                         !! Get number of spins
		m = self%h_units                                                                  !! Get number of hidden units
		num_samples = size(markov_chain, dim=2)                                                !! Get number of samples
		delta = 0.1_rk                                                                  !! Set regularization parameter
        denom = 1.0_rk/(num_samples - 1)                                         !! Set sample covariance normalization

        e_local_cent = (e_local - cas_sum(e_local)/num_samples)                            !! Center the local energies

		dlna = transpose(markov_chain)                        !! O_a(k,j) = 𝜕/𝜕a_j ln[𝜓(s_k)] = s_jk, k=1,…,K , j=1,…,n

		update_a: block !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            real(rk), allocatable, dimension(:,:) :: dlna_cent                                         !! Centered data
			complex(rk), allocatable, dimension(:) :: forces, sr_matrix, x                      !! Linear system arrays

            allocate( dlna_cent(num_samples, n), forces(n) )         !! Allocate centered data storage, gradient vector

            grad: do concurrent (j = 1:n)                                      !! Generalized forces (gradient of E[𝜓])
                dlna_cent(:,j) = dlna(:,j) - cas_sum(dlna(:,j))/num_samples        !! Center each column about its mean
                forces(j) = cas_sum(dlna_cent(:,j)*e_local_cent)*denom                            !! F(j) = 𝜕/𝜕a_j E[𝜓]
            end do grad

            allocate( sr_matrix((n*(n+1))/2) )                            !! Allocate stochastic reconfiguration matrix

            cov_mat: do concurrent (jj = 1:n, j = 1:n, j>=jj) local(ind) shared(sr_matrix)
                ind = n*(jj-1) - ((jj-2)*(jj-1))/2 + (j-jj) + 1                                 !! Packed index mapping
                sr_matrix(ind) = cas_sum(dlna_cent(:,j)*dlna_cent(:,jj))*denom                   !! Two-pass covariance
                if (j == jj) sr_matrix(ind) = sr_matrix(ind)%re + delta              !! Add regularization to diagonals
            end do cov_mat

			allocate( x, mold=forces )                                                            !! Allocate solutions

			call ppsvx(AP=sr_matrix, b=forces, x=x, uplo='L', fact='E')                !! Solves Sx = F for x = S^{-1}F

            call ADAM_a(self, grad=x, epoch=epoch)                                                    !! ADAM optimizer

			self%a = self%a - x                                                !! Use gradient descent to update biases
		end block update_a !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		dlnb = matmul(self%w, markov_chain)                                                               !! 𝜃 - b = ws

		get_effective_angles: do concurrent (k = 1:num_samples)
            dlnb(:,k) = self%b + dlnb(:,k)                                                !! 𝜃 = b + ws for each sample
		end do get_effective_angles

		dlnb = transpose(tanh(dlnb))                  !! O_b(k,i) = 𝜕/𝜕b_i ln[𝜓(s_k)] = tanh(𝜃(k,i)), k=1,…,K , i=1,…,m

		update_b: block !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            complex(rk), allocatable, dimension(:,:) :: dlnb_cent, dlnb_cent_conj                      !! Centered data
			complex(rk), allocatable, dimension(:) :: forces, sr_matrix, x                      !! Linear system arrays

            allocate( dlnb_cent(num_samples, m), dlnb_cent_conj(num_samples, m), forces(m) ) !! Centered data, gradient

            grad: do concurrent (i = 1:m)                                      !! Generalized forces (gradient of E[𝜓])
                dlnb_cent(:,i) = dlnb(:,i) - cas_sum(dlnb(:,i))/num_samples        !! Center each column about its mean
                dlnb_cent_conj(:,i) = conjg(dlnb_cent(:,i))                        !! Cache conjugates of centered data
                forces(i) = cas_sum(dlnb_cent_conj(:,i)*e_local_cent)*denom                       !! F(i) = 𝜕/𝜕b_i E[𝜓]
            end do grad

            allocate( sr_matrix((m*(m+1))/2) )                            !! Allocate stochastic reconfiguration matrix

            cov_mat: do concurrent (ii = 1:m, i = 1:m, i>=ii) local(ind) shared(sr_matrix)
                ind = m*(ii-1) - ((ii-2)*(ii-1))/2 + (i-ii) + 1                                 !! Packed index mapping
                sr_matrix(ind) = cas_sum(dlnb_cent_conj(:,i)*dlnb_cent(:,ii))*denom              !! Two-pass covariance
                if (i == ii) sr_matrix(ind) = sr_matrix(ind)%re + delta              !! Add regularization to diagonals
            end do cov_mat

			allocate( x, mold=forces )                                                            !! Allocate solutions

			call ppsvx(AP=sr_matrix, b=forces, x=x, uplo='L', fact='E')                !! Solves Sx = F for x = S^{-1}F

            call ADAM_b(self, grad=x, epoch=epoch)                                                    !! ADAM optimizer

			self%b = self%b - x                                                !! Use gradient descent to update biases
		end block update_b !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		allocate( dlnw(num_samples, m, n) )                                              !! k=1,…,K , i=1,…,m , j=1,…,n

		do concurrent (j = 1:n, i = 1:m)
			dlnw(:,i,j) = dlna(:,j)*dlnb(:,i)                    !! O_w(k,i,j) = 𝜕/𝜕w_ij ln[𝜓(s_k)] = s_jk tanh(𝜃(k,i))
		end do

		deallocate(dlna, dlnb)                                                                     !! Free local memory

		update_w: block !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            complex(rk), allocatable, dimension(:,:,:) :: dlnw_cent, dlnw_cent_conj                    !! Centered data
			complex(rk), allocatable, dimension(:,:) :: forces, sr_matrix, x                    !! Linear system arrays

            allocate( dlnw_cent(num_samples, m, n), dlnw_cent_conj(num_samples, m, n), forces(m, n) )

            grad: do concurrent (j = 1:n, i = 1:m)                             !! Generalized forces (gradient of E[𝜓])
                dlnw_cent(:,i,j) = dlnw(:,i,j) - cas_sum(dlnw(:,i,j))/num_samples  !! Center each column about its mean
                dlnw_cent_conj(:,i,j) = conjg(dlnw_cent(:,i,j))                    !! Cache conjugates of centered data
                forces(i,j) = cas_sum(dlnw_cent_conj(:,i,j)*e_local_cent)*denom                !! F(i,j) = 𝜕/𝜕w_ij E[𝜓]
            end do grad

            allocate( sr_matrix((m*(m+1))/2, n) )                         !! Allocate stochastic reconfiguration matrix

            cov_mat: do concurrent (j = 1:n, ii = 1:m, i = 1:m, i>=ii) local(ind) shared(sr_matrix)
                ind = m*(ii-1) - ((ii-2)*(ii-1))/2 + (i-ii) + 1                                 !! Packed index mapping
                sr_matrix(ind,j) = cas_sum(dlnw_cent_conj(:,i,j)*dlnw_cent(:,ii,j))*denom        !! Two-pass covariance
                if (i == ii) sr_matrix(ind,j) = sr_matrix(ind,j)%re + delta          !! Add regularization to diagonals
            end do cov_mat

            allocate( x, mold=forces )                                                            !! Allocate solutions

			stochastic_reconfiguration: do concurrent (j = 1:n) shared(forces, sr_matrix, x)
				call ppsvx(AP=sr_matrix(:,j), b=forces(:,j), x=x(:,j), uplo='L', fact='E')         !! x_j = S_j^{-1}F_j
			end do stochastic_reconfiguration

            call ADAM_w(self, grad=x, epoch=epoch)                                                    !! ADAM optimizer

			self%w = self%w - x                                               !! Use gradient descent to update weights
		end block update_w !! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	end subroutine gradient_descent

    pure subroutine ADAM_a(self, grad, epoch)
        !! Procedure for optimizing gradient with adaptive moment estimator
        class(RestrictedBoltzmannMachine), intent(inout) :: self                                   !! Boltzmann machine
        complex(rk), contiguous, dimension(:), intent(inout) :: grad                            !! Gradient to optimize
        integer, intent(in) :: epoch                                                                   !! Current epoch

        real(rk), allocatable :: beta_1, beta_2, step, epsilon

        beta_1 = 0.99_rk
        beta_2 = 0.999_rk
        step = 0.002_rk
        epsilon = 0.00000001_rk

        self%p_a = beta_1*self%p_a + (1.0_rk - beta_1)*grad
        self%r_a = beta_2*self%r_a + (1.0_rk - beta_2)*(grad**2)

        grad = step*( self%p_a/(1.0_rk - beta_1**epoch) )/sqrt((self%r_a/(1.0_rk - beta_2**epoch)) + epsilon)
    end subroutine ADAM_a

    pure subroutine ADAM_b(self, grad, epoch)
        !! Procedure for optimizing gradient with adaptive moment estimator
        class(RestrictedBoltzmannMachine), intent(inout) :: self                                   !! Boltzmann machine
        complex(rk), contiguous, dimension(:), intent(inout) :: grad                            !! Gradient to optimize
        integer, intent(in) :: epoch                                                                   !! Current epoch

        real(rk), allocatable :: beta_1, beta_2, step, epsilon

        beta_1 = 0.99_rk
        beta_2 = 0.999_rk
        step = 0.002_rk
        epsilon = 0.00000001_rk

        self%p_b = beta_1*self%p_b + (1.0_rk - beta_1)*grad
        self%r_b = beta_2*self%r_b + (1.0_rk - beta_2)*(grad**2)

        grad = step*( self%p_b/(1.0_rk - beta_1**epoch) )/sqrt((self%r_b/(1.0_rk - beta_2**epoch)) + epsilon)
    end subroutine ADAM_b

    pure subroutine ADAM_w(self, grad, epoch)
        !! Procedure for optimizing gradient with adaptive moment estimator
        class(RestrictedBoltzmannMachine), intent(inout) :: self                                   !! Boltzmann machine
        complex(rk), contiguous, dimension(:,:), intent(inout) :: grad                          !! Gradient to optimize
        integer, intent(in) :: epoch                                                                   !! Current epoch

        real(rk), allocatable :: beta_1, beta_2, step, epsilon

        beta_1 = 0.99_rk
        beta_2 = 0.999_rk
        step = 0.002_rk
        epsilon = 0.00000001_rk

        self%p_w = beta_1*self%p_w + (1.0_rk - beta_1)*grad
        self%r_w = beta_2*self%r_w + (1.0_rk - beta_2)*(grad**2)

        grad = step*( self%p_w/(1.0_rk - beta_1**epoch) )/sqrt((self%r_w/(1.0_rk - beta_2**epoch)) + epsilon)
    end subroutine ADAM_w

	!! Supporting Procedures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    pure complex(rk) elemental function sech(z) result(hyperbolic_secant)
        complex(rk), intent(in) :: z
        hyperbolic_secant = 2.0_rk/( exp(z) + exp(-z) )
    end function sech

    pure real(rk) recursive function cas_sum_real(x) result(psum)
        !! Cascading summation for minimizing accumulation of round-off errors
        real(rk), contiguous, dimension(:), intent(in) :: x
        integer :: n, i

        n = size(x)
        if ( n <= 10 ) then                                                                                !! Base case
            psum = sum(x)
        else
            psum = cas_sum(x(1:n/2)) + cas_sum(x(n/2+1:n))                                        !! Divide and conquer
        end if
    end function cas_sum_real

    pure complex(rk) recursive function cas_sum_complex(x) result(psum)
        !! Cascading summation for minimizing accumulation of round-off errors
        complex(rk), contiguous, dimension(:), intent(in) :: x
        integer :: n, i

        n = size(x)
        if ( n <= 10 ) then                                                                                !! Base case
            psum = sum(x)
        else
            psum = cas_sum(x(1:n/2)) + cas_sum(x(n/2+1:n))                                        !! Divide and conquer
        end if
    end function cas_sum_complex

    pure real(rk) function var(x) result(variance)
		!! Function for calculating sample variance of a complex vector using canonical two-pass algorithm
        complex(rk), contiguous, dimension(:), intent(in) :: x

		complex(rk), allocatable, dimension(:) :: x_cent
		integer :: n

		n = size(x)
		x_cent = (x - sum(x)/n)

		variance = sum( real(conjg(x_cent)*x_cent, kind=rk) )/(n-1)
	end function var

    pure function corr(A) result(correlations)
		!! Function for calculating spin-spin correlations
		integer(ik), contiguous, dimension(:,:), intent(in) :: A                                     !! Input spin data
		real(rk), allocatable, dimension(:) :: correlations                                      !! Output correlations

		integer(ik), allocatable, dimension(:,:) :: B                                              !! Transformed spins
		integer(ik), allocatable, dimension(:) :: ref_spin                                            !! Reference spin
		integer :: n, num_samples, j, agrees, disagrees                 !! Size and loop variables, agreement variables

		n = size(A, dim=1)                                                                       !! Get number of spins
        num_samples = size(A, dim=2)                                                           !! Get number of samples
		B = transpose(A)                                                                      !! Lay spins down columns
		ref_spin = B(:,n/2+1)                                                           !! Set middle spin as reference

		allocate( correlations(n), source=1.0_rk )

		get_correlations: do concurrent (j = 1:n, j /= n/2+1) local(agrees, disagrees) shared(correlations)
            agrees = count( B(:,j)==ref_spin )                                              !! Get number of agreements
            disagrees = num_samples - agrees                                             !! Get number of disagreements
            correlations(j) = real(agrees - disagrees, kind=rk)/num_samples                 !! Mean agreement on [-1,1]
		end do get_correlations
	end function corr

    impure function gauss_matrix(dims, mu, sig) result(gauss_sample_matrix)
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
        use, intrinsic :: iso_fortran_env, only: dp=>real64
        integer, dimension(2), intent(in) :: dims
        real(rk), intent(in) :: mu, sig
        real(rk), dimension(dims(1), dims(2)) :: gauss_sample_matrix

        real(dp), allocatable :: kC1, kC2, kC3, kD1, kD2, kD3, kHm, kZm, kHp, kZp, kPhln, kHm1
        real(dp), allocatable :: kHp1, kHzm, kHzmp, kAs, kBs, kCs, kB, kX0, kYm, kS, kT
        real(dp) :: rn, x, y, z, res
        integer :: i, j

        if ( any(dims < 1) ) error stop 'All gauss_matrix dimensions must be at least 1... terminating'

        kC1 = 1.448242853_dp
        kC2 = 3.307147487_dp
        kC3 = 1.46754004_dp
        kD1 = 1.036467755_dp
        kD2 = 5.295844968_dp
        kD3 = 3.631288474_dp
        kHm = 0.483941449_dp
        kZm = 0.107981933_dp
        kHp = 4.132731354_dp
        kZp = 18.52161694_dp
        kPhln = 0.4515827053_dp
        kHm1 = 0.516058551_dp
        kHp1 = 3.132731354_dp
        kHzm = 0.375959516_dp
        kHzmp = 0.591923442_dp
        
        kAs = 0.8853395638_dp
        kBs = 0.2452635696_dp
        kCs = 0.2770276848_dp
        kB  = 0.5029324303_dp
        kX0 = 0.4571828819_dp
        kYm = 0.187308492_dp
        kS  = 0.7270572718_dp
        kT  = 0.03895759111_dp

        columns: do j = 1, dims(2)
            rows: do i = 1, dims(1)
                outer: do
                    call random_number(y)

                    if ( y > kHm1 ) then
                        res = kHp*y - kHp1; exit outer
                    else if ( y < kZm ) then
                        rn = kZp*y - 1.0_dp

                        if ( rn > 0.0_dp ) then
                            res = 1.0_dp + rn; exit outer
                        else
                            res = -1.0_dp + rn; exit outer
                        end if
                    else if ( y < kHm ) then
                        call random_number(rn)
                        rn = rn - 1.0_dp + rn

                        if ( rn > 0.0_dp ) then
                            z = 2.0_dp - rn
                        else
                            z = -2.0_dp - rn
                        end if

                        if ( ((kC1-y)*(kC3+abs(z))) < kC2 ) then
                            res = z; exit outer
                        else
                            x = rn*rn
                            if ( ((y+kD1)*(kD3+x)) < kD2 ) then
                                res = rn; exit outer
                            else if ( (kHzmp-y) < exp(-(z*z+kPhln)/2) ) then
                                res = z; exit outer
                            else if ( (y+kHzm) < exp(-(x+kPhln)/2) ) then
                                res = rn; exit outer
                            end if
                        end if
                    end if

                    inner: do
                        call random_number(x)
                        call random_number(y)
                        y = kYm*y
                        z = kX0 - kS*x - y

                        if ( z > 0.0_dp ) then
                            rn = 2.0_dp + y/x
                        else
                            x = 1.0_dp - x
                            y = kYm - y
                            rn = -(2.0_dp + y/x)
                        end if

                        if ( ((y-kAs+x)*(kCs+x)+kBs) < 0.0_dp ) then
                            res = rn; exit inner
                        else if ( y < (x+kT) ) then
                            if ( (rn*rn) < (4.0_dp*(kB-log(x))) ) then
                                res = rn; exit inner
                            end if
                        end if
                    end do inner
                end do outer

                gauss_sample_matrix(i,j) = res*sig + mu
            end do rows
        end do columns
    end function gauss_matrix
end module ising_ml
