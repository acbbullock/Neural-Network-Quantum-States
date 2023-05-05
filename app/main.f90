program main
	!-------------------------------------------------------------------------------------------------------------------
	!!  This program demonstrates the use of the nnqs module.
	!-------------------------------------------------------------------------------------------------------------------
	use, intrinsic :: iso_fortran_env, only: rk=>real32
	use nnqs, only: RestrictedBoltzmannMachine                                                    !! Neural network type
	use omp_lib                                                                                         !! OpenMP module
	implicit none (type,external)                                                     !! No implicit types or interfaces

	!! Variable Declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	type(RestrictedBoltzmannMachine) :: psi                                                            !! Neural network
	integer :: spins, hidden_units                                                   !! Number of spins and hidden units

	!! Begin Executable Code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	call random_init(repeatable=.true., image_distinct=.true.)                     !! Initialize random number generator
	call omp_set_default_device(1)                                                          !! Set OpenMP offload device

	spins = 1024                                                                          !! Set number of visible units
	hidden_units = 64                                                                      !! Set number of hidden units

	psi = RestrictedBoltzmannMachine(v_units=spins, h_units=hidden_units)                             !! Create instance

	call psi%stochastic_optimization(ising_strengths=[ -0.5_rk, 0.1_rk ])                                 !! Input [J,B]

end program main
