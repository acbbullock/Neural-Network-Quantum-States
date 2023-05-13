program main
	!-------------------------------------------------------------------------------------------------------------------
	!!  This program demonstrates the use of the nnqs module.
	!-------------------------------------------------------------------------------------------------------------------
	use nnqs, only: RestrictedBoltzmannMachine                                                    !! Neural network type
	use omp_lib                                                                                         !! OpenMP module
	implicit none (type,external)                                                     !! No implicit types or interfaces

	!! Variable Declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	type(RestrictedBoltzmannMachine) :: psi                                                            !! Neural network
	integer :: n, m                                                                  !! Number of spins and hidden units
	real    :: J, B                                                                                  !! Ising parameters

	!! Begin Executable Code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	call random_init(repeatable=.true., image_distinct=.true.)                    !! Initialize random number generator
	call omp_set_default_device(1)                            !! Set OpenMP offload device (device id depends on system)

	n = 1024                                                                                      !! Set number of spins
	m = 64                                                                                 !! Set number of hidden units
	J = -0.5                                                                  !! Set nearest neighbbor coupling strength
	B = 0.1                                                                                        !! Set field strength

	psi = RestrictedBoltzmannMachine(v_units=n, h_units=m)                                            !! Create instance
	call psi%stochastic_optimization( ising_params=[J,B] )                              !! Input [J,B] and train network
end program main
