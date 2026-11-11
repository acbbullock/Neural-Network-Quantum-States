program main
  !-------------------------------------------------------------------------------------------------------------------
  !!  This program demonstrates the use of the nnqs module.
  !-------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: rk=>real32
  use nnqs, only: RestrictedBoltzmannMachine                                                    !! Neural network type
  implicit none (type,external)                                                     !! No implicit types or interfaces

  !! Variable Declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  type(RestrictedBoltzmannMachine) :: psi                                                            !! Neural network

  integer, parameter :: spins = 1024, hidden_units = 64                            !! Number of spins and hidden units

  !! Begin Executable Code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  call random_init(repeatable=.false., image_distinct=.true.)                    !! Initialize random number generator

  psi = RestrictedBoltzmannMachine(v_units=spins, h_units=hidden_units)                             !! Create instance
  call psi%stochastic_optimization(ising_params=[ -0.5_rk, 0.1_rk ])                  !! Input [J,B] and train network
end program main
