program main
  !-------------------------------------------------------------------------------------------------------------------
  !!  This program demonstrates the use of the nnqs module.
  !!
  !!  Usage: main [spins hidden_units J B]
  !!
  !!  With no arguments it trains the chain of the README's figures: 1024 spins, 64 hidden units, J = -0.5 and B = 0.1.
  !!  The energies and correlations are written to ./data, which must exist.
  !-------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: rk=>real32
  use nnqs, only: RestrictedBoltzmannMachine                                                    !! Neural network type
  implicit none (type,external)                                                     !! No implicit types or interfaces

  !! Variable Declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  type(RestrictedBoltzmannMachine) :: psi                                                            !! Neural network

  integer  :: spins = 1024, hidden_units = 64                                      !! Number of spins and hidden units
  real(rk) :: J = -0.5_rk, B = 0.1_rk                                           !! Coupling and transverse field strength
  character(len=64) :: arg                                                                     !! Command-line argument

  !! Begin Executable Code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  select case ( command_argument_count() )
    case (0)
    case (4)
      call get_command_argument(1, arg); read(arg, *) spins
      call get_command_argument(2, arg); read(arg, *) hidden_units
      call get_command_argument(3, arg); read(arg, *) J
      call get_command_argument(4, arg); read(arg, *) B
    case default
      error stop 'Usage: main [spins hidden_units J B]'
  end select

  call random_init(repeatable=.false., image_distinct=.true.)                    !! Initialize random number generator

  psi = RestrictedBoltzmannMachine(v_units=spins, h_units=hidden_units)                             !! Create instance
  call psi%stochastic_optimization(ising_params=[J, B])                                  !! Input [J,B] and train network
end program main
