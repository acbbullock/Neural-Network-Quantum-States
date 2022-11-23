!!---------------------------------------------------------------------------------------------------------------------
!!  This program demonstrates the use of the nnqs module.
!!---------------------------------------------------------------------------------------------------------------------
program main
    use, intrinsic :: iso_fortran_env, only: rk=>real64                                        !! Import standard kinds
    use nnqs, only: RestrictedBoltzmannMachine                                                        !! Neural network
    implicit none (type,external)                                                    !! No implicit types or interfaces

    !! Variable Declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    type(RestrictedBoltzmannMachine) :: psi                                                           !! Neural network

    integer :: spins, hidden_units                                                  !! Number of spins and hidden units
    real(rk) :: ising_strengths(2)                                                      !! Coupling and field strengths

    !! Begin Executable Code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    call random_init(repeatable=.false., image_distinct=.true.)                   !! Initialize random number generator

    spins = 1001                                                                         !! Set number of visible units
    hidden_units = 50                                                                     !! Set number of hidden units
    ising_strengths = [0.5_rk, 0.1_rk]                                      !! Set coupling strength and field strength

    psi = RestrictedBoltzmannMachine(v_units=spins, h_units=hidden_units)                            !! Create instance

    call psi%optimize(ising_strengths=ising_strengths)                                            !! Learn ground state

end program main
