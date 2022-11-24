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
    real(rk) :: ising_strengths(2)                                          !! Coupling strength J and field strength B

    !! Begin Executable Code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    call random_init(repeatable=.false., image_distinct=.true.)                   !! Initialize random number generator

    spins = 1001                                                                         !! Set number of visible units
    hidden_units = 50                                                                     !! Set number of hidden units
    ising_strengths = [0.5_rk, 0.1_rk]                                  !! Set coupling strength J and field strength B

    psi = RestrictedBoltzmannMachine(v_units=spins, h_units=hidden_units)                            !! Create instance

    call psi%optimize( ising_strengths=ising_strengths, &                                                !! Input [J,B]
                       energies_file='energies.csv', &                                        !! Output energies to csv
                       correlations_file='correlations.csv' )                             !! Output correlations to csv

    !! Note file outputs are optional, .dat binary output also supported, and any text file format is supported.

end program main
