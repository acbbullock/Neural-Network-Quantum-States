!!---------------------------------------------------------------------------------------------------------------------
!!  This program demonstrates the use of the nnqs module.
!!---------------------------------------------------------------------------------------------------------------------
program main
    use, intrinsic :: iso_fortran_env, only: rk=>real64                                        !! Import standard kinds
    use io_mod, only: to_file                                                           !! I/O procedures and constants
    use nnqs, only: RestrictedBoltzmannMachine                                                        !! Neural network
    implicit none (type,external)                                                    !! No implicit types or interfaces

    !! Variable Declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    type(RestrictedBoltzmannMachine) :: psi                                                           !! Neural network

    integer :: spins, hidden_units                                                  !! Number of spins and hidden units
    real(rk) :: ising_strengths(2)                                          !! Coupling strength J and field strength B
    real(rk), allocatable, dimension(:,:) :: energies, correlations                        !! Energies and correlations

    !! Begin Executable Code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    call random_init(repeatable=.false., image_distinct=.true.)                   !! Initialize random number generator

    spins = 1001                                                                         !! Set number of visible units
    hidden_units = 50                                                                     !! Set number of hidden units
    ising_strengths = [0.5_rk, 0.1_rk]                                  !! Set coupling strength J and field strength B

    psi = RestrictedBoltzmannMachine(v_units=spins, h_units=hidden_units)                            !! Create instance

    call psi%stochastic_optimization( ising_strengths=ising_strengths, &                                 !! Input [J,B]
                                      energies=energies, &                                     !! Output energies array
                                      correlations=correlations )                          !! Output correlations array

    if ( this_image() == 1 ) then                                                                  !! Do I/O on image 1
        call to_file(energies, file_name='energies.csv', delim=',', header=['Energy', 'Error'])
        call to_file(correlations, file_name='correlations.csv', delim=',', header=['Epoch'])
    end if

end program main
