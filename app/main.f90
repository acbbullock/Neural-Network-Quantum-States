!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  This program demonstrates the use of the ising_ml module.
!!
!!  To build and run with fpm using ifort on Windows (requires oneAPI Base and HPC toolkits), use the following:
!!  fpm run --compiler ifort --flag "/O3 /arch:CORE-AVX2 /Qiopenmp /Qcoarray /Qcoarray-num-images:16 /heap-arrays:0"
!!  --link-flag "mkl_lapack95_lp64.lib mkl_intel_lp64.lib mkl_intel_thread.lib mkl_core.lib libiomp5md.lib"
!!
!!  To build and run with fpm using ifort on Linux (requires oneAPI Base and HPC toolkits), use the following:
!!  fpm run --compiler ifort --flag "-O3 -arch CORE-AVX2 -qiopenmp -coarray -coarray-num-images=16 -heap-arrays 0 -liomp5"
!!  --link-flag "-Wl,--start-group ${MKLROOT}/lib/intel64/libmkl_lapack95_lp64.a ${MKLROOT}/lib/intel64/libmkl_intel_lp64.a
!!  ${MKLROOT}/lib/intel64/libmkl_intel_thread.a ${MKLROOT}/lib/intel64/libmkl_core.a"
!!
program main
    use, intrinsic :: iso_fortran_env, only: rk=>real64, i64=>int64                            !! Import standard kinds
    use io_mod, only: csvwrite                                                                        !! I/O procedures
    use ising_ml, only: RestrictedBoltzmannMachine                                                    !! Neural network
    implicit none (type,external)                                                    !! No implicit types or interfaces

    !! Variable Declarations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    type(RestrictedBoltzmannMachine) :: psi                                                           !! Neural network

    real(rk), allocatable, dimension(:,:) :: energies, correlations                                 !! Training outputs
    integer :: spins, hidden_units                                                               !! Training parameters
    real(rk) :: ising_params(2)                                                                     !! Ising parameters

    integer(i64) t1, t2                                                                              !! Clock variables
    real(rk) rate, telapse                                                                           !! Clock variables

    !! Begin Executable Code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    call random_init(repeatable=.false., image_distinct=.true.)                   !! Initialize random number generator

    spins = 501                                                                          !! Set number of visible units
    hidden_units = 50                                                                     !! Set number of hidden units
    ising_params = [-10.0_rk, -0.5_rk]                                      !! Set coupling strength and field strength

    psi = RestrictedBoltzmannMachine(v_units=spins, h_units=hidden_units)                            !! Create instance

    if ( this_image() == 1 ) call system_clock(t1)                                                       !! Start clock

    call psi%train(ising_params=ising_params, energies=energies, correlations=correlations)            !! Train network

    if ( this_image() == 1 ) then
        call system_clock(t2, count_rate=rate)                                                            !! Stop clock
        telapse = real((t2-t1), kind=rk)/rate                                     !! Total elapsed wall clock time in s

        print*
        print*, 'Elapsed wall clock time: ', real(telapse), ' seconds for n = ', spins, ' spins.'
        print*

        call csvwrite(energies, 'energies.csv')                                               !! Write energies to file
        call csvwrite(correlations, 'correlations.csv')                                   !! Write correlations to file
    end if
end program main
