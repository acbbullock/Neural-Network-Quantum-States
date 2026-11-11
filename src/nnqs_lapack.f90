module nnqs_lapack
  !-------------------------------------------------------------------------------------------------------------------
  !! An interface to the LAPACK routines the program uses, through the standard Fortran 77 interface that every LAPACK
  !! provides: reference LAPACK, OpenBLAS, Intel MKL and Apple Accelerate.
  !!
  !! `ppsvx` solves A x = b for a symmetric or Hermitian positive definite matrix A held in packed storage, as the
  !! LAPACK95 routine of the same name does when called without `info`: it stops the program if LAPACK reports an
  !! error, a matrix that is not positive definite, or one too ill-conditioned for the solution to be trusted.
  !-------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: sp=>real32
  implicit none (type,external)
  private

  public :: ppsvx

  interface ppsvx
    module procedure :: ppsvx_real, ppsvx_complex
  end interface

  interface
    pure subroutine sppsvx(fact, uplo, n, nrhs, ap, afp, equed, s, b, ldb, x, ldx, rcond, ferr, berr, work, iwork, &
                           info)
      import :: sp
      implicit none
      character, intent(in)    :: fact, uplo
      integer,   intent(in)    :: n, nrhs, ldb, ldx
      real(sp),  intent(inout) :: ap(*), afp(*), s(*), b(ldb,*)
      character, intent(inout) :: equed
      real(sp),  intent(out)   :: x(ldx,*), rcond, ferr(*), berr(*), work(*)
      integer,   intent(out)   :: iwork(*), info
    end subroutine sppsvx

    pure subroutine cppsvx(fact, uplo, n, nrhs, ap, afp, equed, s, b, ldb, x, ldx, rcond, ferr, berr, work, rwork, &
                           info)
      import :: sp
      implicit none
      character,   intent(in)    :: fact, uplo
      integer,     intent(in)    :: n, nrhs, ldb, ldx
      complex(sp), intent(inout) :: ap(*), afp(*), b(ldb,*)
      real(sp),    intent(inout) :: s(*)
      character,   intent(inout) :: equed
      complex(sp), intent(out)   :: x(ldx,*), work(*)
      real(sp),    intent(out)   :: rcond, ferr(*), berr(*), rwork(*)
      integer,     intent(out)   :: info
    end subroutine cppsvx
  end interface

  contains !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  pure subroutine ppsvx_real(AP, b, x, uplo, fact)
    real(sp),  contiguous, intent(inout) :: AP(:)                      !! Packed matrix, equilibrated on return if fact='E'
    real(sp),  contiguous, intent(inout) :: b(:)                       !! Right-hand side, scaled on return if equilibrated
    real(sp),  contiguous, intent(out)   :: x(:)                                                             !! Solution
    character, intent(in) :: uplo, fact                         !! Triangle held in AP ('U' or 'L'), and 'N' or 'E'

    real(sp), allocatable :: afp(:), s(:), work(:)
    integer,  allocatable :: iwork(:)
    real(sp)  :: rcond, ferr(1), berr(1)
    character :: equed
    integer   :: n, info

    n = size(b)
    allocate(afp(size(AP)), s(n), work(3*n), iwork(n))
    equed = 'N'
    call sppsvx(fact, uplo, n, 1, AP, afp, equed, s, b, n, x, n, rcond, ferr, berr, work, iwork, info)
    call check_info(info, n)
  end subroutine ppsvx_real

  pure subroutine ppsvx_complex(AP, b, x, uplo, fact)
    complex(sp), contiguous, intent(inout) :: AP(:)                    !! Packed matrix, equilibrated on return if fact='E'
    complex(sp), contiguous, intent(inout) :: b(:)                     !! Right-hand side, scaled on return if equilibrated
    complex(sp), contiguous, intent(out)   :: x(:)                                                           !! Solution
    character,   intent(in) :: uplo, fact                       !! Triangle held in AP ('U' or 'L'), and 'N' or 'E'

    complex(sp), allocatable :: afp(:), work(:)
    real(sp),    allocatable :: s(:), rwork(:)
    real(sp)  :: rcond, ferr(1), berr(1)
    character :: equed
    integer   :: n, info

    n = size(b)
    allocate(afp(size(AP)), s(n), work(2*n), rwork(n))
    equed = 'N'
    call cppsvx(fact, uplo, n, 1, AP, afp, equed, s, b, n, x, n, rcond, ferr, berr, work, rwork, info)
    call check_info(info, n)
  end subroutine ppsvx_complex

  pure subroutine check_info(info, n)
    integer, intent(in) :: info, n

    if ( info < 0 ) then
      error stop 'FATAL: ppsvx passed an invalid argument to LAPACK.'
    else if ( info > 0 .and. info <= n ) then
      error stop 'FATAL: ppsvx was given a matrix that is not positive definite.'
    else if ( info > n ) then
      error stop 'FATAL: ppsvx was given a matrix too ill-conditioned to solve in working precision.'
    end if
  end subroutine check_info

end module nnqs_lapack
