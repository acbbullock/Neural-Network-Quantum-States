program test_nnqs
  !-------------------------------------------------------------------------------------------------------------------
  !! Checks the building blocks of the optimization against independent calculations: the LAPACK interface against
  !! systems with known solutions, and the network's probability ratios and local energies against values computed
  !! directly from ln ψ for random parameters.
  !-------------------------------------------------------------------------------------------------------------------
  use, intrinsic :: iso_fortran_env, only: rk=>real32, ik=>int8, output_unit
  use nnqs, only: RestrictedBoltzmannMachine
  use nnqs_lapack, only: ppsvx
  implicit none (type,external)

  real(rk), parameter :: tol = 1e-4_rk                                     !! Relative tolerance in single precision
  integer :: failures

  failures = 0
  call random_init(repeatable=.true., image_distinct=.true.)

  call test_ppsvx_real()
  call test_ppsvx_complex()
  call test_prob_ratio()
  call test_ising_energy()

  if ( failures > 0 ) then
    write(output_unit, '(i0,a)') failures, ' check(s) failed.'
    error stop 1
  end if
  write(output_unit, '(a)') 'All checks passed.'

  contains !!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  subroutine check(ok, label, err)
    logical,      intent(in) :: ok
    character(*), intent(in) :: label
    real(rk),     intent(in) :: err

    if ( ok ) then
      write(output_unit, '(a,es9.2,a)') 'PASS  '//label//'  (relative error ', err, ')'
    else
      write(output_unit, '(a,es9.2,a,es9.2,a)') 'FAIL  '//label//'  (relative error ', err, ', tolerance ', tol, ')'
      failures = failures + 1
    end if
  end subroutine check

  pure integer function packed(i, j, n)
    !! Index of A(i,j), i >= j, in the lower triangle of an n x n matrix in packed storage
    integer, intent(in) :: i, j, n
    packed = i + ((j-1)*(2*n-j))/2
  end function packed

  subroutine test_ppsvx_real()
    integer, parameter :: n = 7
    real(rk) :: m(n,n), a(n,n), ap((n*(n+1))/2), x_true(n), b(n), x(n), err
    integer  :: i, j

    call random_number(m); call random_number(x_true)
    a = matmul(transpose(m), m)
    do i = 1, n; a(i,i) = a(i,i) + 1.0_rk; end do                                    !! Symmetric positive definite
    b = matmul(a, x_true)
    do j = 1, n; do i = j, n; ap(packed(i,j,n)) = a(i,j); end do; end do

    call ppsvx(AP=ap, b=b, x=x, uplo='L', fact='E')
    err = norm2(x - x_true)/norm2(x_true)
    call check(err < tol, 'ppsvx solves a real symmetric positive definite system', err)
  end subroutine test_ppsvx_real

  subroutine test_ppsvx_complex()
    integer, parameter :: n = 7
    real(rk)    :: re(n,n), im(n,n), xr(n), xi(n), err
    complex(rk) :: m(n,n), a(n,n), ap((n*(n+1))/2), x_true(n), b(n), x(n)
    integer     :: i, j

    call random_number(re); call random_number(im); m = cmplx(re, im, kind=rk)
    call random_number(xr); call random_number(xi); x_true = cmplx(xr, xi, kind=rk)
    a = matmul(conjg(transpose(m)), m)
    do i = 1, n; a(i,i) = a(i,i) + 1.0_rk; end do                                  !! Hermitian positive definite
    b = matmul(a, x_true)
    do j = 1, n; do i = j, n; ap(packed(i,j,n)) = a(i,j); end do; end do

    call ppsvx(AP=ap, b=b, x=x, uplo='L', fact='E')
    err = sqrt(sum(abs(x - x_true)**2)/sum(abs(x_true)**2))
    call check(err < tol, 'ppsvx solves a complex Hermitian positive definite system', err)
  end subroutine test_ppsvx_complex

  subroutine random_machine(n, m, psi, a, b, w)
    !! A machine with parameters drawn uniformly from [-0.5, 0.5), real and imaginary parts alike
    integer, intent(in) :: n, m
    type(RestrictedBoltzmannMachine), intent(out) :: psi
    real(rk),    intent(out) :: a(n)
    complex(rk), intent(out) :: b(m), w(m,n)
    real(rk) :: br(m), bi(m), wr(m,n), wi(m,n)

    call random_number(a); call random_number(br); call random_number(bi)
    call random_number(wr); call random_number(wi)
    a = a - 0.5_rk
    b = cmplx(br - 0.5_rk, bi - 0.5_rk, kind=rk)
    w = cmplx(wr - 0.5_rk, wi - 0.5_rk, kind=rk)
    psi = RestrictedBoltzmannMachine(v_units=n, h_units=m, a=a, b=b, w=w)
  end subroutine random_machine

  subroutine random_configuration(s)
    integer(ik), intent(out) :: s(:)
    real(rk) :: r(size(s))
    call random_number(r)
    s = merge(1_ik, 0_ik, r < 0.5_rk)
  end subroutine random_configuration

  subroutine test_prob_ratio()
    integer, parameter :: n = 8, m = 5, trials = 20
    type(RestrictedBoltzmannMachine) :: psi
    real(rk)    :: a(n), p, p_direct, err, worst
    complex(rk) :: b(m), w(m,n), theta1(m), theta2(m)
    integer(ik) :: s1(n), s2(n)
    integer     :: trial, ind

    call random_machine(n, m, psi, a, b, w)
    worst = 0.0_rk
    do trial = 1, trials
      call random_configuration(s1)
      theta1 = conjg(b) + matmul(w, real(s1, kind=rk))
      do ind = 1, n
        s2 = s1; s2(ind) = 1_ik - s2(ind)
        call psi%prob_ratio(ind=ind, s1=s1, s2=s2, theta1=theta1, theta2=theta2, p=p)
        p_direct = exp(2.0_rk*real(psi%log_amplitude(s2) - psi%log_amplitude(s1), kind=rk))
        worst = max(worst, abs(p - p_direct)/p_direct)
      end do
    end do
    err = worst
    call check(err < tol, 'prob_ratio equals |ψ(s2)/ψ(s1)|^2 for every single flip', err)
  end subroutine test_prob_ratio

  subroutine test_ising_energy()
    !! For H = -J Σ σz_j σz_{j+1} - B Σ σx_j with open ends, the local energy of s is
    !! E(s) = -J Σ σ_j σ_{j+1} - B Σ_j ψ(s^j)/ψ(s), where σ_j = 1 - 2 s_j and s^j is s with spin j flipped.
    !! ising_energy keeps the real part.
    integer, parameter :: n = 8, m = 5, trials = 20
    real(rk), parameter :: J = 0.7_rk, B = 0.3_rk
    type(RestrictedBoltzmannMachine) :: psi
    real(rk)    :: a(n), sigma(n), s_map(n), energy, e_direct, err, worst
    complex(rk) :: bias(m), w(m,n), theta(m), arg_theta(m), field
    integer(ik) :: s(n), s_flip(n)
    integer     :: trial, k

    call random_machine(n, m, psi, a, bias, w)
    worst = 0.0_rk
    do trial = 1, trials
      call random_configuration(s)
      theta = conjg(bias) + matmul(w, real(s, kind=rk))
      call psi%ising_energy(s=s, theta=theta, ising_params=[J, B], s_map=s_map, arg_theta=arg_theta, energy=energy)

      sigma = 1.0_rk - 2.0_rk*real(s, kind=rk)
      field = (0.0_rk, 0.0_rk)
      do k = 1, n
        s_flip = s; s_flip(k) = 1_ik - s_flip(k)
        field = field + exp(psi%log_amplitude(s_flip) - psi%log_amplitude(s))
      end do
      e_direct = -J*sum(sigma(1:n-1)*sigma(2:n)) - B*real(field, kind=rk)
      worst = max(worst, abs(energy - e_direct)/max(abs(e_direct), 1.0_rk))
    end do
    err = worst
    call check(err < tol, 'ising_energy equals the local energy of the transverse-field Ising chain', err)
  end subroutine test_ising_energy

end program test_nnqs
