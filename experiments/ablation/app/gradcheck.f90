program gradcheck
  !! Exact check of the force the code computes for b and w, against finite differences of the exact energy.
  !! For a complex parameter p = x + iy, a real function E has dE/dx = 2 Re(dE/dp) and dE/dy = -2 Im(dE/dp).
  !! The code's force is F = <O* dE_loc> with O = d ln psi / d(conj b) for b (psi uses conj b) and d ln psi / dw
  !! for w. This prints, for each parameter, dE/dx and dE/dy by finite differences beside 2Re(F) and 2Im(F).
  use, intrinsic :: iso_fortran_env, only: rk=>real32, dp=>real64, ik=>int8
  use nnqs, only: RestrictedBoltzmannMachine
  implicit none (type,external)

  integer,  parameter :: n = 6, m = 4
  real(dp), parameter :: J = 0.5_dp, h = 0.3_dp, eps = 1e-3_dp
  real(rk)    :: a(n), br(m), bi(m), wr(m,n), wi(m,n)
  complex(rk) :: b(m), w(m,n), bp(m), wp(m,n)
  complex(dp) :: Fb(m), Fw(m,n)
  real(dp)    :: dx, dy
  integer     :: i, k, put(64)

  put = [(12345 + 17*i, i = 1, 64)]
  call random_seed(put=put(1:sizeof_seed()))
  call random_number(a); call random_number(br); call random_number(bi); call random_number(wr); call random_number(wi)
  a = a - 0.5; b = cmplx(br - 0.5, bi - 0.5, rk); w = cmplx(wr - 0.5, wi - 0.5, rk)

  call forces(a, b, w, Fb, Fw)

  write(*,'(a)') 'param        dE/dRe (FD)   2Re(F)       dE/dIm (FD)   2Im(F)'
  do i = 1, m
    bp = b; bp(i) = bp(i) + cmplx(eps, 0, rk); dx = energy(a, bp, w)
    bp = b; bp(i) = bp(i) - cmplx(eps, 0, rk); dx = (dx - energy(a, bp, w))/(2*eps)
    bp = b; bp(i) = bp(i) + cmplx(0, eps, rk); dy = energy(a, bp, w)
    bp = b; bp(i) = bp(i) - cmplx(0, eps, rk); dy = (dy - energy(a, bp, w))/(2*eps)
    write(*,'(a,i0,a,4f13.6)') 'b(', i, ')     ', dx, 2*Fb(i)%re, dy, 2*Fb(i)%im
  end do
  do k = 1, 2
    i = k
    wp = w; wp(i,k) = wp(i,k) + cmplx(eps, 0, rk); dx = energy(a, b, wp)
    wp = w; wp(i,k) = wp(i,k) - cmplx(eps, 0, rk); dx = (dx - energy(a, b, wp))/(2*eps)
    wp = w; wp(i,k) = wp(i,k) + cmplx(0, eps, rk); dy = energy(a, b, wp)
    wp = w; wp(i,k) = wp(i,k) - cmplx(0, eps, rk); dy = (dy - energy(a, b, wp))/(2*eps)
    write(*,'(a,i0,a,i0,a,4f13.6)') 'w(', i, ',', k, ')   ', dx, 2*Fw(i,k)%re, dy, 2*Fw(i,k)%im
  end do

contains

  integer function sizeof_seed()
    call random_seed(size=sizeof_seed)
  end function sizeof_seed

  subroutine amplitudes(a, b, w, lp, eloc)
    !! ln psi for every configuration, and the full complex local energy
    real(rk),    intent(in)  :: a(n)
    complex(rk), intent(in)  :: b(m), w(m,n)
    complex(dp), intent(out) :: lp(0:2**n-1), eloc(0:2**n-1)
    type(RestrictedBoltzmannMachine) :: psi
    integer(ik) :: s(n)
    integer     :: idx, jj
    real(dp)    :: sig(n)
    psi = RestrictedBoltzmannMachine(v_units=n, h_units=m, a=a, b=b, w=w)
    do idx = 0, 2**n-1
      do jj = 1, n; s(jj) = int(ibits(idx, jj-1, 1), ik); end do
      lp(idx) = cmplx(psi%log_amplitude(s), kind=dp)
    end do
    do idx = 0, 2**n-1
      do jj = 1, n; sig(jj) = 1.0_dp - 2.0_dp*ibits(idx, jj-1, 1); end do
      eloc(idx) = -J*sum(sig(1:n-1)*sig(2:n))
      do jj = 1, n
        eloc(idx) = eloc(idx) - h*exp(lp(ieor(idx, 2**(jj-1))) - lp(idx))
      end do
    end do
  end subroutine amplitudes

  real(dp) function energy(a, b, w)
    real(rk),    intent(in) :: a(n)
    complex(rk), intent(in) :: b(m), w(m,n)
    complex(dp) :: lp(0:2**n-1), eloc(0:2**n-1)
    real(dp)    :: p(0:2**n-1)
    call amplitudes(a, b, w, lp, eloc)
    p = exp(2*(lp%re - maxval(lp%re))); p = p/sum(p)
    energy = real(sum(p*eloc), dp)
  end function energy

  subroutine forces(a, b, w, Fb, Fw)
    !! F = <conj(O) E_loc> - <conj(O)><E_loc> under |psi|^2, with the code's O:
    !! O_b(i) = sigmoid(theta_i), O_w(i,j) = s_j sigmoid(theta_i), theta = conj(b) + w s
    real(rk),    intent(in)  :: a(n)
    complex(rk), intent(in)  :: b(m), w(m,n)
    complex(dp), intent(out) :: Fb(m), Fw(m,n)
    complex(dp) :: lp(0:2**n-1), eloc(0:2**n-1), th(m), Ob(m), Eav, Obav(m), Owav(m,n), ObE(m), OwE(m,n)
    real(dp)    :: p(0:2**n-1), s(n)
    integer     :: idx, jj
    call amplitudes(a, b, w, lp, eloc)
    p = exp(2*(lp%re - maxval(lp%re))); p = p/sum(p)
    Eav = sum(p*eloc); Obav = 0; Owav = 0; ObE = 0; OwE = 0
    do idx = 0, 2**n-1
      do jj = 1, n; s(jj) = ibits(idx, jj-1, 1); end do
      th = conjg(cmplx(b, kind=dp)) + matmul(cmplx(w, kind=dp), s)
      Ob = exp(th)/(1 + exp(th))
      Obav = Obav + p(idx)*conjg(Ob); ObE = ObE + p(idx)*conjg(Ob)*eloc(idx)
      do jj = 1, n
        Owav(:,jj) = Owav(:,jj) + p(idx)*conjg(Ob)*s(jj); OwE(:,jj) = OwE(:,jj) + p(idx)*conjg(Ob)*s(jj)*eloc(idx)
      end do
    end do
    Fb = ObE - Obav*Eav
    Fw = OwE - Owav*Eav
  end subroutine forces

end program gradcheck
