program experiment
  !! Usage: nnqs n m J B epochs samples seed early_exit
  !! Trains one machine and prints a RESULT line with the Monte Carlo energy, the exact ground-state energy, and
  !! (for n <= 16) the exact energy of the trained machine, summed over all 2^n configurations.
  use, intrinsic :: iso_fortran_env, only: rk=>real32, dp=>real64, ik=>int8
  use nnqs, only: RestrictedBoltzmannMachine, cfg_max_epochs, cfg_num_samples, cfg_early_exit, cfg_quiet, &
                  last_energy, last_stderr, last_epoch, max_theta_drift, cfg_dtau
  implicit none (type,external)

  interface
    subroutine dsterf(n, d, e, info)
      import :: dp
      integer,  intent(in)    :: n
      real(dp), intent(inout) :: d(*), e(*)
      integer,  intent(out)   :: info
    end subroutine dsterf
  end interface

  type(RestrictedBoltzmannMachine) :: psi
  integer  :: n, m, seed, k, i, early
  real(rk) :: J, B
  real(dp) :: e0, e_state
  integer, allocatable :: put(:)
  character(len=64) :: arg

  call get_command_argument(1, arg); read(arg,*) n
  call get_command_argument(2, arg); read(arg,*) m
  call get_command_argument(3, arg); read(arg,*) J
  call get_command_argument(4, arg); read(arg,*) B
  call get_command_argument(5, arg); read(arg,*) cfg_max_epochs
  call get_command_argument(6, arg); read(arg,*) cfg_num_samples
  call get_command_argument(7, arg); read(arg,*) seed
  call get_command_argument(8, arg); read(arg,*) early
  cfg_early_exit = (early /= 0)
  if (command_argument_count() >= 9) then
    call get_command_argument(9, arg); read(arg,*) cfg_dtau
  end if
  cfg_quiet = .true.

  call random_seed(size=k); allocate(put(k))
  put = [(1000003*seed + 7919*i + 12345, i = 1, k)]
  call random_seed(put=put)

  psi = RestrictedBoltzmannMachine(v_units=n, h_units=m)
  call psi%stochastic_optimization(ising_params=[J, B])

  e0 = exact_ground(n, real(abs(J), dp), real(abs(B), dp))
  e_state = huge(1.0_dp)
  if ( n <= 16 ) e_state = exact_energy(psi, n, real(abs(J), dp), real(abs(B), dp))

  write(*, '(a,i0,a,i0,a,i0,a,i0,a,f14.6,a,f10.6,a,f14.6,a,f14.6,a,es10.3,a,es10.3,a,es10.3)') &
    'RESULT n=', n, ' m=', m, ' seed=', seed, ' epochs=', last_epoch, ' E_mc=', last_energy, ' se=', last_stderr, &
    ' E_state=', e_state, ' E0=', e0, ' relerr_mc=', (last_energy - e0)/abs(e0), &
    ' relerr_state=', merge((e_state - e0)/abs(e0), -1.0_dp, n <= 16), ' drift=', max_theta_drift

contains

  real(dp) function exact_ground(n, J, h) result(e)
    !! -sum of singular values of the bidiagonal matrix with diagonal h and superdiagonal J,
    !! from the eigenvalues of its Gram matrix, which is tridiagonal.
    integer,  intent(in) :: n
    real(dp), intent(in) :: J, h
    real(dp) :: d(n), off(max(n-1,1))
    integer  :: info
    d = h**2 + J**2; d(1) = h**2
    off = h*J
    call dsterf(n, d, off, info)
    if (info /= 0) error stop 'dsterf failed'
    e = -sum(sqrt(max(d, 0.0_dp)))
  end function exact_ground

  real(dp) function exact_energy(psi, n, J, h) result(e)
    !! <psi|H|psi>/<psi|psi> with H = -J sum sz sz - h sum sx, summed over every configuration
    type(RestrictedBoltzmannMachine), intent(in) :: psi
    integer,  intent(in) :: n
    real(dp), intent(in) :: J, h
    complex(dp), allocatable :: lp(:)
    integer(ik) :: s(n)
    integer     :: idx, jj, nconf
    real(dp)    :: w, wsum, lpmax, sig(n)
    complex(dp) :: eloc, esum

    nconf = 2**n
    allocate(lp(0:nconf-1))
    do idx = 0, nconf-1
      do jj = 1, n
        s(jj) = int(ibits(idx, jj-1, 1), ik)
      end do
      lp(idx) = cmplx(psi%log_amplitude(s), kind=dp)
    end do
    lpmax = maxval(lp%re)
    wsum = 0; esum = 0
    do idx = 0, nconf-1
      do jj = 1, n
        sig(jj) = 1.0_dp - 2.0_dp*ibits(idx, jj-1, 1)
      end do
      eloc = -J*sum(sig(1:n-1)*sig(2:n))
      do jj = 1, n
        eloc = eloc - h*exp(lp(ieor(idx, 2**(jj-1))) - lp(idx))
      end do
      w = exp(2.0_dp*(lp(idx)%re - lpmax))
      wsum = wsum + w; esum = esum + w*eloc
    end do
    e = real(esum/wsum, dp)
  end function exact_energy

end program experiment
