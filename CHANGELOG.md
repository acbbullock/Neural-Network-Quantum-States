# Changelog

## [2.0.0] - 2026-11-12

The training now reaches the exact ground state. On the README's chains, 1024 spins with $J = -0.5$ and 1000 spins
with $J = 0.5$, both at $B = 0.1$, the energy ends within 0.003% of the exact value, against about 0.8% before. The
project also builds with any Fortran 2018 compiler and any LAPACK, on Linux, macOS and Windows.

### Fixed

- The sampler ran a short chain from the same configuration every time, with a burn-in and spacing that shrank
  with each epoch to almost nothing. It now runs one continuous Markov chain with a fixed burn-in of $5n$ steps and
  $n$ steps between samples. The old sampler gave energies below the exact ground state, error bars of zero, and
  trained toward the classical all-up state.
- $\theta$ is recomputed from the current parameters each epoch. It was cached and never updated.
- The step for the hidden biases is conjugated, since they enter $\psi$ as $\bar b$. The imaginary part of $b$
  was moving uphill.
- The forces keep the imaginary part of the local energy.
- $\ln(1 + e^\theta)$ and the sigmoid are evaluated without overflow, which caused NaN energies on long chains.
- Training runs for all its epochs. It stopped early once 99.99% of the sampled spins were up, which is the
  classical state rather than the ground state.
- The biases are averaged across coarray images as well as the weights, so the images no longer drift apart.
- The reported wall time is no longer negative with gfortran.

### Changed

- Plain stochastic reconfiguration replaces stochastic reconfiguration with Adam, which trapped long chains in the
  classical state.
- The weight "kick" applied when the error bars were small is removed. It destroyed the converged state.
- Each epoch draws 256 samples on each image instead of 16.
- The demo program takes the chain as arguments: `fpm run -- spins hidden_units J B`.
- The build uses LAPACK's Fortran 77 interface, linked as `lapack` and `blas`, instead of Intel's LAPACK95 module.
  Coarrays are optional, turned on by defining `NNQS_COARRAY`.
- The data and figures are regenerated with the fixed training. The correlations are stored gzipped, and
  `data/generate.sh` regenerates everything.

### Added

- CI with gfortran 15 and 16, ifx and flang on Linux, gfortran on macOS and Windows, and four coarray images with
  ifx and with gfortran and OpenCoarrays.
- A test suite, run with `fpm test`.
- `log_amplitude`, a public method that returns $\ln \psi(s)$ for a configuration $s$.
- `CITATION.cff`.

### Known issues

- In the disordered phase, $B > J$, the network stays too ordered, and its energy is 2% to 5% above the exact value
  ([#11](https://github.com/acbbullock/Neural-Network-Quantum-States/issues/11)).
- The error bars don't account for the autocorrelation of the Markov chain, so they are too small.

## [1.0.5] - 2026-11-11

An archival release of the project as it stood in March 2024, with the results shown in its README. It needs
Intel's toolchain; the [release notes](https://github.com/acbbullock/Neural-Network-Quantum-States/releases/tag/v1.0.5)
explain how to build it today.

[2.0.0]: https://github.com/acbbullock/Neural-Network-Quantum-States/compare/v1.0.5...v2.0.0
[1.0.5]: https://github.com/acbbullock/Neural-Network-Quantum-States/releases/tag/v1.0.5
