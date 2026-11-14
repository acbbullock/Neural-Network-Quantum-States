# Numerical experiments

This directory holds the code and results of the tests behind the training fixes in v2.0.0 (#10, #12). It also has
the tests of the disordered phase in issue #11. It isn't part of the program, and fpm in the project root doesn't
build it.

## Layout

- `ablation/` is the 1.0.5 code with a preprocessor switch for each fix, so any combination can be built.
  - `app/main.f90` trains one machine and prints a `RESULT` line. The line gives the Monte Carlo energy and its
    standard error, and the exact energy of the chain. For $n \le 16$ it also gives the exact energy of the
    trained state, summed over all $2^n$ configurations.
  - `app/gradcheck.f90` compares the forces for $b$ and $w$ with finite differences of the exact energy. It is how
    the uphill step for $\mathrm{Im}\,b$ was found.
  - `build.sh` builds the variants into `bin/`.
  - `run1.sh` runs one job.
  - `summarize.py` tabulates a results file.
  - `jobsN.txt` and `resultsN.txt` hold the inputs and outputs of each experiment. Experiments 8 and 9 were started
    without a jobs file; their configurations can be read from the results lines.
- `fixed/` is the fixed code as of #12, with the same command line and `RESULT` line. Its variant is called
  `clean`.
- `v2/` is the released v2.0.0 code with switches that put each defect back, run the full $S$, or drop the sign
  gauge. The teaching paper (branch `paper`, `paper/main.tex`) cites it for its ablation table, its comparison of the
  full and block-diagonal $S$, and the antiferromagnet without the gauge. See [v2.0.0 experiments](#v200-experiments).

## Switches and variants

| Switch | Fix |
|---|---|
| `FIX_SAMPLER` | One continuous Markov chain, burn-in $5n$, $n$ steps between samples |
| `FIX_THETA` | Recompute $\theta$ from the current parameters |
| `FIX_BCONJ` | Conjugate the step for $b$ |
| `FIX_ADAM` | $\lvert x\rvert^2$ in Adam's second moment |
| `FIX_ELOC` | Keep $\mathrm{Im}\,E_\mathrm{loc}$ in the forces |
| `FIX_STABLE` | Overflow-free $\ln(1+e^\theta)$ and sigmoid |
| `NO_ADAM` | Plain stochastic reconfiguration |
| `NO_KICK` | No weight kick |
| `FULL_SR` | Full stochastic reconfiguration through the $N_s \times N_s$ system (Woodbury), instead of the block-diagonal $S$ |

| Variant | Switches |
|---|---|
| `base` | none (the 1.0.5 code) |
| `theta` | `FIX_THETA` |
| `all` | `FIX_THETA FIX_ADAM FIX_ELOC FIX_BCONJ` |
| `all5` | `all` + `FIX_STABLE` |
| `all6` | `all5` + `FIX_SAMPLER` |
| `samp` | `FIX_STABLE FIX_SAMPLER` |
| `sr` | `all6` + `NO_ADAM` |
| `srnk` | `sr` + `NO_KICK`: every fix |
| `srnk_dp` | `srnk` in double precision |
| `fsr` | `srnk` + `FULL_SR` |
| `nk_only` | `NO_ADAM NO_KICK` alone |
| `no_SAMPLER`, `no_THETA`, `no_BCONJ`, `no_ELOC`, `no_STABLE` | `srnk` without that fix |
| `clean` | the code in `fixed/` |

## Running

From `ablation/`, with fpm 0.13 as `fpm-13`, gfortran and LAPACK:

```bash
./build.sh                   # every variant, or name some: ./build.sh srnk fsr clean
mkdir -p runs
./run1.sh srnk 64 16 0.5 0.1 1000 256 1 0          # variant n m J B epochs samples seed early_exit [dtau]
xargs -P 6 -L 1 ./run1.sh < jobs7.txt > my7.txt    # a whole experiment, six jobs at a time
python3 summarize.py my7.txt
```

`summarize.py` prints one row per configuration. It gives the median relative error of the Monte Carlo energy and
its range over seeds, the error of the exact state energy for small $n$, and the number of NaN runs. `se/spread`
is the reported standard error divided by the spread over seeds; a value far below 1 means the error bars are too
small. It also gives the largest drift of the cached $\theta$.

## Experiments

| File | What it tests |
|---|---|
| `results1.txt` | `base`, `theta`, `all` with 16 samples: the fixes to $\theta$, Adam, $E_\mathrm{loc}$ and $b$, without the sampler fix |
| `results3.txt` | `samp`, `all5`, `all6` with 16 and 256 samples |
| `results4.txt` | `all6` against `sr` (Adam removed), and $\Delta\tau$ of 0.01, 0.05 and 0.1 |
| `results5.txt` | `srnk` (kick removed) and `srnk_dp` (double precision) |
| `results6.txt` | `fsr` against `srnk` at the critical point $B = 0.5$, and 1024 spins with `base` and `srnk` |
| `results7.txt` | Leave-one-out ablation: `nk_only` and each `no_*` |
| `results8.txt` | `clean`, the fixed code, with 16 and 256 samples |
| `results9.txt` | The disordered phase, $B = 0.8$ and $0.95$: `clean` with more epochs, larger $\Delta\tau$ and 64 hidden units, and `fsr` |

All runs use $J = -0.5$ except `results9.txt`, which uses $J = 0.5$; the energy is the same for either sign.

## v2.0.0 experiments

`v2/src/nnqs.F90` is `src/nnqs.f90` of v2.0.0 with these additions. With no switch defined it runs the released
algorithm: seed 1 of $n = 64$, $m = 16$, $B = 0.8$ gives the same energy, to the last digit, as `clean`.

- Run settings (`cfg_max_epochs`, `cfg_num_samples`, `cfg_dtau`) and the results the driver prints. A NaN energy ends
  training instead of stopping the program, so that the run still prints its `RESULT` line.
- The switches:

| Switch | Effect |
|---|---|
| `BREAK_SAMPLER` | The 1.0.5 sampler: burn-in $2001 - 2\,\mathrm{epoch}$, every sample restarted from the same configuration, $2n - \min(2\,\mathrm{epoch}, 2n-1)$ steps per sample |
| `BREAK_THETA` | $\theta$ carried over from the previous parameters instead of recomputed |
| `BREAK_BCONJ` | Step for $b$ not conjugated |
| `BREAK_ELOC` | $\mathrm{Im}\,E_\mathrm{loc}$ dropped from the forces |
| `BREAK_STABLE` | $\ln(1+e^z)$ and $e^z/(1+e^z)$ in their overflowing forms |
| `FULL_SR` | The full $S$ instead of the block-diagonal $S$, with the same $\delta = 10^{-3}$. Every complex parameter is split into its real and imaginary parts, so the step is a real $2N \times 2N$ solve (Woodbury) |
| `NO_GAUGE` | Train on the signed $J$, with no sublattice flip, and no staggering of the correlations |

Variants: `v2` (no switch), `no_SAMPLER`, `no_THETA`, `no_BCONJ`, `no_ELOC`, `no_STABLE` (one switch each), `none`
(all five `BREAK_*`), `v2_dp` (`v2` in double precision), `fsr` (`FULL_SR`) and `nogauge` (`NO_GAUGE`).

`app/main.f90` is the driver of `fixed/` with `nnqs n m J B epochs samples seed [dtau]` (no early exit). For
$n \le 16$ it also prints the exact energy of the trained state, using the signed $J$ for `nogauge`.

From `v2/`, with fpm 0.13 as `fpm-13`, gfortran and LAPACK:

```bash
./build.sh                                         # every variant, or name some: ./build.sh v2 fsr
./runall.sh                                        # jobs_long, jobs_fullsr64 and jobs_ablation_small into results.txt
xargs -P 6 -L 1 ./run1.sh < jobs_gauge.txt > results_gauge.txt
python3 summarize.py results.txt
```

All runs use 1000 epochs, 256 samples, $\Delta\tau = 0.01$, one image, gfortran 16.2 and single precision (except
`v2_dp`). `summarize.py` prints one row per configuration: the median relative error of the final Monte Carlo
energy and its range over seeds, the median error of the exact energy of the trained state for $n \le 16$, how many
runs stopped with an error (`FAIL`), ended in NaN, or ended below $E_0$, and the median standard error divided by the
spread over seeds.

| File | What it tests |
|---|---|
| `jobs_ablation_small.txt`, part of `jobs_long.txt` | Leave-one-out ablation at $J = -0.5$, $B = 0.1$ and $0.5$: $n = 12$, $m = 12$ (10 seeds), $n = 64$, $m = 4$ and $n = 256$, $m = 16$ (5 seeds) |
| `jobs_fullsr64.txt`, part of `jobs_long.txt` | `fsr` against `v2` at $J = 0.5$: $n = 64$, $m = 16$ at seven fields (5 seeds), $n = 256$, $m = 16$ at $B = 0.1, 0.5, 0.8$ (3 seeds) |
| `jobs_gauge.txt` | `nogauge` at $J = -0.5$, $B = 0.1$: $n = 12$ (10 seeds), $n = 64$, $m = 16$ (5 seeds, with `v2` for comparison), $n = 256$, $m = 16$ (3 seeds) |

Since `v2` uses only $|J|$, its runs at $J = 0.5$ and $J = -0.5$ with the same seed are identical.
