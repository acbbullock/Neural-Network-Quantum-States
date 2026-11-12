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
