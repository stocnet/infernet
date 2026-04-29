# infernet 0.1.0

## Package

- Initialised package.

## New functions

- `test_random()`: Conditional uniform graph (CUG) test of any network-level
  statistic against a distribution of measures on random networks of the same
  size.
- `test_configuration()`: CUG test conditioning on the observed degree
  sequence.
- `test_permutation()`: Quadratic assignment procedure (QAP) test of any
  network-level statistic against a distribution of measures on permutations
  of the original network.
- `net_regression()`: Multiple regression QAP (MRQAP) with Dekker's double
  semi-partialling plus (`"qap"`) or permute-y-only (`"qapy"`) inference.
  Supports gaussian, binomial, Poisson, negative binomial, ZIP, and
  multinomial families; OLS, GLM, mixed models (lme4/glmmTMB), fixed effects
  (fixest), and GMM estimation; optional HC3 robust standard errors and
  GPU-accelerated batch OLS via torch.
- `net_regression_control()`: Ergonomic helper to build the `control = list()`
  argument of `net_regression()` with tab-completion and inline documentation.
- `net_from_edgelist()`: Convert a long-format edge list data frame into a
  named list of adjacency matrices suitable for use with `net_regression()`.
- `plot.network_test()`: Histogram of the permutation distribution with a
  vertical line at the observed statistic.
- `print.network_test()`, `print.net_regression()`: Formatted console output
  for test and regression objects.

## Design decisions

- The formula DSL (`ego()`, `alter()`, `same()`, `dist()`, `sim()`,
  `tertius()`) is ported from migraph and provides a familiar interface for
  manynet users.
- The QAP engine is ported from the MrQAP package (Robert W. Krause).
- Parallelism uses `future.apply::future_lapply()` throughout (no furrr
  dependency), controlled by the `strategy` parameter.
- Specification advice (suggesting supplementary ego/alter terms when using
  `sim()` or `same()`) can be silenced with
  `options(infernet_advice = FALSE)`.
- The CSS (3D-array) engine is ported from MrQAP but not yet wired to
  `net_regression()`; it will be exposed via `control = list(css = TRUE)` once
  manynet provides a stable 3D network representation.

## Dependencies

- Removed `furrr` dependency (superseded by `future.apply`).
- Added `graphics` to Imports (for `plot.network_test()`).
- Requires `manynet >= 1.0.0`.
