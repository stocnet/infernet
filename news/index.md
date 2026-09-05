# Changelog

## infernet 0.1.1

### Package

- Updated `DESCRIPTION`
  - Raised the R minimum to 4.1.0, since the examples use the native
    pipe
  - Pinned the minimum versions of
    [manynet](https://stocnet.github.io/manynet/) and
    [netrics](https://stocnet.github.io/netrics/)
- Updated CONTRIBUTING to document the architecture and the house
  conventions
- Added `README.Rmd` and `pkgdown/_pkgdown.yml` for this package
- Improved the loading messages
- Updated the Github Actions workflows
  - Added the PR metadata checks for the version bump and the PR title
    and body
  - Release notes are now taken from the matching `NEWS.md` section
  - Updated the action versions in `prchecks` and `pushrelease`
- Improved console messaging to use the `snet_*()` wrappers from
  [manynet](https://stocnet.github.io/manynet/)
  - Informational output is now silent by default, and follows
    `snet_verbosity`
  - Errors name what is missing and what is available
  - Added `thisRequires()`, which names the install command for a
    suggested package

### Tests

- Improved
  [`test_permutation()`](https://stocnet.github.io/infernet/reference/tests.md)
  by dropping two unused computations
- Updated the `tests` documentation to describe
  [`test_configuration()`](https://stocnet.github.io/infernet/reference/tests.md)
- Updated the examples to use the native pipe `|>`

### Regression

- Added a test suite for the regression engine, in four files
  - `test-qap_estimators.R` compares each estimator’s baseline against
    the equivalent [`lm()`](https://rdrr.io/r/stats/lm.html),
    [`glm()`](https://rdrr.io/r/stats/glm.html), `MASS`, `pscl`, `lme4`,
    or `fixest` fit
  - `test-qap_shapes.R` counts the dyads that reach the model for a
    directed, an undirected, a two-mode, a pooled, and a partly missing
    network
  - `test-qap_reproducibility.R` fixes the seed contract, sequential and
    parallel
  - `test-qap_control.R` covers the control list and the null-hypothesis
    choice
  - `helper-infernet.R` holds the seeded fixtures and
    `expect_qap_shape()`
- Fixed a two-mode network being read as a square one
  - An 18x14 incidence matrix produced 306 dyads rather than 252
  - `RMPerm()` now permutes the rows and the columns of a rectangular
    matrix independently, rather than erroring on the shorter side
  - [`dist()`](https://rdrr.io/r/stats/dist.html) and `sim()` now read
    each mode separately, as `ego()` and `alter()` already did
- Fixed an undirected network contributing each dyad twice, shrinking
  the standard error
- Fixed `family = "zip"` failing during permutation
  - Several estimators returned backticked coefficient names, which
    double semi-partialling could not look up
  - Coefficient names are now cleaned on the one path every estimator
    takes
- Fixed the [fixest](https://lrberge.github.io/fixest/) path reporting
  two intercepts where none was absorbed
- Fixed crossed sender and receiver intercepts aborting the run
  - Residualising falls back to no random intercepts, with a warning,
    where the mixed fit is singular
- Fixed `use_gpu = TRUE` aborting where
  [torch](https://torch.mlverse.org/docs) or CUDA is unavailable
- Improved `control` to reject a name it does not take, and offer the
  nearest
- Improved the permutation loop to hold back a fitter’s convergence
  warnings
  - These printed once per draw; the count of failed draws is still
    reported
- Improved `lower`, `larger`, and `abs` to carry the same row names
  under both null hypotheses
- Improved `HC3()` by dropping a
  [`gc()`](https://rdrr.io/r/base/gc.html) call that ran once per
  permutation
- Improved the missing-predictor error to name the sender, receiver, and
  network indices that a formula can also use
- Fixed `tertius()` rejecting a quoted summary function
  - `tertius(x, "mean")`, the documented spelling, now works alongside
    `tertius(x, mean)`
- Added tests for the `tertius()` spellings and for the
  missing-attribute error
- Fixed `R/model_regression.R`, which a bad merge left unable to parse
  - Restored `.default_control()`, `.is_list_of_graphs()`, and the head
    of `.prepare_list_of_graphs()`
  - Removed `vectorise_list()`, and the copies of `logit_moments()` and
    `logit_resid()` that duplicate `R/qap_gmm.R`

## infernet 0.1.0

### Package

- Initialised package
