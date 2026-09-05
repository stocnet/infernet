# infernet 0.2.0

## Package

- Merged the two engines into one, `QAPengine()`
  - `QAPglm()` and `QAPcss()` were 55% the same code, so every fix had to be
    made twice; one of them was made in only one place
  - What the two shapes do differently is now four functions in
    `R/qap_shapes.R`: how to vectorise, how to permute, how to put a
    residualised predictor back, and which random intercepts exist
  - A random intercept a shape does not have now aborts by name, so a
    perceiver intercept on a dyadic network says so
  - The engine files fall from 791 lines to 552, with no duplication left
- Branched off five model extensions, to settle the architecture first
  - Each is on its own `feature/*` branch, and each strip is one commit that
    `git revert` reinstates
  - `Suggests` falls from eight modelling packages to three
  - See the Github issues for the order they come back in
- Renamed the engine's vocabulary to the front end's, so one word means one
  thing on both sides of the seam
  - `reps` is now `times`, everywhere including on the returned fit
  - `mode` is now `directed`, a logical, and `"digraph"`/`"graph"` are gone;
    `mode` is reserved for a nodeset, as in one-mode and two-mode
  - `nullhyp` is now `permute`, and its values name what is shuffled:
    `"predictor"` for Dekker's double semi-partialling, `"outcome"` for
    permuting the dependent matrix alone
  - `data` is retired as an identifier: it named the network in one half of
    `R/model_regression.R` and the matrix list in the other, one letter away
    from `.data`
    - `matlist` is the named list of matrices the engine fits
    - `net` is one coerced network, inside the formula front end
    - `.data` remains the network the user passes in
- Updated CONTRIBUTING with the vocabulary table and the reporting rule

## Regression

- Removed the `torch` GPU path (`feature/torch-gpu`)
  - Gaussian only, duplicated for CSS, no test, and no hosted runner has a
    CUDA device; `{torch}` in Suggests broke the CI build
- Removed the `gmm` estimator and the `estimator` control (`feature/gmm-estimator`)
  - It warned that the coefficient covariance matrix was singular on every
    family, on well-conditioned data
- Removed the mixed negbin and mixed zip paths (`feature/glmmtmb-mixed`)
  - `{glmmTMB}` carries 62 recursive dependencies and must match `{TMB}`
  - The standard `negbin` and `zip` paths are unaffected
- Removed `family = "multinom"` and the `comparison`/`reference` controls
  (`feature/multinomial-comparison`)
  - Unreachable from the front end, and its pairwise branch forked both
    engines at 21 points
- Removed the `fixest_se_cluster` control (`feature/fixest-fixed-effects`)
  - A bar in the formula now means an `{lme4}` random-effect term, and
    nothing else; `parse_qap_formula()` drops from three branches to one
- Fixed `net_regression()` failing on a two-mode network with more columns than
  rows (closing #4)
  - The validity mask was built as rows-by-rows, so a wider predictor extended
    it with `NA` and the dyad count came back as `NA`
  - The reported 448 by 12489 network now fits, on all 5,595,072 dyads
- Renamed the `method` control to `permute`
  - `method = "qap"` is now `permute = "predictor"`, and `method = "qapy"` is
    now `permute = "outcome"`
- Renamed the `mode` control to `directed`
  - `mode = "undirected"` is now `directed = FALSE`
- Added reporting of every default the model resolves for itself
  - The family chosen from the outcome's values
  - The directedness read from the network
  - `permute = "predictor"` falling back to `"outcome"` with one predictor
  - These use `snet_info()`, so `options(snet_verbosity = "verbose")` shows them

## Tests

- Added `test-qap_shape_css.R`, which fits a cognitive social structure through
  the merged engine
  - `net_regression()` has no CSS entry point yet, so the merge would
    otherwise be untested on the shape it was merged for
- Added a wide two-mode fixture and two regression tests for #4
- Added `test-qap_reporting.R`, which runs with `snet_verbosity = "verbose"`
  - Informational output is silent in every other test, so a message that
    `{cli}` cannot parse was invisible until it aborted; two shipped that way

# infernet 0.1.1

## Package

- Updated `DESCRIPTION`
  - Raised the R minimum to 4.1.0, since the examples use the native pipe
  - Pinned the minimum versions of `{manynet}` and `{netrics}`
- Updated CONTRIBUTING to document the architecture and the house conventions
- Added `README.Rmd` and `pkgdown/_pkgdown.yml` for this package
- Improved the loading messages
- Updated the Github Actions workflows
  - Added the PR metadata checks for the version bump and the PR title and body
  - Release notes are now taken from the matching `NEWS.md` section
  - Updated the action versions in `prchecks` and `pushrelease`
- Improved console messaging to use the `snet_*()` wrappers from `{manynet}`
  - Informational output is now silent by default, and follows `snet_verbosity`
  - Errors name what is missing and what is available
  - Added `thisRequires()`, which names the install command for a suggested package

## Tests

- Improved `test_permutation()` by dropping two unused computations
- Updated the `tests` documentation to describe `test_configuration()`
- Updated the examples to use the native pipe `|>`

## Regression

- Added a test suite for the regression engine, in four files
  - `test-qap_estimators.R` compares each estimator's baseline against the
    equivalent `lm()`, `glm()`, `MASS`, `pscl`, `lme4`, or `fixest` fit
  - `test-qap_shapes.R` counts the dyads that reach the model for a directed,
    an undirected, a two-mode, a pooled, and a partly missing network
  - `test-qap_reproducibility.R` fixes the seed contract, sequential and parallel
  - `test-qap_control.R` covers the control list and the null-hypothesis choice
  - `helper-infernet.R` holds the seeded fixtures and `expect_qap_shape()`
- Fixed a two-mode network being read as a square one
  - An 18x14 incidence matrix produced 306 dyads rather than 252
  - `RMPerm()` now permutes the rows and the columns of a rectangular matrix
    independently, rather than erroring on the shorter side
  - `dist()` and `sim()` now read each mode separately, as `ego()` and
    `alter()` already did
- Fixed an undirected network contributing each dyad twice, shrinking the standard error
- Fixed `family = "zip"` failing during permutation
  - Several estimators returned backticked coefficient names, which double
    semi-partialling could not look up
  - Coefficient names are now cleaned on the one path every estimator takes
- Fixed the `{fixest}` path reporting two intercepts where none was absorbed
- Fixed crossed sender and receiver intercepts aborting the run
  - Residualising falls back to no random intercepts, with a warning, where the
    mixed fit is singular
- Fixed `use_gpu = TRUE` aborting where `{torch}` or CUDA is unavailable
- Improved `control` to reject a name it does not take, and offer the nearest
- Improved the permutation loop to hold back a fitter's convergence warnings
  - These printed once per draw; the count of failed draws is still reported
- Improved `lower`, `larger`, and `abs` to carry the same row names under both
  null hypotheses
- Improved `HC3()` by dropping a `gc()` call that ran once per permutation
- Improved the missing-predictor error to name the sender, receiver, and
  network indices that a formula can also use
- Fixed `tertius()` rejecting a quoted summary function
  - `tertius(x, "mean")`, the documented spelling, now works alongside
    `tertius(x, mean)`
- Added tests for the `tertius()` spellings and for the missing-attribute error
- Fixed `R/model_regression.R`, which a bad merge left unable to parse
  - Restored `.default_control()`, `.is_list_of_graphs()`, and the head of
    `.prepare_list_of_graphs()`
  - Removed `vectorise_list()`, and the copies of `logit_moments()` and
    `logit_resid()` that duplicate `R/qap_gmm.R`

# infernet 0.1.0

## Package

- Initialised package
