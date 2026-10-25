# infernet 0.4.0

## Package

- Updated CONTRIBUTING with formula conventions for the stocnet packages
- Removed `{purrr}` from `Imports`, since the new formula parser does not need it

## Regression

- Improved the formula of `net_regression()`, which is now parsed into a specification
  - Added named arguments to formula terms, e.g. `indeg(friends, type = "ego")`
  - Added interactions of any order, e.g. `ego(age) * alter(age) * ego(tenure)`
  - Added `info$focal` as the outcome of `. ~`, and a named outcome still takes priority
  - Added a clear error for a bar in the formula, which never reached a model,
    pointing to the random intercepts in `control`
- Added formula terms from goldfish, computed on another layer
  - Added `exchange()` and `recip()`, the reverse tie in another layer
  - Added `recip` on its own, or `recip()`, as the outcome's reverse tie, as goldfish reads it, with a warning
  - Added `indeg()` and `outdeg()`, a node's degree on the receiver's or the sender's side
  - Added `tertius_diff()`, how far ego lies from the receiver's other in-neighbours
  - Added `tie()`, a layer as a dyadic covariate, 0/1 unless `weighted = TRUE`
  - Added `global()`, a network-level variable, in a list of networks
- Added formula terms from ergm for categorical attributes
  - Added `match()`, one dummy for each category that sender and receiver share
  - Added `mix()`, one dummy for each pair of categories, less a reference pair
- Renamed `dist()` to `diff()`, as goldfish calls it
  - Added `transformer_fn =`, so `diff(x, transformer_fn = identity)` is signed
  - `dist()` still works, with its old column name, and warns that it is deprecated
- Fixed `tertius()` with `"mean"` dividing by the number of nodes rather than
  of in-neighbours, which mixed the receiver's in-degree into the attribute
  - It is now the mean over the receiver's other in-neighbours, so results change
  - A receiver with no other in-neighbour takes the mean of the defined cells
  - Added a layer as its first argument, e.g. `tertius(friends, age)`
  - `tertius(age)` still reads the outcome's own ties, and now warns that
    permuting the outcome does not permute this predictor with it
- Fixed a layer term reading the outcome's own ties: `recip()`, `indeg()`,
  `outdeg()`, and `tie()` refuse the outcome layer
- Fixed `ego()` and `alter()` of a categorical attribute returning NA p-values
  under `permute = "predictor"`, since the fitter dummy-coded the attribute and
  left nothing numeric to residualise
  - They are dummy-coded again, one column per category less the first, as in `{migraph}`
  - Each column names its reference category, e.g. `ego Discipline Sociology [Anthropology]`,
    and a message names it too

## Results

- Added `tidy()` and `glance()` methods for `netreg` and `network_test` results
  - `tidy()` gives the permutation p-value that `alternative` names, two-sided by default
  - Moved from `{migraph}` the methods for `netlm`, `netlogit`, `ergm`, and `sienaFit`, and the print methods for `netlm` and `netlogit`
  - Fixed `tidy()` on a `netlm` or `netlogit` ignoring `conf.level`
  - Fixed `glance()` on an `ergm` adding the MCMC columns when it says they are omitted
- Moved `print()`, `tidy()`, and `glance()` to one help page, `?results`
- Renamed the observed test statistics of a `netreg` from `t` to `obs`
  
## Tutorials  
  
- Added the "Diversity and Regression" tutorial from `{migraph}`, now teaching `{infernet}`
  - Replaced `over_membership()`, which stays in `{migraph}`, with `to_subgraphs()`

# infernet 0.3.0

## Package

- Added design principles to the README and the contributing guide
- Raised the minimum version of `{manynet}` to 2.4.0, for its CSS construct

## Regression

- Added cognitive social structures to `net_regression()`
  - A CSS is detected from the `by` column of its ties, not declared
  - Each reported dyad is one observation: sender, receiver, and reporter
  - A permutation relabels senders, receivers, and reporters together
  - Added `random_intercept_reporter` to `control`
  - `tertius()` is computed within each report
  - One-mode, two-mode, undirected, and pooled CSS networks all fit
  - Removed `df_to_mat()`, since `{manynet}` now builds CSS arrays
- Added a warning naming each reporter whose report holds no ties
- Added layer names to the formula, e.g. `advice ~ friends + reports`
- Fixed a dyadic covariate named on its own, as in `y ~ reports`
- Fixed a list of `stocnet` networks being read as one network
- Fixed the CSS shape assuming one reporter per node and a square array
- Improved the CSS print method to say how many reporters it was fitted on
- Renamed the classes of a `net_regression()` result to a hierarchy under `netreg`
  - `c("netreg_lm", "netreg")` for a gaussian model, `c("netreg_glm", "netreg")`
    for any other family, and `"netreg_css"` in front for a CSS
  - Replaces `net_regression`, `QAPRegression`, `QAPGLM`, and `QAPCSS`
  - Each specific class prints through its own method
- Fixed `print.QAPConfusionMatrix()` not being registered as an S3 method
  - `print(fit$confusion_matrix)` printed a raw list from the console

# infernet 0.2.0

## Package

- Branching off five model extensions reduces `Suggests` packages from eight to three
- Updated CONTRIBUTING with the vocabulary table and the reporting rule

## Regression

- Fixed `net_regression()` failing on a two-mode network with more columns than
  rows (closes #4)
  - Validity was built rows-by-rows, so wider predictor extended it with `NA` 
    and the dyad count came back as `NA`
  - Reported 448 by 12489 network now fits on all 5,595,072 dyads
- Fixed `groups=` being refused on a two-mode network unless it matched the row
  mode, though either mode may be the one that is blocked
- Standardised vocabulary in the engine to match the front end:
  - Renamed `reps=` to `times=` including on the returned fit
  - Renamed `method=`/`nullhyp=` to `permute=`, as method can be ambiguous
    - `method = "qap"`/`nullhyp = "qapspp"` is now `permute = "predictor"`
    - `method = "qapy"` is now `permute = "outcome"`
  - Renamed `mode=` to `directed=`, reserving mode for one-mode and two-mode networks
    - `mode = "undirected"` is now `directed = FALSE`
  - `data` is retired as potentially confusing:
    - `.data` remains the network the user passes in
    - `matlist` is the named list of matrices the engine fits
    - `net` is one coerced network, inside the formula front end
- Added `snet_info()` reporting of every default the model resolves for itself
  - Family chosen from the outcome's values
  - Directedness from the network
  - `permute = "predictor"` falling back to `"outcome"` with one predictor
- Merged `QAPglm()` and `QAPcss()` engines into one, `QAPengine()`
  - 55% the same code, reduces code from 791 lines to 552
  - Differences in treatment are now four functions: vectorisation, permutation, 
    returning residuals, and identifying random intercepts
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

## Tests

- Added a wide two-mode fixture and two regression tests for #4
- Added `test-qap_reporting.R`, which runs with `snet_verbosity = "verbose"`
- Added `test-qap_shape_css.R`, which fits a cognitive social structure

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
