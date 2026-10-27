# infernet 0.6.0

## Package

- Updated CONTRIBUTING
  - Renamed `type =` of `indeg()` and `outdeg()` to `node =` in the formula dictionary and rule 7
  - Added the `reporter` terms to the formula dictionary

## Specification

- Renamed `type =` of `indeg()`/`outdeg()` to `node =`, as stocnet/goldfish#122 proposes (thanks @auzaheta)
  - `type =` still works, and warns once that it is deprecated
  - `outdeg()` reads the sender's outdegree by default, `node = "ego"`
  - `specify_mrqap()` and `specify_ergm()` read a goldfish formula's with `type =` as `node =`
  - `specify_dynam()` writes `node =` as goldfish's `type =`
- Added terms for the reporter of a CSS
  - Added `reporter(x, levels = NULL)` for the reporter's value of a node attribute
  - Added `reporter_ego` where the reporter is the sender
  - Added `reporter_alter` where the reporter is the receiver
  - Added `reporter_same(x, node =)` where the reporter shares a value with the `"ego"`/`"alter"`
  - These terms abort on a network that is not a CSS, the last 3 for undirected networks,
    and the last for a two-mode network 
  - `specify_dynam()` and `specify_ergm()` drop these terms
  - A `| reporter` fixed effect absorbs `reporter()`, and names it

## Regression

- Fixed `estimate_mrqap()` failing on an interaction with `{fixest}` fixed effects

# infernet 0.5.0

## Package

- Updated CONTRIBUTING
  - Added a formula dictionary with an MRQAP column and goldfish's effects
  - Added a rule on when a categorical term leaves out a reference
  - Added a rule on `levels =`
  - Added a rule on `ncores` before `strategy`, and on `loops` rather than `diag`
  - Added a rule that a simulation-heavy function takes its controls through `control =`
- Added `{fixest}` to `Suggests`, for fixed effects in models that are not gaussian
- Improved the tutorial test to cap each `times =` at 20, since it checks only that the code runs
- Added the diversity tutorial to the website, as a static article under "Tutorials"

## Tests

- Changed `test_random()`, `test_configuration()`, and `test_permutation()` to take `control =`
  - Reads `ncores`, `strategy`, and `seed` from a list or from `control_mrqap()`, and leaves the other controls alone
  - Takes the controls of a model whole, e.g. `ctrl <- control_mrqap(ncores = 4, seed = 1)` for both a model and a test
  - `ncores` sets the plan whatever `strategy` says, as in `control_mrqap()`
  - `strategy =` and `ncores =` given by name still work, and warn that they are now controls
  - Restores the `{future}` plan and `future.globals.maxSize` after the test
- Renamed the `diag` element of a test's result to `loops`
- Improved the speed of `test_permutation()` ~5x and `test_random()` ~3x on a stocnet
  - Draws from a `tbl_graph` made once, where the statistic gives the same value on it

## Specification

- Improved the term registry behind the formula
  - Added goldfish's effects, e.g. `inertia`, `trans`, `four`
  - Added each term's MRQAP status, and its goldfish, RSiena, and ergm names
  - Added the nearest term to the error for an unknown term
- Added `specify_mrqap()`, `specify_dynam()`, and `specify_ergm()` to move a formula between models
  - Reads the model a formula is for from `input =`, the formula's class, or its names
  - Rewrites each term with an equivalent, e.g. `nodematch("dept")` as `same(dept)`
  - Names each term that means something different, e.g. goldfish's `sim()`
  - Drops each term the model cannot fit, with one warning naming each
  - Aborts only where no term is left to fit
  - `estimate_mrqap()` reads every formula with `specify_mrqap()`
  - `specify_ergm()` with `.data` writes other terms as `edgecov()` of infernet's matrix
  - Records what the model makes of every term: it fits, adapts, interprets, warns about, drops, or refuses it
  - Added `tidy()` for the result, one row for each term, which fits nothing
  - `tidy()` of an MRQAP formula with `.data` names the columns `estimate_mrqap()` builds
- Named the other models in the term registry for the model: `dynam`, `saom`, and `ergm`
- Renamed `match(x)` to `same(x, diff = TRUE)`, as in ergm's `nodematch(x, diff = TRUE)`
  - `match()` still works, with its old column names, and warns that it is deprecated
  - `same(x, diff = TRUE)` names its columns for `same`, e.g. `same_dept_sales` rather than `match_dept_sales`
  - Aborts where `same()` names one attribute more than once, since `same(x, diff = TRUE)` adds up to `same(x)`
  - `mix()` stays its own term
- Added `levels =` to `ego()`, `alter()`, `same()`, and `mix()` to choose the categories that get a column
  - Takes names, e.g. `ego(dept, levels = c("sales", "hr"))`, or `mix(dept, levels = "sales -> hr")`
  - Takes positions, and negative positions to leave categories out, e.g. `levels = -1`
  - The categories left out are the reference together, and where every one is chosen, the first chosen is
  - An unknown name aborts and lists the categories
  - Warns where a term has more than ten categories and no `levels =`
  - `specify_mrqap()` reads ergm's `levels =`, and `levels2 =` of `nodemix()`; `specify_ergm()` writes them
  - `specify_dynam()` drops a term with `levels =`, which goldfish does not have yet
- Changed the order of the columns of `mix()` to ergm's, the sender's category changing fastest, 
  so that a position in `levels =` is one in ergm's `levels2 =`
- Added fixed effects after a bare bar to the `specify_*()` functions
  - `specify_ergm()` writes `| sender + receiver` as ergm's `sender` and `receiver`
  - `specify_dynam()` drops them

## Regression

- Added `control_mrqap()`, with a help page for every control of `estimate_mrqap()`
  - `estimate_mrqap()` reads a `control` list through it, so each value is checked before a fit
  - Converts old names, e.g. `nullhyp = "qapy"` to `permute = "outcome"`, with a warning
  - Drops goldfish's estimation options, e.g. `max_iterations`, with one warning that names each
  - Reads the result of goldfish's `set_estimation_opt()` whole
  - Fixed reading goldfish 2.0's options: `control = set_algorithm_newton()` and 
    `control = set_preprocessing()` now drop each option
  - Aborts on any other name, with the nearest control
- Renamed the `diag` control to `loops`, which says what it includes and does not mask `base::diag()`
  - `control = list(diag = TRUE)` still works, and warns that it is now `loops`
  - The fit stores it as `fit$loops`, not `fit$diag`
- Changed `ncores` to set the plan whatever `strategy` says
  - `ncores = 1` now runs sequentially, where before it left the plan to `strategy`
  - `ncores` comes before `strategy` in `control_mrqap()`, as the simpler of the two
- Added fixed effects to `estimate_mrqap()`, as in `{fixest}`: `y ~ x | sender + receiver`
  (thanks @RWKrause, closes #8)
  - Takes `sender`, `receiver`, `reporter` for a CSS, and `network` for a list
  - Absorbs them once in a gaussian model, so each permutation fits a plain `lm()`
  - Fits `fixest::feglm()` or `fixest::fenegbin()` in each permutation otherwise
  - Nests each node's group in its network in a pooled list, not in a panel
  - Names a predictor that the fixed effects absorb, e.g. `ego()` with `| sender`
- Fixed the sender and receiver random intercepts that a two-mode network dropped
- Improved the speed of a small fit about tenfold, by not searching the permutation function for globals
- Fixed the permutations of a network with missing data to use the dyads of the observed model
  - Permutes nodes only among nodes with the same unobserved cells
  - Keeps each node with scattered unobserved cells in place
  - Warns where few distinct permutations remain
  - Fixed residualised predictors to hold no raw values outside the model, and to be symmetric for undirected networks
  - Fixed a blocking group of one node, which `sample()` read as a range
- Added `control = list(missing = )` with `"available"` (the default) and `"fail"`
  - Leaves out the ties that `{manynet}` marks as `fixed`, and keeps them in place under permutation
  - Reports the dyads dropped on each variable, and stores them as `fit$missing`
  - Documented a structural zero as a limitation
- Added panel MRQAP for a longitudinal network
  - Detects a panel with `manynet::is_longitudinal()`, and fits each wave against the previous one
  - Permutes the nodes the same way in every wave
  - Fits `inertia`, `recip`, and `trans` on the previous wave
- Renamed `net_regression()` to `estimate_mrqap()` to match other stocnet packages
  - `net_regression()` still works, and warns once that it is deprecated
  - Result classes stay `netreg`, `netreg_lm`, `netreg_glm`, and `netreg_css`
- Added a term table to the fit, `fit$terms`, with the parts of each coefficient's name
  - Gives the `effect`, `layer`, `attribute`, `category`, and `reference` of each coefficient
  - Gives the `operands` of each interaction
  - Keeps a category as the data hold it, e.g. `"Social Science"` for `alter_dept_Social_Science`
  - Writes a `mix()` pair as `"sales -> hr"`, or `"sales -- hr"` where the pair is unordered
  - `tidy()` adds these columns to the estimates

# infernet 0.4.0

## Package

- Updated CONTRIBUTING with formula conventions for the stocnet packages
- Removed `{purrr}` from `Imports`
- Removed `{furrr}` from `Imports`: `{future.apply}` now maps the simulations of the test family too
- Removed `{reformulas}` from `Imports`: a few lines of base R drop the random-effect terms

## Regression

- Improved the formula of `net_regression()`, which is now parsed into a specification
  - Added named arguments to formula terms, e.g. `indeg(friends, type = "ego")`
  - Added interactions of any order, e.g. `ego(age) * alter(age) * ego(tenure)`
  - Added `info$focal` as the outcome of `. ~`, and a named outcome still takes priority
  - Added random intercepts to the formula, as in `{lme4}`: `(1 | sender)`, `(1 | receiver)`,
    `(1 | reporter)`, and `(1 | network)` set the same switches as their controls
    (thanks @auzaheta and @RWKrause, #8)
  - A bar never reached a model before: the grouping index it named was looked for among the layers
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
- Fixed a layer term reading the outcome's own ties: `indeg()`, `outdeg()`,
  and `tie()` refuse the outcome layer, and `recip()` warns
- Fixed `ego()` and `alter()` of a categorical attribute returning NA p-values
  under `permute = "predictor"`, since the fitter dummy-coded the attribute and
  left nothing numeric to residualise
  - They are dummy-coded again, one column per category less the first, as in `{migraph}`
  - A message names the reference category
- Changed the column names of formula terms to join their parts with underscores
  - For example, `ego_Age` rather than `ego Age`
  - A name needs no backticks in `coef(fit)[...]` or in `tidy()` output
  - Code that looked up a coefficient by its old name must use the new one

## Results

- Moved `print.netlm()` and `print.netlogit()` from `{migraph}`
- Added `tidy()` and `glance()` methods for `netreg` and `network_test` results
  - `tidy()` gives the permutation p-value that `alternative` names, two-sided by default
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
