# Contributing

Contributions to `infernet`, 
whether in the form of issue identification, bug fixes, new code or documentation 
are encouraged and welcome.

Please note that the `infernet` project is released with a 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). 
By contributing to this project, you agree to abide by its terms.

## Git

`stocnet` projects are maintained using the git version control system.
A plain-English introduction to git can be found [here](https://blog.red-badger.com/2016/11/29/gitgithub-in-plain-english).
I recommend you read this before continuing. 
A more recent motivation can be found [here](https://www.r-bloggers.com/2024/04/git-gud-version-control-best-practices/).
It will explain the basics of git version control, committing and repos, pulling and pushing,
branching and merging.

Using git from the command line on your lap- or desktop can be intimidating,
but I recommend [Fork](https://git-fork.com) software for Mac and Windows.
This allows mostly visual management of commits, diffs, branches, etc.
There are various other git software packages available, but this one is fairly fully featured.

The GitHub page allows to access the issues assigned to you and check the commits.
You can also access the documents in the repository, 
although this won't be necessary after you have cloned it on your computer via Fork.

### Identifying issues

Please use the issues tracker on GitHub to identify any function-related issues.
You can use these issues to track progress on the issue and
to comment or continue a conversation on that issue.
The most useful issues are ones that precisely identify an error,
or propose a test that should pass but instead fails.
Examples for documentation are also most welcome.

Issues that belong to another package in the family should be transferred there
rather than fixed here: `gh issue transfer <number> stocnet/<package>`.
See the division of labour below.

### Cloning

Once you have downloaded Fork, the first thing you have to do is to 
clone the remote repository on your computer. 
Before cloning, you will be able to choose on which `branch` you want to work: 
develop or main. 

### Pull

This command allows you to `pull` changes from the remote repository to your local repository.
Make sure you do that before starting working on your files so you have the newest versions. 
When pulling, make sure you choose main or develop, 
depending on the branch you decided to work with. 
Once you pulled, you have now all the new commits and files and 
you can start working on your assigned tasks.

### Commit and Push

Once you have made modifications on a file and saved them, it will appear in your `commit` window. 
Here you can control one last time your file, write the commit message with the 
issue reference (see below) and commit. 
Once your commit is ready, you can `push` them to the origin/main repository.
If you are working on a separate branch, 
it is important to select this branch when pushing to origin/main.

Commits may reference an existing GitHub issue number.
Where the issue number is preceded by `resolve`/`resolves`/`resolved`,
`close`/`closes`/`closed`, or `fix`/`fixes`/`fixed` (capitalised or not),
GitHub updates the status of the issue automatically.

### Branching and CI

- `main` is the release branch; `develop` is the working branch (clone/work on `develop`).
- PRs into `main` trigger [prchecks.yml](workflows/prchecks.yml): R CMD check
  (macOS/Windows/Linux), binary build, codecov, lintr, spell check,
  and PR metadata checks (DESCRIPTION version bump, PR title/description conventions).
- Merges/pushes to `main` trigger [pushrelease.yml](workflows/pushrelease.yml):
  check, auto-bump version tag, GitHub release with binaries and release notes
  taken from `NEWS.md`, then pkgdown site deploy.
- The PR metadata job requires that each PR into `main` bumps the `Version:` field in
  `DESCRIPTION` by the appropriate increment, names that new version in the PR title,
  and itemizes its changes in the PR description under `##` subsection titles matching
  the `NEWS.md` conventions below.
- Development dependencies are declared in `DESCRIPTION` under `Config/Needs/build`,
  `Config/Needs/check`, and `Config/Needs/website` rather than `Suggests` —
  the workflows install them via `needs:` in `setup-r-dependencies`.
- A merge that touches [R/model_regression.R](../R/model_regression.R) or the
  `R/qap_*.R` engine files deserves a parse check before it is pushed
  (`Rscript -e 'devtools::load_all()'`).
  The merge that created the current engine silently dropped three function
  headers and left an orphan function body, so the package did not parse at all.

## Design Principles

### Project overview

`infernet` is an R package (part of the [stocnet](https://github.com/stocnet) ecosystem)
providing the *inferential layer* for network analysis:
conditional uniform graph (CUG) and quadratic assignment procedure (QAP) tests of network
statistics, and multiple regression QAP (MRQAP) for network data.
Because it builds on `{manynet}`, every function accepts matrices, edgelists,
`{igraph}`, `{network}`, `{tidygraph}` or `stocnet` objects, 
and one-mode or two-mode networks alike.

`infernet` combines two lines of work:

- the formula front end, multimodal treatment, 
  and ease of use of `{migraph}`'s `net_regression()`, and
- the estimator range, missing-data handling, and cognitive social structure (CSS)
  treatment of Robert Krause's `MrQAP`, which is where the `R/qap_*.R` engine
  files were ported from.

Division of labour to keep in mind when adding functions:

- `{manynet}`: network classes/coercion (`as_*()`), making and manipulating networks,
  and network-level logical tests (e.g. `is_directed()`, `is_twomode()`).
- `{netrics}`: everything analytic — marks, measures, memberships, motifs —
  at the node, tie, and network level.
- `{autograph}`: drawing graphs and plotting analytic, modelling, or diagnostic results,
  along with deep (often institutional) theming. *All* plot methods should live there.
- `{infernet}` (this package): testing and modelling, e.g. CUG/QAP/MRQAP.
- `{migraph}`: the software companion to *Multimodal Political Networks*, holding the
  `mpn_*` datasets and the diffusion models.
  `{migraph}` still carries older copies, as `net_regression()`, and the `test_*()` family;
  those are what `{infernet}` supersedes, so a fix made there needs porting here,
  and a fix made here does not need porting back.
- `{goldfish}`: another stocnet package for estimating network event models 
  such as relational event models and dynamic network actor models. 
  Note that these two packages, `{infernet}` and `{goldfish}`, 
  should reuse similar vocabulary and syntax where possible 
  to improve the "pleasant predictability" of each package.
  
All `stocnet` packages aim for the same three things:
declarative simplicity, pleasant predictability, and graceful inclusivity.
The principles are shared across the packages;
the rules and examples below are those of `{infernet}`.

### Declarative simplicity

Functions should do one clearly named thing with few arguments.

- **Names describe actions, not progenitors.**
  A function is named after what it does,
  and the authors of a procedure are given in the documentation.
  A test is named for the null distribution it draws:
  `test_permutation()`, `test_random()`, and `test_configuration()`
  rather than names built on QAP or CUG.
  An estimator is named `estimate_<model>()`, as across stocnet,
  so that a user who moves between packages finds the same verb:
  `estimate_mrqap()` here, and `estimate_dynam()` and `estimate_rem()` in `{goldfish}`.
- **Few arguments per function.**
  Every argument is something the user must read about before they can trust the result,
  so use sensible defaults instead.
  Example: `estimate_mrqap()` takes a formula, a network, and `times`,
  and everything less common goes through its `control` list.
- **Features are detected, not declared.**
  A function finds out whether a network is directed or two-mode, and adjusts,
  so the user does not supply an argument for it.
  Example: `estimate_mrqap()` reads directedness from the outcome network with
  `manynet::is_directed()`, unless `control` overrides it.

### Pleasant predictability

Users should be able to guess a function's behaviour from its family.

- **Families share a prefix and their arguments.**
  Example: `test_random()`, `test_configuration()`, and `test_permutation()`
  all take `.data`, `FUN`, and `times`, in that order.
- **One word means one thing.**
  Function and argument names follow the house rules (see [Vocabulary](#vocabulary) below).
- **A specification can be reused.**
  Models are specified with a formula, so the same specification can be moved
  between networks and between model types,
  and its terms (`ego()`, `alter()`, `same()`, `sim()`) mean the same in each.

### Graceful inclusivity

Functions should accept whatever network users bring and return something they can use.

- **Inputs span the common classes.**
  Every function accepts any network that `{manynet}` can coerce:
  matrices, edgelists, and `igraph`, `network`, `tidygraph`, and `stocnet` objects.
- **Many types of network work.**
  A function should give a considered answer for a signed, a weighted, a directed,
  a two-mode, a multiplex, a multilevel, and a longitudinal network,
  and its documentation should say how it treats each type it treats differently
  (see [Input shapes](#input-shapes)).
- **Missing data stays missing.**
  An unobserved dyad is never read as a zero tie,
  and a function reports what it dropped rather than quietly shrinking the sample.
- **Only truly unsupported cases fail.**
  Where a function really cannot proceed, the error says why and suggests what would work.
  Example: `dist()` on a categorical attribute points users to `same()`.
- **Key steps are reported.**
  Functions say what they did at the verbosity the user has chosen
  (see [Console messaging](#console-messaging)).
  Example: `estimate_mrqap()` reports the family and the directedness it read from the outcome.
- **Results describe themselves.**
  A printed result is short, and carries what the user needs to interpret it.
  Example: a printed `estimate_mrqap()` says what was permuted and how many times,
  how loops were treated, and whether the outcome was read as directed.

### Vocabulary

One word means one thing, on both sides of the seam between the formula front
end and the engine. The engine was ported from `MrQAP` and used its own
vocabulary; the front end's words won, since those are the ones users read:

| Word | Means | Not |
|---|---|---|
| `times` | how many permutations | `reps` |
| `directed` | logical, whether i→j differs from j→i | `mode`, `"digraph"`/`"graph"` |
| `permute` | what the null distribution permutes: `"predictor"` or `"outcome"` | `nullhyp`, `method`, `"qapspp"`/`"qapy"` |
| `.data` | the network the user passes in | — |
| `matlist` | the named list of matrices the engine fits | `data` |
| `net` | one coerced network, inside the formula front end | `data` |
| reporter | the person whose report a CSS slice holds, after `{manynet}`'s `by` column | perceiver |
| `bv` | the reporter index in the dyad-level data, after `by` | `pv` |
| `testdist` | a null distribution: the statistics from every permutation or simulation, on a test or a fit | `dist` |

`mode` is reserved for a nodeset, as in one-mode and two-mode, which is what it
means everywhere else in the ecosystem. Do not use it for directedness.
`permute` replaced `method` because "method" says nothing about what differs;
`"predictor"` and `"outcome"` name the thing that is actually shuffled.
`data` is retired as an identifier: it named the network in one half of
[R/model_regression.R](../R/model_regression.R) and the matrix list in the
other, one letter away from `.data`. Reserve `data =` for the argument a model
fitter takes.

When writing documentation or NEWS items, prefer breaking lines at punctuation.

Make it clear when you are referring to functions by adding backticks and parentheses,
e.g. `a_function()`, and arguments by adding an equals sign, e.g. `argument=`.
Argument values or variables can be in double quotation marks, e.g. "value".

## Package architecture

### Common commands

This is a standard R package developed with `devtools`/`roxygen2`.
Run these from an R console with the working directory set to the package root
(or via `Rscript -e`).

- Load package for interactive development: `devtools::load_all()`
- Regenerate docs & NAMESPACE after editing roxygen comments: `devtools::document()`
- Run full test suite: `devtools::test()`
- Run a single test file: `devtools::test(filter = "estimate_mrqap")`
  (matches `test-estimate_mrqap.R`), or `testthat::test_file("tests/testthat/test-estimate_mrqap.R")`
- Full package check (mirrors CI): `devtools::check()` or `rcmdcheck::rcmdcheck()`
- Lint: `lintr::lint_package()`
- Spell check: `spelling::spell_check_package()`
- Code coverage: `covr::package_coverage()`
- Rebuild `README.md` from `README.Rmd`: `devtools::build_readme()`
- Check every topic is in the pkgdown index: `pkgdown::check_pkgdown()`
- Build pkgdown site locally: `pkgdown::build_site()`

There is no non-R build system — no package.json/Makefile.
Roxygen is configured with `markdown = TRUE`;
`NAMESPACE` and all `man/*.Rd` files are generated — never hand-edit them.
Likewise `README.md` is generated from `README.Rmd` — edit the `.Rmd` and re-knit.

### File organization

`R/` files are grouped by theme rather than one file per function.
The user-facing files are named `model_*.R`;
the internal engine ported from `MrQAP` is named `qap_*.R`.

| File | Contains |
|---|---|
| `model_tests.R` | the test family: `test_random()` (CUG), `test_configuration()`, `test_permutation()` (QAP), and `print.network_test()` |
| `model_regression.R` | `estimate_mrqap()`, the list path, and the `print.*` methods for its results, documented on the `results` page |
| `model_results.R` | the `results` help page, and `tidy()` and `glance()` for `netreg` and `network_test` results and for the `{migraph}`, `{ergm}`, and `{RSiena}` results |
| `model_specify.R` | `specify_mrqap()`, `specify_dynam()`, and `specify_ergm()`: one reader of a formula written for any model, one writer for each model, the record of what each model makes of each term, and `tidy()` of it |
| `model_formula_ergm.R` | the `to_ergm` writer of each term, which `specify_ergm()` calls |
| `model_formula.R` | the formula front end: `.specify()`, `.evaluate_spec()`, the term registry `.formula_terms()` and its builders, and `specificationAdvice()` |
| `qap_engine.R` | `QAPengine()` and `QAPPermEst()` — the one matrix-level engine, for both a dyadic network and a cognitive social structure |
| `qap_shapes.R` | the four things the two shapes do differently, and nothing else |
| `qap_utils.R` | formula parsing, input validation, `future` plumbing, matrix permutation (`RMPerm()`), the model-fitting dispatcher `fit_qap_model()`, and the permutation aggregators |
| `qap_css.R` | what a CSS needs that a dyadic network does not: a vectoriser for a three-dimensional array, and a print method |
| `qap_gmm.R` | GMM moment conditions and residual functions for the `estimator = "gmm"` path |
| `qap_gpu.R` | the optional `{torch}` batch OLS path, `gpu_batch_ols()` |
| `qap_confusion.R` | probabilistic confusion matrices for binary outcomes |
| `qap_misc.R` | small combining and reshaping helpers |
| `infernet-package.R` | package-level doc, global variables, and the shim silencing R CMD check's unused-import note |
| `zzz.R` | the attach-time greeting and the cached stocnet version check |

Only `model_*.R` holds exported functions.
Everything in `qap_*.R` is internal, and carries `@keywords internal` and `@noRd`.
Keep it that way: the engine's argument names are the `MrQAP` ones, and
exporting them would freeze an interface we still intend to tidy.

### Input shapes

Most defects reported against this package are not wrong arithmetic.
They are a network shape the function did not expect.
Before finishing a function, run it on a signed, a weighted, a directed, a two-mode,
a multiplex, a multilevel and a longitudinal network, and decide each case deliberately.
Document each decision in the roxygen block with an `@section` named for the shape,
e.g. `@section Two-mode networks:`.

Missing data deserves particular care here, since handling it well is a reason
this package exists.
An unobserved dyad is `NA`, never a zero tie and never a source's numeric code.
`make_qap_data()` drops NA and diagonal cells before fitting, so a predictor
that introduces `NA` shrinks the sample.
`QAPengine()` counts the unobserved dyads on each variable with
`.missing_summary()` before anything is dropped, reports them, and stores them
in `fit$missing`. The `missing` control chooses the treatment:
`"available"` (the default) drops them, and `"fail"` stops on them.
Multiple imputation is planned, but it waits for its own design.

Every permutation must use the dyads of the observed model.
`make_qap_data()` recomputes the dyads after each draw, and a free permutation
moves the `NA` cells of the permuted matrix but not those of the others,
so the draws used to be fitted on more dyads than the observed model.
`.missing_blocks()` now groups the nodes by which of their lines are wholly
unobserved (row, column, and CSS slice), and fixes each node at the end of a
scattered missing cell, so that the `NA` cells map onto themselves.
The blocks combine with the user's `groups`.
`QAPPermEst()` aborts if a draw uses any other number of dyads, since that is
a fault in the engine and not in the data.
For `permute = "predictor"` this needs the residualised predictor to be `NA`
outside the model, and mirrored across the diagonal of an undirected network,
which `residuals_to_matrix()` and `residuals_to_array()` do.

A structural zero, a cell that cannot hold a tie, is not missing data.
`{manynet}` does not mark one, so it arrives as an observed zero.
The loops that the model leaves out and the upper half of an undirected network
are the only cells the engine knows to be structural, and `.structural_mask()`
keeps them out of the counts.

#### Cognitive social structures

A CSS is `{manynet}`'s to define, and this package follows it.
A network is one where its ties carry a `by` column naming the reporter,
`manynet::is_cognitive()` says so, and `manynet::as_matrix()` returns the array
the engine fits. Do not build CSS arrays here: `df_to_mat()` did, and it is gone.

- **Detected, not declared.** `convertToMatrixList()` reads the outcome's
  matrix and passes `css = TRUE` where it has three axes.
  `estimate_mrqap()` reports the detection and the number of reporters.
- **Read all three axes.** A one-mode CSS from `{manynet}` has a slice for
  every node; a two-mode CSS has rows of one mode, columns of the other, and a
  slice for every node of both. `make_css_data()` and `.perm_css()` read each
  axis from `dim()`. The two-mode defect of phase one was a shape read off one
  axis, and the CSS version of it was the same mistake one axis further in.
- **A reporter is a node.** A permutation relabels senders, receivers, and
  reporters together. For a two-mode CSS the slice order is the row order
  followed by the column order, offset by the number of rows.
- **Lift predictors to the outcome.** A nodal term or a layer that is not a CSS
  is repeated along the reporter axis by `.lift_to_css()`. A CSS layer keeps its
  own reports. `tertius()` is computed within each report.
- **Missing stays missing.** `{manynet}` gives a node whose `na` attribute is
  `TRUE` a slice of `NA`, and those reports are dropped. A slice of zeros that is
  not so marked could be a report of no ties or a person who was not asked, so
  `.report_css()` keeps it and names it in a warning.

Test a CSS change on `manynet::ison_hightech`, a real CSS whose `reports` layer
is not one, and on the constructed networks in
[tests/testthat/test-estimate_mrqap_css.R](../tests/testthat/test-estimate_mrqap_css.R)
for the shapes and the missing reports it does not have.

Before this merge the two were `QAPglm()` and `QAPcss()`, 55% the same code, and
every fix had to be made twice. One of them was made in only one place.

### Control flows

#### Bivariate

Test functions consistently:

1. Compute the observed statistic by applying the user-supplied `FUN` to `.data`.
2. Generate `times` random, configuration-preserving, or permuted networks
   via the corresponding `{manynet}` generator (`generate_random()`,
   `generate_configuration()`, `to_permuted()`), rebinding node attributes with
   `manynet::bind_node_attributes()` where the statistic needs them.
3. Recompute `FUN` over each simulated network.
4. Return a `network_test` object recording the test type, observed value,
   simulated distribution, one- and two-tailed p-values,
   and the network's properties (`is_directed()`, `is_complex()`).

#### Multivariate

`estimate_mrqap()` ([R/model_regression.R](../R/model_regression.R)) is the single
regression entry point. Its control flow is:

1. Merge the user's `control` list over `.default_control()`,
   so an unnamed entry falls back to the default rather than to `NULL`.
2. Read the formula with `specify_mrqap()`, which rewrites the terms of other
   models, drops the terms an MRQAP cannot fit, and names each one.
3. Dispatch on the input shape.
   A single network goes to `convertToMatrixList()`, which parses the formula
   with `.specify()` and builds its matrices with `.evaluate_spec()`;
   a list of networks goes to `.prepare_list_of_graphs()`, which converts each
   network, drops the ones that are missing a predictor with a warning,
   and pools the rest.
4. Resolve `family = "auto"` against the dependent variable
   (binomial for a 0/1 outcome, gaussian otherwise), and resolve `directed` and
   `diag` from the network with `manynet::is_directed()` and `manynet::is_complex()`.
   Report each resolution with `snet_info()` so that the user can describe 
   these (default) choices in a paper.
5. Call `QAPengine()`, which parses the formula, fits the baseline model once
   via `fit_qap_model()`, then runs `times` permutations and aggregates them.
6. Attach a probabilistic confusion matrix where the outcome is binary.
7. Attach the term table, `fit$terms`, from the parts that `.evaluate_spec()`
   records for each column, in the order of the coefficients.

The result is a `netreg`, with more specific classes in front of it:

| Result | Class |
|---|---|
| gaussian | `c("netreg_lm", "netreg")` |
| any other family | `c("netreg_glm", "netreg")` |
| a CSS | `c("netreg_css", <family class>, "netreg")` |

Write a method at the level where behaviour actually differs, and let S3 fall
back for the rest: 
`print()` has a method for each of the three specific classes, 
while `{autograph}` needs only `plot.netreg()` until a CSS wants a plot of its own. 
The names say what the result is, not the procedure that made it,
so they carry no `qap`, and none repeats a function's name. 
A new kind of result gets a new specific class in front of `netreg`.

Inside `QAPengine()` the `permute` control names what the null distribution
permutes: `"outcome"` permutes the dependent matrix only, while `"predictor"`
implements Dekker et al.'s double semi-partialling, running one permutation set
per main predictor after residualising it against the others.
With one predictor there is nothing to residualise against, so `"predictor"`
falls back to `"outcome"` and says so.
Permuted coefficients and test statistics are then compared against the baseline
by `compare_perm_to_baseline()` and reduced to `lower`/`larger`/`abs`
p-value matrices by `aggregate_perm_results()`, which also keeps the permuted
test statistics as `testdist`. Keep that matrix whenever the summaries change:
the summaries are read from it, `test-qap_testdist.R` checks that they agree,
and `{autograph}` draws it. The name is the one the test family already uses;
`{sna}` and `{migraph}` call the same thing `dist`, which `{autograph}`'s
`plot.netlm()` reads, but `dist()` is a formula term here.

#### One engine, two shapes

`QAPengine()` fits a dyadic network and a cognitive social structure through the
same skeleton. They differ in four places and nowhere else, and those four live
in a *shape* returned by `.qap_shape()`
([R/qap_shapes.R](../R/qap_shapes.R)):

| Field | Dyadic | Cognitive |
|---|---|---|
| `vectorise()` | `make_qap_data()`, one row per dyad | `make_css_data()`, one row per dyad per reporter |
| `permute()` | `RMPerm()` | `RMPerm(CSS = TRUE)`, which calls `.perm_css()` |
| `unresidualise()` | `residuals_to_matrix()` | `residuals_to_array()` |
| `rand_slots` | sender, receiver, network | and reporter |

A fifth field, `max_trials`, says how many permutations to redraw before giving
up: one for a dyadic network, since a degenerate draw is simply dropped and
counted, and 10,000 for a CSS, whose sparse arrays often permute into an
outcome with a single value.

Add a shape rather than a second engine. A random-intercept slot a shape does
not list cannot be requested, so a reporter intercept on a dyadic network
aborts by name rather than producing a formula that will not parse.

Everything that only one estimator needs is in `Suggests`:
`lme4` (random effects), `MASS` (negative binomial), and `pscl`
(zero-inflated Poisson). The other five are on the branches listed under
[Parked extensions](#parked-extensions).
A code path that depends on a suggested package must guard with
`requireNamespace()` and abort with an actionable message,
and its tests must `skip_if_not_installed()`.
Keeping these optional is deliberate: the common case — a gaussian or binomial
MRQAP — must install and run with no compiler and no heavy dependency tree.

#### Parallelism

Every simulation-heavy function takes:

- `times` — the number of simulations (default `1000`; 1,000–10,000 for publication).
- `strategy` — a `{future}` plan name (default `"sequential"`;
  `"multisession"`/`"multicore"` for multiple cores),
  set with `future::plan(strategy)` and restored via `on.exit()`.
  In `estimate_mrqap()` this is passed through the `control` list.

The `test_*()` family maps simulations with `future.apply::future_lapply()`
and `future_vapply()`, using `future.seed = TRUE` for reproducible parallel RNG.
The regression engine wraps the same machinery in `setup_future_plan()` and
`run_permutations()` ([R/qap_utils.R](../R/qap_utils.R)); use those rather than
calling `future::plan()` from a new engine function.

Progress reporting is not a separate argument.
It is read from `options(snet_verbosity)`, so that one option governs how
talkative every stocnet package is.

#### Console messaging

All user-facing messages go through the `snet_*()` wrappers exported by `{manynet}`,
rather than base `message()`/`stop()`/`warning()` or `{cli}` calls directly:

| Wrapper | Use for |
|---|---|
| `snet_abort()` | errors: the function cannot proceed |
| `snet_warn()` | the function proceeds, but the user should know something |
| `snet_info()` | notable information about what was done, e.g. a defaulted argument or the method dispatched to |
| `snet_minor_info()` | incidental detail |
| `snet_success()` | confirmation that a requested operation completed |
| `snet_unavailable()` | not-yet-implemented features |

Every wrapper except `snet_abort()` is silenced by
`options(snet_verbosity = "quiet")`, which is the *default* —
so informational output must never be load-bearing,
and errors must carry everything the user needs to act.
Users opt in with e.g. `options(snet_verbosity = "verbose")`.

These wrappers pass their input to `{cli}`, so:

- Braces interpolate, replacing `paste()`: `snet_abort("{.val {dep}} is not in the data.")`.
- A brace expression beginning with a dot is read as a *style*, not as code, so
  `{.val {.directed_label(x)}}` aborts with "Invalid cli literal". Resolve a
  call to a dot-prefixed function into a local variable first.
- `snet_info()` pastes its arguments, so pass separate strings for a longer
  message rather than a named `c()` vector: the names are dropped and the
  strings run together without a space.
- Use `{cli}` inline classes to mark up what you refer to — `{.fn}` for functions,
  `{.arg}`/`{.var}` for arguments and variables, `{.val}` for values,
  `{.pkg}` for packages, `{.url}` for links.
- Use `{cli}`'s pluralisation rather than hand-written branches:
  `snet_warn("Dropped {length(dropped)} network{?s}.")`.

Prefer "`{.arg times}` must be a positive whole number" over "invalid input".

Report every default the model resolves for itself, with `snet_info()`: the
family read from the outcome's values, the directedness read from the network,
and any fallback such as `permute = "predictor"` reducing to `"outcome"`.
A model the user did not state is one they cannot describe in a paper.
Because this output is silent by default, a broken message is invisible in
every other test, so cover it in
[tests/testthat/test-qap_reporting.R](../tests/testthat/test-qap_reporting.R),
which runs with `snet_verbosity = "verbose"`.
Where a function needs a package from `Suggests`, name it and say how to get it:
`snet_abort(c("The {.pkg lme4} package is required for random effects.", i = "Install it with {.run install.packages(\"lme4\")}."))`.

The model specification advice printed by `specificationAdvice()` is
informational, not load-bearing, so it belongs at `snet_info()`.

### Formula conventions

A formula is meant to be reusable across models and across stocnet packages,
even if the calculation or interpretation may differ across models.
Currently only infernet, goldfish, and ergm use a formula.
This section records the rules `{infernet}` follows and how terms might
correspond to those in other packages.

| | infernet | goldfish | ergm | RSiena | MoNAn |
|---|---|---|---|---|---|
| Model | `estimate_mrqap(advice ~ ego(age) + friends, net)` | `estimate_dynam(calls ~ inertia + recip + trans(friends))`, or `make_specification(rate = ~ ..., choice = ~ ..., data = net)` | `ergm(net ~ edges + nodematch("x"))` | `set_effect(effs, c(egoX, altX), covar1 = "age")` | `addEffect(effs, same_covariate, attribute.index = "region")` |
| Outcome | the layer on the left-hand side; `.` is `info$focal`, or the whole network | the dependent events on the left-hand side; in `make_specification()`, `layer =`, by default `info$focal`, and a flavoured layer takes a list keyed by flavour, `rate = list(creation ~ ..., dissolution ~ ...)` | the network on the left-hand side | named in the data object | named in the state object |
| An attribute | a bare name | a bare name | a string, `"x"` | a string, `covar1 = "x"` | a string |
| A bare term | a layer or a tie attribute; where none has the name, a term that can stand alone, with its defaults (`recip`), or a goldfish effect that is then dropped (`inertia`) | an effect with its defaults, reading the dependent events: `~ inertia + recip` | — | — | — |
| The argument of a term | a node attribute for an attribute term, a layer for a layer term | a layer, then a node attribute, then a global variable; a layer silently wins over a node attribute, and a node attribute and a global variable of the same name abort | — | — | — |
| Options | named arguments, as goldfish | named arguments: `transformer_fn`, `summarizer_fn`, `weighted`, `window`, `type`, `history`, `sub_type` (DyNAM-i), `is_two_mode`; stocnet/goldfish#122 proposes `perspective` for `type`, and `path_fn` | named arguments: `pow`, `diff`, `levels`, `dir`, `sign.action` | `parameter`, `interaction1`, ... | `attribute.index`, ... |
| Interactions | `:` and `*`, any order, the product of the operands | `:` and `*`, any order, the product of the operands | `:` and `*`, for dyad-independent terms the product of the covariates | `set_interaction()` | — |
| Offsets | not yet | `offset()` | `offset()` | `fix = TRUE` | — |
| Group effects | random intercepts, as `{lme4}`: `(1 \| sender)`, `(1 \| receiver)`, `(1 \| reporter)`, `(1 \| network)`, or the same in `control` | none; `\|` and `I()` are refused in the effects formula, and inside `support_constraint` a bar is a boolean OR | — | — | — |
| Specification | internal: `.specify()` | the formula, or `make_specification()` | the formula | `make_specification()`, a `sienaEffects` | `createEffects()` |

#### Rules

1. **A term is a function named for the matrix or statistics it builds.** 
  Its first argument is a bare name, and options are named arguments 
  that can also be given by position.
2. **The left-hand side names the outcome layer.** 
  `.` means the layer the network declares as `info$focal` (in stocnet objects), 
  or the whole network where it declares none. 
  A named left-hand side takes priority over `info$focal`.
3. **A bare name is a layer, tie attribute, or dyadic covariate.** 
  Both `layer` and `tie(layer)` work, and `tie()` gives 0/1 unless `weighted = TRUE`. 
  Where no layer or tie attribute has the name, a term that can stand alone
  is that term with its defaults, so `~ recip` reads the outcome's reverse tie,
  as goldfish's endogenous `recip` does.
  A name that is both a layer and such a term aborts.
  The argument of a term is read by the term's kind: a node attribute for
  `ego()`, a layer for `indeg()`.
4. **`:` and `*` make interactions of any order.**
  These are the product of the operands. 
  An operand with several columns, e.g. `match()`, gives one product for each.
5. **A two-mode attribute is read on the mode of each side.**
6. **A term a model cannot fit is dropped, and the model says why.**
  A formula written for another model should still run here, without the terms
  this model cannot fit, so that it does not stop the investigation.
  `specify_mrqap()`, which `estimate_mrqap()` calls on every formula,
  drops them with one warning that names each term and the reason,
  and the rest is fitted.
  It rewrites a term that has an MRQAP equivalent, such as ergm's
  `nodematch("dept")` as `same(dept)`, and names a term that means something
  different here, such as goldfish's `sim()`, with `snet_info()`.
  An interaction with such a term is dropped whole.
  `specify_dynam()` and `specify_ergm()` do the same for goldfish and ergm.
  Each reads the formula into infernet's terms, and then writes them for its
  model, so a new model needs one reader and one writer,
  not a translation from each of the others.
  For example, since MRQAPs rely on permutation,
  goldfish's `inertia` or `trans`, which read the outcome's own ties, are dropped.
  A formula aborts only where a name is known to no model,
  which is probably a typing error and gets the nearest name as a suggestion,
  where no term is left, or where a term is refused:
  `tie()` and `indeg()` of the outcome layer would predict the outcome from itself.
7. **A column name joins its parts with underscores.**
  The term comes first, then its layer, attribute, and category:
  `ego_age`, `indeg_friends`, `tertius_friends_age`, `match_dept_sales`,
  and `mix_dept_sales_hr` (the sender's category, then the receiver's).
  `indeg(friends, type = "ego")` gives `ego_indeg_friends`.
  A character that a name cannot hold, such as a space in a category,
  becomes one underscore, and an interaction joins its columns with `:`.
  The name is a label: since attributes and categories can hold underscores,
  do not parse it back into its parts.
  The parts are kept instead in the fit's term table, `fit$terms`,
  one row for each coefficient, and `tidy()` adds them to the estimates:
  `effect` (the term, such as `"match"`, or `"intercept"` or `"interaction"`),
  `layer`, `attribute`, `category` as the data hold it, `reference`,
  and, for an interaction, `operands`, the columns it multiplies.
  A `mix()` pair is written as a tie is in DOT,
  `"sales -> hr"`, or `"sales -- hr"` where the pair is unordered.
  A categorical `ego()`, `alter()`, or `mix()` names its reference category
  in a message and in the term table, not in each column.
  `term` is the column's name, as in `{broom}`;
  goldfish's `coef_layout()` calls the same field `name`.

#### The dictionary

The MRQAP column is what `estimate_mrqap()` does with the term:
it **fits** it, it fits it and **warns**, it **drops** it and fits the rest,
or it **refuses** the formula.
The registry `.formula_terms()` holds the same for each term (`mrqap`),
with the term's name in the other models,
and a test checks that every registered term has a row here.
The registry names each model, not its package:
`dynam` for a DyNAM or REM in goldfish, `saom` for RSiena, and `ergm`.

Each `specify_*()` function records what its model makes of each term of a
formula, and `tidy()` returns that record.
The statuses are one vocabulary, `.statuses()`, for every model:
**fits**, **adapted** (rewritten as the model's equivalent),
**interpreted** (kept, but the model defines it differently),
**warns**, **drops**, and **refuses**.
`test-model_specify_record.R` holds the expected status of every registered
term in every model.

The ergm column is also what `specify_ergm()` writes each term as,
through the term's `to_ergm` function in the registry:
the first ergm term named, or else `edgecov` of the matrix that infernet builds.
An undirected network reads `ego()` and `alter()` as `nodecov` or `nodefactor`.
A term with no ergm name, or with an ergm name but no `to_ergm`,
such as `common_sender`, is dropped.
A cognitive social structure and a list of networks abort.

| infernet / goldfish | MRQAP | Meaning | ERGM (ergm) | SAOM (RSiena) | MoNAn |
|---|---|---|---|---|---|
| `ego(x)` | fits | the sender's value | `nodeocov`, `nodeofactor`; `b1cov`, `b1factor` in a two-mode network | `egoX` | — |
| `alter(x)` | fits | the receiver's value | `nodeicov`, `nodeifactor`; `b2cov`, `b2factor` in a two-mode network | `altX` | `alter_covariate` |
| `same(x)` | fits | 1 where both share a value | `nodematch`; `edgecov` in a two-mode network | `sameX` | `same_covariate` |
| `diff(x)` | fits | `transformer_fn(x_i − x_j)`, absolute by default | `absdiff`, or `diff(sign.action = "abs")` | `absDiffX` | — |
| `diff(x, transformer_fn = identity)` | fits | the signed difference | `diff` | `diffX` | — |
| `dist(x)` | warns | deprecated alias of `diff(x)` | `absdiff` | `absDiffX` | — |
| `sim(x)` | fits | 1 − \|d\| / range | `edgecov` | `simX`, centred | `sim_covariate`, the same 1 − \|d\| / range |
| `match(x)` | fits | one 0/1 column per category both hold | `nodematch(diff = TRUE)` | — | — |
| `mix(x)` | fits | one 0/1 column per pair of categories, less the first | `nodemix` | — | — |
| `tie(layer)`, or the bare name | fits; refuses the outcome layer | another layer as a dyadic covariate | `edgecov` | `X` | `dyadic_covariate` |
| `exchange(layer)`, `recip(layer)` | fits | the reverse tie in another layer | `edgecov` of the transpose | — | — |
| `recip`, `recip()` | warns | the reverse tie in the outcome, which is not permuted with the outcome | `mutual` | `recip` | `reciprocity_basic` |
| `indeg(layer)`, `outdeg(layer)` | fits; refuses the outcome layer | a node's degree in another layer, on the `type` side | `edgecov`, or `nodeicov`/`nodeocov` of the degree | — | — |
| `tertius(layer, x)` | fits | the mean of x over j's other in-neighbours | `edgecov` | — | — |
| `tertius_diff(layer, x)` | fits | \|x_i − `tertius(layer, x)`\| | `edgecov` | — | — |
| `global(x)` | fits, in a list of networks | a network-level variable; in goldfish a main effect in rate models, and in choice models only in interactions | — | — | — |
| `(1 \| sender)` | fits | a random intercept for each sender, receiver, reporter, or network | — | — | — |
| `a:b`, `a*b` | fits | the product, of any order | `:` and `*`, converted as `edgecov` of the product | `set_interaction()` | — |
| `inertia`, `inertia()` | drops | goldfish: the past events from i to j | — | — | — |
| `trans`, `trans()` | drops | goldfish: the two-paths i → k → j | `ttriple`, `dgwesp(type = "OTP")`; `triangle` in an undirected network | `transTrip` | — |
| `cycle`, `cycle()` | drops | goldfish: the two-paths j → k → i | `ctriple`, `dgwesp(type = "ITP")` | `cycle3` | — |
| `common_sender`, `common_sender()` | drops | goldfish: the nodes k with k → i and k → j | `dgwesp(type = "ISP")` | `gwespBF`, nearest | — |
| `common_receiver`, `common_receiver()` | drops | goldfish: the nodes k with i → k and j → k | `dgwesp(type = "OSP")` | `gwespFB`, nearest | — |
| `four`, `four()` | drops | goldfish: the four-cycles that a tie from i to j would close | `cycle(4)` | `cycle4` | — |
| `triangle`, `triangle()` | drops | goldfish: the undirected triangles of the sender, in a rate model | `triangle` | — | — |
| `node_trans`, `node_trans()` | drops | goldfish: the transitive triangles of a node | — | — | — |
| `degree`, `degree()` | drops | goldfish: the undirected degree of the sender, in a rate model | `degree` | — | — |
| `mixed_trans()`, `mixed_cycle()`, `mixed_common_sender()`, `mixed_common_receiver()` | drops | goldfish: a closure through two layers, `list(net1, net2)` | — | — | — |
| `ego_alter_interaction()` | drops | goldfish: the product of the sender's and the receiver's values; write `ego(x):alter(y)` | `nodeocov(x):nodeicov(y)` | `egoXaltX` | — |
| `size()`, `dyad()`, `alterdeg()`, `alterpop()`, `egodeg()`, `egopop()`, `intercept()`, `dyadXdiff()`, `dyadXego()`, `sizeXdiff()`, `sizeXego()` | drops | goldfish: DyNAM-i statistics of interaction groups | — | — | — |

goldfish's endogenous effects read the dependent events where no network is named.
In an MRQAP, such a term reads the outcome's own ties, which permuting the outcome
does not permute with it, so it is dropped.
`trans(friends)` names another layer, and goldfish would read that layer instead.
It is also dropped for now, since its matrix is not yet built here,
and the warning says so.

Two terms still differ from goldfish:

- goldfish defines `sim(x)` as `-transformer_fn(x_i - x_j)`, with `abs` by
  default, where infernet uses 1 − |d| / range.
- goldfish includes ego in `tertius()` and `tertius_diff()`, where infernet
  reads only j's other in-neighbours.

#### Adding a term

1. Add an entry to `.formula_terms()` in
   [R/model_formula.R](../R/model_formula.R): its `formals`, its `kind`, its
   builder, whether it accepts a two-mode network, what it does with the outcome
   layer, its MRQAP status (`mrqap`, with a `reason` where it warns or drops),
   its nearest DyNAM, SAOM, and ERGM terms (`dynam`, `saom`, `ergm`),
   and its writer for ergm (`to_ergm`,
   in [R/model_formula_ergm.R](../R/model_formula_ergm.R)).
   A term whose meaning or name differs in goldfish also needs a line in
   `.dynam_operand()` or `.interpreted_dynam()` in
   [R/model_specify.R](../R/model_specify.R).
   A term of another model that MRQAP cannot fit goes in `.dynam_terms()`
   with `mrqap = "drops"` and a `reason`, and needs no builder.
2. Write the builder. It takes the parsed operand and the context, and returns
   a named list of matrices: one for most terms, several for a term like
   `match()`. `.evaluate_spec()` lifts each to a CSS array where needed.
   `.build_operand()` records each column's term, layer, and attribute
   from the term's `kind`. A builder that knows more, such as each column's
   category, gives it in a `parts` attribute of the list it returns.
3. Add a row to the dictionary above, with its MRQAP status,
   a row to `expected_status` in `test-model_specify_record.R`, with its
   status in each model, and a test to `test-formula_terms.R` that
   checks the matrix against a value computed by hand or by a loop.
4. Add the term to the `formula` parameter of `estimate_mrqap()`.

## Preparing Contributions

### Tests

This package uses the `testthat` package for testing functions.
Please see the [testthat website](https://testthat.r-lib.org) for more details.
`testthat` edition 3 with parallel execution is configured in `DESCRIPTION`
(`Config/testthat/parallel: true`).
`Config/testthat/start-first` should prioritise the test files that take longest to run.

Tests in `tests/testthat/` mirror the `R/` files for the exported functions
(`test-estimate_mrqap.R`, `test-model_tests.R`),
and are grouped by contract for the engine:

| File | Asserts |
|---|---|
| `test-qap_estimators.R` | each `family` and random-effect combination, against the equivalent standard fit |
| `test-qap_shapes.R` | the dyads that reach the model, for each shape of network |
| `test-qap_reproducibility.R` | that a seed reproduces a run, sequentially and in parallel |
| `test-qap_control.R` | the `control` list and the choice of null hypothesis |
| `test-qap_reporting.R` | the messages that report what the model resolved, with verbosity on |
| `test-qap_shape_css.R` | the CSS shape of the engine, on hand-built arrays |
| `test-qap_missing.R` | that every permutation keeps the missing cells, and so the dyads, of the observed model, for each shape, and the `missing` control |
| `test-estimate_mrqap_css.R` | CSS models through `estimate_mrqap()`, from `{manynet}` networks |
| `test-qap_testdist.R` | that the p-values can be read back from the stored null distribution |
| `test-formula_spec.R` | the grammar: named arguments, interactions of any order, the outcome, and refused formulas |
| `test-model_specify.R` | what the terms of each model become in each `specify_*()` function, which are dropped, and how the model a formula was written for is read |
| `test-model_specify_record.R` | the status each model gives each registered term, the record of each term, `tidy()` and the columns it names, and formulas from goldfish's documentation |
| `test-model_specify_ergm.R` | `specify_ergm()` with a network: each term's ergm term or `edgecov()` matrix, two-mode and undirected networks, and an ergm fit of the result |
| `test-formula_terms.R` | each term's matrix against a value computed by hand, and the dictionary |
| `test-model_results.R` | `tidy()` and `glance()`, on hand-built results and on fits |
| `test-tutorials.R` | that the code of each tutorial in `inst/tutorials/` runs without error |

[tests/testthat/helper-infernet.R](../tests/testthat/helper-infernet.R)
holds the shared fixtures and expectations:

- `qap_net_gaussian()`, `qap_net_binary()`, `qap_net_count()`, `qap_net_zip()`,
  `qap_net_undirected()`, `qap_net_twomode()` — each seeded, and each carrying
  real signal, so that every family converges and the comparison is not testing
  noise against noise.
- `formula_net()` — a six-node multiplex network with two layers, small enough
  that a term's matrix can be checked by hand.
- `qap_reference_data()` — rebuilds the dyad-level data frame the engine fits,
  so a baseline coefficient can be compared against `lm()`, `glm()`,
  `MASS::glm.nb()`, `pscl::zeroinfl()`, `lme4::lmer()` or `fixest::feglm()`
  on identical data.
- `expect_qap_shape()` — the shape contract every estimator meets, whatever it
  fits underneath: named coefficients, and `lower`/`larger`/`abs` as
  two-row matrices of proportions with matching dimnames.

Four things are worth asserting for every estimator that is added:

1. That the baseline coefficients match those of the equivalent standard fit on
   the same dyad-level data, using `qap_reference_data()`.
   The permutation inference is what is novel; the point estimates are not,
   and they should agree.
2. That the result meets `expect_qap_shape()`.
   Most engine defects found so far surfaced as a name or a dimension, not as a
   wrong number: a backticked coefficient name broke double semi-partialling,
   and a stray placeholder intercept broke the `{fixest}` path.
3. That a given `seed` reproduces the same p-values.
   Permutation results are only comparable across runs if the RNG is,
   and `future.seed = TRUE` is what makes that
   true in parallel.
4. That the estimator is reached at all.
   Several paths in `fit_qap_model()` are selected by a combination of `family`,
   `estimator` and the random/fixed effects flags,
   so a test that does not name that combination does not cover it.

An estimator that needs a package from `Suggests` takes
`skip_if_not_installed()`, so the suite still passes where that package is
absent. Do not let a path go untested because the package is missing locally:
install it, and check that the test runs before you rely on the skip.

Note that `skip_if_not_installed()` is weaker than it looks. CI installs every
`Suggests`, so the skip does not fire there, and an installed package is not
always a working one: `{torch}` installs as an R package before its Lantern
backend is downloaded, and `torch::cuda_is_available()` then throws rather than
returning `FALSE`. Guard on the capability, not on the package.

A test must not depend on a `{manynet}` feature newer than the CRAN version, or
it passes here and fails on CI. 

Note that `options(snet_verbosity)` is unset under `R CMD check`, 
because manynet's `.onAttach` only sets it in an interactive session. 
Never write a test that depends on `snet_info()` output.

Count the dyads rather than checking that a call returns.
A directed network of *n* nodes contributes *n*(*n*-1) dyads, an undirected one
*n*(*n*-1)/2, and a two-mode one every cell of its incidence matrix.
Each of those was wrong at some point, and each looked like a working model.

A fitter's warning raised inside the permutation loop is held back, since it
would print once per draw; `aggregate_perm_results()` reports the number of
draws that failed outright. A test therefore should not expect a convergence
warning from a permutation, only from the baseline.

The aim is to work towards comprehensive coverage,
so each change should be fully covered by tests.
However, we also need to keep an eye on the clock:
CRAN complains if tests take too long,
so use small fixtures, low `times`, and `skip_on_cran()` for taxing tests.
`# nocov start` and `# nocov end` can be used to exclude lines or functions
that are too difficult to cover.

### Documentation

Roxygen is configured with `markdown = TRUE`;
`NAMESPACE` and all `man/*.Rd` files are generated — never hand-edit them.
Run `devtools::document()` after changing any roxygen comment.

- Related functions share one roxygen block via `@name`/`@rdname`,
  matching the file organisation above.
- Document each argument once.
  `estimate_mrqap()` takes its options through a `control` list, so document
  each entry of that list as a bullet under `@param control` rather than as its
  own `@param`. A stray `@param` for something that is no longer a formal
  argument is an R CMD check warning, and the duplicated blocks that the
  engine merge introduced were exactly that.
- Every exported function needs a runnable `@examples` block:
  examples are run by R CMD check, and they are also the fastest documentation for users.
  Prefer the bundled `ison_*`/`fict_*` networks from `{manynet}` over ad hoc
  constructions, and keep `times` small so the example is fast.
- Use the native pipe `|>` in examples, never `%>%`.
  `{migraph}` shipped examples that called `%>%` after the re-export was
  removed, and every one of them failed R CMD check.
- Cite the source of a method with `@references` in the ecosystem's format
  (authors, year, title, journal, and `\doi{}` where available),
  so that users can trace an implementation back to its definition.
- Documented behaviour and implemented behaviour must agree.
  When you change a default, search the roxygen for it too.

### README and website

The README offers a landing page for new users, both on the GitHub repository
as well as on the website.
As such, it should make a compelling case for the value added of the package,
and not drift out of date.
Note that `README.md` is generated from `README.Rmd` — edit `README.Rmd` and re-knit
(`devtools::build_readme()`), never edit `README.md` directly.

The website is created by pkgdown from [pkgdown/_pkgdown.yml](../pkgdown/_pkgdown.yml),
and is deployed automatically when changes reach `main`.
Please make sure that the pkgdown website will build correctly before opening a PR:

```r
pkgdown::check_pkgdown()              # every topic is in the index
pkgdown::build_site(preview = FALSE)  # everything else
```

The most common failure is a new exported function that is not picked up under the
function overview (the `reference:` section of `_pkgdown.yml`) —
pkgdown requires *every* exported topic to appear there exactly once, or it will not build.
A helper that users are not meant to call takes `@keywords internal` instead.
These `reference:` titles are also the headings used in `NEWS.md` (see below),
so keep the two in step.

### `NEWS.md` conventions

`NEWS.md` groups each version's changes under `##` headings that mirror the website
function overview (`pkgdown/_pkgdown.yml` `reference:` titles).
Lead with `## Package` (package-wide/website/infrastructure changes),
then `## Tests` (the `test_*()` family) and `## Regression` (`estimate_mrqap()`
and the engine behind it).
Each heading appears at most once per version.

Start each bullet with a verb matching the change type:

- `Added ...` — new functionality
- `Fixed ...` — bug fixes; if it relates to a GitHub issue, suffix with `(closing #123)`
- `Renamed ... to ...` — function or data name migrations
- `Improved ...` — functional updates to existing behaviour
- `Updated ...` — documentation changes

If a cited GitHub issue was **not** authored by @jhollway, thank the author with an
`@`-tag in the bullet.

#### Grouping

Group first, and only then write the bullets.
The more entries a version holds, the more this matters.

- Cluster related changes as indented sub-bullets under a lead bullet.
- Where several changes concern one function, lead with an `Improved ...` bullet naming
  the function, and put the individual `Fixed ...`/`Added ...` points beneath it,
  so the cluster groups by function rather than by change type.
- Under such a lead bullet, do not name the function again in the sub-bullets,
  since the lead bullet already carries it.
- Where one decision runs across many functions, lead with the decision rather than
  with each function.
- Sub-bullets indent by two spaces, and nest at most one level further (four spaces).

#### Writing the bullets

`NEWS.md` is read by users scanning for what changed, not by reviewers reading prose,
so each bullet is a headline rather than a sentence,
so avoid over-punctuation or over-explanation.
Details can be added to the function documentation, if necessary.

- No full stop at the end of a bullet
- Keep every bullet to one line of fewer than 81 characters ideally
  (a few more or less is fine)
  - If a bullet wraps, it holds too much: shorten it,
    or split it into a lead bullet and sub-bullets
- One clause where possible, and at most one comma
  - Use a semicolon for a short second clause, e.g. "old spelling still works but warns"
  - Use a sub-bullet where the second clause needs more room than that
- Name the function or object in backticks and say what changed to it,
  dropping scaffolding like "This change ...", "In order to ...", or "as part of an effort to"
- Keep the *what*, and add the *why* only where the behaviour would otherwise look arbitrary
- No trailing rationale, no restating the same change twice in different words,
  and no marketing adjectives such as "comprehensive" or "robust"
- A sub-bullet does not need a verb: it can state the consequence,
  the previous behaviour, or an example call
- Cut a sub-bullet that only restates what the lead bullet already implies
- Where several bullets describe parallel changes, reuse the sentence structure,
  so that a reader sees the parallelism at a glance
- Use one word for one thing throughout a version's entries,
  rather than varying the wording for effect

For example, instead of:

> Fixed a bug where, in some cases, `print.network_test()` was not returning its
> input invisibly, which meant that `x <- print(test)` assigned NULL.

write:

> Fixed `print.network_test()` to return its input invisibly

and instead of:

> Added a new control option, `use_gpu`, which is a useful option that allows the
> permutations to be run in batch on the GPU using torch.

write:

> Added `use_gpu` control for batch OLS permutations on the GPU
