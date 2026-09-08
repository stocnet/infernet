# Contributing

Contributions to `infernet`, whether in the form of issue
identification, bug fixes, new code or documentation are encouraged and
welcome.

Please note that the `infernet` project is released with a [Contributor
Code of Conduct](https://stocnet.github.io/infernet/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Git

`stocnet` projects are maintained using the git version control system.
A plain-English introduction to git can be found
[here](https://blog.red-badger.com/2016/11/29/gitgithub-in-plain-english).
I recommend you read this before continuing. A more recent motivation
can be found
[here](https://www.r-bloggers.com/2024/04/git-gud-version-control-best-practices/).
It will explain the basics of git version control, committing and repos,
pulling and pushing, branching and merging.

Using git from the command line on your lap- or desktop can be
intimidating, but I recommend [Fork](https://git-fork.com) software for
Mac and Windows. This allows mostly visual management of commits, diffs,
branches, etc. There are various other git software packages available,
but this one is fairly fully featured.

The GitHub page allows to access the issues assigned to you and check
the commits. You can also access the documents in the repository,
although this won’t be necessary after you have cloned it on your
computer via Fork.

### Identifying issues

Please use the issues tracker on GitHub to identify any function-related
issues. You can use these issues to track progress on the issue and to
comment or continue a conversation on that issue. The most useful issues
are ones that precisely identify an error, or propose a test that should
pass but instead fails. Examples for documentation are also most
welcome.

Issues that belong to another package in the family should be
transferred there rather than fixed here:
`gh issue transfer <number> stocnet/<package>`. See the division of
labour below.

### Cloning

Once you have downloaded Fork, the first thing you have to do is to
clone the remote repository on your computer. Before cloning, you will
be able to choose on which `branch` you want to work: develop or main.

### Pull

This command allows you to `pull` changes from the remote repository to
your local repository. Make sure you do that before starting working on
your files so you have the newest versions. When pulling, make sure you
choose main or develop, depending on the branch you decided to work
with. Once you pulled, you have now all the new commits and files and
you can start working on your assigned tasks.

### Commit and Push

Once you have made modifications on a file and saved them, it will
appear in your `commit` window. Here you can control one last time your
file, write the commit message with the issue reference (see below) and
commit. Once your commit is ready, you can `push` them to the
origin/main repository. If you are working on a separate branch, it is
important to select this branch when pushing to origin/main.

Commits may reference an existing GitHub issue number. Where the issue
number is preceded by `resolve`/`resolves`/`resolved`,
`close`/`closes`/`closed`, or `fix`/`fixes`/`fixed` (capitalised or
not), GitHub updates the status of the issue automatically.

### Branching and CI

- `main` is the release branch; `develop` is the working branch
  (clone/work on `develop`).
- PRs into `main` trigger
  [prchecks.yml](https://stocnet.github.io/infernet/workflows/prchecks.yml):
  R CMD check (macOS/Windows/Linux), binary build, codecov, lintr, spell
  check, and PR metadata checks (DESCRIPTION version bump, PR
  title/description conventions).
- Merges/pushes to `main` trigger
  [pushrelease.yml](https://stocnet.github.io/infernet/workflows/pushrelease.yml):
  check, auto-bump version tag, GitHub release with binaries and release
  notes taken from `NEWS.md`, then pkgdown site deploy.
- The PR metadata job requires that each PR into `main` bumps the
  `Version:` field in `DESCRIPTION` by the appropriate increment, names
  that new version in the PR title, and itemizes its changes in the PR
  description under `##` subsection titles matching the `NEWS.md`
  conventions below.
- Development dependencies are declared in `DESCRIPTION` under
  `Config/Needs/build`, `Config/Needs/check`, and `Config/Needs/website`
  rather than `Suggests` — the workflows install them via `needs:` in
  `setup-r-dependencies`.
- A merge that touches
  [R/model_regression.R](https://stocnet.github.io/R/model_regression.R)
  or the `R/qap_*.R` engine files deserves a parse check before it is
  pushed (`Rscript -e 'devtools::load_all()'`). The merge that created
  the current engine silently dropped three function headers and left an
  orphan function body, so the package did not parse at all.

## Style

In terms of style, we are aiming for “pleasant predictability” in terms
of user experience. To that end, we have a regular syntax that users can
rely on producing expected effects. Functions in the same family
(`test_*()`, etc.) should share argument order and naming, so that
behaviour is guessable across the family.

We are also aiming for “declarative simplicity”, where functions carry
as few arguments as possible to reduce the documentation burden, as well
as the burden on users to understand all of the options. Use sensible
defaults instead. Function and argument names should also follow the
house rules (see below).

One word means one thing, on both sides of the seam between the formula
front end and the engine. The engine was ported from `MrQAP` and used
its own vocabulary; the front end’s words won, since those are the ones
users read:

| Word | Means | Not |
|----|----|----|
| `times` | how many permutations | `reps` |
| `directed` | logical, whether i→j differs from j→i | `mode`, `"digraph"`/`"graph"` |
| `permute` | what the null distribution permutes: `"predictor"` or `"outcome"` | `nullhyp`, `method`, `"qapspp"`/`"qapy"` |
| `.data` | the network the user passes in | — |
| `matlist` | the named list of matrices the engine fits | `data` |
| `net` | one coerced network, inside the formula front end | `data` |

`mode` is reserved for a nodeset, as in one-mode and two-mode, which is
what it means everywhere else in the ecosystem. Do not use it for
directedness. `permute` replaced `method` because “method” says nothing
about what differs; `"predictor"` and `"outcome"` name the thing that is
actually shuffled. `data` is retired as an identifier: it named the
network in one half of
[R/model_regression.R](https://stocnet.github.io/R/model_regression.R)
and the matrix list in the other, one letter away from `.data`. Reserve
`data =` for the argument a model fitter takes.

When writing documentation or NEWS items, prefer breaking lines at
punctuation.

Make it clear when you are referring to functions by adding backticks
and parentheses, e.g. `a_function()`, and arguments by adding an equals
sign, e.g. `argument=`. Argument values or variables can be in double
quotation marks, e.g. “value”.

## Parked extensions

Five model extensions sit on `feature/*` branches while the architecture
settles, each tracked by a Github issue and each reinstated by reverting
one commit on `develop`:

| Branch | Removes | Issue |
|----|----|----|
| `feature/multinomial-comparison` | `family = "multinom"`, and the `comparison`/`reference` controls | [\#7](https://github.com/stocnet/infernet/issues/7) |
| `feature/fixest-fixed-effects` | the `fixest_se_cluster` control and the [fixest](https://lrberge.github.io/fixest/) branch | [\#8](https://github.com/stocnet/infernet/issues/8) |
| `feature/glmmtmb-mixed` | mixed negbin and mixed zip | [\#9](https://github.com/stocnet/infernet/issues/9) |
| `feature/gmm-estimator` | the `estimator` control and `R/qap_gmm.R` | [\#10](https://github.com/stocnet/infernet/issues/10) |
| `feature/torch-gpu` | `R/qap_gpu.R` and the `use_gpu` control | [\#11](https://github.com/stocnet/infernet/issues/11) |

Do not reinstate one by reverting onto `develop` without reading its
issue: several need rewriting against the merged engine rather than
reverting onto it. Do not add a new model family that needs a new
`Suggests` package until the two engines are one, for the same reason
these left.

## Package architecture

### Project overview

`infernet` is an R package (part of the
[stocnet](https://github.com/stocnet) ecosystem) providing the
*inferential layer* for network analysis: conditional uniform graph
(CUG) and quadratic assignment procedure (QAP) tests of network
statistics, and multiple regression QAP (MRQAP) for network data.
Because it builds on [manynet](https://stocnet.github.io/manynet/),
every function accepts matrices, edgelists,
[igraph](https://r.igraph.org/), [network](https://statnet.org/),
[tidygraph](https://tidygraph.data-imaginist.com) or `stocnet` objects,
and one-mode or two-mode networks alike.

`infernet` combines two lines of work:

- the formula front end, multimodal treatment, and ease of use of
  [migraph](https://stocnet.github.io/migraph/)’s
  [`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md),
  and
- the estimator range, missing-data handling, and cognitive social
  structure (CSS) treatment of Robert Krause’s `MrQAP`, which is where
  the `R/qap_*.R` engine files were ported from.

Division of labour to keep in mind when adding functions:

- [manynet](https://stocnet.github.io/manynet/): network
  classes/coercion (`as_*()`), making and manipulating networks, and
  network-level logical tests (e.g. `is_directed()`, `is_twomode()`).
- [netrics](https://stocnet.github.io/netrics/): everything analytic —
  marks, measures, memberships, motifs — at the node, tie, and network
  level.
- [autograph](https://stocnet.github.io/autograph/): drawing graphs and
  plotting analytic, modelling, or diagnostic results, along with deep
  (often institutional) theming. *All* plot methods should live there.
- [infernet](https://stocnet.github.io/infernet/) (this package):
  testing and modelling, e.g. CUG/QAP/MRQAP.
- [migraph](https://stocnet.github.io/migraph/): the software companion
  to *Multimodal Political Networks*, holding the `mpn_*` datasets and
  the diffusion models. [migraph](https://stocnet.github.io/migraph/)
  still carries older copies of
  [`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md)
  and the `test_*()` family; those are what
  [infernet](https://stocnet.github.io/infernet/) supersedes, so a fix
  made there needs porting here, and a fix made here does not need
  porting back.
- [goldfish](https://stocnet.github.io/goldfish/): another stocnet
  package for estimating network event models such as relational event
  models and dynamic network actor models. Note that these two packages,
  [infernet](https://stocnet.github.io/infernet/) and
  [goldfish](https://stocnet.github.io/goldfish/), should reuse similar
  vocabulary and syntax where possible to improve the “pleasant
  predictability” of each package.

### Common commands

This is a standard R package developed with `devtools`/`roxygen2`. Run
these from an R console with the working directory set to the package
root (or via `Rscript -e`).

- Load package for interactive development:
  [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
- Regenerate docs & NAMESPACE after editing roxygen comments:
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
- Run full test suite:
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
- Run a single test file: `devtools::test(filter = "net_regression")`
  (matches `test-net_regression.R`), or
  `testthat::test_file("tests/testthat/test-net_regression.R")`
- Full package check (mirrors CI):
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
  or
  [`rcmdcheck::rcmdcheck()`](http://r-lib.github.io/rcmdcheck/reference/rcmdcheck.md)
- Lint:
  [`lintr::lint_package()`](https://lintr.r-lib.org/reference/lint.html)
- Spell check:
  [`spelling::spell_check_package()`](https://docs.ropensci.org/spelling//reference/spell_check_package.html)
- Code coverage:
  [`covr::package_coverage()`](http://covr.r-lib.org/reference/package_coverage.md)
- Rebuild `README.md` from `README.Rmd`:
  [`devtools::build_readme()`](https://devtools.r-lib.org/reference/build_readme.html)
- Check every topic is in the pkgdown index:
  [`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html)
- Build pkgdown site locally:
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)

There is no non-R build system — no package.json/Makefile. Roxygen is
configured with `markdown = TRUE`; `NAMESPACE` and all `man/*.Rd` files
are generated — never hand-edit them. Likewise `README.md` is generated
from `README.Rmd` — edit the `.Rmd` and re-knit.

### File organization

`R/` files are grouped by theme rather than one file per function. The
user-facing files are named `model_*.R`; the internal engine ported from
`MrQAP` is named `qap_*.R`.

| File | Contains |
|----|----|
| `model_tests.R` | the test family: [`test_random()`](https://stocnet.github.io/infernet/reference/tests.md) (CUG), [`test_configuration()`](https://stocnet.github.io/infernet/reference/tests.md), [`test_permutation()`](https://stocnet.github.io/infernet/reference/tests.md) (QAP), and `print.network_test()` |
| `model_regression.R` | [`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md), the formula front end (`convertToMatrixList()`, `getRHSNames()`, `specificationAdvice()`), and the `print.*` methods for its results |
| `qap_engine.R` | `QAPengine()` and `QAPPermEst()` — the one matrix-level engine, for both a dyadic network and a cognitive social structure |
| `qap_shapes.R` | the four things the two shapes do differently, and nothing else |
| `qap_utils.R` | formula parsing, input validation, `future` plumbing, matrix permutation (`RMPerm()`), the model-fitting dispatcher `fit_qap_model()`, and the permutation aggregators |
| `qap_css.R` | what a CSS needs that a dyadic network does not: a vectoriser for a three-dimensional array, and a print method |
| `qap_gmm.R` | GMM moment conditions and residual functions for the `estimator = "gmm"` path |
| `qap_gpu.R` | the optional [torch](https://torch.mlverse.org/docs) batch OLS path, `gpu_batch_ols()` |
| `qap_confusion.R` | probabilistic confusion matrices for binary outcomes |
| `qap_misc.R` | small combining and reshaping helpers |
| `infernet-package.R` | package-level doc, global variables, and the shim silencing R CMD check’s unused-import note |
| `zzz.R` | the attach-time greeting and the cached stocnet version check |

Only `model_*.R` holds exported functions. Everything in `qap_*.R` is
internal, and carries `@keywords internal` and `@noRd`. Keep it that
way: the engine’s argument names are the `MrQAP` ones, and exporting
them would freeze an interface we still intend to tidy.

### The `net_regression()` pipeline

[`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md)
([R/model_regression.R](https://stocnet.github.io/R/model_regression.R))
is the single regression entry point. Its control flow is:

1.  Merge the user’s `control` list over `.default_control()`, so an
    unnamed entry falls back to the default rather than to `NULL`.
2.  Dispatch on the input shape. A single network goes to
    `convertToMatrixList()`; a list of networks goes to
    `.prepare_list_of_graphs()`, which converts each network, drops the
    ones that are missing a predictor with a warning, and pools the
    rest.
3.  Resolve `family = "auto"` against the dependent variable (binomial
    for a 0/1 outcome, gaussian otherwise), and resolve `directed` and
    `diag` from the network with `manynet::is_directed()` and
    [`manynet::is_complex()`](https://stocnet.github.io/manynet/reference/mark_format_tie.html).
    Report each resolution with `snet_info()`: a model the user did not
    state is one they cannot describe in a paper.
4.  Call `QAPglm()`, which parses the formula, fits the baseline model
    once via `fit_qap_model()`, then runs `times` permutations and
    aggregates them.
5.  Attach a probabilistic confusion matrix where the outcome is binary,
    and class the result `net_regression`.

Inside `QAPglm()` the `permute` control names what the null distribution
permutes: `"outcome"` permutes the dependent matrix only, while
`"predictor"` implements Dekker et al.’s double semi-partialling,
running one permutation set per main predictor after residualising it
against the others. With one predictor there is nothing to residualise
against, so `"predictor"` falls back to `"outcome"` and says so.
Permuted coefficients and test statistics are then compared against the
baseline by `compare_perm_to_baseline()` and reduced to
`lower`/`larger`/`abs` p-value matrices by `aggregate_perm_results()`.
\### One engine, two shapes

`QAPengine()` fits a dyadic network and a cognitive social structure
through the same skeleton. They differ in four places and nowhere else,
and those four live in a *shape* returned by `.qap_shape()`
([R/qap_shapes.R](https://stocnet.github.io/R/qap_shapes.R)):

| Field | Dyadic | Cognitive |
|----|----|----|
| `vectorise()` | `make_qap_data()`, one row per dyad | `make_css_data()`, one row per dyad per perceiver |
| `permute()` | `RMPerm()` | `RMPerm(CSS = TRUE)` |
| `unresidualise()` | `residuals_to_matrix()` | `residuals_to_array()` |
| `rand_slots` | sender, receiver, network | and perceiver |

A fifth field, `max_trials`, says how many permutations to redraw before
giving up: one for a dyadic network, since a degenerate draw is simply
dropped and counted, and 10,000 for a CSS, whose sparse arrays often
permute into an outcome with a single value.

Add a shape rather than a second engine. A random-intercept slot a shape
does not list cannot be requested, so a perceiver intercept on a dyadic
network aborts by name rather than producing a formula that will not
parse.

Before this merge the two were `QAPglm()` and `QAPcss()`, 55% the same
code, and every fix had to be made twice. One of them was made in only
one place.

The formula front end accepts these terms, and a new one should be added
to `getRHSNames()` and `convertToMatrixList()` together:

- `ego(attr)` — the sender’s value of a nodal attribute,
- `alter(attr)` — the receiver’s value,
- `same(attr)` — 1 where sender and receiver share an attribute value,
- `dist(attr)` — the absolute difference in a numeric attribute,
- `sim(attr)` — the proportional similarity in a numeric attribute,
- `tertius(attr, fn)` — an aggregate of an attribute over a node’s other
  ties,
- a plain name — another network, used as a dyadic covariate.

A formula is meant to be reusable across models, so a term must mean the
same thing whichever family or engine consumes it. Where a term cannot
apply, say so through `snet_abort()` rather than silently dropping it.

### Function body conventions

Test functions consistently:

1.  Compute the observed statistic by applying the user-supplied `FUN`
    to `.data`.
2.  Generate `times` random, configuration-preserving, or permuted
    networks via the corresponding
    [manynet](https://stocnet.github.io/manynet/) generator
    (`generate_random()`, `generate_configuration()`, `to_permuted()`),
    rebinding node attributes with
    [`manynet::bind_node_attributes()`](https://stocnet.github.io/manynet/reference/manip_nodes_attr.html)
    where the statistic needs them.
3.  Recompute `FUN` over each simulated network.
4.  Return a `network_test` object recording the test type, observed
    value, simulated distribution, one- and two-tailed p-values, and the
    network’s properties (`is_directed()`, `is_complex()`).

Properties of the dependent network — modes, directedness, loops — must
always be respected in permutations and analysis; one-mode and two-mode
cases are branched on explicitly rather than projected away. All
`manynet`, `netrics`, `furrr`, and `future` calls use explicit `::`
namespacing (with per-file `@importFrom` roxygen tags for NAMESPACE
generation). Plot methods belong in
[autograph](https://stocnet.github.io/autograph/), not here.

Every `print.*` method returns `invisible(x)`. A print method that
returns the result of its last
[`cat()`](https://rdrr.io/r/base/cat.html) prints `NULL` when its value
is used, which is what `print.network_test()` did before this was fixed.

### Parallelism

Every simulation-heavy function takes:

- `times` — the number of simulations (default `1000`; 1,000–10,000 for
  publication).
- `strategy` — a [future](https://future.futureverse.org) plan name
  (default `"sequential"`; `"multisession"`/`"multicore"` for multiple
  cores), set with `future::plan(strategy)` and restored via
  [`on.exit()`](https://rdrr.io/r/base/on.exit.html). In
  [`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md)
  this is passed through the `control` list.

The `test_*()` family maps simulations with `furrr::future_map*()` using
`furrr::furrr_options(seed = TRUE)` for reproducible parallel RNG. The
regression engine wraps the same machinery in `setup_future_plan()` and
`run_permutations()`
([R/qap_utils.R](https://stocnet.github.io/R/qap_utils.R)); use those
rather than calling
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
from a new engine function.

Progress reporting is not a separate argument. It is read from
`options(snet_verbosity)`, so that one option governs how talkative
every stocnet package is.

### Input shapes

Most defects reported against this package are not wrong arithmetic.
They are a network shape the function did not expect. Before finishing a
function, run it on a signed, a weighted, a directed, a two-mode, a
multiplex, a multilevel and a longitudinal network, and decide each case
deliberately. Document each decision in the roxygen block with an
`@section` named for the shape, e.g. `@section Two-mode networks:`.

Missing data deserves particular care here, since handling it well is a
reason this package exists. An unobserved dyad is `NA`, never a zero tie
and never a source’s numeric code. `make_qap_data()` drops NA and
diagonal cells before fitting, so a predictor that introduces `NA`
silently shrinks the sample: report what was dropped rather than letting
the count change without comment.

### Console messaging

All user-facing messages go through the `snet_*()` wrappers exported by
[manynet](https://stocnet.github.io/manynet/), rather than base
[`message()`](https://rdrr.io/r/base/message.html)/[`stop()`](https://rdrr.io/r/base/stop.html)/[`warning()`](https://rdrr.io/r/base/warning.html)
or [cli](https://cli.r-lib.org) calls directly:

| Wrapper | Use for |
|----|----|
| `snet_abort()` | errors: the function cannot proceed |
| `snet_warn()` | the function proceeds, but the user should know something |
| `snet_info()` | notable information about what was done, e.g. a defaulted argument or the method dispatched to |
| `snet_minor_info()` | incidental detail |
| `snet_success()` | confirmation that a requested operation completed |
| `snet_unavailable()` | not-yet-implemented features |

Every wrapper except `snet_abort()` is silenced by
`options(snet_verbosity = "quiet")`, which is the *default* — so
informational output must never be load-bearing, and errors must carry
everything the user needs to act. Users opt in with
e.g. `options(snet_verbosity = "verbose")`.

These wrappers pass their input to [cli](https://cli.r-lib.org), so:

- Braces interpolate, replacing
  [`paste()`](https://rdrr.io/r/base/paste.html):
  `snet_abort("{.val {dep}} is not in the data.")`.
- A brace expression beginning with a dot is read as a *style*, not as
  code, so `{.val {.directed_label(x)}}` aborts with “Invalid cli
  literal”. Resolve a call to a dot-prefixed function into a local
  variable first.
- `snet_info()` pastes its arguments, so pass separate strings for a
  longer message rather than a named
  [`c()`](https://rdrr.io/r/base/c.html) vector: the names are dropped
  and the strings run together without a space.
- Use [cli](https://cli.r-lib.org) inline classes to mark up what you
  refer to — `{.fn}` for functions, `{.arg}`/`{.var}` for arguments and
  variables, `{.val}` for values, `{.pkg}` for packages, `{.url}` for
  links.
- Use [cli](https://cli.r-lib.org)’s pluralisation rather than
  hand-written branches:
  `snet_warn("Dropped {length(dropped)} network{?s}.")`.

Prefer “`{.arg times}` must be a positive whole number” over “invalid
input”.

Report every default the model resolves for itself, with `snet_info()`:
the family read from the outcome’s values, the directedness read from
the network, and any fallback such as `permute = "predictor"` reducing
to `"outcome"`. A model the user did not state is one they cannot
describe in a paper. Because this output is silent by default, a broken
message is invisible in every other test, so cover it in
[tests/testthat/test-qap_reporting.R](https://stocnet.github.io/tests/testthat/test-qap_reporting.R),
which runs with `snet_verbosity = "verbose"`. Where a function needs a
package from `Suggests`, name it and say how to get it:
`snet_abort(c("The {.pkg lme4} package is required for random effects.", i = "Install it with {.run install.packages(\"lme4\")}."))`.

The model specification advice printed by `specificationAdvice()` is
informational, not load-bearing, so it belongs at `snet_info()`.

### Dependencies

`infernet` `Depends` on [manynet](https://stocnet.github.io/manynet/)
(network classes, coercion and logical tests) and
[netrics](https://stocnet.github.io/netrics/) (measures), and `Imports`
the
[future](https://future.futureverse.org)/[furrr](https://github.com/futureverse/furrr)/[purrr](https://purrr.tidyverse.org/)
stack plus [reformulas](https://github.com/bbolker/reformulas) for
formula surgery.

Everything that only one estimator needs is in `Suggests`: `lme4` and
`glmmTMB` (random effects), `fixest` (fixed effects), `gmm` (GMM
estimation), `MASS` (negative binomial), `pscl` (zero-inflated Poisson),
`nnet` (multinomial), and `torch` (the GPU path). A code path that
depends on a suggested package must guard with
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) and abort
with an actionable message, and its tests must
`skip_if_not_installed()`. Keeping these optional is deliberate: the
common case — a gaussian or binomial MRQAP — must install and run with
no compiler and no heavy dependency tree.

The declared minimum of each `stocnet` dependency is the version on
CRAN, so that CI can install it. Where `infernet` needs something that
only a newer, unreleased [manynet](https://stocnet.github.io/manynet/)
has, reach it through a shim rather than by raising the minimum, and
resolve the name at call time from the namespace. Test for the function
rather than for the version string, because a pre-release development
build can carry the version without yet exporting the function.

### Tests

This package uses the `testthat` package for testing functions. Please
see the [testthat website](https://testthat.r-lib.org) for more details.
`testthat` edition 3 with parallel execution is configured in
`DESCRIPTION` (`Config/testthat/parallel: true`).
`Config/testthat/start-first` should prioritise the test files that take
longest to run.

Tests in `tests/testthat/` mirror the `R/` files for the exported
functions (`test-net_regression.R`, `test-model_tests.R`), and are
grouped by contract for the engine:

| File | Asserts |
|----|----|
| `test-qap_estimators.R` | each `family` and `estimator` combination, against the equivalent standard fit |
| `test-qap_shapes.R` | the dyads that reach the model, for each shape of network |
| `test-qap_reproducibility.R` | that a seed reproduces a run, sequentially and in parallel |
| `test-qap_control.R` | the `control` list and the choice of null hypothesis |

[tests/testthat/helper-infernet.R](https://stocnet.github.io/tests/testthat/helper-infernet.R)
holds the shared fixtures and expectations:

- `qap_net_gaussian()`, `qap_net_binary()`, `qap_net_count()`,
  `qap_net_zip()`, `qap_net_undirected()`, `qap_net_twomode()` — each
  seeded, and each carrying real signal, so that every family converges
  and the comparison is not testing noise against noise.
- `qap_reference_data()` — rebuilds the dyad-level data frame the engine
  fits, so a baseline coefficient can be compared against
  [`lm()`](https://rdrr.io/r/stats/lm.html),
  [`glm()`](https://rdrr.io/r/stats/glm.html),
  [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html),
  [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html),
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) or
  [`fixest::feglm()`](https://lrberge.github.io/fixest/reference/feglm.html)
  on identical data.
- `expect_qap_shape()` — the shape contract every estimator meets,
  whatever it fits underneath: named coefficients, and
  `lower`/`larger`/`abs` as two-row matrices of proportions with
  matching dimnames.

Four things are worth asserting for every estimator that is added:

1.  That the baseline coefficients match those of the equivalent
    standard fit on the same dyad-level data, using
    `qap_reference_data()`. The permutation inference is what is novel;
    the point estimates are not, and they should agree.
2.  That the result meets `expect_qap_shape()`. Most engine defects
    found so far surfaced as a name or a dimension, not as a wrong
    number: a backticked coefficient name broke double semi-partialling,
    and a stray placeholder intercept broke the
    [fixest](https://lrberge.github.io/fixest/) path.
3.  That a given `seed` reproduces the same p-values. Permutation
    results are only comparable across runs if the RNG is, and
    `furrr_options(seed = TRUE)` and `future.seed = TRUE` are what make
    that true in parallel.
4.  That the estimator is reached at all. Several paths in
    `fit_qap_model()` are selected by a combination of `family`,
    `estimator` and the random/fixed effects flags, so a test that does
    not name that combination does not cover it.

An estimator that needs a package from `Suggests` takes
`skip_if_not_installed()`, so the suite still passes where that package
is absent. Do not let a path go untested because the package is missing
locally: install it, and check that the test runs before you rely on the
skip.

Note that `skip_if_not_installed()` is weaker than it looks. CI installs
every `Suggests`, so the skip does not fire there, and an installed
package is not always a working one:
[torch](https://torch.mlverse.org/docs) installs as an R package before
its Lantern backend is downloaded, and
[`torch::cuda_is_available()`](https://torch.mlverse.org/docs/reference/cuda_is_available.html)
then throws rather than returning `FALSE`. Guard on the capability, not
on the package.

A test must not depend on a
[manynet](https://stocnet.github.io/manynet/) feature newer than the
CRAN version, or it passes here and fails on CI.

Note that `options(snet_verbosity)` is unset under `R CMD check`,
because manynet’s `.onAttach` only sets it in an interactive session.
Never write a test that depends on `snet_info()` output.

Count the dyads rather than checking that a call returns. A directed
network of *n* nodes contributes *n*(*n*-1) dyads, an undirected one
*n*(*n*-1)/2, and a two-mode one every cell of its incidence matrix.
Each of those was wrong at some point, and each looked like a working
model.

A fitter’s warning raised inside the permutation loop is held back,
since it would print once per draw; `aggregate_perm_results()` reports
the number of draws that failed outright. A test therefore should not
expect a convergence warning from a permutation, only from the baseline.

The aim is to work towards comprehensive coverage, so each change should
be fully covered by tests. However, we also need to keep an eye on the
clock: CRAN complains if tests take too long, so use small fixtures, low
`times`, and `skip_on_cran()` for taxing tests. `# nocov start` and
`# nocov end` can be used to exclude lines or functions that are too
difficult to cover.

### Documentation

Roxygen is configured with `markdown = TRUE`; `NAMESPACE` and all
`man/*.Rd` files are generated — never hand-edit them. Run
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
after changing any roxygen comment.

- Related functions share one roxygen block via `@name`/`@rdname`,
  matching the file organisation above.
- Document each argument once.
  [`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md)
  takes its options through a `control` list, so document each entry of
  that list as a bullet under `@param control` rather than as its own
  `@param`. A stray `@param` for something that is no longer a formal
  argument is an R CMD check warning, and the duplicated blocks that the
  engine merge introduced were exactly that.
- Every exported function needs a runnable `@examples` block: examples
  are run by R CMD check, and they are also the fastest documentation
  for users. Prefer the bundled `ison_*`/`fict_*` networks from
  [manynet](https://stocnet.github.io/manynet/) over ad hoc
  constructions, and keep `times` small so the example is fast.
- Use the native pipe `|>` in examples, never `%>%`.
  [migraph](https://stocnet.github.io/migraph/) shipped examples that
  called `%>%` after the re-export was removed, and every one of them
  failed R CMD check.
- Cite the source of a method with `@references` in the ecosystem’s
  format (authors, year, title, journal, and `\doi{}` where available),
  so that users can trace an implementation back to its definition.
- Documented behaviour and implemented behaviour must agree. When you
  change a default, search the roxygen for it too.

### README and website

The README offers a landing page for new users, both on the GitHub
repository as well as on the website. As such, it should make a
compelling case for the value added of the package, and not drift out of
date. Note that `README.md` is generated from `README.Rmd` — edit
`README.Rmd` and re-knit
([`devtools::build_readme()`](https://devtools.r-lib.org/reference/build_readme.html)),
never edit `README.md` directly.

The website is created by pkgdown from
[pkgdown/\_pkgdown.yml](https://stocnet.github.io/pkgdown/_pkgdown.yml),
and is deployed automatically when changes reach `main`. Please make
sure that the pkgdown website will build correctly before opening a PR:

``` r

pkgdown::check_pkgdown()              # every topic is in the index
pkgdown::build_site(preview = FALSE)  # everything else
```

The most common failure is a new exported function that is not picked up
under the function overview (the `reference:` section of `_pkgdown.yml`)
— pkgdown requires *every* exported topic to appear there exactly once,
or it will not build. A helper that users are not meant to call takes
`@keywords internal` instead. These `reference:` titles are also the
headings used in `NEWS.md` (see below), so keep the two in step.

### `NEWS.md` conventions

`NEWS.md` groups each version’s changes under `##` headings that mirror
the website function overview (`pkgdown/_pkgdown.yml` `reference:`
titles). Lead with `## Package` (package-wide/website/infrastructure
changes), then `## Tests` (the `test_*()` family) and `## Regression`
([`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md)
and the engine behind it). Each heading appears at most once per
version.

Start each bullet with a verb matching the change type:

- `Added ...` — new functionality
- `Fixed ...` — bug fixes; if it relates to a GitHub issue, suffix with
  `(closing #123)`
- `Renamed ... to ...` — function or data name migrations
- `Improved ...` — functional updates to existing behaviour
- `Updated ...` — documentation changes

If a cited GitHub issue was **not** authored by @jhollway, thank the
author with an `@`-tag in the bullet.

#### Grouping

Group first, and only then write the bullets. The more entries a version
holds, the more this matters.

- Cluster related changes as indented sub-bullets under a lead bullet.
- Where several changes concern one function, lead with an
  `Improved ...` bullet naming the function, and put the individual
  `Fixed ...`/`Added ...` points beneath it, so the cluster groups by
  function rather than by change type.
- Under such a lead bullet, do not name the function again in the
  sub-bullets, since the lead bullet already carries it.
- Where one decision runs across many functions, lead with the decision
  rather than with each function.
- Sub-bullets indent by two spaces, and nest at most one level further
  (four spaces).

#### Writing the bullets

`NEWS.md` is read by users scanning for what changed, not by reviewers
reading prose, so each bullet is a headline rather than a sentence, so
avoid over-punctuation or over-explanation. Details can be added to the
function documentation, if necessary.

- No full stop at the end of a bullet
- Keep every bullet to one line of fewer than 81 characters ideally (a
  few more or less is fine)
  - If a bullet wraps, it holds too much: shorten it, or split it into a
    lead bullet and sub-bullets
- One clause where possible, and at most one comma
  - Use a semicolon for a short second clause, e.g. “old spelling still
    works but warns”
  - Use a sub-bullet where the second clause needs more room than that
- Name the function or object in backticks and say what changed to it,
  dropping scaffolding like “This change …”, “In order to …”, or “as
  part of an effort to”
- Keep the *what*, and add the *why* only where the behaviour would
  otherwise look arbitrary
- No trailing rationale, no restating the same change twice in different
  words, and no marketing adjectives such as “comprehensive” or “robust”
- A sub-bullet does not need a verb: it can state the consequence, the
  previous behaviour, or an example call
- Cut a sub-bullet that only restates what the lead bullet already
  implies
- Where several bullets describe parallel changes, reuse the sentence
  structure, so that a reader sees the parallelism at a glance
- Use one word for one thing throughout a version’s entries, rather than
  varying the wording for effect

For example, instead of:

> Fixed a bug where, in some cases, `print.network_test()` was not
> returning its input invisibly, which meant that `x <- print(test)`
> assigned NULL.

write:

> Fixed `print.network_test()` to return its input invisibly

and instead of:

> Added a new control option, `use_gpu`, which is a useful option that
> allows the permutations to be run in batch on the GPU using torch.

write:

> Added `use_gpu` control for batch OLS permutations on the GPU
