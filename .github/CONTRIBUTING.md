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

## Style

In terms of style, we are aiming for "pleasant predictability" in terms of user experience.
To that end, we have a regular syntax that users can rely on producing expected effects.
Functions in the same family (`test_*()`, etc.) should share
argument order and naming, so that behaviour is guessable across the family.

We are also aiming for "declarative simplicity",
where functions carry as few arguments as possible to reduce the documentation burden,
as well as the burden on users to understand all of the options.
Use sensible defaults instead.
Function and argument names should also follow the house rules (see below).

When writing documentation or NEWS items, prefer breaking lines at punctuation.

Make it clear when you are referring to functions by adding backticks and parentheses,
e.g. `a_function()`, and arguments by adding an equals sign, e.g. `argument=`.
Argument values or variables can be in double quotation marks, e.g. "value".

## Package architecture

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
  `{migraph}` still carries older copies of `net_regression()` and the `test_*()` family;
  those are what `{infernet}` supersedes, so a fix made there needs porting here,
  and a fix made here does not need porting back.
- `{goldfish}`: another stocnet package for estimating network event models 
  such as relational event models and dynamic network actor models. 
  Note that these two packages, `{infernet}` and `{goldfish}`, 
  should reuse similar vocabulary and syntax where possible 
  to improve the "pleasant predictability" of each package.

### Common commands

This is a standard R package developed with `devtools`/`roxygen2`.
Run these from an R console with the working directory set to the package root
(or via `Rscript -e`).

- Load package for interactive development: `devtools::load_all()`
- Regenerate docs & NAMESPACE after editing roxygen comments: `devtools::document()`
- Run full test suite: `devtools::test()`
- Run a single test file: `devtools::test(filter = "net_regression")`
  (matches `test-net_regression.R`), or `testthat::test_file("tests/testthat/test-net_regression.R")`
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
| `model_regression.R` | `net_regression()`, the formula front end (`convertToMatrixList()`, `getRHSNames()`, `specificationAdvice()`), and the `print.*` methods for its results |
| `qap_engine.R` | `QAPglm()` and `QAPglmPermEst()` — the matrix-level engine that performs the baseline fit and the permutation inference |
| `qap_utils.R` | formula parsing, input validation, `future` plumbing, matrix permutation (`RMPerm()`), the model-fitting dispatcher `fit_qap_model()`, and the permutation aggregators |
| `qap_css.R` | `QAPcss()` and `QAPcssPermEst()` — the parallel engine for cognitive social structures |
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

### The `net_regression()` pipeline

`net_regression()` ([R/model_regression.R](../R/model_regression.R)) is the single
regression entry point. Its control flow is:

1. Merge the user's `control` list over `.default_control()`,
   so an unnamed entry falls back to the default rather than to `NULL`.
2. Dispatch on the input shape.
   A single network goes to `convertToMatrixList()`;
   a list of networks goes to `.prepare_list_of_graphs()`, which converts each
   network, drops the ones that are missing a predictor with a warning,
   and pools the rest.
3. Resolve `family = "auto"` against the dependent variable
   (binomial for a 0/1 outcome, gaussian otherwise), and resolve `mode` and
   `diag` from the network with `manynet::is_directed()` and `manynet::is_complex()`.
4. Call `QAPglm()`, which parses the formula, fits the baseline model once
   via `fit_qap_model()`, then runs `reps` permutations and aggregates them.
5. Attach a probabilistic confusion matrix where the outcome is binary,
   and class the result `net_regression`.

Inside `QAPglm()` the null hypothesis decides the permutation scheme:
`"qapy"` permutes the dependent matrix only, while `"qapspp"` implements Dekker
et al.'s double semi-partialling, running one permutation set per main predictor
after residualising it against the others.
Permuted coefficients and test statistics are then compared against the baseline
by `compare_perm_to_baseline()` and reduced to `lower`/`larger`/`abs`
p-value matrices by `aggregate_perm_results()`.
`QAPcss()` mirrors this same permute-refit-aggregate architecture for CSS data.

The formula front end accepts these terms, and a new one should be added
to `getRHSNames()` and `convertToMatrixList()` together:

- `ego(attr)` — the sender's value of a nodal attribute,
- `alter(attr)` — the receiver's value,
- `same(attr)` — 1 where sender and receiver share an attribute value,
- `dist(attr)` — the absolute difference in a numeric attribute,
- `sim(attr)` — the proportional similarity in a numeric attribute,
- `tertius(attr, fn)` — an aggregate of an attribute over a node's other ties,
- a plain name — another network, used as a dyadic covariate.

A formula is meant to be reusable across models,
so a term must mean the same thing whichever family or engine consumes it.
Where a term cannot apply, say so through `snet_abort()` rather than
silently dropping it.

### Function body conventions

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

Properties of the dependent network — modes, directedness, loops — must always be
respected in permutations and analysis;
one-mode and two-mode cases are branched on explicitly rather than projected away.
All `manynet`, `netrics`, `furrr`, and `future` calls use explicit `::` namespacing
(with per-file `@importFrom` roxygen tags for NAMESPACE generation).
Plot methods belong in `{autograph}`, not here.

Every `print.*` method returns `invisible(x)`.
A print method that returns the result of its last `cat()` prints `NULL` when its
value is used, which is what `print.network_test()` did before this was fixed.

### Parallelism

Every simulation-heavy function takes:

- `times` — the number of simulations (default `1000`; 1,000–10,000 for publication).
- `strategy` — a `{future}` plan name (default `"sequential"`;
  `"multisession"`/`"multicore"` for multiple cores),
  set with `future::plan(strategy)` and restored via `on.exit()`.
  In `net_regression()` this is passed through the `control` list.

The `test_*()` family maps simulations with `furrr::future_map*()` using
`furrr::furrr_options(seed = TRUE)` for reproducible parallel RNG.
The regression engine wraps the same machinery in `setup_future_plan()` and
`run_permutations()` ([R/qap_utils.R](../R/qap_utils.R)); use those rather than
calling `future::plan()` from a new engine function.

Progress reporting is not a separate argument.
It is read from `options(snet_verbosity)`, so that one option governs how
talkative every stocnet package is.

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
that introduces `NA` silently shrinks the sample:
report what was dropped rather than letting the count change without comment.

### Console messaging

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
- Use `{cli}` inline classes to mark up what you refer to — `{.fn}` for functions,
  `{.arg}`/`{.var}` for arguments and variables, `{.val}` for values,
  `{.pkg}` for packages, `{.url}` for links.
- Use `{cli}`'s pluralisation rather than hand-written branches:
  `snet_warn("Dropped {length(dropped)} network{?s}.")`.

Prefer "`{.arg times}` must be a positive whole number" over "invalid input".
Where a function needs a package from `Suggests`, name it and say how to get it:
`snet_abort(c("The {.pkg lme4} package is required for random effects.", i = "Install it with {.run install.packages(\"lme4\")}."))`.

The model specification advice printed by `specificationAdvice()` is
informational, not load-bearing, so it belongs at `snet_info()`.

### Dependencies

`infernet` `Depends` on `{manynet}` (network classes, coercion and logical tests)
and `{netrics}` (measures), and `Imports` the `{future}`/`{furrr}`/`{purrr}`
stack plus `{reformulas}` for formula surgery.

Everything that only one estimator needs is in `Suggests`:
`lme4` and `glmmTMB` (random effects), `fixest` (fixed effects),
`gmm` (GMM estimation), `MASS` (negative binomial), `pscl` (zero-inflated Poisson),
`nnet` (multinomial), and `torch` (the GPU path).
A code path that depends on a suggested package must guard with
`requireNamespace()` and abort with an actionable message,
and its tests must `skip_if_not_installed()`.
Keeping these optional is deliberate: the common case — a gaussian or binomial
MRQAP — must install and run with no compiler and no heavy dependency tree.

The declared minimum of each `stocnet` dependency is the version on CRAN,
so that CI can install it.
Where `infernet` needs something that only a newer, unreleased `{manynet}` has,
reach it through a shim rather than by raising the minimum,
and resolve the name at call time from the namespace.
Test for the function rather than for the version string,
because a pre-release development build can carry the version
without yet exporting the function.

### Tests

This package uses the `testthat` package for testing functions.
Please see the [testthat website](https://testthat.r-lib.org) for more details.
`testthat` edition 3 with parallel execution is configured in `DESCRIPTION`
(`Config/testthat/parallel: true`).
`Config/testthat/start-first` should prioritise the test files that take longest to run.

Tests in `tests/testthat/` mirror the `R/` files
(e.g. `test-net_regression.R`, `test-zzz.R`).
Fixtures live in `tests/testthat/testdata/`.

Three things are worth asserting for every estimator that is added:

1. That the baseline coefficients match those of the equivalent
   `lm()`/`glm()` fit on the same vectorised data.
   The permutation inference is what is novel; the point estimates are not,
   and they should agree.
2. That a given `seed` reproduces the same p-values.
   Permutation results are only comparable across runs if the RNG is,
   and `furrr_options(seed = TRUE)` is what makes that true in parallel.
3. That the estimator is reached at all.
   Several paths in `fit_qap_model()` are selected by a combination of `family`,
   `estimator` and the random/fixed effects flags,
   so a test that does not name that combination does not cover it.

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
  `net_regression()` takes its options through a `control` list, so document
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
then `## Tests` (the `test_*()` family) and `## Regression` (`net_regression()`
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
