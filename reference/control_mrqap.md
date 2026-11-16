# Controls for an MRQAP

`control_mrqap()` sets the controls of
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md),
checks each one, and returns them as an object of class `mrqap_control`.
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
passes its `control` argument through this function, so
`control = list(family = "poisson")` and
`control = control_mrqap(family = "poisson")` fit the same model.
Calling `control_mrqap()` first shows the full set of controls, and
finds a mistake before a long run starts. The tests, such as
[`test_permutation()`](https://stocnet.github.io/infernet/reference/test_measures.md),
read `ncores`, `strategy`, and `seed` from the same object, so one set
of controls serves a model and a test alike. These three computation
controls can also be set for the session with
[`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md),
or for one call with
[`control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md).

A control that is not named keeps its default. A name that infernet used
before is converted to its current name, with a warning. An option of
another model that has no meaning in an MRQAP, such as goldfish's
`max_iterations`, is dropped, with one warning that names each. The
result of goldfish's `set_algorithm_newton()` or `set_preprocessing()`
(or `set_estimation_opt()` before goldfish 2.0.0) can be given whole.
Any other name aborts, and the nearest control is suggested.

## Usage

``` r
control_mrqap(
  permute = c("predictor", "outcome"),
  family = NULL,
  directed = NULL,
  loops = NULL,
  seed = NULL,
  groups = NULL,
  ncores = NULL,
  strategy = NULL,
  use_robust_errors = FALSE,
  random_intercept_nets = FALSE,
  random_intercept_sender = FALSE,
  random_intercept_receiver = FALSE,
  random_intercept_reporter = FALSE,
  missing = "available",
  less_mem = FALSE,
  persist = FALSE,
  ...
)
```

## Arguments

- permute:

  What the null distribution permutes. `"predictor"` (the default)
  residualises each main predictor against the others and permutes that
  residual, which is Dekker et al.'s (2007) double semi-partialling.
  `"outcome"` permutes the outcome and leaves the predictors alone. With
  one predictor there is nothing to residualise against, so
  `"predictor"` reduces to `"outcome"`, and the model says so.

- family:

  The family of the model. `NULL` (the default) reads it from the
  outcome's observed values, and says which: `"binomial"` for 0/1
  values; `"poisson"` for whole numbers of 0 or more, with at least one
  above 1; and `"gaussian"` otherwise. A rating or a rank on a scale of
  whole numbers also reads as a count, so name the family of such an
  outcome. `"auto"`, its name before infernet 0.7.0, still works and
  warns. The others are `"gaussian"`, `"binomial"`, `"poisson"`,
  `"negbin"` (with `{MASS}`), and `"zip"`, a zero-inflated Poisson (with
  `{pscl}`).

- directed:

  Logical, whether a tie from i to j differs from one from j to i.
  `NULL` (the default) reads it from the network, and says so.

- loops:

  Logical, whether loops, the ties from a node to itself, enter the
  model. `NULL` (the default) includes them only where the network has
  loops.

- seed:

  A number to set the random seed with before the permutations, so that
  a fit can be reproduced. `NULL` (the default) leaves the seed as it
  is, unless a seed is set with
  [`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md).
  The same seed gives the same result on any `strategy`.

- groups:

  A vector with a group for each node, so that each node is permuted
  only among the nodes of its group. On a two-mode network, its length
  says which mode it groups. `NULL` (the default) permutes all nodes
  together.

- ncores:

  A number of cores to run the permutations on. More than one runs them
  on that many `"multisession"` workers, and one runs them sequentially,
  whatever `strategy` says. `NULL` (the default) leaves the choice to
  `strategy`. A parallel run helps only where each permutation is
  costly: a family other than `"gaussian"`, random intercepts, or a
  cognitive social structure, on a network of more than about 50 nodes,
  with 1,000 or more `times`. Elsewhere the start of the workers costs
  more than it saves: a gaussian model solves its design once, so each
  permutation is cheap. Give `ncores` rather than
  `strategy = "multisession"`, which starts a worker on every core. Set
  it for the session with
  [`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md).

- strategy:

  The `{future}` plan that the permutations run on where `ncores` is
  `NULL`: `"sequential"`, `"multisession"`, `"multicore"`, or
  `"cluster"`. The plan that was set before is restored after the fit.
  `NULL` (the default) uses the plan that was set with
  [`future::plan()`](https://future.futureverse.org/reference/plan.html),
  which is sequential unless it was changed. Set a plan once in a
  session, with `future::plan(future::multisession, workers = 2)`, and
  its workers start once, not once in each call.

- use_robust_errors:

  Logical, whether to use HC3 robust standard errors for the observed
  model's t values. `FALSE` by default.

- random_intercept_nets, random_intercept_sender,
  random_intercept_receiver, random_intercept_reporter:

  Logical, whether to fit an `{lme4}` random intercept for each network
  of a list, each sender, each receiver, or each reporter of a cognitive
  social structure. `FALSE` by default. In the formula, `(1 | network)`,
  `(1 | sender)`, `(1 | receiver)`, and `(1 | reporter)` set the same
  controls.

- missing:

  What to do with an unobserved dyad, that is, an `NA` in the outcome or
  in a predictor. `"available"` (the default) drops the dyad, and
  permutes nodes only among nodes with the same unobserved cells, so
  that every permutation is fitted on the same dyads as the observed
  model. `"fail"` stops instead, and names how many dyads are unobserved
  on each variable. See the section on missing data in
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md).

- less_mem:

  Deprecated. Use `outputs =` of
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md):
  `less_mem = TRUE` leaves `"model"` out of `outputs`, so that the
  result keeps no observed model object.

- persist:

  Deprecated. Use
  [`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md)
  instead. See the section on persistent controls.

- ...:

  Controls by another name: a name that infernet used before, or an
  option of another model. See the description.

## Value

An object of class `mrqap_control`: a list with one element for each
control. Its attribute `source` says whether each control was `"given"`
(in the call or by
[`control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md)),
comes from an `"option"`, or keeps its `"default"`. The print shows each
control and where its value comes from.

## Persistent controls

[`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md)
sets `seed`, `ncores`, and `strategy` for the session, and each call of
`control_mrqap()` then uses them where they are not given. A control
given in the call takes priority, then one from
[`control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md),
then the option, then the default. `control_mrqap()` marks each control
that comes from an option with `(option)`. It reads the options when it
is called, so its result keeps the same controls if the options change
later.

`persist = TRUE` is deprecated, and warns once in a session. It still
writes the computation controls that it is given with
[`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md),
and the other controls to `options(infernet.control)`. That option is
read until infernet 0.8.0, and a computation control in
`options(snet_computation)` takes priority over it. `persist = FALSE`
clears both options.

Every result records the controls that it used, as `control` (an
attribute of a `diffs_model`). Give that record as `control` to fit the
same model again without the options of the session.

## References

Dekker, David, David Krackhardt, and Tom A.B. Snijders. 2007.
"Sensitivity of MRQAP tests to collinearity and autocorrelation
conditions." *Psychometrika* 72(4): 563-581.
[doi:10.1007/s11336-007-9016-1](https://doi.org/10.1007/s11336-007-9016-1)

## See also

Other estimating functions:
[`control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md),
[`other_results`](https://stocnet.github.io/infernet/reference/other_results.md),
[`print.infernetMrqapFit()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)

## Examples

``` r
control_mrqap(family = "poisson", seed = 1)
#> # Controls for an MRQAP: 2 set, 13 at their defaults
#> 
#>   family  "poisson"
#>   seed    1
# A name that infernet used before is converted, with a warning
control_mrqap(nullhyp = "qapy")
#> Warning: `nullhyp = "qapy"` is now `permute = "outcome"`.
#> ℹ The control was renamed in infernet 0.2.0.
#> ℹ See `help('defunct', package = 'manynet')`.
#> # Controls for an MRQAP: 1 set, 14 at their defaults
#> 
#>   permute  "outcome"
# Computation controls for this call only
c(control_mrqap(family = "poisson"), control_computation(seed = 2))
#> # Controls for an MRQAP: 2 set, 13 at their defaults
#> 
#>   family  "poisson"
#>   seed    2
```
