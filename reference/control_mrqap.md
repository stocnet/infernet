# Controls for an MRQAP

`control_mrqap()` sets the controls of
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md),
checks each one, and returns them as an object of class `mrqap_control`.
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
passes its `control` argument through this function, so
`control = list(family = "poisson")` and
`control = control_mrqap(family = "poisson")` fit the same model.
Calling `control_mrqap()` first shows the full set of controls, and
finds a mistake before a long run starts. The tests, such as
[`test_permutation()`](https://stocnet.github.io/infernet/reference/tests.md),
read `ncores`, `strategy`, and `seed` from the same object, so one set
of controls serves a model and a test alike.

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
  family = c("auto", "gaussian", "binomial", "poisson", "negbin", "zip"),
  directed = NULL,
  loops = NULL,
  seed = NULL,
  groups = NULL,
  ncores = NULL,
  strategy = "sequential",
  use_robust_errors = FALSE,
  random_intercept_nets = FALSE,
  random_intercept_sender = FALSE,
  random_intercept_receiver = FALSE,
  random_intercept_reporter = FALSE,
  missing = "available",
  less_mem = FALSE,
  ...
)

# S3 method for class 'mrqap_control'
print(x, ...)
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

  The family of the model. `"auto"` (the default) reads it from the
  outcome: `"binomial"` for a binary outcome, otherwise `"gaussian"`,
  and says which. The others are `"gaussian"`, `"binomial"`,
  `"poisson"`, `"negbin"` (with `{MASS}`), and `"zip"`, a zero-inflated
  Poisson (with `{pscl}`).

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
  is. The same seed gives the same result on any `strategy`.

- groups:

  A vector with a group for each node, so that each node is permuted
  only among the nodes of its group. On a two-mode network, its length
  says which mode it groups. `NULL` (the default) permutes all nodes
  together.

- ncores:

  A number of cores to run the permutations on. More than one runs them
  on that many `"multisession"` workers, and one runs them sequentially,
  whatever `strategy` says. `NULL` (the default) leaves the choice to
  `strategy`.

- strategy:

  The `{future}` plan that the permutations run on where `ncores` is
  `NULL`: `"sequential"` (the default), `"multisession"`, `"multicore"`,
  or `"cluster"`. The plan that was set before is restored after the
  fit.

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
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md).

- less_mem:

  Logical, whether to leave the observed model object out of the result,
  to save memory on a large network. `FALSE` by default.

- ...:

  Controls by another name: a name that infernet used before, or an
  option of another model. See the description.

- x:

  An object of class `mrqap_control`.

## Value

An object of class `mrqap_control`: a list with one element for each
control.

## References

Dekker, David, David Krackhardt, and Tom A.B. Snijders. 2007.
"Sensitivity of MRQAP tests to collinearity and autocorrelation
conditions." *Psychometrika* 72(4): 563-581.
[doi:10.1007/s11336-007-9016-1](https://doi.org/10.1007/s11336-007-9016-1)

## See also

Other models:
[`print.netreg_lm()`](https://stocnet.github.io/infernet/reference/results.md),
[`regression`](https://stocnet.github.io/infernet/reference/regression.md),
[`specify`](https://stocnet.github.io/infernet/reference/specify.md),
[`tests`](https://stocnet.github.io/infernet/reference/tests.md)

## Examples

``` r
control_mrqap(family = "poisson", seed = 1)
#> Controls for an MRQAP
#> 
#>   permute                    predictor
#>   family                     poisson  (set)
#>   directed                   NULL, read from the data
#>   loops                      NULL, read from the data
#>   seed                       1  (set)
#>   groups                     NULL, all nodes together
#>   ncores                     NULL, as strategy says
#>   strategy                   sequential
#>   use_robust_errors          FALSE
#>   random_intercept_nets      FALSE
#>   random_intercept_sender    FALSE
#>   random_intercept_receiver  FALSE
#>   random_intercept_reporter  FALSE
#>   missing                    available
#>   less_mem                   FALSE
# A name that infernet used before is converted, with a warning
control_mrqap(nullhyp = "qapy")
#> Warning: `nullhyp = "qapy"` is now `permute = "outcome"`.
#> The control was renamed in infernet 0.2.0.
#> Controls for an MRQAP
#> 
#>   permute                    outcome  (set)
#>   family                     auto
#>   directed                   NULL, read from the data
#>   loops                      NULL, read from the data
#>   seed                       NULL, not set
#>   groups                     NULL, all nodes together
#>   ncores                     NULL, as strategy says
#>   strategy                   sequential
#>   use_robust_errors          FALSE
#>   random_intercept_nets      FALSE
#>   random_intercept_sender    FALSE
#>   random_intercept_receiver  FALSE
#>   random_intercept_reporter  FALSE
#>   missing                    available
#>   less_mem                   FALSE
```
