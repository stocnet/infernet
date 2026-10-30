# Tests of a model's parameters

These generics test hypotheses about the parameters of a fitted model:

- `test_parameter()` asks whether one or more parameters have the values
  that a hypothesis gives them.

- `test_time()` asks whether parameters are constant over time.

Each has a method for a `netreg`, from
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md).
`{infernet}` holds the generics so that other packages can register
methods for their own models on them.

## Usage

``` r
test_parameter(x, ...)

# Default S3 method
test_parameter(x, ...)

# S3 method for class 'netreg'
test_parameter(x, terms, value = 0, ..., times = 1000, control = list())

test_time(x, ...)

# Default S3 method
test_time(x, ...)

# S3 method for class 'netreg'
test_time(x, terms = NULL, ..., times = 1000, control = list())
```

## Arguments

- x:

  A fitted model, such as a `netreg` from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md).

- ...:

  Arguments for a method. Not used by the methods for a `netreg`.

- terms:

  The names of the coefficients to test, as the model prints them, such
  as `"ego_Age"`. A name that is not a coefficient selects every
  coefficient that starts with it and an underscore, so `"ego_Grp"`
  selects the levels `"ego_Grp_b"` and `"ego_Grp_c"`. For `test_time()`,
  `NULL` tests every coefficient but the intercept.

- value:

  The value of the coefficients under the hypothesis: one number for all
  of them, or one for each.

- times:

  The number of permutations.

- control:

  A list of controls, or the result of
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md).
  The tests read `seed`, `strategy`, `ncores`, and `groups`, and the
  controls set with `control_mrqap(..., persist = TRUE)` where these are
  not named.

## Value

For a `netreg`, a `network_test`, with the statistic, the permutation
distribution, and the p-value as `pgteobs`. It also holds the tested
coefficients as `terms`, the tested values as `value`, and the number of
permutations left out because the model did not converge on them as
`failed`.

## Details

For a `netreg`, both tests compare a model in which the tested
coefficients are free with one in which they are fixed. The statistic is
the deviance that freeing them removes: the residual sum of squares of a
gaussian model, and the deviance of a binomial or poisson one. The
p-value comes from a joint double semi-partialling (Dekker, Krackhardt
and Snijders 2007). Each tested predictor is residualised on the
untested ones, one permutation of the nodes is applied to all the
residuals together, and the model is fitted again. The permutations
exchange only nodes whose unobserved cells match, as
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
does, so every permutation uses the dyads of the observed model. The
p-value is \$\$p = \frac{1 + \\\\s\_{perm} \geq s\_{obs}\\}{1 + k}\$\$
where \\k\\ is the number of permutations that the model could be fitted
to.

With one tested coefficient, this is the test that
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
gives that coefficient under `permute = "predictor"`, with the deviance
in place of the t-statistic. Its value is that it can test several
coefficients together, such as the levels of a categorical attribute,
and test a value other than 0.

`test_parameter()` tests that the coefficients of `terms` equal `value`.
In a gaussian model it subtracts `value` times each predictor from the
outcome, and tests that the coefficients of the result are 0. In a
binomial or poisson model, `value` must be 0, since a value there would
be an offset, and an offset under permutation has no clear meaning.

`test_time()` tests that each coefficient of `terms` is the same in
every fitted wave of a panel. It adds an interaction of each tested
predictor with each fitted wave after the first, and an intercept for
each such wave, and tests that the interactions are all 0. The
intercepts for the waves are not tested, since the density of a network
often changes from wave to wave. One permutation relabels the nodes in
the same way in every wave.

## Network regressions

Both methods read the dyads that the model kept, so they do not need the
network again. They cannot yet test a model of a cognitive social
structure, a model with random intercepts, or a model with fixed effects
that is not gaussian. A model fitted with `groups` needs the same
`groups` in `control`. `test_time()` needs a panel, a longitudinal
network that
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
fits wave by wave. A list of networks is not a panel, since nothing says
that its networks are in time order.

## Other packages

`{RSiena}` and `{goldfish}` export `test_parameter()` and `test_time()`
generics with the same signature, for their own fitted models. If more
than one of these packages is attached, the one attached last masks the
others. Call `infernet::test_parameter()`, `RSiena::test_parameter()`,
or `goldfish::test_parameter()` to choose.

## References

Dekker, David, David Krackhardt, and Tom A. B. Snijders. 2007.
"Sensitivity of MRQAP Tests to Collinearity and Autocorrelation
Conditions." *Psychometrika* 72(4): 563-581.
[doi:10.1007/s11336-007-9016-1](https://doi.org/10.1007/s11336-007-9016-1)

## See also

Other models:
[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md),
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md),
[`other_results`](https://stocnet.github.io/infernet/reference/other_results.md),
[`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md),
[`print.netreg_lm()`](https://stocnet.github.io/infernet/reference/netreg_results.md),
[`regression`](https://stocnet.github.io/infernet/reference/regression.md),
[`specify`](https://stocnet.github.io/infernet/reference/specify.md),
[`specify_terms`](https://stocnet.github.io/infernet/reference/specify_terms.md),
[`terms_mrqap`](https://stocnet.github.io/infernet/reference/terms_mrqap.md),
[`test_fit`](https://stocnet.github.io/infernet/reference/test_fit.md),
[`test_measures`](https://stocnet.github.io/infernet/reference/test_measures.md)

## Examples

``` r
fit <- estimate_mrqap(. ~ ego(Grp) + alter(Age), ison_adolescents |>
  mutate_nodes(Grp = rep(c("a", "b", "c"), length.out = 8),
               Age = 12:19), times = 50)
# Do the groups differ, taking both levels together?
test_parameter(fit, "ego_Grp", times = 100, control = list(seed = 1))
#> # Parameter test of ego_Grp_b, ego_Grp_c = 0, 0 in a logistic MRQAP of ties
#> 
#> # A tibble: 1 × 6
#>   statistic null.mean null.sd p.lower p.upper signif   
#>       <dbl>     <dbl>   <dbl> <chr>   <chr>   <noquote>
#> 1      1.14      2.53    2.21 0.327   0.683            
#> 
#> # Null: 100 permutations of the tested predictors, by double semi-partialling.
#> # The p-value is p.upper: only a larger statistic counts against the null.
#> # Controls: seed = 1
```
