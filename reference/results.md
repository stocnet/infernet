# Reporting network regression and test results

[`print()`](https://rdrr.io/r/base/print.html) shows the result of
[`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md)
or of a test as a report: how the null distribution was made, the
estimates, and the permutation p-values.

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`glance()`](https://generics.r-lib.org/reference/glance.html) return
the same results as a tibble, after `{broom}`.
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) gives one row
for each term of a model, and
[`glance()`](https://generics.r-lib.org/reference/glance.html) gives one
row for the whole model. They cover:

- `netreg`, `netreg_lm`, `netreg_glm`, and `netreg_css`, from
  [`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md)

- `network_test`, from
  [`test_random()`](https://stocnet.github.io/infernet/reference/tests.md),
  [`test_configuration()`](https://stocnet.github.io/infernet/reference/tests.md),
  and
  [`test_permutation()`](https://stocnet.github.io/infernet/reference/tests.md)

- `netlm` and `netlogit`, from
  [`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md)
  in `{migraph}`

- `ergm`, from `ergm()` in `{ergm}`

- `sienaFit`, from `siena07()` in `{RSiena}`

So an MRQAP result can sit in one table beside an ERGM or a SIENA one.

## Usage

``` r
# S3 method for class 'netreg_lm'
print(x, ..., print_b = FALSE, print_random = FALSE)

# S3 method for class 'netreg_glm'
print(x, ..., print_b = FALSE, print_random = FALSE)

# S3 method for class 'netreg'
print(x, ...)

# S3 method for class 'netreg'
tidy(
  x,
  exponentiate = FALSE,
  alternative = c("two.sided", "less", "greater"),
  ...
)

# S3 method for class 'netreg'
glance(x, ...)

# S3 method for class 'netreg_lm'
glance(x, ...)

# S3 method for class 'netreg_glm'
glance(x, ...)

# S3 method for class 'netreg_css'
glance(x, ...)

# S3 method for class 'network_test'
tidy(x, ...)

# S3 method for class 'network_test'
glance(x, ...)

# S3 method for class 'netlm'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'netlogit'
tidy(x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, ...)

# S3 method for class 'netlm'
glance(x, ...)

# S3 method for class 'netlogit'
glance(x, ...)

# S3 method for class 'netlm'
print(x, ...)

# S3 method for class 'netlogit'
print(x, ...)

# S3 method for class 'ergm'
tidy(x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, ...)

# S3 method for class 'ergm'
glance(x, deviance = FALSE, mcmc = FALSE, ...)

# S3 method for class 'sienaFit'
tidy(x, ...)

# S3 method for class 'sienaFit'
glance(x, ...)

# S3 method for class 'network_test'
print(x, ..., max.length = 6, digits = 3)

# S3 method for class 'netreg_css'
print(x, ...)
```

## Arguments

- x:

  A model or test result, e.g. a `netreg` object, as returned by
  [`net_regression()`](https://stocnet.github.io/infernet/reference/regression.md),
  or a `network_test` object, as returned by
  [`test_random()`](https://stocnet.github.io/infernet/reference/tests.md),
  [`test_configuration()`](https://stocnet.github.io/infernet/reference/tests.md),
  or
  [`test_permutation()`](https://stocnet.github.io/infernet/reference/tests.md).

- ...:

  Further arguments, passed to
  [`summary()`](https://rdrr.io/r/base/summary.html) for an `ergm` and
  otherwise not used.

- print_b:

  Logical; also print coefficient-based (not t-based) permutation
  p-values.

- print_random:

  Logical; print random intercepts when present.

- exponentiate:

  Logical, whether to exponentiate the estimates, e.g. to give odds
  ratios for a logistic model.

- alternative:

  For a `netreg`, which permutation p-value to report: `"two.sided"`
  (the default) for Pr(\>=\|t\|), `"less"` for Pr(\<=t), or `"greater"`
  for Pr(\>=t).

- conf.int:

  Logical, whether to add the columns `conf.low` and `conf.high`.

- conf.level:

  The level of the interval that `conf.int` adds.

- deviance, mcmc:

  For an `ergm`, whether to add the deviance columns or the MCMC
  settings.

- max.length, digits:

  Not used.

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns `x`, invisibly.
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`glance()`](https://generics.r-lib.org/reference/glance.html) return a
tibble. [`tidy()`](https://generics.r-lib.org/reference/tidy.html) gives
the columns `term`, `estimate`, `statistic`, and `p.value` for a
regression, and
[`glance()`](https://generics.r-lib.org/reference/glance.html) gives the
fit statistics that the model keeps. For a `network_test`, both give one
row with the observed statistic, the mean and standard deviation of the
null distribution, and the share of the null distribution at or below
(`p.lower`) and at or above (`p.upper`) the observed statistic.

## Details

A permutation test gives a p-value but no standard error, so the tidied
regression results have no `std.error` column. For a `netreg`, `p.value`
is read from the permutations of the test statistic, as
[`print()`](https://rdrr.io/r/base/print.html) shows it. When the
predictors are permuted, the intercept has no test, and its `p.value` is
`NA`.

For a `{migraph}` result, `conf.int = TRUE` adds the quantiles of the
permuted test statistics. These describe the null distribution, not an
interval around the estimate.

## See also

Other models:
[`regression`](https://stocnet.github.io/infernet/reference/regression.md),
[`tests`](https://stocnet.github.io/infernet/reference/tests.md)

## Examples

``` r
css_model <- net_regression(advice ~ same(dept) + reports,
                            manynet::ison_hightech, times = 5)
print(css_model, print_b = TRUE)
#> 
#> Generalized Linear Network Model for a cognitive social structure
#> 
#> Fitted on 8820 reported dyads from 21 reporters.
#> Significance was estimated using Dekker's
#>   'semi-partialling plus' procedure with 5 permutations.
#> Diagonal values (loops) were ignored.
#> The outcome was treated as directed. 
#> 
#> 
#> Coefficients:
#>             Estimate Pr(<=t) Pr(>=t) Pr(>=|t|)
#> (Intercept) -1.074   *       *       *        
#> same_dept    0.552    1       0       0       
#> reports      3.823    1       0       0       
#> 
#> * The intercept has no significance test when predictors are permuted.
#> 
#> AIC of base model: 9983.796
#> BIC of base model: 10005.05
#> 
#> 
#> Probabilistic Confusion Matrix
#> (based on 1000 Monte Carlo draws from predicted probabilities)
#> 
#> Mean Confusion Matrix:
#>       Predicted
#> Actual      0      1
#>      0 4401.4 1682.6
#>      1 1683.6 1052.4
#> 
#> 95% Credible Intervals:
#>       Predicted
#> Actual 0            1           
#>      0 [4336, 4468] [1643, 1725]
#>      1 [1617, 1749] [1010, 1092]
#> 
#> Derived Metrics (mean across draws):
#>   Accuracy:     0.6183  (SD: 0.0046 )
#>   Sensitivity:  0.3848 
#>   Specificity:  0.7233 
#>   Precision:    0.3847 
#>   F1 Score:     0.3847 
#> 
#> For comparison - Threshold (0.5) Confusion Matrix:
#>       Predicted
#> Actual    0    1
#>      0 6068   17
#>      1 2332  403
#>   Accuracy: 0.7337 
#> 
#> 
tidy(css_model)
#> # A tibble: 3 × 4
#>   term        estimate statistic p.value
#>   <chr>          <dbl>     <dbl>   <dbl>
#> 1 (Intercept)   -1.07     -38.5       NA
#> 2 same_dept      0.552      9.53       0
#> 3 reports        3.82      15.3        0
glance(css_model)
#> # A tibble: 1 × 11
#>   family   logLik   AIC    BIC deviance null.deviance df.residual  nobs permute 
#>   <chr>     <dbl> <dbl>  <dbl>    <dbl>         <dbl>       <dbl> <int> <chr>   
#> 1 binomial -4989. 9984. 10005.    9978.        10922.        8817  8820 predict…
#> # ℹ 2 more variables: times <dbl>, reporters <int>
```
