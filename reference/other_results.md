# Tidying the results of other network models

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`glance()`](https://generics.r-lib.org/reference/glance.html) give the
results of other packages' network models as a tibble, after `{broom}`,
with the same columns as for a `netreg` (see
[netreg_results](https://stocnet.github.io/infernet/reference/netreg_results.md))
where they mean the same. So an MRQAP result can sit in one table beside
an ERGM or a SIENA one. They cover:

- `netlm` and `netlogit`, from `net_regression()` in `{migraph}` before
  2.0.0, which also have a
  [`print()`](https://rdrr.io/r/base/print.html) method;

- `ergm`, from `ergm()` in `{ergm}`;

- `sienaFit`, from `siena07()` in `{RSiena}`.

## Usage

``` r
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
```

## Arguments

- x:

  A `netlm`, `netlogit`, `ergm`, or `sienaFit`.

- conf.int:

  Logical, whether to add the columns `conf.low` and `conf.high`.

- conf.level:

  The level of the interval that `conf.int` adds.

- ...:

  Further arguments, passed to
  [`summary()`](https://rdrr.io/r/base/summary.html) for an `ergm` and
  otherwise not used.

- exponentiate:

  Logical, whether to exponentiate the estimates, e.g. to give odds
  ratios for a logistic model.

- deviance, mcmc:

  For an `ergm`, whether to add the deviance columns or the MCMC
  settings.

## Value

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`glance()`](https://generics.r-lib.org/reference/glance.html) return a
tibble, and [`print()`](https://rdrr.io/r/base/print.html) returns `x`,
invisibly.

## Details

For a `{migraph}` result, `conf.int = TRUE` adds the quantiles of the
permuted test statistics. These describe the null distribution, not an
interval around the estimate.

## See also

Other models:
[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md),
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md),
[`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md),
[`print.netreg_lm()`](https://stocnet.github.io/infernet/reference/netreg_results.md),
[`regression`](https://stocnet.github.io/infernet/reference/regression.md),
[`specify`](https://stocnet.github.io/infernet/reference/specify.md),
[`specify_terms`](https://stocnet.github.io/infernet/reference/specify_terms.md),
[`terms_mrqap`](https://stocnet.github.io/infernet/reference/terms_mrqap.md),
[`test_fit`](https://stocnet.github.io/infernet/reference/test_fit.md),
[`test_measures`](https://stocnet.github.io/infernet/reference/test_measures.md),
[`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)

## Examples

``` r
fit <- suppressMessages(
  ergm::ergm(manynet::as_network(manynet::ison_adolescents) ~ edges))
tidy(fit)
#> # A tibble: 1 × 6
#>   term  estimate std.error mcmc.error statistic p.value
#>   <chr>    <dbl>     <dbl>      <dbl>     <dbl>   <dbl>
#> 1 edges   -0.588     0.394          0     -1.49   0.136
glance(fit)
#> # A tibble: 1 × 5
#>   independence iterations logLik   AIC   BIC
#>   <lgl>             <int>  <dbl> <dbl> <dbl>
#> 1 TRUE                  4  -18.2  38.5  39.8
```
