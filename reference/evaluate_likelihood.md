# Comparing network regressions by their likelihood

These methods give what [`AIC()`](https://rdrr.io/r/stats/AIC.html) and
[`BIC()`](https://rdrr.io/r/stats/AIC.html) read, so that two fits from
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
can be compared.

- [`nobs()`](https://rdrr.io/r/stats/nobs.html) gives the number of
  dyads that the model used: unobserved dyads, fixed ties, and the loops
  that the model leaves out are not counted.

- [`logLik()`](https://rdrr.io/r/stats/logLik.html) and
  [`deviance()`](https://rdrr.io/r/stats/deviance.html) give the
  log-likelihood and the deviance of the observed model.

They read the observed model, which the permutations do not change. To
test whether the terms that a larger model adds explain more than
chance, use [`anova()`](https://rdrr.io/r/stats/anova.html) of the two
fits (see
[test_parameter](https://stocnet.github.io/infernet/reference/test_parameter.md)).

## Usage

``` r
# S3 method for class 'infernetMrqapFit'
nobs(object, ...)

# S3 method for class 'infernetMrqapFit'
logLik(object, ...)

# S3 method for class 'infernetMrqapFit'
deviance(object, ...)
```

## Arguments

- object:

  An `infernetMrqapFit`, from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md).

- ...:

  Not used.

## Value

[`nobs()`](https://rdrr.io/r/stats/nobs.html) gives an integer,
[`logLik()`](https://rdrr.io/r/stats/logLik.html) a `logLik` object, and
[`deviance()`](https://rdrr.io/r/stats/deviance.html) a number.

## Fixed effects

A gaussian model absorbs its fixed effects by demeaning. Its
log-likelihood and deviance are those of the model with a dummy for each
group, and [`logLik()`](https://rdrr.io/r/stats/logLik.html) counts the
absorbed coefficients in its `df`, so that
[`AIC()`](https://rdrr.io/r/stats/AIC.html),
[`BIC()`](https://rdrr.io/r/stats/AIC.html), and
[`glance()`](https://generics.r-lib.org/reference/glance.html) are those
of that model too.

## See also

Other evaluating functions:
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md),
[`diagnose_outliers()`](https://stocnet.github.io/infernet/reference/diagnose_outliers.md),
[`diagnose_residuals`](https://stocnet.github.io/infernet/reference/diagnose_residuals.md),
[`diagnose_time`](https://stocnet.github.io/infernet/reference/diagnose_time.md),
[`test_fit`](https://stocnet.github.io/infernet/reference/test_fit.md),
[`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)

## Examples

``` r
css_model <- estimate_mrqap(advice ~ same(dept) + reports,
                            manynet::ison_hightech, times = 5)
nobs(css_model)
#> [1] 8820
logLik(css_model)
#> 'log Lik.' -4988.898 (df=3)
AIC(css_model)
#> [1] 9983.796
```
