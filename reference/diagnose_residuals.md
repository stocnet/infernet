# The fitted values and residuals of a network regression

[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
[`residuals()`](https://rdrr.io/r/stats/residuals.html) give one value
for each dyad that a model from
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
used, in the rows of the dyad-level data, `x$pred`. They are the input
of a diagnosis, such as
[`diagnose_outliers()`](https://stocnet.github.io/infernet/reference/diagnose_outliers.md),
which reads the Pearson residuals.

They read the observed model, which the permutations do not change. For
the predicted values of new dyads, see
[predict_mrqap](https://stocnet.github.io/infernet/reference/predict_mrqap.md).

## Usage

``` r
# S3 method for class 'infernetMrqapFit'
fitted(object, ...)

# S3 method for class 'infernetMrqapFit'
residuals(object, ...)
```

## Arguments

- object:

  An `infernetMrqapFit`, from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md).

- ...:

  For [`residuals()`](https://rdrr.io/r/stats/residuals.html), passed to
  the [`residuals()`](https://rdrr.io/r/stats/residuals.html) method of
  the observed model, such as `type = "pearson"` for a `glm`. Not used
  by [`fitted()`](https://rdrr.io/r/stats/fitted.values.html).

## Value

A numeric vector, with one value for each dyad that the model used.

## Fixed effects

A gaussian model absorbs its fixed effects by demeaning.
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) gives the
observed values less the residuals, not the demeaned fitted values.

## See also

Other evaluating functions:
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md),
[`diagnose_outliers()`](https://stocnet.github.io/infernet/reference/diagnose_outliers.md),
[`diagnose_time`](https://stocnet.github.io/infernet/reference/diagnose_time.md),
[`evaluate_likelihood`](https://stocnet.github.io/infernet/reference/evaluate_likelihood.md),
[`test_fit`](https://stocnet.github.io/infernet/reference/test_fit.md),
[`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)

## Examples

``` r
css_model <- estimate_mrqap(advice ~ same(dept) + reports,
                            manynet::ison_hightech, times = 5)
head(fitted(css_model))
#> [1] 0.3723726 0.2546663 0.3723726 0.2546663 0.2546663 0.2546663
head(residuals(css_model, type = "pearson"))
#> [1] 1.298261 1.710762 1.298261 1.710762 1.710762 1.710762
```
