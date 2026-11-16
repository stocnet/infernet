# Finding the outliers of a fitted model

`diagnose_outliers()` finds the observations that a model fits least
well. `{infernet}` holds the generic, so that `{goldfish}` and other
packages register methods for their own fits on the same generic, and a
user calls one function for any model. It has a method for an
`infernetMrqapFit`, from
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md),
which finds the dyads with the most extreme residuals.

[`diagnose_changepoints()`](https://stocnet.github.io/infernet/reference/diagnose_time.md),
[`diagnose_onset()`](https://stocnet.github.io/infernet/reference/diagnose_time.md),
and
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md)
are diagnostics of the same family.

## Usage

``` r
diagnose_outliers(x, ...)

# Default S3 method
diagnose_outliers(x, ...)

# S3 method for class 'infernetMrqapFit'
diagnose_outliers(x, method = c("Hampel", "IQR", "Top"), threshold = 3, ...)
```

## Arguments

- x:

  A fitted model, such as an `infernetMrqapFit` from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md).

- ...:

  Arguments for a method. Not used by the methods here.

- method:

  How to flag an outlier: `"Hampel"` (the default), `"IQR"`, or `"Top"`,
  as in `{goldfish}`.

- threshold:

  The number of scaled median absolute deviations for `"Hampel"`, the
  number of half interquartile ranges for `"IQR"`, or the number of
  dyads to flag for `"Top"`.

## Value

For an `infernetMrqapFit`, an object of class `infernetOutliers`: a
tibble of one row for each dyad of the model, with its `sender` and
`receiver`, its `network` for a list of networks or a panel, its
`reporter` for a cognitive social structure, its `.residual`, whether it
is an `outlier`, and a `label` of the dyad where it is one. Its
attributes `method` and `threshold` record how it was made. The print
shows the outliers only.

## Details

For an `infernetMrqapFit`, `diagnose_outliers()` reads the Pearson
residual of each dyad of the observed model: the residual of a gaussian
model, and the residual over its standard deviation for another family.

- `"Hampel"` flags a dyad whose residual is further than `threshold`
  times the scaled median absolute deviation from the median residual. A
  network has no order of its dyads, so the Hampel identifier reads all
  the dyads together, not in a moving window.

- `"IQR"` flags a dyad whose residual is further than `threshold / 2`
  times the interquartile range from the median residual.

- `"Top"` flags the `threshold` dyads with the largest absolute
  residuals.

The residuals come from the observed model, which the permutations do
not change, so the result says nothing about the p-values.

## See also

Other evaluating functions:
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md),
[`diagnose_residuals`](https://stocnet.github.io/infernet/reference/diagnose_residuals.md),
[`diagnose_time`](https://stocnet.github.io/infernet/reference/diagnose_time.md),
[`evaluate_likelihood`](https://stocnet.github.io/infernet/reference/evaluate_likelihood.md),
[`test_fit`](https://stocnet.github.io/infernet/reference/test_fit.md),
[`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)

## Examples

``` r
fit <- estimate_mrqap(weight ~ ego(Citations) + alter(Citations),
                      manynet::ison_networkers, times = 5)
diagnose_outliers(fit, method = "Top", threshold = 5)
#> # Outliers of a poisson MRQAP of weight
#> # 5 of 992 dyads flagged by Top, with a threshold of 5
#> 
#> # A tibble: 5 × 4
#>   sender        receiver      .residual outlier
#>   <chr>         <chr>             <dbl> <lgl>  
#> 1 Doug White    Lin Freeman        76.8 TRUE   
#> 2 Barry Wellman Lin Freeman       134.  TRUE   
#> 3 Lin Freeman   Doug White        110.  TRUE   
#> 4 Lin Freeman   Russ Bernard       81.0 TRUE   
#> 5 Lin Freeman   Barry Wellman      93.7 TRUE   
```
