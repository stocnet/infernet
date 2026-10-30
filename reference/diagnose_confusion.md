# Diagnosing how well a model classifies ties

`diagnose_confusion()` compares the ties that a model of a binary
outcome predicts with the ties that were observed. It has a method for a
`netreg` from
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md):
a logistic model, or a linear model of a 0/1 outcome, read as a linear
probability model.

- The probabilistic confusion matrix draws each tie from its predicted
  probability, `times` times, and gives the mean count of each cell with
  a 95% interval, so it shows how uncertain the classification is.

- The metrics are the means over those draws: the accuracy, the
  sensitivity (the share of observed ties that are predicted), the
  specificity (the share of observed non-ties that are predicted), the
  precision (the share of predicted ties that are observed), and F1.

- The confusion matrix at `threshold` classifies each dyad as a tie
  where its predicted probability is at least `threshold`.

- The ROC curve gives the true positive rate against the false positive
  rate at every threshold, and the AUC is the area under it: the
  probability that an observed tie has a higher predicted probability
  than an observed non-tie.

[`print()`](https://rdrr.io/r/base/print.html) shows the matrix,
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) gives the ROC
curve, and
[`glance()`](https://generics.r-lib.org/reference/glance.html) gives the
metrics in one row, after `{broom}`. `{autograph}` plots it.

## Usage

``` r
diagnose_confusion(x, ...)

# Default S3 method
diagnose_confusion(x, ...)

# S3 method for class 'netreg_glm'
diagnose_confusion(x, threshold = 0.5, times = 1000, control = list(), ...)

# S3 method for class 'netreg_lm'
diagnose_confusion(x, threshold = 0.5, times = 1000, control = list(), ...)

# S3 method for class 'diagnose_confusion'
print(x, ...)

# S3 method for class 'diagnose_confusion'
tidy(x, ...)

# S3 method for class 'diagnose_confusion'
glance(x, ...)
```

## Arguments

- x:

  A `netreg` of a binary outcome, from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md).
  For [`print()`](https://rdrr.io/r/base/print.html),
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html), and
  [`glance()`](https://generics.r-lib.org/reference/glance.html), the
  result of `diagnose_confusion()`.

- ...:

  Not used.

- threshold:

  The predicted probability at or above which a dyad is classified as a
  tie.

- times:

  The number of draws of the probabilistic confusion matrix.

- control:

  A list with a `seed`, as for
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md).
  Without one, the seed of the fit is used.

## Value

`diagnose_confusion()` returns an object of class `diagnose_confusion`,
a list with the mean `confusion_matrix`, its `confusion_matrix_sd`,
`confusion_matrix_ci_lower`, and `confusion_matrix_ci_upper`, the
metrics, the `threshold_confusion_matrix`, the `roc` curve, and the
`auc`. [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns a tibble of `threshold`, `fpr`, and `tpr`, and
[`glance()`](https://generics.r-lib.org/reference/glance.html) a tibble
of one row of the metrics.
[`print()`](https://rdrr.io/r/base/print.html) returns `x`, invisibly.

## Details

The predicted probabilities are those of the observed model, which the
permutations do not change. A linear probability model can predict a
value below 0 or above 1, which is taken as 0 or 1.

[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
keeps the result for a binary outcome as `$confusion_matrix`, from 1000
draws with the seed of the fit.

## See also

Other models:
[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md),
[`other_results`](https://stocnet.github.io/infernet/reference/other_results.md),
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
fit <- estimate_mrqap(. ~ ego(Age) + alter(Age),
                      manynet::ison_adolescents |>
                        manynet::mutate_nodes(Age = 12:19),
                      times = 20, control = list(seed = 1))
(conf <- diagnose_confusion(fit))
#> # Confusion matrix of a logistic MRQAP of ties
#> # From 1000 draws of the predicted probabilities, with 95% intervals
#> 
#>       Predicted
#> Actual 0             1         
#>      0 13.6 [11, 16] 4.5 [2, 7]
#>      1 4.4 [2, 7]    5.5 [3, 8]
#> 
#>  accuracy sensitivity specificity precision    f1   auc threshold
#>     0.682       0.554       0.753      0.56 0.551 0.822       0.5
#>  threshold.accuracy nobs draws
#>               0.786   28  1000
glance(conf)
#> # A tibble: 1 × 10
#>   accuracy sensitivity specificity precision    f1   auc threshold
#>      <dbl>       <dbl>       <dbl>     <dbl> <dbl> <dbl>     <dbl>
#> 1    0.682       0.554       0.753     0.560 0.551 0.822       0.5
#> # ℹ 3 more variables: threshold.accuracy <dbl>, nobs <int>, draws <int>
tidy(conf)
#> # A tibble: 29 × 3
#>    threshold    fpr   tpr
#>        <dbl>  <dbl> <dbl>
#>  1   Inf     0        0  
#>  2     0.843 0        0.1
#>  3     0.808 0        0.2
#>  4     0.768 0        0.3
#>  5     0.722 0        0.4
#>  6     0.671 0.0556   0.4
#>  7     0.641 0.111    0.4
#>  8     0.616 0.111    0.5
#>  9     0.584 0.167    0.5
#> 10     0.557 0.167    0.6
#> # ℹ 19 more rows
```
