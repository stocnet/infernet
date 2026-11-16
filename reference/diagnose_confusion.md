# Diagnosing how well a model classifies ties

`diagnose_confusion()` compares the ties that a model of a binary
outcome predicts with the ties that were observed. It has a method for
an `infernetMrqapFit` from
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md):
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
metrics in one row, after `{broom}`. `{autograph}` plots it. It also has
a method for an `ergm` of a binary network, from
[`ergm::ergm()`](https://rdrr.io/pkg/ergm/man/ergm.html).

## Usage

``` r
diagnose_confusion(x, ...)

# Default S3 method
diagnose_confusion(x, ...)

# S3 method for class 'infernetMrqapFit'
diagnose_confusion(x, threshold = 0.5, times = 1000, control = list(), ...)

# S3 method for class 'ergm'
diagnose_confusion(x, threshold = 0.5, times = 1000, control = list(), ...)

# S3 method for class 'infernetConfusion'
tidy(x, ...)

# S3 method for class 'infernetConfusion'
glance(x, ...)
```

## Arguments

- x:

  An `infernetMrqapFit` of a binary outcome, from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md),
  or an `ergm` of a binary network, from
  [`ergm::ergm()`](https://rdrr.io/pkg/ergm/man/ergm.html). For
  [`print()`](https://rdrr.io/r/base/print.html),
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

`diagnose_confusion()` returns an object of class `infernetConfusion`, a
list with the mean `confusion_matrix`, its `confusion_matrix_sd`,
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

With `"confusion"` in `outputs`, as by default,
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
keeps the result for a binary outcome as `$confusion_matrix`, from 1000
draws with the seed of the fit.

For an `ergm` from
[`ergm::ergm()`](https://rdrr.io/pkg/ergm/man/ergm.html), the predicted
probability of each dyad is `{ergm}`'s
[`predict()`](https://stocnet.github.io/infernet/reference/predict_mrqap.md):
the probability of a tie, given the rest of the observed network. For a
model without dependence terms, such as one of `edges`, `nodecov()`, and
`nodematch()`, this is the probability of the tie under the model. For a
model with dependence terms, such as `gwesp()` or `triangle`, it holds
only where every other tie is as observed, so the matrix describes how
well the model predicts one tie from the others, and a warning says so.
Unobserved dyads are left out.

## See also

Other evaluating functions:
[`diagnose_outliers()`](https://stocnet.github.io/infernet/reference/diagnose_outliers.md),
[`diagnose_residuals`](https://stocnet.github.io/infernet/reference/diagnose_residuals.md),
[`diagnose_time`](https://stocnet.github.io/infernet/reference/diagnose_time.md),
[`evaluate_likelihood`](https://stocnet.github.io/infernet/reference/evaluate_likelihood.md),
[`test_fit`](https://stocnet.github.io/infernet/reference/test_fit.md),
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
