# Predicting from a network regression, and its terms

`predict()` gives the predicted value of the outcome for each dyad.

- For an `infernetMrqapFit`, from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md),
  it predicts either the dyads that the model was fitted on, or the
  dyads of `newdata`.

- For a `netlm` or `netlogit`, from `net_regression()` in `{migraph}`
  before 2.0.0, it predicts the rows of a design matrix.

[`formula()`](https://rdrr.io/r/stats/formula.html) and
[`terms()`](https://rdrr.io/r/stats/terms.html) of an `infernetMrqapFit`
give the formula that was fitted and the columns of its coefficients.

## Usage

``` r
# S3 method for class 'infernetMrqapFit'
predict(object, newdata = NULL, type = c("link", "response"), ...)

# S3 method for class 'netlm'
predict(object, newdata = NULL, ...)

# S3 method for class 'netlogit'
predict(object, newdata = NULL, type = c("link", "response"), ...)

# S3 method for class 'infernetMrqapFit'
formula(x, ...)

# S3 method for class 'infernetMrqapFit'
terms(x, ...)
```

## Arguments

- object:

  An `infernetMrqapFit` object, as returned by
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md),
  or a `netlm` or `netlogit` object.

- newdata:

  The dyads to predict. For an `infernetMrqapFit`, `NULL` (the default)
  predicts the dyads that the model was fitted on. Otherwise, a data
  frame or a matrix with a column for each predictor, named as the
  coefficients are, e.g. `ego_Citations`. A column for an interaction,
  e.g. `ego_Age:same_Grp`, is computed from its parts where it is
  absent. The model's own dyad-level data, `object$pred`, has these
  columns. A matrix without column names needs a column for each
  coefficient, in their order, with the intercept first. For a `netlm`
  or `netlogit`, a design matrix with the same columns as the fitted
  model.

- type:

  The scale of the predictions: `"link"` (the default), the scale of the
  linear predictor, or `"response"`, the scale of the outcome, e.g.
  probabilities for a binomial model and expected counts for a count
  model. For a gaussian model the two are the same.

- ...:

  Not used.

- x:

  For [`formula()`](https://rdrr.io/r/stats/formula.html) and
  [`terms()`](https://rdrr.io/r/stats/terms.html), an
  `infernetMrqapFit`, from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md).

## Value

A numeric vector of predicted values, one for each dyad or row.

[`formula()`](https://rdrr.io/r/stats/formula.html) of a fit gives a
formula, and [`terms()`](https://rdrr.io/r/stats/terms.html) a `terms`
object.

## Details

Without `newdata`, the prediction for an `infernetMrqapFit` is the
fitted value of each dyad of the model, with any random intercepts and
absorbed fixed effects.

With `newdata`, the prediction comes from the coefficients only. A
random intercept is then zero, as for a dyad of a node the model has not
seen. A fixed effect is absorbed, so it has no coefficient, and a model
with fixed effects predicts only the dyads it was fitted on. For a
zero-inflated Poisson model, the zero-inflation part reads the same
columns as the count part.

## The formula and the terms of a fit

[`formula()`](https://rdrr.io/r/stats/formula.html) of a fit from
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
gives the formula that was fitted, after
[`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md)
read it, and [`terms()`](https://rdrr.io/r/stats/terms.html) gives the
terms of the columns that were fitted, as the coefficients name them.
These are the columns that `newdata` needs, and that each coefficient is
the effect of. For the formula of chosen rows of a table of terms, see
[specify_terms](https://stocnet.github.io/infernet/reference/specify_terms.md).

## See also

Other interpreting functions:
[`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md),
[`simulate_mrqap`](https://stocnet.github.io/infernet/reference/simulate_mrqap.md),
[`sweep`](https://stocnet.github.io/infernet/reference/sweep.md)

## Examples

``` r
networkers <- manynet::ison_networkers |>
  manynet::to_subgraph(Discipline == "Sociology")
model1 <- estimate_mrqap(weight ~ ego(Citations) + alter(Citations),
                         networkers, times = 5)
# The fitted value of each dyad
head(predict(model1))
#> [1] 2.870599 2.404405 3.089984 2.020481 2.239866 3.035138
# A sender with 10 citations, to a receiver with 50
predict(model1, data.frame(ego_Citations = 10, alter_Citations = 50))
#> [1] 2.650721
fit <- estimate_mrqap(advice ~ same(dept) + reports,
                      manynet::ison_hightech, times = 5)
formula(fit)
#> advice ~ same(dept) + reports
#> <environment: 0x87d43dc78>
terms(fit)
#> advice ~ same_dept + reports
#> attr(,"variables")
#> list(advice, same_dept, reports)
#> attr(,"factors")
#>           same_dept reports
#> advice            0       0
#> same_dept         1       0
#> reports           0       1
#> attr(,"term.labels")
#> [1] "same_dept" "reports"  
#> attr(,"order")
#> [1] 1 1
#> attr(,"intercept")
#> [1] 1
#> attr(,"response")
#> [1] 1
#> attr(,".Environment")
#> <environment: 0x8875d1188>
```
