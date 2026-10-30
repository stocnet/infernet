# Predicting from a network regression

`predict()` gives the predicted value of the outcome for each dyad.

- For a `netreg`, from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md),
  it predicts either the dyads that the model was fitted on, or the
  dyads of `newdata`.

- For a `netlm` or `netlogit`, from `net_regression()` in `{migraph}`
  before 2.0.0, it predicts the rows of a design matrix.

## Usage

``` r
# S3 method for class 'netreg'
predict(object, newdata = NULL, type = c("link", "response"), ...)

# S3 method for class 'netlm'
predict(object, newdata = NULL, ...)

# S3 method for class 'netlogit'
predict(object, newdata = NULL, type = c("link", "response"), ...)
```

## Arguments

- object:

  A `netreg` object, as returned by
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md),
  or a `netlm` or `netlogit` object.

- newdata:

  The dyads to predict. For a `netreg`, `NULL` (the default) predicts
  the dyads that the model was fitted on. Otherwise, a data frame or a
  matrix with a column for each predictor, named as the coefficients
  are, e.g. `ego_Citations`. A column for an interaction, e.g.
  `ego_Age:same_Grp`, is computed from its parts where it is absent. The
  model's own dyad-level data, `object$pred`, has these columns. A
  matrix without column names needs a column for each coefficient, in
  their order, with the intercept first. For a `netlm` or `netlogit`, a
  design matrix with the same columns as the fitted model.

- type:

  The scale of the predictions: `"link"` (the default), the scale of the
  linear predictor, or `"response"`, the scale of the outcome, e.g.
  probabilities for a binomial model and expected counts for a count
  model. For a gaussian model the two are the same.

- ...:

  Not used.

## Value

A numeric vector of predicted values, one for each dyad or row.

## Details

Without `newdata`, the prediction for a `netreg` is the fitted value of
each dyad of the model, with any random intercepts and absorbed fixed
effects.

With `newdata`, the prediction comes from the coefficients only. A
random intercept is then zero, as for a dyad of a node the model has not
seen. A fixed effect is absorbed, so it has no coefficient, and a model
with fixed effects predicts only the dyads it was fitted on. For a
zero-inflated Poisson model, the zero-inflation part reads the same
columns as the count part.

## Examples

``` r
networkers <- manynet::ison_networkers |>
  manynet::to_subgraph(Discipline == "Sociology")
model1 <- estimate_mrqap(weight ~ ego(Citations) + alter(Citations),
                         networkers, times = 5)
# The fitted value of each dyad
head(predict(model1))
#> [1] 20.209569 13.467296 23.382403  7.914836 11.087671 22.589195
# A sender with 10 citations, to a receiver with 50
predict(model1, data.frame(ego_Citations = 10, alter_Citations = 50))
#> [1] 16.4984
```
