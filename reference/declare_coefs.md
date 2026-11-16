# Declare the coefficients of a model

`declare_coefs()` makes a set of coefficients for the terms of a model:
one value for each column that the specification builds on a network. A
set of coefficients gives the initial values of an estimation, or the
model that [`simulate()`](https://rdrr.io/r/stats/simulate.html) draws
networks from.

It is a generic, with a method for each model:

- for a specification from
  [`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md),
  the values stated in `...`;

- for an `infernetMrqapFit`, from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md),
  the estimated coefficients, with the specification, the family, and
  the residual standard deviation of the fit.

A coefficient is the estimated or stated value of a model's term. A
value that is not stated is `NA`.

## Usage

``` r
declare_coefs(x, ...)

# Default S3 method
declare_coefs(x, ...)

# S3 method for class 'mrqap_formula'
declare_coefs(x, ..., family = NULL, sigma = NULL, .data = NULL)

# S3 method for class 'infernetMrqapFit'
declare_coefs(x, ...)

# S3 method for class 'infernetMrqapCoefs'
coef(object, ...)

# S3 method for class 'infernetMrqapFit'
coef(object, ...)
```

## Arguments

- x:

  A specification from
  [`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md),
  or a fitted model, such as an `infernetMrqapFit` from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md).

- ...:

  For a specification, the values of the coefficients, each named for
  its column, such as `"(Intercept)" = -2, ego_age = 0.1`, or as one
  named vector, such as `stats::coef(fit)`. For a fit, not used.

- family:

  The family of the model: `"gaussian"`, `"binomial"`, `"poisson"`,
  `"negbin"`, or `"zip"`, as in
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md).
  `NULL`, the default, reads it from the values of the outcome in
  `.data`, as
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
  does: `"binomial"` for a 0/1 outcome, `"poisson"` for a count, and
  `"gaussian"` for any other (see
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)).
  Without `.data`, `NULL` aborts.

- sigma:

  For a `"gaussian"` model, the standard deviation of the residuals.
  Other families do not use it.

- .data:

  Optionally, the network that the specification builds its columns on,
  to check the names of the values.

- object:

  A set of coefficients from `declare_coefs()`, or an `infernetMrqapFit`
  from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md).

## Value

An object of class `infernetMrqapCoefs`: a named numeric vector of the
coefficients, with `NA` for a value that is not stated. Its attributes
are `spec`, the specification; `family`, the family; and `sigma`, the
standard deviation of the residuals, or `NULL`. Printing it gives the
family, the formula, and the values.
[`stats::coef()`](https://rdrr.io/r/stats/coef.html) gives the values as
a plain named vector.
[`stats::coef()`](https://rdrr.io/r/stats/coef.html) of an
`infernetMrqapFit` gives its estimated coefficients the same way, so
`coef(declare_coefs(fit))` equals `coef(fit)`.

## Details

The names of the values are the names of the columns that the
specification builds, as
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
names its coefficients: `"(Intercept)"`, and then a column such as
`"ego_age"` or `"same_dept"`.
`summary(specify_mrqap(formula, .data), .data)` lists the columns of
each term in its `columns` column. With `.data`, `declare_coefs()`
checks each name against these columns, and gives each column without a
value an `NA`. Without it, the names are checked when
[`simulate()`](https://rdrr.io/r/stats/simulate.html) builds the
columns.

[`simulate()`](https://rdrr.io/r/stats/simulate.html) reads a set of
coefficients of a `"gaussian"`, `"binomial"`, or `"poisson"` model. A
`"gaussian"` model also needs `sigma`, the standard deviation of its
residuals, also where the family was read from the outcome.
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
uses a set of coefficients as initial values where its estimator
iterates.

## See also

[simulate_mrqap](https://stocnet.github.io/infernet/reference/simulate_mrqap.md)
to simulate networks from a set of coefficients, and
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
for the initial values of an estimation.

Other specifying functions:
[`declare_coefs_ergm`](https://stocnet.github.io/infernet/reference/declare_coefs_ergm.md),
[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify_ergm.md),
[`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md),
[`specify_saom()`](https://stocnet.github.io/infernet/reference/specify_saom.md),
[`specify_terms`](https://stocnet.github.io/infernet/reference/specify_terms.md),
[`terms_mrqap`](https://stocnet.github.io/infernet/reference/terms_mrqap.md)

## Examples

``` r
spec <- specify_mrqap(friends ~ social + tasks)
coefs <- declare_coefs(spec, "(Intercept)" = -1.5, social = 0.2,
                       tasks = -0.4, family = "poisson",
                       .data = ison_algebra)
coefs
#> # Coefficients of a poisson MRQAP
#> # 3 of 3 coefficients stated
#> 
#> friends ~ social + tasks
#> 
#> (Intercept)      social       tasks 
#>        -1.5         0.2        -0.4 
stats::coef(coefs)
#> (Intercept)      social       tasks 
#>        -1.5         0.2        -0.4 

# The coefficients of a fit
fit <- estimate_mrqap(friends ~ social + tasks, ison_algebra, times = 5,
                      control = list(family = "poisson"))
declare_coefs(fit)
#> # Coefficients of a poisson MRQAP
#> # 3 of 3 coefficients stated
#> 
#> friends ~ social + tasks
#> 
#> (Intercept)      social       tasks 
#>  -1.6058039   0.2325937  -0.4087764 
```
