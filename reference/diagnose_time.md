# Diagnosing a model of a sequence over time

These generics diagnose a model of a sequence of events or of waves.

- `diagnose_changepoints()` finds where the fit of a model of a sequence
  changes.

- `diagnose_onset()` finds when a model of a sequence starts to have
  information about its coefficients.

`{infernet}` holds these generics so that `{goldfish}` and `{RSiena}`
can share them. Each package can then register a method for its own
fits, such as a `{goldfish}` model of relational events or an `{RSiena}`
model of network panel waves, and a user calls the same function for
each model. The two generics take `{goldfish}`'s signature, `(x, ...)`,
as
[`diagnose_outliers()`](https://stocnet.github.io/infernet/reference/diagnose_outliers.md)
does.

`{infernet}` itself has no method for these generics yet, so the default
method gives an error that names the class of `x`.

## Usage

``` r
diagnose_changepoints(x, ...)

# Default S3 method
diagnose_changepoints(x, ...)

diagnose_onset(x, ...)

# Default S3 method
diagnose_onset(x, ...)
```

## Arguments

- x:

  A fitted model of a sequence.

- ...:

  Arguments for a method.

## Value

The diagnosis that the method for the class of `x` gives. The default
method gives an error.

## See also

Other evaluating functions:
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md),
[`diagnose_outliers()`](https://stocnet.github.io/infernet/reference/diagnose_outliers.md),
[`diagnose_residuals`](https://stocnet.github.io/infernet/reference/diagnose_residuals.md),
[`evaluate_likelihood`](https://stocnet.github.io/infernet/reference/evaluate_likelihood.md),
[`test_fit`](https://stocnet.github.io/infernet/reference/test_fit.md),
[`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)
