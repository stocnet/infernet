# Reporting network regression results

These methods report a `netreg`, the result of
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md),
whether its class is `netreg_lm`, `netreg_glm`, or `netreg_css`.

- [`print()`](https://rdrr.io/r/base/print.html) shows the result in
  three parts: a title that says what was fitted,
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) of the
  estimates with significance marks, and a footer that says how the null
  distribution was made and the controls that differ from the default.
  At `options(snet_verbosity = "debug")`, the footer also names the
  methods that give more.

- [`summary()`](https://rdrr.io/r/base/summary.html) gives the full
  table of the coefficients: the columns of
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html), the lower
  and upper permutation p-values, a plain `title` of each effect, and
  the parts of each term. Its print leaves out `title` and the parts,
  which are too wide to read there, and adds the random intercepts of a
  mixed model,
  [`glance()`](https://generics.r-lib.org/reference/glance.html), the
  unobserved dyads of each variable, and the terms that the formula
  dropped or changed, each after a blank line.
  [`glance()`](https://generics.r-lib.org/reference/glance.html) gives
  the permutations and the number of dyads, so the footer does not say
  them again.

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`glance()`](https://generics.r-lib.org/reference/glance.html) give
  the results as a tibble, after `{broom}`: one row for each
  coefficient, and one row for the whole model. So an MRQAP result can
  sit in one table beside an ERGM or a SIENA one (see
  [other_results](https://stocnet.github.io/infernet/reference/other_results.md)).

- [`nobs()`](https://rdrr.io/r/stats/nobs.html) gives the number of
  dyads that the model used: unobserved dyads, fixed ties, and the loops
  that the model leaves out are not counted.

- [`logLik()`](https://rdrr.io/r/stats/logLik.html) and
  [`deviance()`](https://rdrr.io/r/stats/deviance.html) give the
  log-likelihood and the deviance of the observed model, so
  [`AIC()`](https://rdrr.io/r/stats/AIC.html) and
  [`BIC()`](https://rdrr.io/r/stats/AIC.html) work too.

- [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) give one value
  for each dyad that the model used, in the rows of the dyad-level data.

- [`formula()`](https://rdrr.io/r/stats/formula.html) gives the formula
  that was fitted, after
  [`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify.md)
  read it, and [`terms()`](https://rdrr.io/r/stats/terms.html) gives the
  terms of the columns that were fitted, as the coefficients name them.
  For the formula of chosen rows of a table of terms, see
  [specify_terms](https://stocnet.github.io/infernet/reference/specify_terms.md).

## Usage

``` r
# S3 method for class 'netreg_lm'
print(x, ..., print_b = FALSE, print_random = FALSE)

# S3 method for class 'netreg_glm'
print(x, ..., print_b = FALSE, print_random = FALSE)

# S3 method for class 'netreg_css'
print(x, ..., print_b = FALSE, print_random = FALSE)

# S3 method for class 'netreg'
print(x, ...)

# S3 method for class 'netreg'
tidy(
  x,
  exponentiate = FALSE,
  alternative = c("two.sided", "less", "greater"),
  ...
)

# S3 method for class 'netreg'
summary(object, ...)

# S3 method for class 'summary_netreg'
print(x, ...)

# S3 method for class 'netreg'
glance(x, ...)

# S3 method for class 'netreg_lm'
glance(x, ...)

# S3 method for class 'netreg_glm'
glance(x, ...)

# S3 method for class 'netreg_css'
glance(x, ...)

# S3 method for class 'netreg'
nobs(object, ...)

# S3 method for class 'netreg'
logLik(object, ...)

# S3 method for class 'netreg'
deviance(object, ...)

# S3 method for class 'netreg'
fitted(object, ...)

# S3 method for class 'netreg'
residuals(object, ...)

# S3 method for class 'netreg'
formula(x, ...)

# S3 method for class 'netreg'
terms(x, ...)
```

## Arguments

- x, object:

  A `netreg`, from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md).

- ...:

  For [`residuals()`](https://rdrr.io/r/stats/residuals.html), passed to
  the [`residuals()`](https://rdrr.io/r/stats/residuals.html) method of
  the observed model, such as `type = "pearson"` for a `glm`. For
  [`summary()`](https://rdrr.io/r/base/summary.html), passed to
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html), such as
  `alternative = "greater"`. Otherwise not used.

- print_b, print_random:

  Deprecated. `print_b = TRUE` also prints the p-values of the
  coefficients, rather than of their test statistics, and
  `print_random = TRUE` the random intercepts. Use
  [`summary()`](https://rdrr.io/r/base/summary.html), which gives both.

- exponentiate:

  Logical, whether to exponentiate the estimates, e.g. to give odds
  ratios for a logistic model.

- alternative:

  Which permutation p-value to report: `"two.sided"` (the default) for
  Pr(\>=\|t\|), `"less"` for Pr(\<=t), or `"greater"` for Pr(\>=t).

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns `x`, invisibly.
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`glance()`](https://generics.r-lib.org/reference/glance.html) return a
tibble; the tibble of [`summary()`](https://rdrr.io/r/base/summary.html)
has the class `summary_netreg` too, for its print.
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) gives the
columns `term`, `estimate`, `statistic`, and `p.value`.
[`summary()`](https://rdrr.io/r/base/summary.html) gives these columns,
`p.lower` and `p.upper`, a `title` that defines each effect, and the
columns of the fit's term table, `fit$terms`: the `effect` (the formula
term) each coefficient comes from, its `layer`, `attribute`, `category`,
and `reference` category, and for an interaction its `operands`. These
group the coefficients without parsing their names.
[`glance()`](https://generics.r-lib.org/reference/glance.html) gives the
fit statistics that the model keeps.
[`nobs()`](https://rdrr.io/r/stats/nobs.html) gives an integer,
[`logLik()`](https://rdrr.io/r/stats/logLik.html) a `logLik` object,
[`deviance()`](https://rdrr.io/r/stats/deviance.html) a number,
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
[`residuals()`](https://rdrr.io/r/stats/residuals.html) a numeric
vector, [`formula()`](https://rdrr.io/r/stats/formula.html) a formula,
and [`terms()`](https://rdrr.io/r/stats/terms.html) a `terms` object.

## Details

The p-values come from the permutations. Every other method reads the
observed model, which the permutations do not change. A permutation test
gives a p-value but no standard error, so
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) have no `std.error`
column. `p.value` is read from the permutations of the test statistic,
as [`print()`](https://rdrr.io/r/base/print.html) shows it. Where no
permutation reached the observed statistic,
[`print()`](https://rdrr.io/r/base/print.html) shows the p-value as
below `1/times`, the smallest that the permutations can give, and marks
it as that bound. For the confusion matrix of a binary outcome, see
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md).
When the predictors are permuted, the intercept has no test, and its
`p.value` is `NA`.

Two methods of `{stats}` work without a method here:
[`coef()`](https://rdrr.io/r/stats/coef.html) gives the estimates, and
[`update()`](https://rdrr.io/r/stats/update.html) fits the model again,
with a changed formula, `times`, or `control`, from the call that the
result keeps.

## Methods left out

- [`vcov()`](https://rdrr.io/r/stats/vcov.html) and
  [`confint()`](https://rdrr.io/r/stats/confint.html): a covariance or
  an interval from the observed model would not come from the
  permutations, and could be read as the basis of the permutation
  p-values.

- [`weights()`](https://rdrr.io/r/stats/weights.html): a network
  regression uses no weights.

- `[`: a result has no rows to select. Use `$` or `[[` for an element.

- [`profile()`](https://rdrr.io/r/stats/profile.html): a permutation fit
  has no likelihood to profile.

## Fixed effects

A gaussian model absorbs its fixed effects by demeaning. Its
log-likelihood and deviance are those of the model with a dummy for each
group, and [`logLik()`](https://rdrr.io/r/stats/logLik.html) counts the
absorbed parameters in its `df`, so that
[`AIC()`](https://rdrr.io/r/stats/AIC.html),
[`BIC()`](https://rdrr.io/r/stats/AIC.html), and
[`glance()`](https://generics.r-lib.org/reference/glance.html) are those
of that model too.
[`fitted()`](https://rdrr.io/r/stats/fitted.values.html) gives the
observed values less the residuals, not the demeaned fitted values.

## See also

Other models:
[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md),
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md),
[`other_results`](https://stocnet.github.io/infernet/reference/other_results.md),
[`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md),
[`regression`](https://stocnet.github.io/infernet/reference/regression.md),
[`specify`](https://stocnet.github.io/infernet/reference/specify.md),
[`specify_terms`](https://stocnet.github.io/infernet/reference/specify_terms.md),
[`terms_mrqap`](https://stocnet.github.io/infernet/reference/terms_mrqap.md),
[`test_fit`](https://stocnet.github.io/infernet/reference/test_fit.md),
[`test_measures`](https://stocnet.github.io/infernet/reference/test_measures.md),
[`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)

## Examples

``` r
css_model <- estimate_mrqap(advice ~ same(dept) + reports,
                            manynet::ison_hightech, times = 5)
css_model
#> # Logistic MRQAP of advice
#> 
#> # A tibble: 3 × 5
#>   term        estimate statistic p.value signif   
#>   <chr>          <dbl>     <dbl> <chr>   <noquote>
#> 1 (Intercept)   -1.07     -38.5  NA               
#> 2 same_dept      0.552      9.53 < 0.2            
#> 3 reports        3.82      15.3  < 0.2            
#> 
#> # Null: 5 permutations of each predictor, by double semi-partialling.
#> # Permutations relabel senders, receivers, and reporters alike.
summary(css_model)
#> # Logistic MRQAP of advice
#> 
#> # A tibble: 3 × 7
#>   term        estimate statistic p.lower p.upper p.value signif   
#>   <chr>          <dbl>     <dbl> <chr>   <chr>   <chr>   <noquote>
#> 1 (Intercept)   -1.07     -38.5  NA      NA      NA               
#> 2 same_dept      0.552      9.53 1       < 0.2   < 0.2            
#> 3 reports        3.82      15.3  1       < 0.2   < 0.2            
#> 
#> # Fit of the observed model
#>    family logLik  AIC   BIC deviance null.deviance df.residual nobs   permute
#>  binomial  -4989 9984 10005     9978         10922        8817 8820 predictor
#>  times reporters
#>      5        21
#> 
#> # Permutations relabel senders, receivers, and reporters alike.
tidy(css_model)
#> # A tibble: 3 × 4
#>   term        estimate statistic p.value
#>   <chr>          <dbl>     <dbl>   <dbl>
#> 1 (Intercept)   -1.07     -38.5       NA
#> 2 same_dept      0.552      9.53       0
#> 3 reports        3.82      15.3        0
glance(css_model)
#> # A tibble: 1 × 11
#>   family   logLik   AIC    BIC deviance null.deviance df.residual  nobs permute 
#>   <chr>     <dbl> <dbl>  <dbl>    <dbl>         <dbl>       <dbl> <int> <chr>   
#> 1 binomial -4989. 9984. 10005.    9978.        10922.        8817  8820 predict…
#> # ℹ 2 more variables: times <dbl>, reporters <int>
nobs(css_model)
#> [1] 8820
AIC(css_model)
#> [1] 9983.796
formula(css_model)
#> advice ~ same(dept) + reports
#> <environment: 0xbb3c4ee08>
head(fitted(css_model))
#> [1] 0.3723726 0.2546663 0.3723726 0.2546663 0.2546663 0.2546663
```
