# Reporting network test results

These methods report a `network_test`, the result of
[`test_random()`](https://stocnet.github.io/infernet/reference/test_measures.md),
[`test_configuration()`](https://stocnet.github.io/infernet/reference/test_measures.md),
[`test_permutation()`](https://stocnet.github.io/infernet/reference/test_measures.md),
[`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md),
[`test_distributions()`](https://stocnet.github.io/infernet/reference/test_fit.md),
[`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md),
and
[`test_time()`](https://stocnet.github.io/infernet/reference/test_parameter.md).

- [`print()`](https://rdrr.io/r/base/print.html) shows a title that says
  what was tested, a blank line,
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) of the
  result, and a footer that says how the null distribution was made and
  the controls that differ from the default. For a test whose statistic
  is a distance, from
  [`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md),
  [`test_distributions()`](https://stocnet.github.io/infernet/reference/test_fit.md),
  [`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md),
  or
  [`test_time()`](https://stocnet.github.io/infernet/reference/test_parameter.md),
  only the upper tail is a p-value, and only it is marked.

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`glance()`](https://generics.r-lib.org/reference/glance.html) give
  the same as a tibble of one row, after `{broom}`.

## Usage

``` r
# S3 method for class 'network_test'
tidy(x, ...)

# S3 method for class 'network_test'
glance(x, ...)

# S3 method for class 'network_test'
print(x, ..., max.length = 6, digits = 3)
```

## Arguments

- x:

  A `network_test`.

- ...:

  Not used.

- max.length, digits:

  Not used.

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns `x`, invisibly.
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`glance()`](https://generics.r-lib.org/reference/glance.html) give one
row with the observed statistic, the mean and standard deviation of the
null distribution, and the share of the null distribution at or below
(`p.lower`) and at or above (`p.upper`) the observed statistic. For
[`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md)
and
[`test_distributions()`](https://stocnet.github.io/infernet/reference/test_fit.md),
the statistic is a distance, the null distribution is that of the
reference simulations, and `p.upper` is the p-value.

## Examples

``` r
qaptest <- test_permutation(manynet::ison_adolescents,
                            netrics::net_by_density, times = 20)
print(qaptest)
#> # QAP test of netrics::net_by_density()
#> 
#> # A tibble: 1 × 5
#>   statistic null.mean null.sd p.lower p.upper
#>       <dbl>     <dbl>   <dbl> <chr>   <chr>  
#> 1     0.357     0.357       0 1       1      
#> 
#> # Null: 20 permutations of the node labels.
tidy(qaptest)
#> # A tibble: 1 × 7
#>   method statistic null.mean null.sd p.lower p.upper times
#>   <chr>      <dbl>     <dbl>   <dbl>   <dbl>   <dbl> <dbl>
#> 1 QAP        0.357     0.357       0       1       1    20
```
