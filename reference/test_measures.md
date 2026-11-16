# Tests of network measures

These functions conduct tests of any network-level statistic:

- `test_random()` performs a conditional uniform graph (CUG) test of a
  measure against a distribution of measures on random networks of the
  same dimensions.

- `test_configuration()` performs a CUG test against a distribution of
  measures on random networks that preserve the degree sequence of the
  original network.

- `test_permutation()` performs a quadratic assignment procedure (QAP)
  test of a measure against a distribution of measures on permutations
  of the original network. Each permutation keeps the node attributes
  and the changes of a diffusion, so a measure can read the times of
  adoption. For a null that rewires the network, see
  `netdiffuseR::struct_test()`.

## Usage

``` r
test_random(.data, FUN, ..., times = 1000, control = list())

test_configuration(.data, FUN, ..., times = 1000, control = list())

test_permutation(.data, FUN, ..., times = 1000, control = list())

# S3 method for class 'network_test'
tidy(x, ...)

# S3 method for class 'network_test'
summary(object, probs = c(0.025, 0.5, 0.975), ...)

# S3 method for class 'network_test'
glance(x, ...)
```

## Arguments

- .data:

  A manynet-consistent network (see
  [`manynet::as_tidygraph()`](https://stocnet.github.io/manynet/reference/coerce_graph.html)),
  or a list of such networks. When a list is supplied the model is fit
  jointly; graphs missing any predictor are dropped with a warning.

- FUN:

  A graph-level statistic function to test.

- ...:

  For a `test_*()` function, more arguments for `FUN`, such as the name
  of the attribute. For a method, not used.

- times:

  Integer. Number of permutations for the null distribution. 1000 is the
  default; publication-ready work usually needs 1000-10000.

- control:

  The controls of the simulations, as the result of
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
  or a named list of its arguments, such as
  `list(ncores = 4, seed = 1)`. A test reads `ncores`, `strategy`, and
  `seed`, and leaves the other controls alone, so that the controls of a
  model can be given to a test whole. `ncores` is generally useful only
  when `times` \> 1000. A control that is not named keeps its default,
  or the value set with
  [`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md),
  and the result records the controls that it used as `control`. See
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
  for each control and its default.

- x:

  A `network_test`.

- object:

  A `network_test`.

- probs:

  For [`summary()`](https://rdrr.io/r/base/summary.html), the
  probabilities of the quantiles of the null distribution.

## Value

A `network_test`, which [`print()`](https://rdrr.io/r/base/print.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html), and
[`glance()`](https://generics.r-lib.org/reference/glance.html) report.
It holds the observed statistic in `testval`, the statistics of the
simulations in `testdist`, and the p-values of the observed statistic:
in `plteobs` from the simulations at or below it, and in `pgteobs` from
those at or above it. Each p-value is \\(1 + b) / (B + 1)\\, where \\b\\
of the \\B\\ simulations reach the observed statistic, since under the
null the observed network is one more draw (Phipson and Smyth 2010). So
a p-value is never 0, and the smallest is \\1 / (B + 1)\\. It also holds
the number of simulations as `times`, the name of the statistic as
`statistic`, and the controls that the test used as `control`.

[`summary()`](https://rdrr.io/r/base/summary.html) gives a tibble with
the `statistic`, a column for each quantile of the null distribution,
such as `null.2.5%`, and `p.lower` and `p.upper`.
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

## Methods

These methods report a `network_test`, the result of `test_random()`,
`test_configuration()`, `test_permutation()`,
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

- [`summary()`](https://rdrr.io/r/base/summary.html) gives the observed
  statistic, quantiles of the null distribution, and the two p-values,
  as a tibble of class `infernetTestSummary`, with one row for each
  statistic.

## References

Phipson, Belinda, and Gordon K. Smyth. 2010. "Permutation P-values
Should Never Be Zero: Calculating Exact P-values When Permutations Are
Randomly Drawn." *Statistical Applications in Genetics and Molecular
Biology* 9(1): 39.
[doi:10.2202/1544-6115.1585](https://doi.org/10.2202/1544-6115.1585) .

## Examples

``` r
marvel_friends <- fict_marvel |> to_layer("relationship") |> 
  to_unsigned() |> to_giant() |> 
  to_subgraph(PowerOrigin == "Human")
(cugtest <- test_random(marvel_friends, net_by_heterophily, attribute = "Attractive",
   times = 50))
#> # CUG test of net_by_heterophily()
#> 
#> # A tibble: 1 × 5
#>   statistic null.mean null.sd p.lower p.upper
#>       <dbl>     <dbl>   <dbl> <chr>   <chr>  
#> 1    -0.857    -0.616  0.0766 0.0196  1      
#> 
#> # Null: 50 random networks with the same size and density.
# In a two-mode network with repeated ties, such as texts and the terms
# they use, each draw keeps the number of tokens in each text and the
# number of times each term is used, and so is a shuffled text.
# Project inside the statistic to test the co-occurrence of the terms
# against such shuffled texts.
tokens <- manynet::as_igraph(data.frame(
  from = c("A","A","A","A","B","B","B","C","C","C","C"),
  to = c("w","w","x","y","x","x","z","y","z","z","w")), twomode = TRUE)
(configtest <- test_configuration(tokens,
   function(g) net_by_transitivity(to_mode(g, 2)), times = 50))
#> # Configuration test of a statistic
#> 
#> # A tibble: 1 × 5
#>   statistic null.mean null.sd p.lower p.upper
#>       <dbl>     <dbl>   <dbl> <chr>   <chr>  
#> 1         1     0.879   0.200 1       0.667  
#> 
#> # Null: 50 random networks with the same degrees.
qaptest <- test_permutation(manynet::ison_adolescents,
                            netrics::net_by_density, times = 20)
qaptest
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
summary(qaptest)
#> # QAP test of netrics::net_by_density()
#> 
#> # A tibble: 1 × 6
#>   statistic `null.2.5%` `null.50%` `null.97.5%` p.lower p.upper
#>       <dbl>       <dbl>      <dbl>        <dbl>   <dbl>   <dbl>
#> 1     0.357       0.357      0.357        0.357       1       1
#> 
#> # Quantiles of the null distribution, from 20 draws.
```
