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

  Additional arguments to be passed on to FUN, e.g. the name of the
  attribute.

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
  or the value set with `control_mrqap(..., persist = TRUE)`, and the
  result records the controls that it used as `control`. See
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
  for each control and its default.

## Value

A `network_test`, which [`print()`](https://rdrr.io/r/base/print.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html), and
[`glance()`](https://generics.r-lib.org/reference/glance.html) report.
It holds the observed statistic in `testval`, the statistics of the
simulations in `testdist`, and the share of these at or below the
observed statistic in `plteobs` and at or above it in `pgteobs`. It also
holds the number of simulations as `times`, the name of the statistic as
`statistic`, and the controls that the test used as `control`.

## See also

Other models:
[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md),
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md),
[`other_results`](https://stocnet.github.io/infernet/reference/other_results.md),
[`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md),
[`print.netreg_lm()`](https://stocnet.github.io/infernet/reference/netreg_results.md),
[`regression`](https://stocnet.github.io/infernet/reference/regression.md),
[`specify`](https://stocnet.github.io/infernet/reference/specify.md),
[`specify_terms`](https://stocnet.github.io/infernet/reference/specify_terms.md),
[`terms_mrqap`](https://stocnet.github.io/infernet/reference/terms_mrqap.md),
[`test_fit`](https://stocnet.github.io/infernet/reference/test_fit.md),
[`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)

## Examples

``` r
marvel_friends <- fict_marvel |> to_uniplex("relationship") |> 
  to_unsigned() |> to_giant() |> 
  to_subgraph(PowerOrigin == "Human")
(cugtest <- test_random(marvel_friends, net_by_heterophily, attribute = "Attractive",
   times = 200))
#> # CUG test of net_by_heterophily()
#> 
#> # A tibble: 1 × 5
#>   statistic null.mean null.sd p.lower p.upper
#>       <dbl>     <dbl>   <dbl> <chr>   <chr>  
#> 1    -0.857    -0.612  0.0916 0.005   0.995  
#> 
#> # Null: 200 random networks with the same size and density.
# plot(cugtest)
# (qaptest <- test_permutation(marvel_friends, 
#                 net_by_heterophily, attribute = "Attractive",
#                 times = 200))
# plot(qaptest)
```
