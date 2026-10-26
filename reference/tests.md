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
  of the original network.

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
  when `times` \> 1000. See
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
  for each control and its default.

## See also

Other models:
[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md),
[`print.netreg_lm()`](https://stocnet.github.io/infernet/reference/results.md),
[`regression`](https://stocnet.github.io/infernet/reference/regression.md),
[`specify`](https://stocnet.github.io/infernet/reference/specify.md)

## Examples

``` r
marvel_friends <- fict_marvel |> to_uniplex("relationship") |> 
  to_unsigned() |> to_giant() |> 
  to_subgraph(PowerOrigin == "Human")
(cugtest <- test_random(marvel_friends, net_by_heterophily, attribute = "Attractive",
   times = 200))
#> 
#>  CUG Test Results
#> 
#> Observed Value: -0.8571429 
#> Pr(X>=Obs): 1 
#> Pr(X<=Obs): 0 
#> 
# plot(cugtest)
# (qaptest <- test_permutation(marvel_friends, 
#                 net_by_heterophily, attribute = "Attractive",
#                 times = 200))
# plot(qaptest)
```
