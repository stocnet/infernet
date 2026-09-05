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
test_random(.data, FUN, ..., times = 1000, strategy = "sequential")

test_configuration(.data, FUN, ..., times = 1000, strategy = "sequential")

test_permutation(.data, FUN, ..., times = 1000, strategy = "sequential")
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

- strategy:

  If `{furrr}` is installed, then multiple cores can be used to
  accelerate the function. By default `"sequential"`, but if multiple
  cores available, then `"multisession"` or `"multicore"` may be useful.
  Generally this is useful only when `times` \> 1000. See
  [`{furrr}`](https://furrr.futureverse.org) for more.

## See also

Other models:
[`regression`](https://stocnet.github.io/infernet/reference/regression.md)

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
