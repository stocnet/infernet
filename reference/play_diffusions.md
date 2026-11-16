# Simulating many diffusions

`play_diffusions()` runs
[`manynet::play_diffusion()`](https://stocnet.github.io/manynet/reference/make_play.html)
`times` times on the same network, and binds the reports of every run
into one `diffs_model`. Give the result to
[`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md),
to compare an observed diffusion with it, or to
[`test_distributions()`](https://stocnet.github.io/infernet/reference/test_fit.md),
to compare it with another model.

Runs in which nothing spreads beyond the seeds return no report from
[`manynet::as_diffusion()`](https://stocnet.github.io/manynet/reference/make_play.html),
and are dropped with a warning.

## Usage

``` r
play_diffusions(.data, ..., times = 5, control = list())

# S3 method for class 'diffs_model'
summary(object, ...)

# S3 method for class 'diffs_model'
x[i, j, drop = FALSE]
```

## Arguments

- .data:

  A network of a manynet-consistent class, such as a matrix, an
  edgelist, an igraph, a network, a tbl_graph, or a stocnet object.

- ...:

  Arguments passed on to
  [`manynet::play_diffusion()`](https://stocnet.github.io/manynet/reference/make_play.html),
  such as `transmissibility`, `recovery`, or `steps`.

- times:

  The number of diffusions to simulate. By default 5, but 1,000-10,000
  simulations are recommended for a test.

- control:

  The controls of the simulations, as the result of
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
  or a named list of its arguments, such as
  `list(ncores = 4, seed = 1)`. It reads `ncores`, `strategy`, and
  `seed`, and leaves the other controls alone. The same seed gives the
  same diffusions sequentially and in parallel. A control that is not
  named keeps its default, or the value set with
  [`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md).
  The result records the controls that it used in its attribute
  `control`.

- object, x:

  A `diffs_model`.

- i, j, drop:

  As for a tibble.

## Value

A `diffs_model`: a tibble with one row for each step of each run, the
run in `sim`, the step in `time`, and the counts of
[`manynet::as_diffusion()`](https://stocnet.github.io/manynet/reference/make_play.html).
[`summary()`](https://rdrr.io/r/base/summary.html) gives the time of
adoption, the number of steps until every node is infected, for each
run. [`print()`](https://rdrr.io/r/base/print.html) returns `x`,
invisibly.

## Details

[`print()`](https://rdrr.io/r/base/print.html) shows a title that says
how many diffusions were simulated, the report of each step, and a
footer with the steps until every node adopted.
[`summary()`](https://rdrr.io/r/base/summary.html) gives those steps for
each simulation.

## See also

[`sweep_diffusion()`](https://stocnet.github.io/infernet/reference/sweep.md)
to play a diffusion for each of several settings, and to see how the
result changes with them.

Other interpreting functions:
[`predict_mrqap`](https://stocnet.github.io/infernet/reference/predict_mrqap.md),
[`simulate_mrqap`](https://stocnet.github.io/infernet/reference/simulate_mrqap.md),
[`sweep`](https://stocnet.github.io/infernet/reference/sweep.md)

## Examples

``` r
play_diffusions(ison_adolescents, transmissibility = 0.3, times = 5,
                control = list(seed = 1))
#> # 5 simulated SI diffusions
#> 
#> # A tibble: 42 × 6
#>      sim  time     n     S     s     I
#>    <int> <int> <dbl> <int> <dbl> <int>
#>  1     1     0     8     7     1     1
#>  2     1     1     8     7     1     1
#>  3     1     2     8     7     1     1
#>  4     1     3     8     7     1     1
#>  5     1     4     8     7     1     1
#>  6     1     5     8     6     3     2
#>  7     1     6     8     6     3     2
#>  8     1     7     8     5     3     3
#>  9     1     8     8     4     3     4
#> 10     2     0     8     7     1     1
#> # ℹ 32 more rows
#> 
#> # Steps until every node adopted: median 10, from 8 to 10.
#> # Controls: seed = 1
```
