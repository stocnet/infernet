# Sweeping a diffusion or an opinion dynamic over its settings

`sweep_diffusion()` plays
[`manynet::play_diffusion()`](https://stocnet.github.io/manynet/reference/make_play.html),
and `sweep_opinion()` plays
[`manynet::play_opinion()`](https://stocnet.github.io/manynet/reference/make_opinion.html),
`times` times for each setting of the arguments in `vary`, to show how
the result changes with them: for example, the share of the network
infected for each chance of transmission, or the number of opinion
clusters for each confidence.

A sweep explores a model. To compare a model with an observed diffusion,
or two models with each other, use
[`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md)
with
[`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md)
or
[`test_distributions()`](https://stocnet.github.io/infernet/reference/test_fit.md).

## Usage

``` r
sweep_diffusion(.data, ..., vary, times = 5, control = list())

sweep_opinion(.data, opinions, ..., vary, times = 5, control = list())

# S3 method for class 'diffusionSweep'
summary(object, ...)

# S3 method for class 'opinionSweep'
summary(object, ...)
```

## Arguments

- .data:

  A network of a manynet-consistent class, such as a matrix, an
  edgelist, an igraph, a network, a tbl_graph, or a stocnet object.

- ...:

  Other arguments of
  [`manynet::play_diffusion()`](https://stocnet.github.io/manynet/reference/make_play.html)
  or
  [`manynet::play_opinion()`](https://stocnet.github.io/manynet/reference/make_opinion.html),
  which are the same in every setting, such as `steps` or `rule`.

- vary:

  A named list of the values to try for one or more arguments, such as
  `list(transmissibility = c(0.1, 0.3, 0.5))`. Each value of a vector is
  one setting. For a setting that is itself a vector, such as a set of
  seeds, give a list: `list(seeds = list(1:10, 1:20))`. A named list
  names its settings: `list(seeds = list(hub = 1, edge = 5))`. With more
  than one argument, every combination of their values is played.

- times:

  The number of runs for each setting. By default 5. A model without
  chance, such as a diffusion with a `transmissibility` of 1, or the
  DeGroot or Hegselmann–Krause rule, gives the same result in every run.

- control:

  The controls of the runs, as a named list such as
  `list(ncores = 4, seed = 1)`. It reads `ncores`, `strategy`, and
  `seed`. The same seed gives the same runs sequentially and in
  parallel. A control that is not named keeps its default, or the value
  set with
  [`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md).

- opinions:

  For `sweep_opinion()`, the opinions at the start, as in
  [`manynet::play_opinion()`](https://stocnet.github.io/manynet/reference/make_opinion.html):
  one value for each node, or the name of a node attribute. Give several
  sets of opinions in `vary` instead, as
  `vary = list(opinions = list(...))`.

- object:

  A `diffusionSweep` or an `opinionSweep`.

## Value

A table with a column first for each argument in `vary`, and the run in
`sim`. Each column of a setting holds the value of the setting where
each value is one number or string, the names of a named list, the
number of seeds for a list of seeds, and otherwise the place of the
setting.

`sweep_diffusion()` returns a `diffusionSweep`, with one row for each
step of each run, and the counts of
[`manynet::as_diffusion()`](https://stocnet.github.io/manynet/reference/make_play.html).
Its [`summary()`](https://rdrr.io/r/base/summary.html) gives, for each
setting and run, the share of the nodes that were infected (`infected`,
which counts a node again where it is reinfected), and the steps until
every node adopted (`toa`), which is `NA` where not every node adopted.

`sweep_opinion()` returns an `opinionSweep`, with one row for each run:
the number of opinion `clusters` at the end, whether they reach a
`consensus`, the `spread` (standard deviation) and `mean` of the
opinions at the end, the number of `steps` played, and whether the
opinions `settled` before the last step. Two opinions are in one cluster
where they are within 0.01 of each other, or of an opinion between them.

[`print()`](https://rdrr.io/r/base/print.html) gives the mean of each
for each setting, and returns `x`, invisibly.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) from
`{autograph}` draws the result against the first argument in `vary`.

## See also

Other interpreting functions:
[`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md),
[`predict_mrqap`](https://stocnet.github.io/infernet/reference/predict_mrqap.md),
[`simulate_mrqap`](https://stocnet.github.io/infernet/reference/simulate_mrqap.md)

## Examples

``` r
# The share infected for three chances of transmission
sweep_diffusion(ison_adolescents, steps = 10,
                vary = list(transmissibility = c(0.1, 0.3, 0.5)),
                control = list(seed = 1))
#> # 15 simulated diffusions over 3 settings of transmissibility
#> 
#> # A tibble: 3 × 4
#>   transmissibility infected complete   toa
#>              <dbl>    <dbl>    <dbl> <dbl>
#> 1              0.1    0.275      0    NA  
#> 2              0.3    0.75       0.4  10  
#> 3              0.5    1          1     7.6
#> 
#> # For each setting: the mean share infected, the share of runs in which every
#> #   node adopted, and their mean steps until then.
#> # Controls: seed = 1
# The number of opinion clusters for three widths of confidence
set.seed(1)
sweep_opinion(ison_networkers, stats::runif(32), rule = "hegselmann_krause",
              vary = list(confidence = c(0.1, 0.2, 0.3)), times = 1)
#> # 3 simulated opinion dynamics over 3 settings of confidence
#> 
#> # A tibble: 3 × 5
#>   confidence clusters consensus spread settled
#>        <dbl>    <dbl>     <dbl>  <dbl>   <dbl>
#> 1        0.1        8         0 0.276        0
#> 2        0.2        3         0 0.195        1
#> 3        0.3        2         0 0.0854       1
#> 
#> # For each setting: the mean number of clusters at the end, and the share of
#> #   runs that reach a consensus or settle.
```
