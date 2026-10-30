# Tests of model fit

These functions test whether a model makes data like the observed data,
or whether two models make the same data:

- `test_gof()` compares an observed statistic with the same statistic on
  data simulated from a model. It is a generic, with a method for each
  kind of model:

  - a `diffs_model`, from
    [`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md),
    compares an observed diffusion with the simulated diffusions;

  - a `netreg`, from
    [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md),
    simulates outcome networks from the fitted model and compares a
    network statistic, `FUN`, on each with the statistic on the observed
    network.

- `test_distributions()` compares the data simulated from two models. It
  is also a generic, with a method for two `diffs_model` objects.

Both give a p-value from simulations, not from a distribution that the
statistic is assumed to follow.

## Usage

``` r
test_gof(object, ...)

# Default S3 method
test_gof(object, observed, ...)

# S3 method for class 'diff_model'
test_gof(object, observed, ...)

# S3 method for class 'diffs_model'
test_gof(object, observed, ...)

# S3 method for class 'netreg'
test_gof(object, observed, FUN, ..., times = 1000, control = list())

test_distributions(object1, object2, ...)

# Default S3 method
test_distributions(object1, object2, ...)

# S3 method for class 'diffs_model'
test_distributions(object1, object2, times = 1000, control = list(), ...)
```

## Arguments

- object:

  A model to simulate from: a `diffs_model`, from
  [`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md),
  or a `netreg`, from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md).
  For `test_distributions()`, `object1` and `object2` are two
  `diffs_model` objects.

- ...:

  For a `netreg`, additional arguments to be passed on to `FUN`. Other
  methods do not use them.

- observed:

  The observed data. For a `diffs_model`, a `diff_model` from
  [`manynet::as_diffusion()`](https://stocnet.github.io/manynet/reference/coerce_graph.html),
  or a network with a diffusion record from
  [`manynet::play_diffusion()`](https://stocnet.github.io/manynet/reference/make_play.html).
  For a `netreg`, the network that the model was fitted on.

- FUN:

  For a `netreg`, a function of a network that returns a number or a
  vector of numbers, such as
  [`netrics::net_by_transitivity()`](https://stocnet.github.io/netrics/reference/measure_closure.html).

- times:

  For a `netreg`, the number of outcomes to simulate. For
  `test_distributions()`, the number of permutations. A `diffs_model`
  brings its own simulations.

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

- object1, object2:

  Two `diffs_model` objects, from
  [`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md).

## Value

A `network_test`, which [`print()`](https://rdrr.io/r/base/print.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html), and
[`glance()`](https://generics.r-lib.org/reference/glance.html) report.
It holds the observed distance in `testval` and the reference distances
in `testdist`.

## Details

`test_gof()` takes the vector of statistics of each simulation, such as
the number of infected nodes in each step of a diffusion, and splits the
simulations into two halves. The first half gives the mean and the
covariance of the statistics. The second half is the reference. The test
statistic is the Mahalanobis distance from that mean, and the p-value is
the share of the reference that is at least as far from the mean as the
observed data: \$\$p = \frac{1 + \\\\d\_{ref} \geq d\_{obs}\\}{1 +
k}\$\$ where \\k\\ is the number of reference simulations. If the model
made the observed data, the observed data and the reference are
exchangeable, so this p-value is exact for any number of simulations. A
reference simulation at the same distance as the observed data counts
against it, so the test is conservative where distances tie. The
reference does not use the simulations that estimate the mean and the
covariance, so it needs no assumption that the statistics are normal.

A small ridge, a thousandth of the mean variance, is added to the
diagonal of the covariance. Statistics such as an adoption curve are
bounded and often constant in some steps, so their covariance is
singular. With the ridge, a difference in a step where the simulations
do not vary makes the distance large, and is not ignored.

`test_distributions()` measures the Mahalanobis distance between the
mean adoption curves of the two models, with their pooled covariance and
the same ridge. It then permutes which model each simulation came from,
`times` times. As the simulations of each model are independent, this
permutation test is exact.

A large distance is evidence against the model, so the p-value is
`Pr(X>=Obs)` in [`print()`](https://rdrr.io/r/base/print.html), and
`p.upper` in [`tidy()`](https://generics.r-lib.org/reference/tidy.html).

## Diffusions

The statistic of a diffusion is its curve of infected nodes, `I`, in
each step after the seeds. A diffusion that ends early keeps its last
value until the longest diffusion ends. The diffusions can come from
[`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md).
Keep `times` large enough that the reference holds at least 19
simulations, or no p-value can be below 0.05.

## Network regressions

The `netreg` method draws each simulated outcome from the fitted model
with [`stats::simulate()`](https://rdrr.io/r/stats/simulate.html), in
the dyads that the model used. The other cells keep their observed
values: the diagonal, the fixed ties, and the cells that the model left
out. An unobserved cell is an absent tie in both the observed and the
simulated networks, so they do not differ there. An undirected network
is mirrored, and the simulated network takes the node attributes of the
observed one.

The model needs its `simple_fit`, so a model fitted with
`less_mem = TRUE` cannot be tested. A list of networks, a panel, and a
cognitive social structure cannot be tested yet. Nor can a `gaussian`
model: its draws are continuous, so every dyad of a draw would hold a
tie, and the test would always reject. Fit a family whose outcomes are
ties or counts.

An MRQAP models each dyad apart from the others. A statistic of the
dependence between dyads, such as transitivity or reciprocity, shows
what that misses.

## Other packages

`{RSiena}` and `{goldfish}` export `test_gof()` generics with the same
signature, for their own fitted models. If more than one of these
packages is attached, the one attached last masks the others. Call
`infernet::test_gof()`, `RSiena::test_gof()`, or `goldfish::test_gof()`
to choose.

## References

Hunter, David R., Steven M. Goodreau, and Mark S. Handcock. 2008.
"Goodness of fit of social network models". *Journal of the American
Statistical Association* 103(481): 248-258.
[doi:10.1198/016214507000000446](https://doi.org/10.1198/016214507000000446)

Lospinoso, Josh, and Tom A.B. Snijders. 2019. "Goodness of fit for
stochastic actor-oriented models". *Methodological Innovations* 12(3).
[doi:10.1177/2059799119884282](https://doi.org/10.1177/2059799119884282)

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
[`test_measures`](https://stocnet.github.io/infernet/reference/test_measures.md),
[`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)

## Examples

``` r
fast <- play_diffusions(ison_adolescents, transmissibility = 0.5,
                        times = 40, control = list(seed = 1))
slow <- play_diffusions(ison_adolescents, transmissibility = 0.2,
                        times = 40, control = list(seed = 1))
test_distributions(fast, slow, times = 200)
#> # Two-sample test of two sets of simulated diffusions
#> 
#> # A tibble: 1 × 6
#>   statistic null.mean null.sd p.lower p.upper signif   
#>       <dbl>     <dbl>   <dbl> <chr>   <chr>   <noquote>
#> 1      72.0      8.81    4.35 1       0.00498 **       
#> 
#> # Null: 200 permutations of the pooled diffusions.
#> # The p-value is p.upper: only a larger statistic counts against the null.
#> # Signif. codes: *** 0.001, ** 0.01, * 0.05, . 0.1
obs <- manynet::as_diffusion(manynet::play_diffusion(ison_adolescents,
                                                     transmissibility = 0.2))
test_gof(slow, obs)
#> # Goodness-of-fit test of the adoption curve for the simulated diffusions
#> 
#> # A tibble: 1 × 6
#>   statistic null.mean null.sd p.lower p.upper signif   
#>       <dbl>     <dbl>   <dbl> <chr>   <chr>   <noquote>
#> 1      16.4      26.0    31.1 0.571   0.476            
#> 
#> # Null: 40 simulations of the model; 20 estimate the mean and covariance.
#> # The p-value is p.upper: only a larger statistic counts against the null.

# Does an MRQAP make as many reciprocated ties as the observed network?
fit <- estimate_mrqap(friends ~ social + tasks, ison_algebra, times = 20,
                      control = list(family = "poisson"))
test_gof(fit, ison_algebra, net_by_reciprocity, times = 100,
         control = list(seed = 1))
#> # Goodness-of-fit test of net_by_reciprocity() for a poisson MRQAP
#> 
#> # A tibble: 1 × 6
#>   statistic null.mean null.sd p.lower p.upper signif   
#>       <dbl>     <dbl>   <dbl> <chr>   <chr>   <noquote>
#> 1      21.4      1.24    1.73 1       0.0196  *        
#> 
#> # Null: 100 simulations of the model; 50 estimate the mean and covariance.
#> # The p-value is p.upper: only a larger statistic counts against the null.
#> # Controls: seed = 1
#> # Signif. codes: *** 0.001, ** 0.01, * 0.05, . 0.1
```
