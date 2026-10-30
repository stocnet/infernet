# infernet

## About the package

[infernet](https://stocnet.github.io/infernet/) is the inferential layer
of the [stocnet](https://github.com/stocnet) ecosystem. It asks whether
what we see in a network could have come about by chance. Each of its
answers compares an observed value with a distribution of values from
networks that a null hypothesis allows: random networks, networks with
the same degrees, or the same network with its nodes relabelled.

The package has five families of functions:

- **Tests** of a network statistic, of a model’s parameters, or of a
  model’s fit:
  [`test_random()`](https://stocnet.github.io/infernet/reference/test_measures.md),
  [`test_configuration()`](https://stocnet.github.io/infernet/reference/test_measures.md),
  [`test_permutation()`](https://stocnet.github.io/infernet/reference/test_measures.md),
  [`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md),
  [`test_time()`](https://stocnet.github.io/infernet/reference/test_parameter.md),
  [`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md),
  and
  [`test_distributions()`](https://stocnet.github.io/infernet/reference/test_fit.md).
- **Specification** of a formula that one model can read and another can
  fit:
  [`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify.md)
  and
  [`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify.md).
- **Regression** of a network on nodal and dyadic covariates, by
  multiple regression quadratic assignment procedure (MRQAP):
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
  and
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md).
- **Diffusion**, simulated many times, to test a model of diffusion
  against:
  [`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md).
- **Results**, as tidy data frames:
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`glance()`](https://generics.r-lib.org/reference/glance.html), and
  [`predict()`](https://stocnet.github.io/infernet/reference/predict.md).

Every function accepts the networks that
[manynet](https://stocnet.github.io/manynet/) reads: matrices,
edgelists, [igraph](https://r.igraph.org/),
[network](https://statnet.org/), and
[tidygraph](https://tidygraph.data-imaginist.com) objects, one-mode or
two-mode, directed or undirected, cross-sectional or longitudinal.

## Where infernet sits in stocnet

[infernet](https://stocnet.github.io/infernet/) does one job, and leaves
the other jobs to its neighbours:

- [`{manynet}`](https://stocnet.github.io/manynet/) holds the data: the
  network classes, the conversions between them, and the networks that
  the examples below use.
- [`{netrics}`](https://stocnet.github.io/netrics/) holds the measures,
  such as
  [`net_by_heterophily()`](https://stocnet.github.io/netrics/reference/measure_assort_net.html),
  that the tests take as their statistic.
- [`{autograph}`](https://stocnet.github.io/autograph/) draws the
  results: [`plot()`](https://rdrr.io/r/graphics/plot.default.html) of a
  test or a diffusion.
- [`{goldfish}`](https://stocnet.github.io/goldfish/) and
  [`{ergm}`](https://statnet.org) fit the models that a formula can move
  to, with
  [`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify.md)
  here and `specify_dynam()` in
  [goldfish](https://stocnet.github.io/goldfish/).
- [`{migraph}`](https://stocnet.github.io/migraph/) holds the book and
  the teaching material that uses all of these.

[infernet](https://stocnet.github.io/infernet/) loads
[manynet](https://stocnet.github.io/manynet/) and
[netrics](https://stocnet.github.io/netrics/), so their networks and
measures are available after
[`library(infernet)`](https://stocnet.github.io/infernet/).

``` r

library(infernet)
```

## Tests

A conditional uniform graph (CUG) test compares a statistic with its
values on random networks of the same size and density. A configuration
test keeps the degree of each node too. A quadratic assignment procedure
(QAP) test relabels the nodes of the observed network, so that the ties
stay as they are and only the attributes move. Each test takes a
network, a statistic from [netrics](https://stocnet.github.io/netrics/),
any arguments of that statistic, and a number of simulations as `times`:

``` r

marvel <- fict_marvel |> to_uniplex("relationship") |>
  to_unsigned() |> to_giant()
test_random(marvel, net_by_heterophily, attribute = "Gender",
            times = 200, control = list(seed = 1))
test_configuration(marvel, net_by_heterophily, attribute = "Gender",
                   times = 200, control = list(seed = 1))
test_permutation(marvel, net_by_heterophily, attribute = "Gender",
                 times = 200, control = list(seed = 1))
```

`control` takes a seed, and a number of cores to run the simulations on.
The same seed gives the same result, sequentially or in parallel.

## Specification

A model is written as a formula. The terms on the right-hand side
construct a matrix each:

| Term | Constructs a matrix of |
|----|----|
| `ego(attr)`, `alter(attr)` | the sender’s or the receiver’s value of a nodal attribute |
| `same(attr)` | 1 where sender and receiver share a value |
| `diff(attr)`, `sim(attr)` | the difference, or the similarity, of a numeric attribute |
| `tie(layer)`, or the name of a layer | the ties of another layer of the network |
| `recip(layer)`, `exchange(layer)` | the reverse tie, in this or another layer |
| `indeg(layer)`, `outdeg(layer)` | the degree of the sender or the receiver |
| `tertius(attr)` | an aggregate of an attribute over a node’s other ties |
| `inertia`, `recip`, `trans` | the outcome’s own ties, in the previous wave of a panel |

The same formula can be read for an MRQAP or an ERGM, and a formula
written for a goldfish DyNAM can be read for either. A `specify_*()`
function reads a formula written for any of the three models, rewrites
each term with its equivalent in its own model, and drops, with a
warning, each term that its model cannot fit:

``` r

# An ergm formula, read for an MRQAP
specify_mrqap(advice ~ edges + mutual + nodematch("dept") + edgecov("friends"))
#> # An MRQAP formula read from a formula for ergm
#> 
#> advice ~ recip + same(dept) + friends
#> 
#> # adapted edges: is the intercept that the model always has
#> # adapted mutual -> recip
#> # adapted nodematch("dept") -> same(dept)
#> # adapted edgecov("friends") -> friends
# A goldfish formula, written for ergm
specify_ergm(advice ~ ego(age) + same(dept) + recip + trans)
#> # An ergm formula read from a formula for goldfish
#> 
#> advice ~ edges + nodeocov("age") + nodematch("dept") + mutual + 
#>     ttriple
#> 
#> # adapted ego(age) -> nodeocov("age")
#> # adapted same(dept) -> nodematch("dept")
#> # adapted recip -> mutual
#> # adapted trans -> ttriple
```

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) of a
specification gives one row for each term, with what the model makes of
it on a network:

``` r

spec <- specify_mrqap(advice ~ ego(age) + same(dept) + recip(friends) + inertia,
                      ison_hightech)
tidy(spec, ison_hightech)[, c("term", "status", "reason")]
#> # A tibble: 4 x 3
#>   term           status reason                                                  
#>   <chr>          <chr>  <chr>                                                   
#> 1 ego(age)       fits   <NA>                                                    
#> 2 same(dept)     fits   <NA>                                                    
#> 3 recip(friends) fits   <NA>                                                    
#> 4 inertia        drops  needs a longitudinal network, where it reads the outcom~
```

With a network as `.data`,
[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify.md)
also gives the matrices of the terms that ergm has no name for, so that
[`ergm::ergm()`](https://rdrr.io/pkg/ergm/man/ergm.html) can fit the
formula that it returns.

## Regression

[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
fits a regression to a network, and permutes the nodes to find the
p-value of each coefficient. By default it uses Dekker, Krackhardt, and
Snijders’ (2007) double semi-partialling, which permutes the residuals
of each predictor:

``` r

networkers <- ison_networkers |> to_subgraph(Discipline == "Sociology")
model <- estimate_mrqap(weight ~ ego(Citations) + alter(Citations) + sim(Citations),
                        networkers, times = 200, control = list(seed = 1))
model
#> # Linear MRQAP of weight
#> 
#> # A tibble: 4 x 5
#>   term            estimate statistic p.value signif   
#>   <chr>              <dbl>     <dbl> <chr>   <noquote>
#> 1 (Intercept)        2.93      0.242 <NA>             
#> 2 ego_Citations     -0.269    -1.69  0.48             
#> 3 alter_Citations   -0.158    -0.997 0.51             
#> 4 sim_Citations     39.4       3.13  0.04    *        
#> 
#> # Null: 200 permutations of each predictor, by double semi-partialling.
#> # Controls: seed = 1
#> # Signif. codes: *** 0.001, ** 0.01, * 0.05, . 0.1
```

[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
holds the other choices of the model: the family, what the permutations
permute, robust standard errors, how to treat missing data, and the
cores to run on. The family is read from the outcome when `control` does
not give one.

``` r

control_mrqap(family = "poisson", permute = "outcome", seed = 1)
#> # Controls for an MRQAP: 3 set, 12 at their defaults
#> 
#>   permute  "outcome"
#>   family   "poisson"
#>   seed     1
```

A formula adds random intercepts as
[lme4](https://github.com/lme4/lme4/) does, and absorbs fixed effects
after a bar, as [fixest](https://lrberge.github.io/fixest/) does:

``` r

estimate_mrqap(weight ~ sim(Citations) + (1 | sender) + (1 | receiver),
               networkers, times = 50, control = list(seed = 1))
estimate_mrqap(weight ~ sim(Citations) | sender + receiver,
               networkers, times = 50, control = list(seed = 1))
```

A cognitive social structure (CSS) records each person’s report of the
whole network.
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
reads a CSS from the network itself, and fits one observation for each
sender, receiver, and reporter. The `reporter` terms describe the person
who made the report:

``` r

css <- estimate_mrqap(advice ~ same(dept) + reports + reporter_ego,
                      ison_hightech, times = 50, control = list(seed = 1))
tidy(css)[, c("term", "estimate", "p.value")]
```

A longitudinal network is fitted as a panel. Each wave after the first
is an outcome, and one permutation relabels the nodes in the same way in
every wave. `inertia`, `recip`, and `trans` then read the previous wave:

``` r

panel <- estimate_mrqap(like ~ inertia + recip + same(groups),
                        ison_monks, times = 50, control = list(seed = 1))
tidy(panel)[, c("term", "estimate", "p.value")]
```

An unobserved tie is missing, not absent. By default, the model drops
each unobserved dyad, and permutes the nodes only among nodes with the
same unobserved cells, so that every permutation uses the same dyads as
the observed model. `control_mrqap(missing = "fail")` stops instead. One
student in the second wave of Knecht’s classmates did not report:

``` r

classmates <- to_wave(ison_classmates, 2)
missing <- estimate_mrqap(friends ~ ego(sex) + alter(sex) + same(sex),
                          classmates, times = 50, control = list(seed = 1))
glance(missing)[, c("nobs", "permute", "times")]
```

[`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)
tests one or more coefficients together, such as the levels of a
categorical attribute.
[`test_time()`](https://stocnet.github.io/infernet/reference/test_parameter.md)
tests whether coefficients are the same in each wave of a panel.

## Diffusion

[`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md)
simulates a diffusion on a network many times.
[`test_distributions()`](https://stocnet.github.io/infernet/reference/test_fit.md)
tests whether two models of diffusion differ, and
[`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md)
tests whether a model fits an observed diffusion:

``` r

fast <- play_diffusions(ison_adolescents, transmissibility = 0.5,
                        times = 40, control = list(seed = 1))
slow <- play_diffusions(ison_adolescents, transmissibility = 0.2,
                        times = 40, control = list(seed = 1))
test_distributions(fast, slow, times = 200)
observed <- as_diffusion(play_diffusion(ison_adolescents, transmissibility = 0.2))
test_gof(slow, observed)
```

[`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md)
also tests the fit of a network regression to a statistic of the
observed network, from the outcomes that the model simulates.

## Results

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) gives one row
for each coefficient, and
[`glance()`](https://generics.r-lib.org/reference/glance.html) one row
for the model. Both work on a regression and on a test, and both give a
tibble that other tools can read:

``` r

tidy(model)[, c("term", "estimate", "statistic", "p.value")]
#> # A tibble: 4 x 4
#>   term            estimate statistic p.value
#>   <chr>              <dbl>     <dbl>   <dbl>
#> 1 (Intercept)        2.93      0.242   NA   
#> 2 ego_Citations     -0.269    -1.69     0.48
#> 3 alter_Citations   -0.158    -0.997    0.51
#> 4 sim_Citations     39.4       3.13     0.04
glance(model)[, c("r.squared", "nobs", "permute", "times")]
#> # A tibble: 1 x 4
#>   r.squared  nobs permute   times
#>       <dbl> <int> <chr>     <dbl>
#> 1    0.0674   272 predictor   200
```

[`predict()`](https://stocnet.github.io/infernet/reference/predict.md)
gives the fitted value of each dyad, or of new data:

``` r

predict(model, data.frame(ego_Citations = 10, alter_Citations = 50,
                          sim_Citations = 0.5))
#> [1] 12.0213
```

[autograph](https://stocnet.github.io/autograph/) draws a test or a
diffusion with [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Learn more

The [website](https://stocnet.github.io/infernet/) has a page for every
function, and two tutorials:

- “Diversity and Regression” moves from univariate tests to network
  regression.
- “Diffusion and Learning” simulates diffusions and tests their fit.

To run them interactively, install
[migraph](https://stocnet.github.io/migraph/) and run
`migraph::run_tute("diversity")` or `migraph::run_tute("diffusion")`.

## Design principles

[infernet](https://stocnet.github.io/infernet/) and the other
[stocnet](https://github.com/stocnet) packages aim for three things:

- **Declarative simplicity**: functions do one clearly named thing with
  few arguments. For example,
  [`test_permutation()`](https://stocnet.github.io/infernet/reference/test_measures.md)
  is named for the null distribution it draws, and
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
  follows the `estimate_*()` verb of
  [goldfish](https://stocnet.github.io/goldfish/).
- **Pleasant predictability**: users can guess a function’s behaviour
  from its family. For example, the `test_*()` functions all take a
  network, a statistic, and a number of `times`.
- **Graceful inclusivity**: functions accept whatever network users
  bring and return something they can use. For example,
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
  works on any network that
  [manynet](https://stocnet.github.io/manynet/) can convert, one-mode or
  two-mode, and keeps missing data missing.

See the [contributing
guide](https://github.com/stocnet/infernet/blob/main/.github/CONTRIBUTING.md#style)
for how these principles translate into rules for the package.

## Installation

### Development

[infernet](https://stocnet.github.io/infernet/) is not yet on CRAN. The
latest binary releases for all major OSes – Windows, Mac, and Linux –
can be found
[here](https://github.com/stocnet/infernet/releases/latest). Download
the appropriate binary for your operating system, and install using an
adapted version of the following commands:

- For Windows:
  `install.packages("~/Downloads/infernet_winOS.zip", repos = NULL)`
- For Mac:
  `install.packages("~/Downloads/infernet_macOS.tgz", repos = NULL)`
- For Unix:
  `install.packages("~/Downloads/infernet_linuxOS.tar.gz", repos = NULL)`

To install from source, please install the
[remotes](https://remotes.r-lib.org) package from CRAN and then:

- For latest stable version:
  `remotes::install_github("stocnet/infernet")`
- For latest development version:
  `remotes::install_github("stocnet/infernet@develop")`

## Funding details

Development on this package has been funded by the Swiss National
Science Foundation (SNSF) [Grant Number
188976](https://data.snf.ch/grants/grant/188976): “Power and Networks
and the Rate of Change in Institutional Complexes” (PANARCHIC).
