# Simulate networks from an MRQAP

[`simulate()`](https://rdrr.io/r/stats/simulate.html) draws outcome
networks from an MRQAP, conditional on its covariates. It has three
methods, which give the same networks for the same coefficients, the
same network, and the same seed:

- `simulate(fit)`, for an `infernetMrqapFit` from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md);

- `simulate(coefs, .data = net)`, for a set of coefficients from
  [`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md);

- `simulate(spec, coefs = , .data = net)`, for a specification from
  [`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md)
  and its coefficients.

[`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md)
draws its simulations of an `infernetMrqapFit` with the same simulator.

## Usage

``` r
# S3 method for class 'infernetMrqapFit'
simulate(
  object,
  nsim = 1,
  seed = getOption("snet_computation")$seed,
  .data = NULL,
  ...
)

# S3 method for class 'infernetMrqapCoefs'
simulate(
  object,
  nsim = 1,
  seed = getOption("snet_computation")$seed,
  .data,
  ...
)

# S3 method for class 'mrqap_formula'
simulate(
  object,
  nsim = 1,
  seed = getOption("snet_computation")$seed,
  .data,
  coefs,
  family = NULL,
  sigma = NULL,
  ...
)
```

## Arguments

- object:

  An `infernetMrqapFit` from
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md),
  a set of coefficients from
  [`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md),
  or a specification from
  [`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md).

- nsim:

  The number of networks to simulate.

- seed:

  A seed for the random draws. By default, the seed set with
  [`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md),
  if any. The random state of the session is put back after a draw with
  a seed.

- .data:

  The network that the model builds its columns on. For a fit, the
  network that it was fitted on. It is optional for a fit: without it,
  the result is a list of `stocnet` networks, in which the cells outside
  the model are empty.

- ...:

  Not used.

- coefs:

  For a specification, the coefficients: a set from
  [`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md),
  or a named vector such as `stats::coef(fit)`.

- family, sigma:

  For a specification with a plain vector of coefficients, the family
  and, for a `"gaussian"` model, the standard deviation of the
  residuals, as in
  [`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md).
  `family = NULL`, the default, takes the family of a set from
  [`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md),
  or else reads it from the outcome in `.data`, as
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
  does.

## Value

A list of `nsim` networks, of the class of `.data`, or of class
`stocnet` without it.

## Details

Each simulation draws a tie in each dyad that the model used, from the
model's predicted value there: a Bernoulli draw for `"binomial"`, a
Poisson draw for `"poisson"`, and a normal draw with standard deviation
`sigma` for `"gaussian"`. The other cells of the outcome, such as an
unobserved dyad or a fixed tie, keep their value in `.data`, or are
empty without it. An undirected network is mirrored.

A fit draws from its coefficients where its family is `"gaussian"`,
`"binomial"`, or `"poisson"` and it has no random or fixed effects.
Otherwise it draws with
[`stats::simulate()`](https://rdrr.io/r/stats/simulate.html) of the
observed model, which it keeps with `"model"` in `outputs`. A set of
coefficients and a specification draw only from a model without random
or fixed effects. A list of networks, a panel, and a cognitive social
structure cannot yet be simulated.

[`simulate()`](https://rdrr.io/r/stats/simulate.html) is conditional on
the covariates of the network. `{manynet}`'s `generate_*()` functions
are not.

Each simulation has its own random stream, so a seed gives the same
networks whether they are drawn sequentially or in parallel. The
`ncores` and `strategy` set with
[`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md)
choose the plan.

## See also

[`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md)
for a set of coefficients, and
[`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md),
which compares the observed network with these simulations.

Other interpreting functions:
[`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md),
[`predict_mrqap`](https://stocnet.github.io/infernet/reference/predict_mrqap.md),
[`sweep`](https://stocnet.github.io/infernet/reference/sweep.md)

## Examples

``` r
fit <- estimate_mrqap(friends ~ social + tasks, ison_algebra, times = 5,
                      control = list(family = "poisson"))
nets <- simulate(fit, nsim = 2, seed = 1, .data = ison_algebra)
nets[[1]]
#> # A weighted, directed network of 16 nodes and 49 arcs
#> 
#> ── Ties 
#> # A tibble: 49 × 3
#>    from    to weight
#>   <int> <int>  <dbl>
#> 1     1     7      1
#> 2     1    10      2
#> 3     2     5      1
#> 4     2     6      1
#> 5     2     7      1
#> 6     2     8      2
#> # ℹ 43 more rows
#> 

# The same networks, from the coefficients of the fit
coefs <- declare_coefs(fit)
same <- simulate(coefs, nsim = 2, seed = 1, .data = ison_algebra)

# Or from a specification and coefficients that are stated
spec <- specify_mrqap(friends ~ social + tasks)
sims <- simulate(spec, nsim = 2, seed = 1, .data = ison_algebra,
                 coefs = c("(Intercept)" = -1, social = 0.5, tasks = 0),
                 family = "poisson")

# The family read from a 0/1 outcome
adolescents <- manynet::mutate_nodes(ison_adolescents, age = 11:18)
binary <- simulate(specify_mrqap(. ~ ego(age)), seed = 1, .data = adolescents,
                   coefs = c("(Intercept)" = -2, ego_age = 0.1))
```
