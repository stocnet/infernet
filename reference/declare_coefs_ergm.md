# Declare the coefficients of an ERGM

[`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md)
also makes a set of coefficients for an exponential random graph model
from `{ergm}`:

- for an ergm formula, such as the result of
  [`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify_ergm.md),
  the values stated in `...`;

- for an `ergm`, from
  [`ergm::ergm()`](https://rdrr.io/pkg/ergm/man/ergm.html), the
  estimated coefficients, with the formula and the network of the fit.

The set is in the form that `{ergm}` reads.
[`stats::coef()`](https://rdrr.io/r/stats/coef.html) of it is a named
vector in the order of ergm's coefficients, so it can be given as
`coef =` to `{ergm}`'s
[`simulate()`](https://rdrr.io/r/stats/simulate.html), or as `init =` to
[`ergm::control.ergm()`](https://rdrr.io/pkg/ergm/man/control.ergm.html).
[`simulate()`](https://rdrr.io/r/stats/simulate.html) of the set draws
networks from it with `{ergm}`'s
[`simulate()`](https://rdrr.io/r/stats/simulate.html).

## Usage

``` r
# S3 method for class 'ergm_formula'
declare_coefs(x, ..., .data = NULL)

# S3 method for class 'ergm'
declare_coefs(x, ...)

# S3 method for class 'infernetErgmCoefs'
coef(object, ...)

# S3 method for class 'infernetErgmCoefs'
simulate(object, nsim = 1, seed = NULL, ...)
```

## Arguments

- x:

  An ergm formula, such as from
  [`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify_ergm.md),
  or an `ergm` from
  [`ergm::ergm()`](https://rdrr.io/pkg/ergm/man/ergm.html).

- ...:

  For a formula, the values of the coefficients, each named for its
  coefficient, such as `edges = -2, nodecov.wealth = 0.01`, or as one
  named vector, such as `coef(fit)`. For a fit, not used.

- .data:

  Optionally, the network of the model, which ergm reads as `basis`.
  Without it, the network is the outcome of the formula.

- object:

  A set of coefficients of an ERGM, from
  [`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md).

- nsim:

  The number of networks to draw.

- seed:

  A seed for the draws, as `{ergm}`'s
  [`simulate()`](https://rdrr.io/r/stats/simulate.html) reads it.

## Value

An object of class `infernetErgmCoefs`: a named numeric vector of the
coefficients, with `NA` for a value that is not stated. Its attributes
are `spec`, the formula, and `basis`, the network or `NULL`.
[`simulate()`](https://rdrr.io/r/stats/simulate.html) returns what
`{ergm}`'s [`simulate()`](https://rdrr.io/r/stats/simulate.html)
returns: a `network` for one draw, and a `network.list` for more.

## Details

The names of the values are ergm's names for the coefficients, as
[`stats::coef()`](https://rdrr.io/r/stats/coef.html) of a fit gives
them, such as `"edges"`, `"nodematch.dept"`, or `"gwesp.fixed.0.25"`. A
curved term without `fixed = TRUE` has a coefficient for its decay too,
such as `"gwesp.decay"`. Where ergm can find the network, as in the
result of `specify_ergm(formula, .data)`, or with `.data`, each name is
checked against these, and each coefficient without a value is `NA`.

[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify_ergm.md)
gives a formula that `{ergm}` reads, so
`simulate(specify_ergm(formula, .data), coef = coef(fit))` already draws
from `{ergm}`, without a set of coefficients.

## See also

Other specifying functions:
[`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md),
[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify_ergm.md),
[`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md),
[`specify_saom()`](https://stocnet.github.io/infernet/reference/specify_saom.md),
[`specify_terms`](https://stocnet.github.io/infernet/reference/specify_terms.md),
[`terms_mrqap`](https://stocnet.github.io/infernet/reference/terms_mrqap.md)

## Examples

``` r
data(florentine, package = "ergm")
coefs <- declare_coefs(specify_ergm(flomarriage ~ edges + nodecov("wealth")),
                       edges = -3, nodecov.wealth = 0.01)
coefs
#> # Coefficients of an ERGM
#> # 2 of 2 coefficients stated
#> 
#> flomarriage ~ edges + nodecov("wealth")
#> 
#>          edges nodecov.wealth 
#>          -3.00           0.01 
simulate(coefs, seed = 1)
#>  Network attributes:
#>   vertices = 16 
#>   directed = FALSE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   total edges= 9 
#>     missing edges= 0 
#>     non-missing edges= 9 
#> 
#>  Vertex attribute names: 
#>     priorates totalties vertex.names wealth 
#> 
#> No edge attributes

fit <- ergm::ergm(flomarriage ~ edges + nodecov("wealth"))
#> Starting maximum pseudolikelihood estimation (MPLE):
#> Obtaining the responsible dyads.
#> Evaluating the predictor and response matrix.
#> Maximizing the pseudolikelihood.
#> Finished MPLE.
#> Evaluating log-likelihood at the estimate. 
#> 
declare_coefs(fit)
#> # Coefficients of an ERGM
#> # 2 of 2 coefficients stated
#> 
#> flomarriage ~ edges + nodecov("wealth")
#> 
#>          edges nodecov.wealth 
#>    -2.59492903     0.01054591 
```
