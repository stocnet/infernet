# Specify an MRQAP from a formula written for any model

`specify_mrqap()` reads a formula, whichever model it was written for,
and returns the formula that
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
fits from it.
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
calls it on every formula.
[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify_ergm.md)
reads the same formulas for
[`ergm::ergm()`](https://rdrr.io/pkg/ergm/man/ergm.html),
[`specify_saom()`](https://stocnet.github.io/infernet/reference/specify_saom.md)
for [`RSiena::siena()`](https://rdrr.io/pkg/RSiena/man/siena.html), and
`{goldfish}`'s own `specify_dynam()` for a DyNAM or REM.

A term with an equivalent in the model is rewritten as that term. A term
that means something different in the model is kept and named. A term
the model cannot fit is dropped and named, so that the rest of the model
still fits. Rewritten and differently interpreted terms are reported
with
[`manynet::snet_info()`](https://stocnet.github.io/manynet/reference/interface.html),
which is silent unless `options(snet_verbosity = "verbose")`, and
dropped terms are reported in one warning. A formula aborts only where
no term is left to fit. A name that no model knows is left for the
fitting function, which then aborts and suggests the nearest name.

## Usage

``` r
specify_mrqap(formula, .data = NULL, input = NULL)

# S3 method for class 'mrqap_formula'
summary(object, .data = NULL, ...)

# S3 method for class 'mrqap_formula'
tidy(x, .data = NULL, ...)
```

## Arguments

- formula:

  A formula written for an MRQAP, goldfish, ergm, or RSiena, or a
  `sienaEffects` object. Without it, or with `.` as its right-hand side,
  the table of every term for `.data`, as
  [specify_terms](https://stocnet.github.io/infernet/reference/specify_terms.md)
  describes.

- .data:

  Optionally, the network the formula is for. With it, a bare name that
  is a layer of the network stays a layer. In
  [`summary()`](https://rdrr.io/r/base/summary.html), the network to
  build the columns from.

- input:

  The model the formula was written for: `"mrqap"`, `"dynam"`, `"rem"`,
  `"ergm"`, or `"saom"`. `NULL`, the default, reads it from the class of
  the formula, such as goldfish's `"goldfishFormula"`, or else from the
  names in the formula.

- object, x:

  A formula returned by `specify_mrqap()`.

- ...:

  Not used.

## Value

A formula of class `c("mrqap_formula", "formula")`. Its `input`
attribute names the model it was read from, and its `record` attribute
holds what the model makes of each term. Printing the formula lists the
terms that do not fit as written and returns the formula, invisibly.
[`summary()`](https://rdrr.io/r/base/summary.html) returns every term as
a tibble, with the columns `term`, `status`, `as`, `reason`, and
`columns`. [`tidy()`](https://generics.r-lib.org/reference/tidy.html) of
a specification is deprecated: it gives the same table and warns once.
Without a formula, the table of every term for `.data`, as
[specify_terms](https://stocnet.github.io/infernet/reference/specify_terms.md)
describes.

## What each model makes of each term

[`summary()`](https://rdrr.io/r/base/summary.html) of the result has one
row for each term of the formula, and fits nothing. Its `status` is what
the model makes of the term:

- `"fits"`: the model fits the term as it is written;

- `"adapted"`: the term is rewritten as its equivalent in the model, in
  `as`;

- `"interpreted"`: the model has a term of the same name that it defines
  differently, as `reason` says;

- `"warns"`: the model fits the term, but with a warning, as `reason`
  says;

- `"drops"`: the model cannot fit the term, so it is left out, and the
  rest is fitted;

- `"refuses"`: the model aborts, as `reason` says.

An MRQAP warns for a term that reads the outcome's own ties, such as
`recip`, and refuses `tie()`, `indeg()`, and `outdeg()` of the outcome
layer. It drops an offset, such as goldfish's `offset(recip, coef = 0)`,
since it estimates every coefficient; ergm keeps an ergm one. For an
MRQAP, [`summary()`](https://rdrr.io/r/base/summary.html) with `.data`
also builds each term and names its `columns`, as
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
names them.

## Reading the model a formula was written for

The model a formula was written for is read, in order, from `input`,
from the class of the formula, such as `"ergm_formula"`, or from the
names in the formula:

- `"ergm"` where a term is named only in ergm, such as `edges`,
  `mutual`, or `nodematch("x")`;

- `"saom"` where an effect is named only in RSiena, such as `transTrip`,
  `inPop`, or `egoX(sex)`;

- `"dynam"` where a term is named only in goldfish, such as `inertia` or
  `trans`;

- `"mrqap"` otherwise.

A `sienaEffects` object of `{RSiena}` is read as the formula of the
evaluation effects it includes for its first dependent network.

The model that was read is reported. Where a formula mixes ergm's names
with goldfish's, it is read as an ergm formula, and goldfish's names in
it are read as they are in goldfish. The result of each function carries
its model as its class, so that `specify_ergm(specify_mrqap(f))` reads
its input without a guess.

## ergm and infernet

ergm names an attribute with a string, and infernet with a bare name.
Both read `levels =` as names or positions. infernet does not read
ergm's functions for `levels =`, such as `LARGEST`, or `nodemix()`'s
`levels =`, which changes the positions of `levels2 =`: such a term is
left to ergm, and an MRQAP drops it.
[terms_mrqap](https://stocnet.github.io/infernet/reference/terms_mrqap.md)
gives each term's ergm equivalent.

Read from ergm, `nodecov("x")` becomes `ego(x) + alter(x)`, and `edges`
and `density` are the intercept, which an MRQAP always has. ergm's terms
of the outcome's own structure, such as `gwesp()`, and its node fixed
effects, `sender`, `receiver`, and `sociality`, are kept for ergm and
dropped for an MRQAP, where `(1 | sender)` adds a random intercept
instead. The left-hand side of an ergm formula names the network rather
than a layer, so where it names no layer or tie attribute of `.data`,
the outcome is read as `.`.

## goldfish and infernet

goldfish's terms are infernet's where the two packages share them, as
[terms_mrqap](https://stocnet.github.io/infernet/reference/terms_mrqap.md)
lists. `sim()` is 1 - \|d\| / range in infernet and -\|d\| in goldfish,
which changes the scale of its coefficient but not its test, and
`tertius()` and `tertius_diff()` leave out the sender in infernet and
include it in goldfish. Both are kept and named.

For an MRQAP, goldfish's endogenous effects, such as `cycle`,
`common_sender`, `common_receiver`, and `four`, read the outcome's own
ties, which permuting the outcome would not permute with them, so they
are dropped. So are goldfish's closures through two layers and the
DyNAM-i statistics of groups. `inertia`, `recip`, and `trans` read the
outcome's previous wave where `.data` is a longitudinal network, and fit
there; on a cross-sectional network, `inertia` and `trans` are dropped
too.

goldfish names the side of `indeg()` and `outdeg()` with `type =`, and
reads the receiver for both by default, where infernet's `outdeg()`
reads the sender. A goldfish formula is read as goldfish reads it:
`outdeg(friends)` becomes `outdeg(friends, node = "alter")`, with a
warning, and `type =` becomes `node =`. `indeg` or `outdeg` on its own
reads the outcome's own degree in goldfish, and is dropped for an MRQAP
and for ergm. goldfish's older formulas name a node attribute with its
table of nodes, as in `ego(actors$floor)`, which is read as
`ego(floor)`, since the network holds its nodes' attributes.

## See also

[terms_mrqap](https://stocnet.github.io/infernet/reference/terms_mrqap.md)
for what each term measures, and
[specify_terms](https://stocnet.github.io/infernet/reference/specify_terms.md)
for the terms that suit a network.

Other specifying functions:
[`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md),
[`declare_coefs_ergm`](https://stocnet.github.io/infernet/reference/declare_coefs_ergm.md),
[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify_ergm.md),
[`specify_saom()`](https://stocnet.github.io/infernet/reference/specify_saom.md),
[`specify_terms`](https://stocnet.github.io/infernet/reference/specify_terms.md),
[`terms_mrqap`](https://stocnet.github.io/infernet/reference/terms_mrqap.md)

## Examples

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
# A goldfish formula, read for an MRQAP
f <- suppressWarnings(
  specify_mrqap(advice ~ inertia + recip + trans + ego(age)))
f
#> # An MRQAP formula read from a formula for goldfish
#> 
#> advice ~ recip + ego(age)
#> 
#> # drops inertia: needs a longitudinal network, where it reads the outcome's
#> #   previous wave
#> # warns recip: reads the outcome's own ties, which permuting the outcome does
#> #   not permute with it
#> # drops trans: needs a longitudinal network, where it reads the outcome's
#> #   previous wave
# Each term, and what the model makes of it
summary(f)
#> # A tibble: 4 × 5
#>   term     status as       reason                                        columns
#>   <chr>    <chr>  <chr>    <chr>                                         <list> 
#> 1 inertia  drops  NA       needs a longitudinal network, where it reads… <chr>  
#> 2 recip    warns  recip    reads the outcome's own ties, which permutin… <chr>  
#> 3 trans    drops  NA       needs a longitudinal network, where it reads… <chr>  
#> 4 ego(age) fits   ego(age) NA                                            <chr>  
g <- specify_mrqap(advice ~ ego(age) + same(dept) + recip(friends), ison_hightech)
summary(g, ison_hightech)
#> # A tibble: 3 × 5
#>   term           status as             reason columns  
#>   <chr>          <chr>  <chr>          <chr>  <list>   
#> 1 ego(age)       fits   ego(age)       NA     <chr [1]>
#> 2 same(dept)     fits   same(dept)     NA     <chr [1]>
#> 3 recip(friends) fits   recip(friends) NA     <chr [1]>
```
