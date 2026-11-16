# Specify a SAOM from a formula written for any model

`specify_saom()` reads a formula, whichever model it was written for,
and writes each term as the `{RSiena}` effect of a stochastic
actor-oriented model (SAOM). With the network, it returns the
`sienaEffects` object that
[`RSiena::siena()`](https://rdrr.io/pkg/RSiena/man/siena.html) fits,
with the `sienadata` object it is for.
[`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md)
and
[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify_ergm.md)
read the same formulas for their models, and also read a `sienaEffects`
object.

A term with an equivalent in RSiena is rewritten as that effect. A term
RSiena cannot fit is dropped and named, so that the rest of the model
still fits. Rewritten terms are reported with
[`manynet::snet_info()`](https://stocnet.github.io/manynet/reference/interface.html),
which is silent unless `options(snet_verbosity = "verbose")`, and
dropped terms are reported in one warning.

## Usage

``` r
specify_saom(formula, .data = NULL, input = NULL)

# S3 method for class 'saom_formula'
summary(object, ...)
```

## Arguments

- formula:

  A formula written for an MRQAP, goldfish, ergm, or RSiena, a list of
  such formulas, one for each dependent network, or a `sienaEffects`
  object. Without it, or with `.` as its right-hand side, the table of
  every term for `.data`, as
  [specify_terms](https://stocnet.github.io/infernet/reference/specify_terms.md)
  describes, with the RSiena effect of each in its column `as`.

- .data:

  Optionally, the network the formula is for, with at least two waves.
  With it, the result is a `sienaEffects` object, which needs
  `{RSiena}`. Without it, the result is a formula, and RSiena is not
  needed.

- input:

  The model the formula was written for: `"mrqap"`, `"dynam"`, `"rem"`,
  `"ergm"`, or `"saom"`. `NULL`, the default, reads it from the class of
  the formula, such as goldfish's `"goldfishFormula"`, or else from the
  names in the formula.

- object:

  A formula returned by `specify_saom()`.

- ...:

  Not used.

## Value

Without `.data`, a formula of class `c("saom_formula", "formula")`, or a
list of them. Its `input` attribute names the model it was read from,
and its `record` attribute holds what RSiena makes of each term.
[`summary()`](https://rdrr.io/r/base/summary.html) returns every term as
a tibble, with the columns `term`, `status`, `as`, and `reason`.

With `.data`, the `sienaEffects` object of
[`RSiena::getEffects()`](https://rdrr.io/pkg/RSiena/man/make_specification.html),
with the effects of the formula included. Its attribute `data` holds the
`sienadata` object, `formula` the SAOM formula, or the list of them, and
`record` what RSiena makes of each term.

Without a formula, the table of every term for `.data`, as
[specify_terms](https://stocnet.github.io/infernet/reference/specify_terms.md)
describes.

## Writing a formula for RSiena

Each effect is written as its RSiena short name, with the covariate or
network it reads as its argument: `ego(sex)` is `egoX(sex)`, `same(sex)`
is `sameX(sex)`, `diff(age)` is `absDiffX(age)`, a layer such as
`primary` is `X(primary)`, and `trans` is `transTrip`.
[terms_mrqap](https://stocnet.github.io/infernet/reference/terms_mrqap.md)
gives the effect for each term. A formula written in RSiena's short
names, such as `friends ~ recip + transTrip + inPop + egoX(sex)`, is
read as one written for RSiena, and an effect that has no equivalent in
the other models, such as `inPop`, is kept as it is. `density` is always
added, as the outdegree effect that every SAOM of a network has. Every
other evaluation effect is in the model only if the formula names it:
`recip`, which RSiena includes by default, is left out where the formula
does not name it, and a message says so. A random intercept, a fixed
effect, a reporter term, `global()`, and (for now) an interaction are
dropped. Add an interaction to the result with
[`RSiena::includeInteraction()`](https://rdrr.io/pkg/RSiena/man/set_interaction.html).

## The data and the dependent variables

With `.data`, the network is coerced with
[`manynet::as_siena()`](https://stocnet.github.io/manynet/reference/coerce_stocnet.html).
The left-hand side of the formula names the dependent network, which
must be a layer of the network, or `.` for its focal layer. A list of
formulas names several dependent networks, one for each formula. Only
these layers are dependent variables: every other layer becomes a dyadic
covariate, and every node attribute a covariate, constant or changing
across the waves. A dependent behaviour is not yet written here; for
one, build the effects with
[`RSiena::getEffects()`](https://rdrr.io/pkg/RSiena/man/make_specification.html).
Each effect is checked against the effects that RSiena has for the data,
and a name that it does not have aborts with the nearest name. The model
is then fitted with
[`RSiena::siena()`](https://rdrr.io/pkg/RSiena/man/siena.html) of the
data and the effects, as in the last example. `siena()` writes an output
file in the working directory, named after its data argument, unless
`control_out = RSiena::set_output_saom(outputName = )` names another.

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

## See also

[terms_mrqap](https://stocnet.github.io/infernet/reference/terms_mrqap.md)
for the RSiena effect of each term, and
[specify_terms](https://stocnet.github.io/infernet/reference/specify_terms.md)
for the terms that suit a network.

Other specifying functions:
[`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md),
[`declare_coefs_ergm`](https://stocnet.github.io/infernet/reference/declare_coefs_ergm.md),
[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify_ergm.md),
[`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md),
[`specify_terms`](https://stocnet.github.io/infernet/reference/specify_terms.md),
[`terms_mrqap`](https://stocnet.github.io/infernet/reference/terms_mrqap.md)

## Examples

``` r
# An MRQAP formula, written for RSiena
specify_saom(friends ~ recip + trans + ego(sex) + same(sex) + primary)
#> # A SAOM formula read from a formula for goldfish
#> 
#> friends ~ density + recip + transTrip + egoX(sex) + sameX(sex) + 
#>     X(primary)
#> 
#> # adapted trans -> transTrip
#> # adapted ego(sex) -> egoX(sex)
#> # adapted same(sex) -> sameX(sex)
#> # adapted primary -> X(primary)
# The terms that RSiena can take for a network
specify_saom(friends ~ ., manynet::ison_classmates)
#> 30 terms for RSiena of friends
#> # A tibble: 30 × 5
#>    term                                         effect     layer attribute as   
#>    <chr>                                        <chr>      <chr> <chr>     <chr>
#>  1 ego(sex)                                     ego        NA    sex       egoX…
#>  2 alter(sex)                                   alter      NA    sex       altX…
#>  3 same(sex)                                    same       NA    sex       same…
#>  4 ego(age)                                     ego        NA    age       egoX…
#>  5 alter(age)                                   alter      NA    age       altX…
#>  6 diff(age)                                    diff       NA    age       absD…
#>  7 diff(age, transformer_fn = identity)         diff       NA    age       diff…
#>  8 sim(age)                                     sim        NA    age       simX…
#>  9 ego(ethnicity)                               ego        NA    ethnicity egoX…
#> 10 alter(ethnicity)                             alter      NA    ethnicity altX…
#> 11 same(ethnicity)                              same       NA    ethnicity same…
#> 12 ego(religion)                                ego        NA    religion  egoX…
#> 13 alter(religion)                              alter      NA    religion  altX…
#> 14 same(religion)                               same       NA    religion  same…
#> 15 ego(delinquency)                             ego        NA    delinque… egoX…
#> 16 alter(delinquency)                           alter      NA    delinque… altX…
#> 17 diff(delinquency)                            diff       NA    delinque… absD…
#> 18 diff(delinquency, transformer_fn = identity) diff       NA    delinque… diff…
#> 19 sim(delinquency)                             sim        NA    delinque… simX…
#> 20 indeg(friends)                               indeg      frie… NA        inPop
#> 21 indeg(friends, node = 'ego')                 indeg      frie… NA        inAct
#> 22 outdeg(friends)                              outdeg     frie… NA        outA…
#> 23 outdeg(friends, node = 'alter')              outdeg     frie… NA        outP…
#> 24 tie(primary)                                 tie        prim… NA        X(pr…
#> 25 recip                                        recip      NA    NA        recip
#> 26 trans                                        trans      NA    NA        tran…
#> 27 cycle                                        cycle      NA    NA        cycl…
#> 28 common_sender                                common_se… NA    NA        gwes…
#> 29 common_receiver                              common_re… NA    NA        gwes…
#> 30 four                                         four       NA    NA        cycl…
#> Choose rows, and `formula()` of them gives their formula.
# The effects object that RSiena::siena() fits
eff <- specify_saom(friends ~ recip + trans + ego(sex) + same(sex),
                    manynet::ison_classmates)
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
data <- attr(eff, "data")
# \donttest{
fit <- RSiena::siena(data, eff, batch = TRUE, silent = TRUE,
  control_algo = RSiena::set_algorithm_saom(nsub = 1, n3 = 50, seed = 1),
  control_out = RSiena::set_output_saom(outputName = file.path(tempdir(), "classmates")))
# }
summary(specify_saom(friends ~ recip + trans + ego(sex) + tie(primary)))
#> # A tibble: 4 × 4
#>   term         status  as         reason
#>   <chr>        <chr>   <chr>      <chr> 
#> 1 recip        fits    recip      NA    
#> 2 trans        adapted transTrip  NA    
#> 3 ego(sex)     adapted egoX(sex)  NA    
#> 4 tie(primary) adapted X(primary) NA    
```
