# Computation controls for the session or for one call

The computation controls say how the draws of a model or a test run, not
what is fitted: `seed`, `ncores`, and `strategy`. They are the same for
every model, so they can persist across the scripts of a session.

- `set_control_computation()` writes them to
  `options(snet_computation)`, after it checks them. Each later call of
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md),
  [`test_random()`](https://stocnet.github.io/infernet/reference/test_measures.md),
  [`test_configuration()`](https://stocnet.github.io/infernet/reference/test_measures.md),
  [`test_permutation()`](https://stocnet.github.io/infernet/reference/test_measures.md),
  [`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md),
  [`test_distributions()`](https://stocnet.github.io/infernet/reference/test_fit.md),
  [`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md),
  [`test_time()`](https://stocnet.github.io/infernet/reference/test_parameter.md),
  and
  [`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md)
  then uses them where they are not given, and says so with
  `options(snet_verbosity = "verbose")`. `{goldfish}` reads the same
  option.

- `control_computation()` gives them to one call only, as
  `control = c(control_mrqap(...), control_computation(...))`, or
  `control = control_computation(...)` alone.

A control given in a call, such as `control_mrqap(seed = 1)` or
`control = list(seed = 1)`, takes priority. Then comes
`control_computation()`, then `options(snet_computation)`, and then the
default of
[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md).

`set_control_computation()` changes only the controls that it is given.
A control given as `NULL` is removed, and `persist = FALSE` removes all
of them. The option is checked again when it is read, since it can also
be set by hand.

A persistent `seed` sets the same seed in every call, so each call draws
the same random numbers, sequentially or in parallel, and its result can
be reproduced. The print of a result gives the seed that it used, and
every result records its controls as `control`.

## Usage

``` r
set_control_computation(
  seed = NULL,
  ncores = NULL,
  strategy = NULL,
  persist = TRUE
)

control_computation(seed = NULL, ncores = NULL, strategy = NULL)

# S3 method for class 'mrqap_control'
c(...)
```

## Arguments

- seed:

  A number to set the random seed with before the draws. See
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md).

- ncores:

  A whole number of cores to run the draws on. See
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md).

- strategy:

  The `{future}` plan that the draws run on where `ncores` is `NULL`:
  `"sequential"`, `"multisession"`, `"multicore"`, or `"cluster"`. See
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md).

- persist:

  Logical. `TRUE` (the default) writes the controls that are given to
  the option. `FALSE` clears the option, and takes no controls.

- ...:

  For [`c()`](https://rdrr.io/r/base/c.html), the result of
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
  and then of `control_computation()`.

## Value

`set_control_computation()` returns the value of
`options(snet_computation)` before the call, invisibly, so that
`options(snet_computation = old)` restores it. `control_computation()`
returns an object of class `infernetComputationControl`: a list of the
controls that it is given.

## See also

Other estimating functions:
[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md),
[`other_results`](https://stocnet.github.io/infernet/reference/other_results.md),
[`print.infernetMrqapFit()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)

## Examples

``` r
# For the session: later calls use these where they are not given
old <- set_control_computation(seed = 1, ncores = 1)
control_mrqap()
#> # Controls for an MRQAP: 2 set, 13 at their defaults
#> 
#>   seed    1  (option)
#>   ncores  1  (option)
#> 
#> # (option) persists from set_control_computation();
#> #   set_control_computation(persist = FALSE) clears it.
# A control given in a call wins
control_mrqap(seed = 2)$seed
#> [1] 2
# NULL removes one control, and persist = FALSE removes all of them
set_control_computation(ncores = NULL)
getOption("snet_computation")
#> $seed
#> [1] 1
#> 
set_control_computation(persist = FALSE)
options(snet_computation = old)

# For one call only
control_computation(seed = 3)
#> # Computation controls for one call: 1 set
#> 
#>   seed  3
c(control_mrqap(family = "poisson"), control_computation(seed = 3))
#> # Controls for an MRQAP: 2 set, 13 at their defaults
#> 
#>   family  "poisson"
#>   seed    3
```
