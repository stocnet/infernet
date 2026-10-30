# The terms that suit a network

Without a formula, or with `.` as its whole right-hand side, as in
`advice ~ .`,
[`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify.md)
and
[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify.md)
return a table of every term that the model can take for `.data`, as
`RSiena::getEffects()` does for a Siena data object. Each term is
written out for the network's layers and node attributes, such as
`ego(age)`, `same(dept)`, `tie(friends)`, and
`indeg(friends, node = 'ego')`. Choose rows with `[` or
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
and [`formula()`](https://rdrr.io/r/stats/formula.html) of the rows
gives the formula of those terms.
[terms_mrqap](https://stocnet.github.io/infernet/reference/terms_mrqap.md)
says what each term measures.

## Usage

``` r
# S3 method for class 'specify_terms'
formula(x, lhs = NULL, ...)

# S3 method for class 'specify_terms'
tidy(x, ...)
```

## Arguments

- x:

  The table of every term.

- lhs:

  The outcome of the formula, as a name or a string. `NULL`, the
  default, is the outcome the table was made for.

- ...:

  Not used.

## Value

The table is a tibble of class `"mrqap_terms"` or `"ergm_terms"`, and
also `"specify_terms"`, with the columns `term`, `effect`, `layer`, and
`attribute`. [`print()`](https://rdrr.io/r/base/print.html) shows every
row, and [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns the table as a plain tibble.
[`formula()`](https://rdrr.io/r/stats/formula.html) returns the formula
of the chosen rows, as
[`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify.md)
or
[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify.md)
returns it.

## Which terms are listed

- A numeric attribute gets `ego()`, `alter()`,
  [`diff()`](https://rdrr.io/r/base/diff.html), and `sim()`, and a
  categorical one `ego()`, `alter()`, `same()`, and `mix()`. A name, a
  label, or an attribute with a different category for each node gets no
  term.

- An undirected network has no `alter()`, `exchange()`, `outdeg()`, or
  `recip`, since the two sides of its ties are the same.

- A two-mode network reads `ego()` on the first mode and `alter()` on
  the second. A term of both sides needs the attribute on both modes.

- The reporter terms are only for a cognitive social structure, and
  `global()` only for a list of networks. An MRQAP takes `inertia` and
  `trans` only on a panel.

- A term that the model drops or refuses, such as `tie()` of the outcome
  layer, has no row. Nor has a term that reads the outcome's own ties,
  such as `recip` in an MRQAP of one network, since permuting the
  outcome does not permute the term with it.

The terms are for the outcome on the left-hand side, by default `.`.
Where `.` is not one layer, as in a multiplex network without a focal
layer, the terms are for its first layer, and a message says so.

## A formula of chosen rows

[`formula()`](https://rdrr.io/r/stats/formula.html) of the rows gives
the formula of their terms, read by the same `specify_*()` function, for
the same outcome or for `lhs`. What the model makes of each term is in
the record of that formula, as
[specify](https://stocnet.github.io/infernet/reference/specify.md)
describes. Some terms measure the same thing in different ways:
`same(x)`, `same(x, diff = TRUE)`, and `mix(x)`, and `diff(x)` and
`sim(x)`. Of each such set,
[`formula()`](https://rdrr.io/r/stats/formula.html) keeps the first row,
and a message names the others, since a model of all of them is
collinear. For the formula that a regression fitted, see
[netreg_results](https://stocnet.github.io/infernet/reference/netreg_results.md).

## See also

[terms_mrqap](https://stocnet.github.io/infernet/reference/terms_mrqap.md)
for what each term measures, and
[specify](https://stocnet.github.io/infernet/reference/specify.md) for
what each model makes of a formula.

Other models:
[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md),
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md),
[`other_results`](https://stocnet.github.io/infernet/reference/other_results.md),
[`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md),
[`print.netreg_lm()`](https://stocnet.github.io/infernet/reference/netreg_results.md),
[`regression`](https://stocnet.github.io/infernet/reference/regression.md),
[`specify`](https://stocnet.github.io/infernet/reference/specify.md),
[`terms_mrqap`](https://stocnet.github.io/infernet/reference/terms_mrqap.md),
[`test_fit`](https://stocnet.github.io/infernet/reference/test_fit.md),
[`test_measures`](https://stocnet.github.io/infernet/reference/test_measures.md),
[`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)

## Examples

``` r
# Every term for a network, and a formula of some of them
terms <- specify_mrqap(advice ~ ., ison_hightech)
terms
#> 52 terms for an MRQAP of advice
#> # A tibble: 52 × 4
#>    term                                    effect         layer   attribute
#>    <chr>                                   <chr>          <chr>   <chr>    
#>  1 ego(age)                                ego            NA      age      
#>  2 alter(age)                              alter          NA      age      
#>  3 diff(age)                               diff           NA      age      
#>  4 diff(age, transformer_fn = identity)    diff           NA      age      
#>  5 sim(age)                                sim            NA      age      
#>  6 ego(tenure)                             ego            NA      tenure   
#>  7 alter(tenure)                           alter          NA      tenure   
#>  8 diff(tenure)                            diff           NA      tenure   
#>  9 diff(tenure, transformer_fn = identity) diff           NA      tenure   
#> 10 sim(tenure)                             sim            NA      tenure   
#> 11 ego(level)                              ego            NA      level    
#> 12 alter(level)                            alter          NA      level    
#> 13 diff(level)                             diff           NA      level    
#> 14 diff(level, transformer_fn = identity)  diff           NA      level    
#> 15 sim(level)                              sim            NA      level    
#> 16 ego(dept)                               ego            NA      dept     
#> 17 alter(dept)                             alter          NA      dept     
#> 18 same(dept)                              same           NA      dept     
#> 19 same(dept, diff = TRUE)                 same           NA      dept     
#> 20 mix(dept)                               mix            NA      dept     
#> 21 tie(friends)                            tie            friends NA       
#> 22 exchange(friends)                       exchange       friends NA       
#> 23 indeg(friends)                          indeg          friends NA       
#> 24 indeg(friends, node = 'ego')            indeg          friends NA       
#> 25 outdeg(friends)                         outdeg         friends NA       
#> 26 outdeg(friends, node = 'alter')         outdeg         friends NA       
#> 27 tertius(friends, age)                   tertius        friends age      
#> 28 tertius_diff(friends, age)              tertius_diff   friends age      
#> 29 tertius(friends, tenure)                tertius        friends tenure   
#> 30 tertius_diff(friends, tenure)           tertius_diff   friends tenure   
#> 31 tertius(friends, level)                 tertius        friends level    
#> 32 tertius_diff(friends, level)            tertius_diff   friends level    
#> 33 tie(reports)                            tie            reports NA       
#> 34 exchange(reports)                       exchange       reports NA       
#> 35 indeg(reports)                          indeg          reports NA       
#> 36 indeg(reports, node = 'ego')            indeg          reports NA       
#> 37 outdeg(reports)                         outdeg         reports NA       
#> 38 outdeg(reports, node = 'alter')         outdeg         reports NA       
#> 39 tertius(reports, age)                   tertius        reports age      
#> 40 tertius_diff(reports, age)              tertius_diff   reports age      
#> 41 tertius(reports, tenure)                tertius        reports tenure   
#> 42 tertius_diff(reports, tenure)           tertius_diff   reports tenure   
#> 43 tertius(reports, level)                 tertius        reports level    
#> 44 tertius_diff(reports, level)            tertius_diff   reports level    
#> 45 reporter(age)                           reporter       NA      age      
#> 46 reporter(tenure)                        reporter       NA      tenure   
#> 47 reporter(level)                         reporter       NA      level    
#> 48 reporter(dept)                          reporter       NA      dept     
#> 49 reporter_ego                            reporter_ego   NA      NA       
#> 50 reporter_alter                          reporter_alter NA      NA       
#> 51 reporter_same(dept)                     reporter_same  NA      dept     
#> 52 reporter_same(dept, node = 'alter')     reporter_same  NA      dept     
#> Choose rows, and `formula()` of them gives their formula.
formula(terms[terms$term %in% c("ego(age)", "same(dept)", "tie(friends)"), ])
#> # An MRQAP formula
#> 
#> advice ~ ego(age) + same(dept) + tie(friends)
formula(terms[terms$effect == "reporter", ], friends)
#> # An MRQAP formula
#> 
#> friends ~ reporter(age) + reporter(tenure) + reporter(level) + 
#>     reporter(dept)
```
