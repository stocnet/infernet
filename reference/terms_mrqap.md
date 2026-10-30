# Terms of an MRQAP formula

The terms that the right-hand side of a formula for
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
can name, what each one measures, and the equation of the matrix it
builds. Each term builds one or more columns of the model, each a matrix
with one cell for each dyad of the network. Every term is named here as
it is written in a formula, such as `same(x)`; the name of its
coefficient is the name of its column, such as `same_dept`.

`specify_mrqap(.data = net)` lists the terms that suit a network,
written out for its layers and node attributes (see
[specify_terms](https://stocnet.github.io/infernet/reference/specify_terms.md)).
A formula written for `{ergm}` or `{goldfish}` names the terms in their
own way; the last section gives each term's equivalent there, and
[`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify.md)
rewrites a formula from one model for another.

## Notation

A cell \\x\_{ij}\\ of a term's matrix is for the dyad of sender (ego)
\\i\\ and receiver (alter) \\j\\. In a two-mode network, \\i\\ is a node
of the first mode and \\j\\ of the second. \\a_i\\ is node \\i\\'s value
of the attribute `x`, \\t\_{ij}\\ the value of the tie from \\i\\ to
\\j\\ in the layer `layer`, and \\y\_{ij}\\ the value of the tie in the
outcome. \\\mathbf{1}\[\cdot\]\\ is 1 where the condition holds and 0
otherwise. In a one-mode network, the diagonal \\i = j\\ is not a dyad,
and is left out of the model.

## Terms of node attributes

- `ego(x, levels = NULL)` is the sender's value, \$\$x\_{ij} = a_i.\$\$
  Its column is `ego_x`. A categorical attribute gives one 0/1 column
  for each category \\c\\, `ego_x_c`, with \\x\_{ij} = \mathbf{1}\[a_i =
  c\]\\, less the first category as the reference. `levels =` chooses
  the categories that get a column; the categories it leaves out are
  then the reference together.

- `alter(x, levels = NULL)` is the receiver's value, \\x\_{ij} = a_j\\,
  as `alter_x`, and is dummy-coded as `ego()` is.

- `same(x, diff = FALSE, levels = NULL)` is 1 where the sender and the
  receiver share a category, \$\$x\_{ij} = \mathbf{1}\[a_i = a_j\],\$\$
  as `same_x`. With `levels =`, only a category in \\L\\ counts:
  \\x\_{ij} = \mathbf{1}\[a_i = a_j \in L\]\\. With `diff = TRUE`, as in
  ergm's `nodematch()`, it gives one column for each category,
  `same_x_c`, with \\x\_{ij} = \mathbf{1}\[a_i = a_j = c\]\\; a dyad of
  two different categories is in none of them, and is the reference. In
  a two-mode network, no tie joins two nodes of one mode, so `same()` is
  the share of the sender's other ties in the outcome that go to nodes
  like the receiver, where the attribute is only on the second mode,
  \$\$x\_{ij} = \frac{\sum\_{k \neq j} y\_{ik} \mathbf{1}\[a_k =
  a_j\]}{\sum\_{k \neq j} y\_{ik}},\$\$ and otherwise the share of the
  receiver's other ties that come from nodes like the sender,
  \$\$x\_{ij} = \frac{\sum\_{k \neq i} y\_{kj} \mathbf{1}\[a_k =
  a_i\]}{\sum\_{k \neq i} y\_{kj}},\$\$ or 0 where there is no other
  tie. This reads the outcome's ties.

- `mix(x, levels = NULL)` gives one 0/1 column for each pair of a
  sender's category \\c\\ and a receiver's category \\d\\, `mix_x_c_d`,
  with \$\$x\_{ij} = \mathbf{1}\[a_i = c,\\ a_j = d\],\$\$ less the
  first pair as the reference. In an undirected network the pair is
  unordered, and \\x\_{ij}\\ is also 1 where \\a_i = d\\ and \\a_j =
  c\\. The pairs are in ergm's order for `nodemix()`, the sender's
  category changing fastest, and `levels =` names a pair as `"c -> d"`,
  or `"c -- d"` where it is unordered.

- `diff(x, transformer_fn = abs)` is the transformed difference between
  the sender's and the receiver's values, \$\$x\_{ij} = f(a_i -
  a_j),\$\$ as `diff_x`: the absolute difference \\\|a_i - a_j\|\\ by
  default, and the signed difference with `transformer_fn = identity`.

- `sim(x)` is the similarity of the two values, as a share of the
  largest difference between any sender and receiver, \$\$x\_{ij} = 1 -
  \frac{\|a_i - a_j\|}{\max\_{kl} \|a_k - a_l\|},\$\$ as `sim_x`. In a
  one-mode network, the largest difference is the range of the
  attribute.

`same()`, `same(diff = TRUE)`, and `mix()` of one attribute measure its
homophily in different ways, as
[`diff()`](https://rdrr.io/r/base/diff.html) and `sim()` do, so a model
takes one of each. [`diff()`](https://rdrr.io/r/base/diff.html) and
`sim()` need a numeric attribute. `match(x)` is a deprecated alias of
`same(x, diff = TRUE)`, and `dist(x)` of `diff(x)`.

## Terms of other layers

These read another layer of a multiplex network, or a tie attribute, and
refuse the outcome's own layer, since they would predict the outcome's
ties from those same ties.

- `tie(layer, weighted = FALSE)` is 1 where the layer has a tie,
  \$\$x\_{ij} = \mathbf{1}\[t\_{ij} \> 0\],\$\$ or its value, \\x\_{ij}
  = t\_{ij}\\, with `weighted = TRUE`, as `tie_layer`. A layer named on
  its own enters with its values as stored, as `layer`.

- `exchange(layer, weighted = NULL)` is the reverse tie, \$\$x\_{ij} =
  t\_{ji},\$\$ as `exchange_layer`, with its values as stored;
  \\\mathbf{1}\[t\_{ji} \> 0\]\\ with `weighted = FALSE`. `recip(layer)`
  is the same, as `recip_layer`.

- `indeg(layer, node = "alter", weighted = FALSE)` is the receiver's
  number of incoming ties, \\x\_{ij} = \sum\_{k} w\_{kj}\\, as
  `indeg_layer`, or the sender's with `node = "ego"`, \\x\_{ij} =
  \sum\_{k} w\_{ki}\\, as `ego_indeg_layer`.

- `outdeg(layer, node = "ego", weighted = FALSE)` is the sender's number
  of outgoing ties, \\x\_{ij} = \sum\_{k} w\_{ik}\\, as
  `ego_outdeg_layer`, or the receiver's with `node = "alter"`, \\x\_{ij}
  = \sum\_{k} w\_{jk}\\, as `outdeg_layer`.

In the degrees, \\w\_{kl} = \mathbf{1}\[t\_{kl} \> 0\]\\, or \\t\_{kl}\\
with `weighted = TRUE`; a loop \\k = l\\ and an unobserved tie are not
counted. In a two-mode network only the second mode receives and only
the first sends, so `indeg(node = "ego")` and `outdeg(node = "alter")`
abort. `type =` is a deprecated alias of `node =`, and goldfish's name
for it. goldfish reads the receiver for both, so a formula written for
goldfish is read as goldfish reads it, and goldfish's `indeg` or
`outdeg` on its own, the outcome's own degree, is dropped.

## Terms of others' attributes

- `tertius(layer, x, summarizer_fn = "mean")` is the mean value of the
  receiver's other in-neighbours in the layer: the nodes \\k\\, other
  than the sender, with a tie to the receiver, \$\$x\_{ij} =
  \frac{1}{\|N\_{ij}\|} \sum\_{k \in N\_{ij}} a_k, \quad N\_{ij} = \\k
  \neq i : t\_{kj} \> 0\\,\$\$ as `tertius_layer_x` (Haunss and Hollway
  2023). A node \\k\\ with no value is left out of \\N\_{ij}\\, and
  where \\N\_{ij}\\ is empty, \\x\_{ij}\\ is the mean of the other
  cells. With `summarizer_fn = "sum"`, it is the sum weighted by the
  tie's value, \\x\_{ij} = \sum\_{k \neq i} t\_{kj} a_k\\.

- `tertius_diff(layer, x, summarizer_fn = "mean")` is how far the
  sender's value lies from that mean, \$\$x\_{ij} = \left\|a_i -
  \frac{1}{\|N\_{ij}\|} \sum\_{k \in N\_{ij}} a_k\right\|,\$\$ as
  `tertius_diff_layer_x`, and is filled in the same way.

Both need a numeric attribute. `tertius(x)` without a layer reads the
outcome's own ties, and warns.

## Terms of the outcome's previous wave

On a longitudinal network, these read the outcome in the wave before,
\\y^{(w-1)}\\, as goldfish and RSiena read the network before each
change. On a network of one wave, they could only read the outcome's own
ties, so `inertia` and `trans` are dropped, and `recip` warns.

- `inertia` is 1 where the tie was there in the wave before, \$\$x\_{ij}
  = \mathbf{1}\[y^{(w-1)}\_{ij} \> 0\],\$\$ or its value with
  `inertia(weighted = TRUE)`.

- `recip` is the reverse tie in the wave before, \\x\_{ij} =
  y^{(w-1)}\_{ji}\\.

- `trans` is the number of two-paths from the sender to the receiver in
  the wave before, \$\$x\_{ij} = \sum\_{k} b\_{ik} b\_{kj}, \quad
  b\_{kl} = \mathbf{1}\[y^{(w-1)}\_{kl} \> 0\],\$\$ where an unobserved
  tie counts as 0.

## Terms of a list of networks

- `global(x)` is a variable \\g_m\\ of the whole network \\m\\, from its
  `globals`, for every dyad of that network: \\x\_{ij} = g_m\\, as
  `global_x`. On one network it is the intercept, so it needs a list of
  networks.

## Terms of the reporter of a cognitive social structure

A cognitive social structure (CSS) holds each reporter \\r\\'s report of
the network, so each cell is \\x\_{ijr}\\, for the dyad as \\r\\ reports
it (Krackhardt 1987).

- `reporter(x, levels = NULL)` is the reporter's value, \\x\_{ijr} =
  a_r\\, as `reporter_x`, and is dummy-coded as `ego()` is.

- `reporter_ego` is 1 where the reporter is the sender, \\x\_{ijr} =
  \mathbf{1}\[r = i\]\\, and `reporter_alter` where the reporter is the
  receiver, \\x\_{ijr} = \mathbf{1}\[r = j\]\\: a report of the
  reporter's own ties.

- `reporter_same(x, node = "ego")` is 1 where the reporter shares the
  sender's category, \\x\_{ijr} = \mathbf{1}\[a_r = a_i\]\\, as
  `reporter_same_ego_x`, or the receiver's with `node = "alter"`,
  \\x\_{ijr} = \mathbf{1}\[a_r = a_j\]\\.

See the section on cognitive social structures in
[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md).

## Choosing categories with levels =

`levels =` chooses the categories that get a column in `ego()`,
`alter()`, and `same()`, or the pairs in `mix()`, as in ergm. Give the
names, such as `levels = c("sales", "hr")` or, for `mix()`,
`levels = "sales -> hr"`, written as in `fit$terms` (`--` for an
undirected network). Or give positions in the order of the columns,
where a negative position leaves that one out: `levels = -1` is the
default of `ego()`, `alter()`, and `mix()`. In `ego()`, `alter()`, and
`mix()`, the categories left out are the reference together; where every
one is chosen, the first chosen is the reference. In `same()`, a dyad
that shares a category left out is not counted. An unknown name aborts
and lists the categories. A term with more than ten categories and no
`levels =` warns, since under `permute = "predictor"` each column is
permuted on its own.

## Interactions

`:` and `*` give interactions of any order. An interaction is the
product of its terms' cells, \\x\_{ij} = x^{(1)}\_{ij} x^{(2)}\_{ij}\\,
with one column for each pair of their columns, named by joining their
names with `:`, such as `ego_age:same_office`.

## Equivalents in other models

A formula written for `{ergm}` or `{goldfish}` can be used as it is, and
[`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify.md)
and
[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify.md)
rewrite a formula for each other's model. The nearest term of each is:

|  |  |  |  |
|----|----|----|----|
| infernet | ergm | goldfish | RSiena |
| `ego(x)` | `nodeocov("x")`, `nodeofactor("x")`; `b1cov("x")` in a two-mode network; `nodecov("x")` in an undirected one | `ego(x)` | `egoX` |
| `alter(x)` | `nodeicov("x")`, `nodeifactor("x")`; `b2cov("x")` in a two-mode network | `alter(x)` | `altX` |
| `same(x)` | `nodematch("x")` | `same(x)` | `sameX` |
| `same(x, diff = TRUE)` | `nodematch("x", diff = TRUE)` |  |  |
| `mix(x)` | `nodemix("x")` |  |  |
| `ego(x, levels = )`, `same(x, levels = )` | `nodeofactor("x", levels = )`, `nodematch("x", levels = )` |  |  |
| `mix(x, levels = )` | `nodemix("x", levels2 = )` |  |  |
| `diff(x)` | `absdiff("x")` | `diff(x)` | `absDiffX` |
| `diff(x, transformer_fn = identity)` | `diff("x")` | `diff(x, transformer_fn = identity)` | `diffX` |
| `sim(x)` | `edgecov()` | `sim(x)`, as minus the absolute difference | `simX`, centred |
| a layer, or `tie(layer)` | `edgecov()` | `tie(layer)` | `X` |
| `exchange(layer)` | `edgecov()` of the transpose | `recip(layer)` |  |
| `indeg(layer)`, `outdeg(layer)` | `edgecov()` | `indeg(layer)`, `outdeg(layer, type = "ego")` |  |
| `tertius(layer, x)`, `tertius_diff(layer, x)` | `edgecov()` | the same, with the sender among the in-neighbours |  |
| `global(x)` |  | `global(x)` |  |
| `recip` | `mutual` | `recip` | `recip` |
| `inertia` |  | `inertia` |  |
| `trans` | `ttriple` | `trans` | `transTrip` |

`sim()` is on another scale in goldfish, which changes its coefficient
but not its test, and goldfish's `tertius()` counts the sender among the
in-neighbours, so
[`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify.md)
names both where it reads a goldfish formula. ergm's `edges` is the
intercept, which an MRQAP always has. The reporter terms have no
equivalent, since no other model has a reporter. ergm's terms of the
outcome's own structure, such as `gwesp()`, and goldfish's endogenous
effects, such as `cycle`, have no MRQAP term, and an MRQAP drops them
(see
[specify](https://stocnet.github.io/infernet/reference/specify.md)).

## References

Haunss, Sebastian, and James Hollway. 2023. "Multimodal mechanisms of
political discourse dynamics and the case of Germany's nuclear energy
phase-out." *Network Science* 11(2): 205-223.
[doi:10.1017/nws.2022.31](https://doi.org/10.1017/nws.2022.31) .

Krackhardt, David. 1987. "Cognitive Social Structures." *Social
Networks* 9(2): 109-134.
[doi:10.1016/0378-8733(87)90009-8](https://doi.org/10.1016/0378-8733%2887%2990009-8)
.

Morris, Martina, Mark S. Handcock, and David R. Hunter. 2008.
"Specification of Exponential-Family Random Graph Models: Terms and
Computational Aspects." *Journal of Statistical Software* 24(4): 1-24.
[doi:10.18637/jss.v024.i04](https://doi.org/10.18637/jss.v024.i04) .

## See also

[`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
fits a formula of these terms;
[specify_terms](https://stocnet.github.io/infernet/reference/specify_terms.md)
lists the terms that suit a network.

Other models:
[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md),
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md),
[`other_results`](https://stocnet.github.io/infernet/reference/other_results.md),
[`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md),
[`print.netreg_lm()`](https://stocnet.github.io/infernet/reference/netreg_results.md),
[`regression`](https://stocnet.github.io/infernet/reference/regression.md),
[`specify`](https://stocnet.github.io/infernet/reference/specify.md),
[`specify_terms`](https://stocnet.github.io/infernet/reference/specify_terms.md),
[`test_fit`](https://stocnet.github.io/infernet/reference/test_fit.md),
[`test_measures`](https://stocnet.github.io/infernet/reference/test_measures.md),
[`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)

## Examples

``` r
# Every term that suits a network, and the columns of some of them
specify_mrqap(advice ~ ., manynet::ison_lawfirm)
#> 55 terms for an MRQAP of advice
#> # A tibble: 55 × 4
#>    term                                       effect       layer   attribute
#>    <chr>                                      <chr>        <chr>   <chr>    
#>  1 ego(status)                                ego          NA      status   
#>  2 alter(status)                              alter        NA      status   
#>  3 same(status)                               same         NA      status   
#>  4 same(status, diff = TRUE)                  same         NA      status   
#>  5 mix(status)                                mix          NA      status   
#>  6 ego(gender)                                ego          NA      gender   
#>  7 alter(gender)                              alter        NA      gender   
#>  8 same(gender)                               same         NA      gender   
#>  9 same(gender, diff = TRUE)                  same         NA      gender   
#> 10 mix(gender)                                mix          NA      gender   
#> 11 ego(office)                                ego          NA      office   
#> 12 alter(office)                              alter        NA      office   
#> 13 same(office)                               same         NA      office   
#> 14 same(office, diff = TRUE)                  same         NA      office   
#> 15 mix(office)                                mix          NA      office   
#> 16 ego(seniority)                             ego          NA      seniority
#> 17 alter(seniority)                           alter        NA      seniority
#> 18 diff(seniority)                            diff         NA      seniority
#> 19 diff(seniority, transformer_fn = identity) diff         NA      seniority
#> 20 sim(seniority)                             sim          NA      seniority
#> 21 ego(age)                                   ego          NA      age      
#> 22 alter(age)                                 alter        NA      age      
#> 23 diff(age)                                  diff         NA      age      
#> 24 diff(age, transformer_fn = identity)       diff         NA      age      
#> 25 sim(age)                                   sim          NA      age      
#> 26 ego(practice)                              ego          NA      practice 
#> 27 alter(practice)                            alter        NA      practice 
#> 28 same(practice)                             same         NA      practice 
#> 29 same(practice, diff = TRUE)                same         NA      practice 
#> 30 mix(practice)                              mix          NA      practice 
#> 31 ego(school)                                ego          NA      school   
#> 32 alter(school)                              alter        NA      school   
#> 33 same(school)                               same         NA      school   
#> 34 same(school, diff = TRUE)                  same         NA      school   
#> 35 mix(school)                                mix          NA      school   
#> 36 tie(friends)                               tie          friends NA       
#> 37 exchange(friends)                          exchange     friends NA       
#> 38 indeg(friends)                             indeg        friends NA       
#> 39 indeg(friends, node = 'ego')               indeg        friends NA       
#> 40 outdeg(friends)                            outdeg       friends NA       
#> 41 outdeg(friends, node = 'alter')            outdeg       friends NA       
#> 42 tertius(friends, seniority)                tertius      friends seniority
#> 43 tertius_diff(friends, seniority)           tertius_diff friends seniority
#> 44 tertius(friends, age)                      tertius      friends age      
#> 45 tertius_diff(friends, age)                 tertius_diff friends age      
#> 46 tie(cowork)                                tie          cowork  NA       
#> 47 exchange(cowork)                           exchange     cowork  NA       
#> 48 indeg(cowork)                              indeg        cowork  NA       
#> 49 indeg(cowork, node = 'ego')                indeg        cowork  NA       
#> 50 outdeg(cowork)                             outdeg       cowork  NA       
#> 51 outdeg(cowork, node = 'alter')             outdeg       cowork  NA       
#> 52 tertius(cowork, seniority)                 tertius      cowork  seniority
#> 53 tertius_diff(cowork, seniority)            tertius_diff cowork  seniority
#> 54 tertius(cowork, age)                       tertius      cowork  age      
#> 55 tertius_diff(cowork, age)                  tertius_diff cowork  age      
#> Choose rows, and `formula()` of them gives their formula.
tidy(specify_mrqap(advice ~ ego(age) + same(office) + sim(age)),
     manynet::ison_lawfirm)
#> # A tibble: 3 × 5
#>   term         status as           reason columns  
#>   <chr>        <chr>  <chr>        <chr>  <list>   
#> 1 ego(age)     fits   ego(age)     NA     <chr [1]>
#> 2 same(office) fits   same(office) NA     <chr [1]>
#> 3 sim(age)     fits   sim(age)     NA     <chr [1]>
```
