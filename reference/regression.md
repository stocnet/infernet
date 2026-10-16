# Regression for network data

Fits a regression model to one (or a list of) manynet-compatible
network(s) using the multiple regression quadratic assignment procedure
(MRQAP).

The formula front-end is the familiar
`y ~ ego(attr) + alter(attr) + same(attr) + dist(attr) + sim(attr) + tertius(attr, fn)`
syntax, plus plain references to other networks in the object.
Internally the response and predictors are packed into matrices and
handed to a QAP engine (ported from MrQAP) that supports:

- gaussian, binomial, poisson, negative binomial, and zero-inflated
  Poisson families;

- two permutation schemes: `"predictor"` (Dekker's double
  semi-partialling) and `"outcome"`;

- random intercepts (lme4);

- robust (HC3) standard errors;

- lists of networks, in which graphs that are missing any predictor are
  dropped with a warning and the remaining networks are pooled;

- cognitive social structures, detected rather than declared (see
  below).

## Usage

``` r
net_regression(formula, .data, times = 1000, control = list())

# S3 method for class 'netreg_lm'
print(x, ..., print_b = FALSE, print_random = FALSE)

# S3 method for class 'netreg_glm'
print(x, ..., print_b = FALSE, print_random = FALSE)

# S3 method for class 'netreg'
print(x, ...)

# S3 method for class 'netreg_css'
print(x, ...)
```

## Arguments

- formula:

  A formula describing the model. The left-hand side is the dependent
  network (e.g. `weight` for a weighted graph, or `.` for an unweighted
  graph). The right-hand side accepts:

  - `ego(attr)` — matrix of the sender's value of a node attribute,

  - `alter(attr)` — matrix of the receiver's value,

  - `same(attr)` — 1 where sender and receiver share an attribute value,

  - `dist(attr)` — absolute difference in a numeric attribute,

  - `sim(attr)` — proportional similarity in a numeric attribute,

  - `tertius(attr, fn)` — aggregate of an attribute across a node's
    other ties (`fn` is `"mean"` or `"sum"`),

  - plain layer or tie-attribute names, which pull in a second network
    as a dyadic covariate.

- .data:

  A manynet-consistent network (see
  [`manynet::as_tidygraph()`](https://stocnet.github.io/manynet/reference/coerce_graph.html)),
  or a list of such networks. When a list is supplied the model is fit
  jointly; graphs missing any predictor are dropped with a warning.

- times:

  Integer. Number of permutations for the null distribution. 1000 is the
  default; publication-ready work usually needs 1000-10000.

- control:

  Named list of additional controls; unspecified entries fall back to
  the defaults below.

  - `permute`: what the null distribution permutes. `"predictor"` (the
    default) residualises each main predictor against the others and
    permutes that residual, following Dekker et al. (2007). `"outcome"`
    permutes the dependent matrix and leaves the predictors alone. With
    one predictor there is nothing to residualise against, so
    `"predictor"` reduces to `"outcome"` and says so.

  - `strategy`: future plan, e.g. `"sequential"` (default),
    `"multisession"`.

  - `family`: `"auto"` (default; gaussian for weighted networks,
    binomial for binary), `"gaussian"`, `"binomial"`, `"poisson"`,
    `"negbin"`, or `"zip"`.

  - `directed`: logical, whether a tie from i to j differs from one from
    j to i. Read from `.data` unless given, and reported when read.

  - `diag`: logical, include loops (default auto-detected).

  - `seed`, `groups`, `ncores`: passed through to the engine.

  - `use_robust_errors`: HC3 standard errors.

  - `random_intercept_nets` / `_sender` / `_receiver`: lme4-style REs.

  - `random_intercept_reporter`: an lme4-style random intercept for each
    reporter of a cognitive social structure.

  - `less_mem`: drop the baseline model object from the return.

- x:

  A `netreg` object, as returned by `net_regression()`.

- ...:

  Additional arguments passed to the underlying print helpers.

- print_b:

  Logical; also print coefficient-based (not t-based) permutation
  p-values.

- print_random:

  Logical; print random intercepts when present.

## Value

An object of class `netreg`, with a more specific class in front:
`netreg_lm` for a gaussian model and `netreg_glm` for any other family.
A cognitive social structure also puts `netreg_css` first, so
`c("netreg_css", "netreg_glm", "netreg")` for a binomial CSS model. A
method can be written for any of these, and falls back to the next.
Among its elements:

- `coefficients` and `t`, the estimates and test statistics of the
  observed model;

- `lower`, `larger`, and `abs`, the share of permutations whose
  coefficient (first row) or test statistic (second row) is at or below,
  at or above, or beyond the observed one;

- `testdist`, the permuted test statistics themselves, as a matrix of
  one row per permutation and one column per coefficient. The shares in
  `lower`, `larger`, and `abs` are read from it. Under
  `permute = "predictor"` each column comes from its own set of
  permutations, so rows pair columns only by position, and the intercept
  column is `NA`. A permutation that failed leaves its row `NA`.

When the outcome is binary – either `family = "binomial"` or
`"gaussian"` with a 0/1 dependent – a probabilistic confusion matrix is
attached as `$confusion_matrix`; for the gaussian-binary case the linear
probability fitted values are clamped to 0/1 before sampling.

## Cognitive social structures

A cognitive social structure (CSS) records each person's report of the
whole network (Krackhardt 1987). `{manynet}` holds one as a network
whose ties carry a `by` column naming the reporter, and
[`manynet::is_cognitive()`](https://stocnet.github.io/manynet/reference/mark_format_cognitive.html)
says whether a network is one. Where the outcome is a CSS, the model is
fitted on every reported dyad: one observation for each sender,
receiver, and reporter, rather than for each sender and receiver.

- A nodal term such as `ego(age)` is the same in every report.

- A layer that is itself a CSS, such as another relation that every
  reporter described, enters as each reporter's own report of it.

- A layer that is not a CSS, such as a formal reporting structure,
  enters as one matrix that every report shares.

- `tertius()` is computed within each report, since each reporter sees a
  different set of a node's other ties.

A permutation relabels the nodes, and a reporter is a node, so the
senders, receivers, and reporters are relabelled together.
`control = list(random_intercept_reporter = TRUE)` adds a random
intercept for each reporter.

A person who was not asked has a report of zeros unless the network
marks them as missing, for example with
`manynet::mutate_nodes(net, na = label %in% c(...))`. Their reports are
then dropped. A report of zeros that is not so marked is kept, and named
in a warning, since it may be either.

## Multiplex networks

The left-hand side of the formula can name a layer of a multiplex
network, and the right-hand side can name other layers as dyadic
covariates, e.g. `advice ~ friends + reports` on
[`manynet::ison_hightech`](https://stocnet.github.io/manynet/reference/ison_hightech.html).
A multiplex network whose layers cannot be combined into one matrix,
such as one with a CSS layer, must name its outcome layer.

## References

Krackhardt, David. 1987. "Cognitive Social Structures." *Social
Networks* 9(2): 109-134.
[doi:10.1016/0378-8733(87)90009-8](https://doi.org/10.1016/0378-8733%2887%2990009-8)
.

Krackhardt, David. 1988. "Predicting with Networks: Nonparametric
Multiple Regression Analysis of Dyadic Data." *Social Networks*
10(4):359-81.
[doi:10.1016/0378-8733(88)90004-4](https://doi.org/10.1016/0378-8733%2888%2990004-4)
.

Dekker, David, David Krackhardt, and Tom A. B. Snijders. 2007.
"Sensitivity of MRQAP tests to collinearity and autocorrelation
conditions." *Psychometrika* 72(4): 563-581.
[doi:10.1007/s11336-007-9016-1](https://doi.org/10.1007/s11336-007-9016-1)
.

## See also

Other models:
[`tests`](https://stocnet.github.io/infernet/reference/tests.md)

## Examples

``` r
if (FALSE) { # \dontrun{
networkers <- manynet::ison_networkers |>
  manynet::to_subgraph(Discipline == "Sociology")
model1 <- net_regression(
  weight ~ ego(Citations) + alter(Citations) + sim(Citations),
  networkers, times = 20)
print(model1)
} # }
# A cognitive social structure: each of 21 managers' report of who goes to
# whom for advice, against the formal reporting structure that all share.
css_model <- net_regression(advice ~ same(dept) + reports,
                            manynet::ison_hightech, times = 5)
```
