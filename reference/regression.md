# Regression for network data

Fits a regression model to one (or a list of) manynet-compatible
network(s) using the multiple regression quadratic assignment procedure
(MRQAP).

The formula front-end is the familiar
`y ~ ego(attr) + alter(attr) + same(attr) + dist(attr) + sim(attr) + tertius(attr, fn)`
syntax, plus plain references to other networks in the object.
Internally the response and predictors are packed into matrices and
handed to a QAP engine (ported from MrQAP) that supports:

- gaussian, binomial, poisson, negbin, zero-inflated Poisson, and
  multinomial families;

- `"qap"` (Dekker's double semi-partialling plus) and `"qapy"`
  (permute-y-only) null hypotheses;

- random intercepts (lme4 / glmmTMB) and fixed effects (fixest);

- robust (HC3) standard errors;

- optional torch-based batch OLS on the GPU;

- lists of networks, in which graphs that are missing any predictor are
  dropped with a warning and the remaining networks are pooled.

## Usage

``` r
net_regression(formula, .data, times = 1000, control = list())

# S3 method for class 'net_regression'
print(x, ..., print_b = FALSE, print_random = FALSE)
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

  - plain tie-attribute names, which pull in a second network as a
    dyadic covariate.

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

  - `method`: `"qap"` (double semi-partialling plus, default) or
    `"qapy"` (permute y only).

  - `strategy`: future plan, e.g. `"sequential"` (default),
    `"multisession"`.

  - `family`: `"auto"` (default; gaussian for weighted networks,
    binomial for binary), `"gaussian"`, `"binomial"`, `"poisson"`,
    `"negbin"`, `"zip"`, or `"multinom"`.

  - `estimator`: `"standard"` (default) or `"gmm"` (binomial/poisson/
    negbin/zip).

  - `mode`: `"directed"` / `"undirected"` (default auto-detected from
    `.data`).

  - `diag`: logical, include loops (default auto-detected).

  - `seed`, `groups`, `ncores`: passed through to the engine.

  - `use_robust_errors`: HC3 standard errors.

  - `fixest_se_cluster`: cluster variable for fixest.

  - `reference`, `comparison`: multinomial / pairwise-comparison
    options.

  - `random_intercept_nets` / `_sender` / `_receiver`: lme4-style REs.

  - `less_mem`: drop the baseline model object from the return.

  - `use_gpu`: torch-based batch OLS (gaussian only).

- x:

  A `net_regression` object.

- ...:

  Additional arguments passed to the underlying print helpers.

- print_b:

  Logical; also print coefficient-based (not t-based) permutation
  p-values.

- print_random:

  Logical; print random intercepts when present.

## Value

An object of class `net_regression` inheriting from either
`QAPRegression` (gaussian) or `QAPGLM` (other families). When the
outcome is binary – either `family = "binomial"` or `"gaussian"` with a
0/1 dependent – a probabilistic confusion matrix is attached as
`$confusion_matrix`; for the gaussian-binary case the linear probability
fitted values are clamped to 0/1 before sampling.

## References

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
```
