# Regression for network data

Fits a regression model to one (or a list of) manynet-compatible
network(s) using the multiple regression quadratic assignment procedure
(MRQAP).

The formula front end is the familiar
`y ~ ego(attr) + alter(attr) + same(attr) + diff(attr)` syntax, plus
terms computed on other layers of the network, such as `recip(friends)`,
and plain references to those layers. Its terms follow `{goldfish}`'s
where the two packages share them. Internally the response and
predictors are packed into matrices and handed to a QAP engine (ported
from MrQAP) that supports:

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
```

## Arguments

- formula:

  A formula describing the model. The left-hand side is the dependent
  network: a layer, a tie attribute such as `weight`, or `.` for the
  network's focal layer or the whole network. The right-hand side
  accepts these terms, whose arguments can be given by position or by
  name:

  - `ego(attr)`, `alter(attr)` — the sender's or the receiver's value of
    a node attribute; a categorical attribute is dummy-coded,

  - `same(attr)` — 1 where sender and receiver share an attribute value,

  - `diff(attr, transformer_fn = abs)` — the transformed difference
    between the sender's and the receiver's values: absolute by default,
    and signed with `transformer_fn = identity`. `dist(attr)` is a
    deprecated alias,

  - `sim(attr)` — proportional similarity in a numeric attribute, 1 -
    \|d\| / range,

  - `match(attr)` — one 0/1 column for each category, 1 where sender and
    receiver both hold it,

  - `mix(attr)` — one 0/1 column for each pair of a sender's and a
    receiver's category, less the first pair as the reference,

  - `tie(layer, weighted = FALSE)` — another layer as a dyadic
    covariate, 0/1 unless `weighted = TRUE`; a layer or tie attribute
    named on its own enters with its values as stored,

  - `exchange(layer)` — the reverse tie, j to i, in another layer;
    `recip(layer)` is the same, and `recip()`, or `recip` on its own,
    reads the outcome's own reverse tie, and warns,

  - `indeg(layer, type = "alter", weighted = FALSE)` and
    `outdeg(layer, type = "alter", weighted = FALSE)` — a node's number
    of incoming or outgoing ties in another layer, for the receiver
    (`"alter"`) or the sender (`"ego"`),

  - `tertius(layer, attr, summarizer_fn = "mean")` — the mean (or the
    tie-weighted sum, with `"sum"`) of an attribute over the receiver's
    in-neighbours in a layer, other than the sender. A receiver with no
    other in-neighbour takes the mean of the defined cells. The older
    `tertius(attr)` reads the outcome's own ties, and warns,

  - `tertius_diff(layer, attr, summarizer_fn = "mean")` — how far the
    sender's value lies from that mean, as an absolute difference,

  - `global(var)` — a network-level variable from the network's
    `globals`, in a list of networks; on one network it is the
    intercept.

  `:` and `*` give interactions of any order, as the product of the
  terms.

  A bar adds a random intercept, as in `{lme4}`: `(1 | sender)`,
  `(1 | receiver)`, `(1 | reporter)` for a cognitive social structure,
  or `(1 | network)` for a list of networks. Each sets the same switch
  as its `random_intercept_*` control. Random slopes are not supported.

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

## Value

An object of class `netreg`, with a more specific class in front:
`netreg_lm` for a gaussian model and `netreg_glm` for any other family.
A cognitive social structure also puts `netreg_css` first, so
`c("netreg_css", "netreg_glm", "netreg")` for a binomial CSS model. A
method can be written for any of these, and falls back to the next.
Among its elements:

- `coefficients` and `obs`, the estimates and the observed test
  statistics of the model;

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

- `tertius()`, `recip()`, `indeg()`, and `outdeg()` on a CSS layer are
  computed within each report, since each reporter sees different ties.

A permutation relabels the nodes, and a reporter is a node, so the
senders, receivers, and reporters are relabelled together.
`(1 | reporter)` in the formula adds a random intercept for each
reporter.

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
A network that declares a focal layer, as `info$focal`, takes it as the
outcome of `. ~`, and says so. A named left-hand side takes priority. A
multiplex network whose layers cannot be combined into one matrix, such
as one with a CSS layer, must name its outcome layer or declare one.

A term built from a layer must not read the outcome's own ties, since
permuting the outcome does not permute such a predictor with it.
`tie()`, `indeg()`, and `outdeg()` refuse the outcome layer. `recip()`,
`tertius()`, and `tertius_diff()` without a layer read the outcome, and
warn.

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

Haunss, Sebastian, and James Hollway. 2023. "Multimodal mechanisms of
political discourse dynamics and the case of Germany's nuclear energy
phase-out." *Network Science* 11(2): 205-223.
[doi:10.1017/nws.2022.31](https://doi.org/10.1017/nws.2022.31) .

Dekker, David, David Krackhardt, and Tom A. B. Snijders. 2007.
"Sensitivity of MRQAP tests to collinearity and autocorrelation
conditions." *Psychometrika* 72(4): 563-581.
[doi:10.1007/s11336-007-9016-1](https://doi.org/10.1007/s11336-007-9016-1)
.

## See also

Other models:
[`print.netreg_lm()`](https://stocnet.github.io/infernet/reference/results.md),
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
