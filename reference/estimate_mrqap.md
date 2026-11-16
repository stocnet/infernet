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

- random intercepts (lme4), and fixed effects, absorbed as in
  `{fixest}`;

- robust (HC3) standard errors;

- lists of networks, in which graphs that are missing any predictor are
  dropped with a warning and the remaining networks are pooled;

- cognitive social structures, detected rather than declared (see
  below).

## Usage

``` r
# S3 method for class 'infernetMrqapFit'
print(x, ..., print_b = FALSE, print_random = FALSE)

estimate_mrqap(
  formula,
  .data,
  times = 1000,
  control = list(),
  outputs = c("model", "confusion")
)

# S3 method for class 'infernetMrqapFit'
tidy(
  x,
  exponentiate = FALSE,
  alternative = c("two.sided", "less", "greater"),
  ...
)

# S3 method for class 'infernetMrqapFit'
summary(object, ...)

# S3 method for class 'infernetMrqapFit'
glance(x, ...)
```

## Arguments

- x, object:

  An `infernetMrqapFit`, from `estimate_mrqap()`.

- ...:

  For [`summary()`](https://rdrr.io/r/base/summary.html), passed to
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html), such as
  `alternative = "greater"`. Otherwise not used.

- print_b, print_random:

  Deprecated. `print_b = TRUE` also prints the p-values of the
  coefficients, rather than of their test statistics, and
  `print_random = TRUE` the random intercepts. Use
  [`summary()`](https://rdrr.io/r/base/summary.html), which gives both.

- formula:

  A formula describing the model. The left-hand side is the dependent
  network: a layer, a tie attribute such as `weight`, or `.` for the
  network's focal layer or the whole network. The right-hand side
  accepts these terms, whose arguments can be given by position or by
  name.
  [terms_mrqap](https://stocnet.github.io/infernet/reference/terms_mrqap.md)
  says what each one measures, with its equation, and
  [specify_terms](https://stocnet.github.io/infernet/reference/specify_terms.md)
  lists those that suit a network.

  - of node attributes: `ego(x)`, `alter(x)`, `same(x)`, `mix(x)`,
    `diff(x)`, and `sim(x)`;

  - of other layers: `tie(layer)`, or the layer named on its own,
    `exchange(layer)`, `indeg(layer)`, and `outdeg(layer)`;

  - of others' attributes: `tertius(layer, x)` and
    `tertius_diff(layer, x)`;

  - of the outcome's previous wave, on a longitudinal network:
    `inertia`, `recip`, and `trans`;

  - of a list of networks: `global(x)`;

  - of the reporter of a cognitive social structure: `reporter(x)`,
    `reporter_ego`, `reporter_alter`, and `reporter_same(x)`, for which
    see the section on cognitive social structures.

  `:` and `*` give interactions of any order, as the product of the
  terms. `match(x)` and `dist(x)` are deprecated aliases of
  `same(x, diff = TRUE)` and `diff(x)`.

  A formula written for `{goldfish}` or `{ergm}` can be used as it is.
  It is first read by
  [`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md),
  which rewrites each term that has an MRQAP equivalent, such as ergm's
  `nodematch("dept")` as `same(dept)`, and drops each term that an MRQAP
  cannot fit, such as goldfish's `inertia` or ergm's `gwesp()`, which
  read the outcome's own ties. One warning names each dropped term and
  the reason, and the rest of the model is fitted. A name that no model
  knows aborts and suggests the nearest term, as does a formula with no
  term left to fit.

  A bar adds a random intercept, as in `{lme4}`: `(1 | sender)`,
  `(1 | receiver)`, `(1 | reporter)` for a cognitive social structure,
  or `(1 | network)` for a list of networks. Each sets the same switch
  as its `random_intercept_*` control. Random slopes are not supported.

  A bar outside parentheses absorbs fixed effects, as in `{fixest}`:
  `y ~ x | sender + receiver` (see the section on fixed effects).

  In place of a formula, `formula` can also be a set of coefficients
  from
  [`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md),
  or an earlier fit. The model then fits their specification, with their
  family where `control` names none, and takes their values as initial
  values (see the section on initial values).

- .data:

  A manynet-consistent network (see
  [`manynet::as_tidygraph()`](https://stocnet.github.io/manynet/reference/coerce_graph.html)),
  or a list of such networks. When a list is supplied the model is fit
  jointly; graphs missing any predictor are dropped with a warning.

- times:

  Integer. Number of permutations for the null distribution. 1000 is the
  default; publication-ready work usually needs 1000-10000.

- control:

  The controls of the model, as the result of
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
  or a named list of its arguments, such as
  `list(family = "poisson", seed = 1)`. A control that is not named
  keeps its default, or the value set with
  [`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md).
  See
  [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
  for each control, its default, and the names it converts from other
  models and earlier versions. The result records the controls that it
  used as `control`.

- outputs:

  The optional parts of the result, as a character vector of any of:

  - `"model"`, the observed model object, as `simple_fit`, which
    [`simulate()`](https://rdrr.io/r/stats/simulate.html) and
    [`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md)
    draw from where a model has random or fixed effects, or a `"negbin"`
    or `"zip"` family;

  - `"confusion"`, the probabilistic confusion matrix of a binary
    outcome, as `confusion_matrix`;

  - `"draws"`, the coefficient of each permutation, as `draws`.

  The default, `c("model", "confusion")`, keeps what a result kept
  before infernet 0.7.0. Leave out `"model"` and `"confusion"` to save
  memory on a large network: every method still reads the baseline fit
  in `base`, and
  [`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md)
  makes the confusion matrix again.

- exponentiate:

  Logical, whether to exponentiate the estimates, e.g. to give odds
  ratios for a logistic model.

- alternative:

  Which permutation p-value to report: `"two.sided"` (the default) for
  Pr(\>=\|t\|), `"less"` for Pr(\<=t), or `"greater"` for Pr(\>=t).

## Value

An object of class `c("infernetMrqapFit", "netreg")`. `netreg` is kept
for `{autograph}` and `{migraph}` in infernet 0.7.x, and goes in 0.8.0.
The family, a cognitive social structure, and a panel are not classes,
but the elements `family`, `css`, and `panel`. It always has these
elements:

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

- `family`, the family that was fitted; `css`, whether the outcome is a
  cognitive social structure; and `panel`, whether the network was
  fitted as a panel of its waves.

- `missing`, a list of the treatment of unobserved dyads, the number of
  dyads the model could use (`possible`) and did use (`used`), the
  number unobserved on each variable (`unobserved`), the number of fixed
  ties left out (`fixed_ties`), and the number of nodes that stayed in
  place in each permutation set (`fixed_nodes`).

- `fixed_effects`, the groups whose fixed effects were absorbed;

- `pred`, the dyad-level data, with the observed values even where a
  gaussian model was fitted to demeaned ones;

- `terms`, a tibble of one row for each coefficient, with the parts that
  its name joins: `term`, the coefficient's name; `effect`, the formula
  term it comes from, such as `"ego"` or `"same"`, or `"intercept"` or
  `"interaction"`; its `layer` and `attribute`; its `category` and the
  `reference` category it is compared with, where the term is
  categorical; and for an interaction, its `operands`, the names of the
  columns it multiplies.
  [`summary()`](https://rdrr.io/r/base/summary.html) adds these columns
  to the estimates.

- `call`, `formula`, and `model_formula`: the call, the formula as
  [`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md)
  read it, and the formula of the columns that were fitted.
  [`update()`](https://rdrr.io/r/stats/update.html),
  [`formula()`](https://rdrr.io/r/stats/formula.html), and
  [`terms()`](https://rdrr.io/r/stats/terms.html) read them (see
  estimate_mrqap).

- `fixed_df`, the number of coefficients that a gaussian model absorbed
  by demeaning, or `NULL`.

- `node_names`, the names of the nodes, which name the random
  intercepts; `control`, the controls as the fit resolved them;
  `outputs`, the optional parts that it keeps; and `record`, what
  [`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md)
  made of each term. [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) read them.

- `dims` and `twomode`, the dimensions of the outcome matrix and whether
  the network is two-mode, which
  [`simulate()`](https://rdrr.io/r/stats/simulate.html) reads without
  the network; they are `NULL` for a list of networks or a panel.

- `start`, the initial values that the estimator used, or `NULL`.

Each value of `outputs` adds one element:

- `"model"` adds `simple_fit`, the observed `lm`, `glm`, or other model
  object.

- `"confusion"` adds `confusion_matrix` where the outcome is binary,
  either with `family = "binomial"` or with `"gaussian"` and a 0/1
  outcome. It is the result of
  [`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md)
  with 1000 draws and the seed of the fit. For the gaussian case the
  linear probability fitted values are clamped to 0/1 before the draws.

- `"draws"` adds `draws`, the permuted coefficients, as a matrix with
  the rows and columns of `testdist`.

[`print()`](https://rdrr.io/r/base/print.html) returns `x`, invisibly.
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`glance()`](https://generics.r-lib.org/reference/glance.html) return a
tibble; the tibble of [`summary()`](https://rdrr.io/r/base/summary.html)
has the class `infernetMrqapSummary` too, for its print.
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) gives the
columns `term`, `estimate`, `statistic`, and `p.value`.
[`summary()`](https://rdrr.io/r/base/summary.html) gives these columns,
`p.lower` and `p.upper`, a `title` that defines each effect, and the
columns of the fit's term table, `fit$terms`: the `effect` (the formula
term) each coefficient comes from, its `layer`, `attribute`, `category`,
and `reference` category, and for an interaction its `operands`. These
group the coefficients without parsing their names.
[`glance()`](https://generics.r-lib.org/reference/glance.html) gives the
fit statistics that the model keeps. For a gaussian model with fixed
effects, these are the statistics of the model with a dummy for each
group (see
[evaluate_likelihood](https://stocnet.github.io/infernet/reference/evaluate_likelihood.md)).

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

The `reporter` terms describe the reporter of each report, since a
report about one's own ties can differ from a report about the ties of
others (Krackhardt 1987):

- `reporter(age)` is the reporter's age, the same over their whole
  report;

- `reporter_ego` is 1 in a report about the reporter's own outgoing tie,
  and `reporter_alter` in a report about a tie to the reporter;

- `reporter_same(dept, node = "alter")` is 1 where the reporter is of
  the receiver's department.

`reporter_ego`, `reporter_alter`, and `reporter_same()` need a directed
or a two-mode network, since an undirected tie has no sender or
receiver. In a two-mode CSS the column nodes report after the row nodes,
and `reporter_same()` is not yet available. An attribute that only one
mode holds gives `reporter()` an `NA` for every report of the other
mode, so those reports are dropped. On a network that is not a CSS, a
`reporter` term aborts. ergm has no reporter, so
[`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify_ergm.md)
drops these terms, and so does goldfish's `specify_dynam()`.

A permutation relabels the nodes, and a reporter is a node, so the
senders, receivers, and reporters are relabelled together.
`(1 | reporter)` in the formula adds a random intercept for each
reporter. A `| reporter` fixed effect absorbs `reporter()`, which is
then refused, but not `reporter_ego` or `reporter_alter`, which vary
within a report.

A person who was not asked has a report of zeros unless the network
marks them as missing, for example with
`manynet::mutate_nodes(net, na = label %in% c(...))`. Their reports are
then dropped. A report of zeros that is not so marked is kept, and named
in a warning, since it may be either.

## Missing data

An unobserved dyad is an `NA` in the outcome or in a predictor, never a
zero tie. `{manynet}` records unobserved ties, and a CSS reporter who
was not asked, and
[`manynet::as_matrix()`](https://stocnet.github.io/manynet/reference/coerce_matrix.html)
turns both into `NA`. By default
(`control = list(missing = "available")`), the model drops each
unobserved dyad and says how many it dropped on each variable.

A permutation must then be fitted on the same dyads as the observed
model, or the null distribution rests on more data than the estimate it
is compared with. So nodes are permuted only among nodes with the same
unobserved cells: a node that did not report, a node that nobody
reported on, a reporter who was not asked, and a fully observed node are
each exchanged only with nodes of their own kind. A node with scattered
unobserved cells stays in place. This restricts the null hypothesis to
exchangeability within those blocks, and with many fixed nodes it leaves
few distinct permutations, which is reported with a warning.

`control = list(missing = "fail")` stops instead, for a user who wants
no dyad dropped without their decision. Multiple imputation is not yet
available.

A tie that `{manynet}` marks as fixed, in its `fixed` tie column, was
set by the design rather than chosen, so it is no observation of the
process. It is left out of the model, counted apart from the unobserved
dyads, and kept in place under permutation, as an unobserved dyad is.
`"fail"` does not stop on it.

A cell that cannot hold a tie, such as a tie between two levels of a
multilevel network that the design rules out, is a structural zero
rather than missing data. `{manynet}` does not yet mark such cells, so
they enter the model as observed zeros. Remove them, or set them to
`NA`, before fitting.

## Longitudinal networks

A longitudinal network, one that
[`manynet::is_longitudinal()`](https://stocnet.github.io/manynet/reference/mark_format_change.html)
says has more than one wave, is fitted as a panel. Each wave but the
first is an outcome, and the waves are pooled, so the network index is
the wave. One permutation relabels the nodes the same way in every wave,
so that each node keeps its own trajectory under the null hypothesis.

Three of goldfish's effects then read the outcome's previous wave, as
goldfish and RSiena read the state of the network before each change:

- `inertia`, the tie from i to j in the previous wave, as 0/1, or its
  value with `inertia(weighted = TRUE)`, as on a complete ranking, where
  every dyad holds a tie;

- `recip`, the tie from j to i in the previous wave;

- `trans`, the number of two-paths i -\> k -\> j in the previous wave,
  counting only observed ties.

On a cross-sectional network, `inertia` and `trans` are dropped with a
warning, since they could only read the outcome's own ties, and `recip`
reads the outcome's reverse tie, with a warning. Every other term,
including a layer such as `tie(friends)` and a node attribute, reads the
same wave as the outcome; see stocnet/infernet#18. Every wave must hold
the same nodes: mark a node that is absent from a wave as missing there.
A list of networks is read as independent networks, not as a panel, even
where it holds the waves of one network.

## Fixed effects

A bar outside parentheses absorbs a fixed effect for each group named
after it, as in `{fixest}`: `weight ~ sim(age) | sender + receiver`. The
groups are `sender`, `receiver`, `reporter` for a cognitive social
structure, and `network` for a list of networks. In a pooled list each
node's group is within its own network; in a panel a node keeps one
group across the waves, and `network` is one effect for each wave. The
intercept is one of the absorbed effects, so its estimate is `NA`.

A gaussian model is demeaned once by the fixed effects, and each
permutation fits a plain model of the demeaned variables. A node
permutation maps senders onto senders and receivers onto receivers, and
every permutation fits the dyads of the observed model, so this gives
the same estimates as a dummy for every group (the Frisch-Waugh-Lovell
theorem), at the cost of a model without them. Its R-squared is the
within R-squared. Any other family is fitted with
[`fixest::feglm()`](https://lrberge.github.io/fixest/reference/feglm.html),
or
[`fixest::fenegbin()`](https://lrberge.github.io/fixest/reference/femlm.html)
for `"negbin"`, in every permutation, which needs `{fixest}`. That
leaves out the dyads of a group whose outcome has one value, and says
how many.

A predictor that does not vary within the groups, such as `ego(age)`
with a sender effect, is absorbed by them, and aborts with its name. An
undirected network has no senders or receivers, so it drops those
effects with a warning, as it does those random intercepts. A two-mode
network keeps both, since its rows and columns are different nodes.
Fixed effects cannot yet be combined with random intercepts, robust
standard errors, or the `"zip"` family.

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

## Initial values

A set of coefficients from
[`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md),
or an earlier fit, gives the initial values of the observed model. Only
an estimator that iterates uses them:
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) for a `"binomial"` or
`"poisson"` model, and
[`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) for
`"negbin"`. A converged estimate does not depend on them, and the
permutations do not use them. A `"gaussian"` model, a `"zip"` model, and
a model with random or fixed effects do not use them, and say so, as
does a set with a value that is not stated. Each column of the model
needs one value, named as its coefficient.

## Methods

These methods report an `infernetMrqapFit` of any family, and of a
cognitive social structure or a panel.

- [`print()`](https://rdrr.io/r/base/print.html) shows the result in
  three parts: a title that says what was fitted,
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) of the
  estimates with significance marks, and a footer that says how the null
  distribution was made and the controls that differ from the default.
  At `options(snet_verbosity = "debug")`, the footer also names the
  methods that give more.

- [`summary()`](https://rdrr.io/r/base/summary.html) gives the full
  table of the coefficients: the columns of
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html), the lower
  and upper permutation p-values, a plain `title` of each effect, and
  the parts of each term. Its print leaves out `title` and the parts,
  which are too wide to read there, and adds the random intercepts of a
  mixed model,
  [`glance()`](https://generics.r-lib.org/reference/glance.html), the
  unobserved dyads of each variable, and the terms that the formula
  dropped or changed, each after a blank line.
  [`glance()`](https://generics.r-lib.org/reference/glance.html) gives
  the permutations and the number of dyads, so the footer does not say
  them again.

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`glance()`](https://generics.r-lib.org/reference/glance.html) give
  the results as a tibble, after `{broom}`: one row for each
  coefficient, and one row for the whole model. So an MRQAP result can
  sit in one table beside an ERGM or a SIENA one (see
  [other_results](https://stocnet.github.io/infernet/reference/other_results.md)).

The other methods of a fit are on the page of the stage that uses them:

- [fitted()](https://stocnet.github.io/infernet/reference/diagnose_residuals.md)
  and
  [residuals()](https://stocnet.github.io/infernet/reference/diagnose_residuals.md),
  on
  [diagnose_residuals](https://stocnet.github.io/infernet/reference/diagnose_residuals.md),
  give the input of a diagnosis.

- [nobs()](https://stocnet.github.io/infernet/reference/evaluate_likelihood.md),
  [logLik()](https://stocnet.github.io/infernet/reference/evaluate_likelihood.md),
  and
  [deviance()](https://stocnet.github.io/infernet/reference/evaluate_likelihood.md),
  on
  [evaluate_likelihood](https://stocnet.github.io/infernet/reference/evaluate_likelihood.md),
  compare models, through [`AIC()`](https://rdrr.io/r/stats/AIC.html)
  and [`BIC()`](https://rdrr.io/r/stats/AIC.html).

- [anova()](https://stocnet.github.io/infernet/reference/test_parameter.md),
  on
  [test_parameter](https://stocnet.github.io/infernet/reference/test_parameter.md),
  tests the terms that a larger of two nested fits adds.

- [predict()](https://stocnet.github.io/infernet/reference/predict_mrqap.md),
  on
  [predict_mrqap](https://stocnet.github.io/infernet/reference/predict_mrqap.md),
  gives the outcome for new values of the predictors.

- [formula()](https://stocnet.github.io/infernet/reference/predict_mrqap.md)
  and
  [terms()](https://stocnet.github.io/infernet/reference/predict_mrqap.md),
  on
  [predict_mrqap](https://stocnet.github.io/infernet/reference/predict_mrqap.md),
  give the formula that was fitted and the columns of its coefficients.

## P-values and the observed model

The p-values come from the permutations. Every other method reads the
observed model, which the permutations do not change. A permutation test
gives a p-value but no standard error, so
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) have no `std.error`
column. `p.value` is read from the permutations of the test statistic,
as [`print()`](https://rdrr.io/r/base/print.html) shows it. Each p-value
is \\(1 + b) / (B + 1)\\, where \\b\\ of the \\B\\ permutations reach
the observed statistic (Phipson and Smyth 2010), so it is never 0, and
the smallest is \\1 / (B + 1)\\. For the confusion matrix of a binary
outcome, see
[`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md).
When the predictors are permuted, the intercept has no test, and its
`p.value` is `NA`.

[`coef()`](https://rdrr.io/r/stats/coef.html) gives the estimated
coefficients as a plain named vector (see
[declare_coefs](https://stocnet.github.io/infernet/reference/declare_coefs.md)).
[`update()`](https://rdrr.io/r/stats/update.html) of `{stats}` works
without a method here: it fits the model again, with a changed formula,
`times`, or `control`, from the call that the result keeps.

## Methods left out

- [`vcov()`](https://rdrr.io/r/stats/vcov.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html), and a `std.error`
  column: a covariance, an interval, or a standard error from the
  observed model would not come from the permutations, and could be read
  as the basis of the permutation p-values.

- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
  [`as.matrix()`](https://rdrr.io/r/base/matrix.html): `x$pred` is the
  dyad-level data, and
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) give one value
  for each of its rows.

- [`weights()`](https://rdrr.io/r/stats/weights.html): a network
  regression uses no weights.

- `[`: a result has no rows to select. Use `$` or `[[` for an element.

- [`profile()`](https://rdrr.io/r/stats/profile.html): a permutation fit
  has no likelihood to profile.

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

Phipson, Belinda, and Gordon K. Smyth. 2010. "Permutation P-values
Should Never Be Zero: Calculating Exact P-values When Permutations Are
Randomly Drawn." *Statistical Applications in Genetics and Molecular
Biology* 9(1): 39.
[doi:10.2202/1544-6115.1585](https://doi.org/10.2202/1544-6115.1585) .

## See also

Other estimating functions:
[`control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md),
[`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md),
[`other_results`](https://stocnet.github.io/infernet/reference/other_results.md)

## Examples

``` r
networkers <- manynet::ison_networkers |>
  manynet::to_subgraph(Discipline == "Sociology")
model1 <- estimate_mrqap(
  weight ~ ego(Citations) + alter(Citations) + sim(Citations),
  networkers, times = 20)
print(model1)
#> # Poisson MRQAP of weight
#> 
#> # A tibble: 4 × 5
#>   term             estimate statistic p.value signif   
#>   <chr>               <dbl>     <dbl> <chr>   <noquote>
#> 1 (Intercept)      0.523        5.65  NA               
#> 2 ego_Citations   -0.0211     -18.3   0.524            
#> 3 alter_Citations  0.000359     0.310 1                
#> 4 sim_Citations    3.72        37.5   0.0952  .        
#> 
#> # Null: 20 permutations of each predictor, by double semi-partialling.
#> # Signif. codes: *** 0.001, ** 0.01, * 0.05, . 0.1
# A cognitive social structure: each of 21 managers' report of who goes to
# whom for advice, against the formal reporting structure that all share.
css_model <- estimate_mrqap(advice ~ same(dept) + reports,
                            manynet::ison_hightech, times = 5)
# Whether a manager reports their own advice ties differently, and whether
# senior managers report more advice.
css_reporter <- estimate_mrqap(
  advice ~ reporter(tenure) + reporter_ego + reporter_alter + friends,
  manynet::ison_hightech, times = 5)
# A fit, refitted from the initial values of an earlier fit
counts <- estimate_mrqap(friends ~ social + tasks, ison_algebra, times = 5,
                         control = list(family = "poisson"))
again <- estimate_mrqap(counts, ison_algebra, times = 5)
css_model <- estimate_mrqap(advice ~ same(dept) + reports,
                            manynet::ison_hightech, times = 5)
css_model
#> # Logistic MRQAP of advice
#> 
#> # A tibble: 3 × 5
#>   term        estimate statistic p.value signif   
#>   <chr>          <dbl>     <dbl> <chr>   <noquote>
#> 1 (Intercept)   -1.07     -38.5  NA               
#> 2 same_dept      0.552      9.53 0.167            
#> 3 reports        3.82      15.3  0.167            
#> 
#> # Null: 5 permutations of each predictor, by double semi-partialling.
#> # Permutations relabel senders, receivers, and reporters alike.
summary(css_model)
#> # Logistic MRQAP of advice
#> 
#> # A tibble: 3 × 7
#>   term        estimate statistic p.lower p.upper p.value signif   
#>   <chr>          <dbl>     <dbl> <chr>   <chr>   <chr>   <noquote>
#> 1 (Intercept)   -1.07     -38.5  NA      NA      NA               
#> 2 same_dept      0.552      9.53 1       0.167   0.167            
#> 3 reports        3.82      15.3  1       0.167   0.167            
#> 
#> # Fit of the observed model
#>    family logLik  AIC   BIC deviance null.deviance df.residual nobs   permute
#>  binomial  -4989 9984 10005     9978         10922        8817 8820 predictor
#>  times reporters
#>      5        21
#> 
#> # Permutations relabel senders, receivers, and reporters alike.
tidy(css_model)
#> # A tibble: 3 × 4
#>   term        estimate statistic p.value
#>   <chr>          <dbl>     <dbl>   <dbl>
#> 1 (Intercept)   -1.07     -38.5   NA    
#> 2 same_dept      0.552      9.53   0.167
#> 3 reports        3.82      15.3    0.167
glance(css_model)
#> # A tibble: 1 × 11
#>   family   logLik   AIC    BIC deviance null.deviance df.residual  nobs permute 
#>   <chr>     <dbl> <dbl>  <dbl>    <dbl>         <dbl>       <dbl> <int> <chr>   
#> 1 binomial -4989. 9984. 10005.    9978.        10922.        8817  8820 predict…
#> # ℹ 2 more variables: times <dbl>, reporters <int>
```
