#' Regression for network data
#'
#' @description
#' Fits a regression model to one (or a list of) manynet-compatible network(s)
#' using the multiple regression quadratic assignment procedure (MRQAP).
#'
#' The formula front end is the familiar
#' `y ~ ego(attr) + alter(attr) + same(attr) + diff(attr)` syntax, plus terms
#' computed on other layers of the network, such as `recip(friends)`, and plain
#' references to those layers. Its terms follow `{goldfish}`'s where the two
#' packages share them. Internally the response and predictors are packed into
#' matrices and handed to a QAP engine (ported from MrQAP) that supports:
#'
#' - gaussian, binomial, poisson, negative binomial, and zero-inflated
#'   Poisson families;
#' - two permutation schemes: `"predictor"` (Dekker's double semi-partialling)
#'   and `"outcome"`;
#' - random intercepts (lme4), and fixed effects, absorbed as in `{fixest}`;
#' - robust (HC3) standard errors;
#' - lists of networks, in which graphs that are missing any predictor are
#'   dropped with a warning and the remaining networks are pooled;
#' - cognitive social structures, detected rather than declared (see below).
#'
#' @section Cognitive social structures:
#'   A cognitive social structure (CSS) records each person's report of the
#'   whole network (Krackhardt 1987). `{manynet}` holds one as a network whose
#'   ties carry a `by` column naming the reporter, and `manynet::is_cognitive()`
#'   says whether a network is one. Where the outcome is a CSS, the model is
#'   fitted on every reported dyad: one observation for each sender, receiver,
#'   and reporter, rather than for each sender and receiver.
#'
#'   - A nodal term such as `ego(age)` is the same in every report.
#'   - A layer that is itself a CSS, such as another relation that every
#'     reporter described, enters as each reporter's own report of it.
#'   - A layer that is not a CSS, such as a formal reporting structure, enters
#'     as one matrix that every report shares.
#'   - `tertius()`, `recip()`, `indeg()`, and `outdeg()` on a CSS layer are
#'     computed within each report, since each reporter sees different ties.
#'
#'   The `reporter` terms describe the reporter of each report, since a
#'   report about one's own ties can differ from a report about the ties of
#'   others (Krackhardt 1987):
#'
#'   - `reporter(age)` is the reporter's age, the same over their whole report;
#'   - `reporter_ego` is 1 in a report about the reporter's own outgoing tie,
#'     and `reporter_alter` in a report about a tie to the reporter;
#'   - `reporter_same(dept, node = "alter")` is 1 where the reporter is of the
#'     receiver's department.
#'
#'   `reporter_ego`, `reporter_alter`, and `reporter_same()` need a directed or
#'   a two-mode network, since an undirected tie has no sender or receiver.
#'   In a two-mode CSS the column nodes report after the row nodes, and
#'   `reporter_same()` is not yet available. An attribute that only one mode
#'   holds gives `reporter()` an `NA` for every report of the other mode,
#'   so those reports are dropped.
#'   On a network that is not a CSS, a `reporter` term aborts.
#'   Goldfish and ergm have no reporter, so [specify_dynam()] and
#'   [specify_ergm()] drop these terms.
#'
#'   A permutation relabels the nodes, and a reporter is a node, so the senders,
#'   receivers, and reporters are relabelled together.
#'   `(1 | reporter)` in the formula adds a random intercept for each reporter.
#'   A `| reporter` fixed effect absorbs `reporter()`, which is then refused,
#'   but not `reporter_ego` or `reporter_alter`, which vary within a report.
#'
#'   A person who was not asked has a report of zeros unless the network marks
#'   them as missing, for example with
#'   `manynet::mutate_nodes(net, na = label %in% c(...))`.
#'   Their reports are then dropped. A report of zeros that is not so marked is
#'   kept, and named in a warning, since it may be either.
#'
#' @section Missing data:
#'   An unobserved dyad is an `NA` in the outcome or in a predictor, never a
#'   zero tie. `{manynet}` records unobserved ties, and a CSS reporter who was
#'   not asked, and `manynet::as_matrix()` turns both into `NA`.
#'   By default (`control = list(missing = "available")`), the model drops each
#'   unobserved dyad and says how many it dropped on each variable.
#'
#'   A permutation must then be fitted on the same dyads as the observed model,
#'   or the null distribution rests on more data than the estimate it is
#'   compared with. So nodes are permuted only among nodes with the same
#'   unobserved cells: a node that did not report, a node that nobody reported
#'   on, a reporter who was not asked, and a fully observed node are each
#'   exchanged only with nodes of their own kind. A node with scattered
#'   unobserved cells stays in place. This restricts the null hypothesis to
#'   exchangeability within those blocks, and with many fixed nodes it leaves
#'   few distinct permutations, which is reported with a warning.
#'
#'   `control = list(missing = "fail")` stops instead, for a user who wants no
#'   dyad dropped without their decision.
#'   Multiple imputation is not yet available.
#'
#'   A tie that `{manynet}` marks as fixed, in its `fixed` tie column, was set
#'   by the design rather than chosen, so it is no observation of the process.
#'   It is left out of the model, counted apart from the unobserved dyads, and
#'   kept in place under permutation, as an unobserved dyad is. `"fail"` does
#'   not stop on it.
#'
#'   A cell that cannot hold a tie, such as a tie between two levels of a
#'   multilevel network that the design rules out, is a structural zero rather
#'   than missing data. `{manynet}` does not yet mark such cells, so they enter
#'   the model as observed zeros. Remove them, or set them to `NA`, before
#'   fitting.
#'
#' @section Longitudinal networks:
#'   A longitudinal network, one that `manynet::is_longitudinal()` says has
#'   more than one wave, is fitted as a panel. Each wave but the first is an
#'   outcome, and the waves are pooled, so the network index is the wave.
#'   One permutation relabels the nodes the same way in every wave, so that
#'   each node keeps its own trajectory under the null hypothesis.
#'
#'   Three of goldfish's effects then read the outcome's previous wave, as
#'   goldfish and RSiena read the state of the network before each change:
#'
#'   - `inertia`, the tie from i to j in the previous wave;
#'   - `recip`, the tie from j to i in the previous wave;
#'   - `trans`, the number of two-paths i -> k -> j in the previous wave,
#'     counting only observed ties.
#'
#'   On a cross-sectional network, `inertia` and `trans` are dropped with a
#'   warning, since they could only read the outcome's own ties, and `recip`
#'   reads the outcome's reverse tie, with a warning.
#'   Every other term, including a layer such as `tie(friends)` and a node
#'   attribute, reads the same wave as the outcome; see stocnet/infernet#18.
#'   Every wave must hold the same nodes: mark a node that is absent from a wave
#'   as missing there. A list of networks is read as independent networks, not
#'   as a panel, even where it holds the waves of one network.
#'
#' @section Fixed effects:
#'   A bar outside parentheses absorbs a fixed effect for each group named after
#'   it, as in `{fixest}`: `weight ~ sim(age) | sender + receiver`.
#'   The groups are `sender`, `receiver`, `reporter` for a cognitive social
#'   structure, and `network` for a list of networks. In a pooled list each
#'   node's group is within its own network; in a panel a node keeps one group
#'   across the waves, and `network` is one effect for each wave.
#'   The intercept is one of the absorbed effects, so its estimate is `NA`.
#'
#'   A gaussian model is demeaned once by the fixed effects, and each
#'   permutation fits a plain model of the demeaned variables. A node
#'   permutation maps senders onto senders and receivers onto receivers, and
#'   every permutation fits the dyads of the observed model, so this gives the
#'   same estimates as a dummy for every group (the Frisch-Waugh-Lovell
#'   theorem), at the cost of a model without them. Its R-squared is the within
#'   R-squared. Any other family is fitted with `fixest::feglm()`, or
#'   `fixest::fenegbin()` for `"negbin"`, in every permutation, which needs
#'   `{fixest}`. That leaves out the dyads of a group whose outcome has one
#'   value, and says how many.
#'
#'   A predictor that does not vary within the groups, such as `ego(age)` with
#'   a sender effect, is absorbed by them, and aborts with its name.
#'   An undirected network has no senders or receivers, so it drops those
#'   effects with a warning, as it does those random intercepts. A two-mode
#'   network keeps both, since its rows and columns are different nodes.
#'   Fixed effects cannot yet be combined with random intercepts, robust
#'   standard errors, or the `"zip"` family.
#'
#' @section Multiplex networks:
#'   The left-hand side of the formula can name a layer of a multiplex network,
#'   and the right-hand side can name other layers as dyadic covariates,
#'   e.g. `advice ~ friends + reports` on `manynet::ison_hightech`.
#'   A network that declares a focal layer, as `info$focal`, takes it as the
#'   outcome of `. ~`, and says so. A named left-hand side takes priority.
#'   A multiplex network whose layers cannot be combined into one matrix,
#'   such as one with a CSS layer, must name its outcome layer or declare one.
#'
#'   A term built from a layer must not read the outcome's own ties, since
#'   permuting the outcome does not permute such a predictor with it.
#'   `tie()`, `indeg()`, and `outdeg()` refuse the outcome layer.
#'   `recip()`, `tertius()`, and `tertius_diff()` without a layer read the
#'   outcome, and warn.
#'
#' @param formula A formula describing the model.  The left-hand side is the
#'   dependent network: a layer, a tie attribute such as `weight`, or `.` for
#'   the network's focal layer or the whole network.
#'   The right-hand side accepts these terms, whose arguments can be given by
#'   position or by name:
#'   - `ego(attr, levels = NULL)`, `alter(attr, levels = NULL)` — the sender's
#'     or the receiver's value of a node attribute; a categorical attribute is
#'     dummy-coded, with the first category as the reference,
#'   - `same(attr, diff = FALSE, levels = NULL)` — 1 where sender and receiver
#'     share an attribute value; with `diff = TRUE`, as in ergm's
#'     `nodematch()`, one 0/1 column for each category, 1 where sender and
#'     receiver both hold it.
#'     `match(attr)` is a deprecated alias of `same(attr, diff = TRUE)`,
#'   - `diff(attr, transformer_fn = abs)` — the transformed difference between
#'     the sender's and the receiver's values: absolute by default, and signed
#'     with `transformer_fn = identity`. `dist(attr)` is a deprecated alias,
#'   - `sim(attr)` — proportional similarity in a numeric attribute,
#'     1 - |d| / range,
#'   - `mix(attr, levels = NULL)` — one 0/1 column for each pair of a
#'     sender's and a receiver's category, less the first pair as the
#'     reference. The pairs are in ergm's order for `nodemix()`, the sender's
#'     category changing fastest,
#'   - `tie(layer, weighted = FALSE)` — another layer as a dyadic covariate,
#'     0/1 unless `weighted = TRUE`; a layer or tie attribute named on its own
#'     enters with its values as stored,
#'   - `exchange(layer, weighted = NULL)` — the reverse tie, j to i, in another
#'     layer; `recip(layer)` is the same, and `recip()`, or `recip` on its own,
#'     reads the outcome's own reverse tie, and warns. `weighted = FALSE` gives
#'     0/1, and `weighted = TRUE` the values, as goldfish's `recip()` does;
#'     without it, the values are as stored,
#'   - `indeg(layer, node = "alter", weighted = FALSE)` and
#'     `outdeg(layer, node = "ego", weighted = FALSE)` — a node's number of
#'     incoming or outgoing ties in another layer, for the receiver
#'     (`"alter"`) or the sender (`"ego"`). By default `indeg()` reads the
#'     receiver and `outdeg()` the sender. `type =` is a deprecated alias of
#'     `node =`, and goldfish's name for it; goldfish reads the receiver for
#'     both, so a formula written for goldfish is read as goldfish reads it.
#'     goldfish's `indeg` or `outdeg` on its own reads the outcome's own
#'     degree, and is dropped,
#'   - `tertius(layer, attr, summarizer_fn = "mean")` — the mean (or the
#'     tie-weighted sum, with `"sum"`) of an attribute over the receiver's
#'     in-neighbours in a layer, other than the sender. A receiver with no
#'     other in-neighbour takes the mean of the defined cells. The older
#'     `tertius(attr)` reads the outcome's own ties, and warns,
#'   - `tertius_diff(layer, attr, summarizer_fn = "mean")` — how far the
#'     sender's value lies from that mean, as an absolute difference,
#'   - `global(var)` — a network-level variable from the network's `globals`,
#'     in a list of networks; on one network it is the intercept,
#'   - `reporter(attr, levels = NULL)` — the reporter's value of a node
#'     attribute in a cognitive social structure, dummy-coded as `ego()` is,
#'   - `reporter_ego` and `reporter_alter`, or `reporter_ego()` and
#'     `reporter_alter()` — 1 where the reporter is the sender or the
#'     receiver of the dyad, so a report about the reporter's own ties,
#'   - `reporter_same(attr, node = "ego")` — 1 where the reporter shares a
#'     value of an attribute with the sender (`"ego"`) or the receiver
#'     (`"alter"`).
#'
#'   The `reporter` terms are for a cognitive social structure only (see the
#'   section on cognitive social structures).
#'
#'   `:` and `*` give interactions of any order, as the product of the terms.
#'
#'   `levels =` chooses the categories that get a column in `ego()`,
#'   `alter()`, and `same()`, or the pairs in `mix()`, as in ergm. Give the
#'   names, such as `levels = c("sales", "hr")` or, for `mix()`,
#'   `levels = "sales -> hr"`, written as in `fit$terms` (`--` for an
#'   undirected network). Or give positions in the order of the columns,
#'   where a negative position leaves that one out: `levels = -1` is the
#'   default of `ego()`, `alter()`, and `mix()`. In `ego()`, `alter()`, and
#'   `mix()`, the categories left out are the reference together; where every
#'   one is chosen, the first chosen is the reference. In `same()`, a dyad
#'   that shares a category left out is not counted. An unknown name aborts
#'   and lists the categories. A term with more than ten categories and no
#'   `levels =` warns, since under `permute = "predictor"` each column is
#'   permuted on its own.
#'
#'   A formula written for `{goldfish}` or `{ergm}` can be used as it is.
#'   It is first read by [specify_mrqap()], which rewrites each term that has
#'   an MRQAP equivalent, such as ergm's `nodematch("dept")` as `same(dept)`,
#'   and drops each term that an MRQAP cannot fit, such as goldfish's
#'   `inertia` or ergm's `gwesp()`, which read the outcome's own ties.
#'   One warning names each dropped term and the reason, and the rest of the
#'   model is fitted.
#'   A name that no model knows aborts and suggests the nearest term,
#'   as does a formula with no term left to fit.
#'
#'   A bar adds a random intercept, as in `{lme4}`: `(1 | sender)`,
#'   `(1 | receiver)`, `(1 | reporter)` for a cognitive social structure, or
#'   `(1 | network)` for a list of networks. Each sets the same switch as its
#'   `random_intercept_*` control. Random slopes are not supported.
#'
#'   A bar outside parentheses absorbs fixed effects, as in `{fixest}`:
#'   `y ~ x | sender + receiver` (see the section on fixed effects).
#' @param .data A manynet-consistent network (see
#'   [manynet::as_tidygraph()]), or a list of such networks.  When a list is
#'   supplied the model is fit jointly; graphs missing any predictor are
#'   dropped with a warning.
#' @param times Integer.  Number of permutations for the null distribution.
#'   1000 is the default; publication-ready work usually needs 1000-10000.
#' @param control The controls of the model, as the result of
#'   [control_mrqap()] or a named list of its arguments, such as
#'   `list(family = "poisson", seed = 1)`. A control that is not named keeps
#'   its default. See [control_mrqap()] for each control, its default, and the
#'   names it converts from other models and earlier versions.
#' @return An object of class `netreg`, with a more specific class in front:
#'   `netreg_lm` for a gaussian model and `netreg_glm` for any other family.
#'   A cognitive social structure also puts `netreg_css` first, so
#'   `c("netreg_css", "netreg_glm", "netreg")` for a binomial CSS model.
#'   A method can be written for any of these, and falls back to the next.
#'   Among its elements:
#'   - `coefficients` and `obs`, the estimates and the observed test
#'     statistics of the model;
#'   - `lower`, `larger`, and `abs`, the share of permutations whose
#'     coefficient (first row) or test statistic (second row) is at or below,
#'     at or above, or beyond the observed one;
#'   - `testdist`, the permuted test statistics themselves, as a matrix of one
#'     row per permutation and one column per coefficient. The shares in
#'     `lower`, `larger`, and `abs` are read from it. Under
#'     `permute = "predictor"` each column comes from its own set of
#'     permutations, so rows pair columns only by position, and the intercept
#'     column is `NA`. A permutation that failed leaves its row `NA`.
#'   - `panel`, whether the network was fitted as a panel of its waves.
#'   - `missing`, a list of the treatment of unobserved dyads, the number of
#'     dyads the model could use (`possible`) and did use (`used`), the number
#'     unobserved on each variable (`unobserved`), the number of fixed ties left
#'     out (`fixed_ties`), and the number of nodes that
#'     stayed in place in each permutation set (`fixed_nodes`).
#'   - `fixed_effects`, the groups whose fixed effects were absorbed;
#'   - `pred`, the dyad-level data, with the observed values even where a
#'     gaussian model was fitted to demeaned ones;
#'   - `terms`, a tibble of one row for each coefficient, with the parts that
#'     its name joins: `term`, the coefficient's name; `effect`, the formula
#'     term it comes from, such as `"ego"` or `"same"`, or `"intercept"` or
#'     `"interaction"`; its `layer` and `attribute`; its `category` and the
#'     `reference` category it is compared with, where the term is
#'     categorical; and for an interaction, its `operands`, the names of the
#'     columns it multiplies. `tidy()` adds these columns to the estimates.
#'
#'   When the outcome is binary -- either `family = "binomial"` or
#'   `"gaussian"` with a 0/1 dependent -- a probabilistic confusion matrix is
#'   attached as `$confusion_matrix`; for the gaussian-binary case the linear
#'   probability fitted values are clamped to 0/1 before sampling.
#' @name regression
#' @family models
#' @references
#'   Krackhardt, David. 1987.
#'   "Cognitive Social Structures."
#'   _Social Networks_ 9(2): 109-134.
#'   \doi{10.1016/0378-8733(87)90009-8}.
#'
#'   Krackhardt, David. 1988.
#'   "Predicting with Networks: Nonparametric Multiple Regression Analysis
#'   of Dyadic Data."
#'   _Social Networks_ 10(4):359-81.
#'   \doi{10.1016/0378-8733(88)90004-4}.
#'
#'   Haunss, Sebastian, and James Hollway. 2023.
#'   "Multimodal mechanisms of political discourse dynamics and the case of
#'   Germany's nuclear energy phase-out."
#'   _Network Science_ 11(2): 205-223.
#'   \doi{10.1017/nws.2022.31}.
#'
#'   Dekker, David, David Krackhardt, and Tom A. B. Snijders. 2007.
#'   "Sensitivity of MRQAP tests to collinearity and autocorrelation
#'   conditions."
#'   _Psychometrika_ 72(4): 563-581.
#'   \doi{10.1007/s11336-007-9016-1}.
#' @examples
#' \dontrun{
#' networkers <- manynet::ison_networkers |>
#'   manynet::to_subgraph(Discipline == "Sociology")
#' model1 <- estimate_mrqap(
#'   weight ~ ego(Citations) + alter(Citations) + sim(Citations),
#'   networkers, times = 20)
#' print(model1)
#' }
#' # A cognitive social structure: each of 21 managers' report of who goes to
#' # whom for advice, against the formal reporting structure that all share.
#' css_model <- estimate_mrqap(advice ~ same(dept) + reports,
#'                             manynet::ison_hightech, times = 5)
#' # Whether a manager reports their own advice ties differently, and whether
#' # senior managers report more advice.
#' css_reporter <- estimate_mrqap(
#'   advice ~ reporter(tenure) + reporter_ego + reporter_alter + friends,
#'   manynet::ison_hightech, times = 5)
#' @export
estimate_mrqap <- function(formula,
                           .data,
                           times   = 1000,
                           control = list()) {

  ctrl <- .resolve_control(control)
  # A formula written for another model is read as an MRQAP reads it, and the
  # terms it cannot fit are dropped and named.
  formula <- specify_mrqap(formula, .data)
  # `(1 | sender)` in the formula sets the same switch as its control, and
  # `| sender` after a bare bar absorbs a fixed effect.
  spec <- .specify(formula)
  for (group in spec$random) {
    ctrl[[.random_groups()[[group]]]] <- TRUE
  }

  panel <- .is_panel(.data)
  if (panel || .is_list_of_graphs(.data)) {
    prepared <- if (panel) .prepare_panel(formula, .data) else
      .prepare_list_of_graphs(formula, .data)
    matlist     <- prepared$matlist
    formula     <- prepared$formula
    first_graph <- prepared$first_graph
    css         <- prepared$css
    terms       <- prepared$terms
  } else {
    ml <- convertToMatrixList(formula, .data)
    matlist     <- ml$mydata
    formula     <- ml$formula
    first_graph <- manynet::as_tidygraph(.data)
    css         <- ml$css
    terms       <- ml$terms
  }

  dep <- .dep_name(formula)

  # A cognitive social structure is detected from the network, as directedness
  # is, rather than declared: `{manynet}` records who reported each tie in a
  # `by` column, and the outcome then arrives with a slice for each reporter.
  if (css) {
    .report_css(matlist[[dep]])
  }

  # Each of these is resolved from the data rather than stated by the user, so
  # each is reported. A defaulted family or a fallback that nobody sees is a
  # model the user cannot describe in a paper.
  if (identical(ctrl$family, "auto")) {
    ctrl$family <- if (.is_binary_outcome(matlist[[dep]])) "binomial" else "gaussian"
    manynet::snet_info(
      "Treating the outcome as {.val {ctrl$family}}, from its values.")
  }
  user_requested_gaussian_binary <-
    identical(ctrl$family, "gaussian") && .is_binary_outcome(matlist[[dep]])

  if (is.null(ctrl$directed)) {
    ctrl$directed <- manynet::is_directed(first_graph)
    # `{cli}` reads a brace expression beginning with a dot as a style, so a
    # call to a dot-prefixed function has to be resolved before interpolation.
    direction <- .directed_label(ctrl$directed)
    manynet::snet_info("Reading the network as {.val {direction}}.")
  }
  if (is.null(ctrl$loops)) {
    ctrl$loops <- isTRUE(manynet::is_complex(first_graph))
  }

  fit <- QAPengine(
    formula   = formula,
    matlist   = matlist,
    css       = css,
    family    = ctrl$family,
    directed  = ctrl$directed,
    loops     = ctrl$loops,
    permute   = ctrl$permute,
    times      = times,
    seed      = ctrl$seed,
    groups    = ctrl$groups,
    strategy  = ctrl$strategy,
    ncores    = ctrl$ncores,
    random_intercept_nets     = ctrl$random_intercept_nets,
    random_intercept_sender   = ctrl$random_intercept_sender,
    random_intercept_receiver = ctrl$random_intercept_receiver,
    random_intercept_reporter = ctrl$random_intercept_reporter,
    use_robust_errors = ctrl$use_robust_errors,
    missing  = ctrl$missing,
    panel    = panel,
    fixed_effects = spec$fixed,
    less_mem = ctrl$less_mem
  )

  if (user_requested_gaussian_binary) {
    fit$confusion_matrix <- .lpm_confusion_matrix(fit, ctrl$seed)
  }
  fit$terms <- .coef_terms(names(fit$coefficients), terms)

  fit
}

#' @describeIn regression The name this function had in `{migraph}`, and in
#'   infernet before 0.4.0. It still works, and warns once that it is deprecated.
#' @export
net_regression <- function(formula,
                           .data,
                           times   = 1000,
                           control = list()) {
  if (!isTRUE(.infernet_state$net_regression_warned)) {
    .infernet_state$net_regression_warned <- TRUE
    manynet::snet_warn(
      c("{.fn net_regression} is deprecated, and is now called {.fn estimate_mrqap}.",
        i = "It takes the same arguments, and gives the same result."))
  }
  estimate_mrqap(formula, .data, times = times, control = control)
}


# ---- permutation schemes and missing data --------------------------------------

# What the permutation scheme permutes, which is the only thing that separates
# the two. "predictor" is Dekker et al's double semi-partialling: each main
# predictor is residualised against the others and that residual is permuted.
# "outcome" permutes the dependent matrix and leaves the predictors alone.
#' @keywords internal
#' @noRd
.permute_schemes <- function() c("predictor", "outcome")

#' @keywords internal
#' @noRd
.permute_label <- function(permute) {
  switch(permute,
         predictor = "each predictor's residuals (Dekker's double semi-partialling)",
         outcome   = "the outcome only")
}

# The treatments of unobserved dyads. Multiple imputation is planned, but
# pooling permutation p-values over imputations needs its own design first.
#' @keywords internal
#' @noRd
.missing_treatments <- function() c("available", "fail")

#' @keywords internal
#' @noRd
.check_missing_control <- function(missing) {
  if (identical(missing, "impute")) {
    manynet::snet_abort(
      c("{.code missing = \"impute\"} is not yet available.",
        i = "Use {.val available} to drop unobserved dyads, or {.val fail} to stop on them."))
  }
  if (!(is.character(missing) && length(missing) == 1 &&
        missing %in% .missing_treatments())) {
    choices <- .missing_treatments()
    manynet::snet_abort(
      "{.arg missing} must be one of {.val {choices}}.")
  }
  invisible(missing)
}


# ---- input dispatch --------------------------------------------------------

.is_list_of_graphs <- function(x) {
  if (!is.list(x)) return(FALSE)
  # A 'stocnet' is itself a list, of its nodes, ties, and so on, so it has to
  # be recognised as one network before it is mistaken for a list of them.
  if (inherits(x, c("igraph", "tbl_graph", "network", "data.frame", "stocnet")))
    return(FALSE)
  if (length(x) == 0) return(FALSE)
  first <- x[[1]]
  inherits(first, c("igraph", "tbl_graph", "network", "stocnet")) ||
    is.matrix(first)
}


.prepare_list_of_graphs <- function(formula, glist) {
  ml_list <- vector("list", length(glist))
  fail_idx <- integer(0)
  fail_reason <- character(0)

  for (i in seq_along(glist)) {
    res <- tryCatch(
      convertToMatrixList(formula, glist[[i]], advise = FALSE, pooled = TRUE),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(res$error)) {
      fail_idx <- c(fail_idx, i)
      fail_reason <- c(fail_reason, res$error)
      ml_list[[i]] <- NULL
    } else {
      ml_list[[i]] <- res
    }
  }

  keep <- setdiff(seq_along(glist), fail_idx)
  if (length(keep) == 0) {
    manynet::snet_abort(
      c("None of the supplied networks could be converted for this formula.",
        stats::setNames(unique(fail_reason), rep("x", length(unique(fail_reason))))))
  }

  ref_names <- names(ml_list[[keep[1]]]$mydata)
  mismatched <- integer(0)
  for (i in keep) {
    if (!setequal(names(ml_list[[i]]$mydata), ref_names)) {
      mismatched <- c(mismatched, i)
    }
  }
  dropped <- sort(unique(c(fail_idx, mismatched)))
  keep <- setdiff(seq_along(glist), dropped)

  if (length(dropped) > 0) {
    nms <- names(glist)
    dropped_labels <- if (!is.null(nms) && all(nzchar(nms[dropped]))) {
      nms[dropped]
    } else {
      paste0("[", dropped, "]")
    }
    manynet::snet_warn(
      "Dropping {length(dropped)} network{?s} missing one or more predictors: {.val {dropped_labels}}.")
  }
  if (length(keep) == 0) {
    manynet::snet_abort("All the supplied networks were dropped for missing predictors.")
  }

  kept_mls <- ml_list[keep]
  ref_names <- names(kept_mls[[1]]$mydata)

  matlist <- vector("list", length(ref_names))
  names(matlist) <- ref_names
  for (nm in ref_names) {
    matlist[[nm]] <- lapply(kept_mls, function(ml) ml$mydata[[nm]])
  }

  first_graph <- manynet::as_tidygraph(glist[[keep[1]]])
  specificationAdvice(.specify(formula), first_graph)

  # A pooled model fits one kind of observation. A dyad in a network and a dyad
  # in one person's report of a network are different observations, so a list
  # cannot mix the two.
  css <- vapply(kept_mls, function(ml) isTRUE(ml$css), logical(1))
  if (length(unique(css)) > 1) {
    manynet::snet_abort(
      c("Cannot pool cognitive social structures with networks that are not.",
        i = "Networks {.val {which(css)}} are cognitive social structures; the others are not."))
  }

  list(
    matlist     = matlist,
    formula     = kept_mls[[1]]$formula,
    terms       = kept_mls[[1]]$terms,
    first_graph = first_graph,
    css         = all(css)
  )
}


# ---- helpers ---------------------------------------------------------------

.dep_name <- function(formula) {
  v <- all.vars(formula)
  if (length(v) == 0) return(NA_character_)
  v[1]
}


.is_binary_outcome <- function(y) {
  vals <- if (is.list(y)) unlist(y, use.names = FALSE) else as.vector(y)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(FALSE)
  all(vals %in% c(0, 1))
}


.lpm_confusion_matrix <- function(fit, seed) {
  bm <- fit$simple_fit
  if (is.null(bm)) return(NULL)
  actual <- fit$pred[[fit$dep]]
  # The observed values less the residuals, rather than `fitted()`, since a
  # model with fixed effects was fitted to demeaned values.
  predicted <- actual - stats::residuals(bm)
  predicted <- pmin(pmax(predicted, 0), 1)
  probabilistic_confusion_matrix(
    actual = as.integer(actual),
    predicted_prob = predicted,
    n_draws = 1000,
    seed = seed
  )
}


# ---- print method ----------------------------------------------------------

# S3 dispatches on the most specific class, so each kind of result has its own
# method and none of them branches on the class of another. `print.netreg()` is
# the fallback for a `netreg` with no more specific class.

#' @rdname results
#' @export
print.netreg_lm <- function(x, ..., print_b = FALSE, print_random = FALSE) {
  .print_qap_regression(x, print_b = print_b, print_random = print_random)
  invisible(x)
}

#' @rdname results
#' @export
print.netreg_glm <- function(x, ..., print_b = FALSE, print_random = FALSE) {
  .print_qap_glm(x, print_b = print_b, print_random = print_random)
  invisible(x)
}

#' @rdname results
#' @export
print.netreg <- function(x, ...) {
  cat("<netreg>\n")
  utils::str(unclass(x), max.level = 1)
  invisible(x)
}


# What a result was fitted on, where that is not every cell of one network:
# the dyads that unobserved cells left, and the waves of a panel. A reader
# needs both to interpret the estimates.
#' @keywords internal
#' @noRd
.sample_lines <- function(x) {
  out <- character(0)
  m <- x$missing
  if (!is.null(m) && m$used < m$possible) {
    out <- c(out, paste("Fitted on", format(m$used), "of", format(m$possible),
                        "dyads; the others were unobserved and dropped."))
  }
  if (isTRUE(m$fixed_ties > 0)) {
    out <- c(out, paste(format(m$fixed_ties),
                        "fixed ties were left out, and kept in place under permutation."))
  }
  if (isTRUE(x$panel)) {
    waves <- length(unique(x$pred$nv))
    out <- c(out, paste0("A panel: each of ", waves, " waves was fitted against ",
                         "the wave before it,"),
             "  and each permutation relabelled the nodes in every wave alike.")
  }
  out
}

.print_qap_regression <- function(x, print_b = FALSE, print_random = FALSE) {
  if (is.null(x$random.intercepts)) {
    cat("\nOLS Network Model\n\n")
  } else {
    cat("\nLinear Mixed Network Model fit by REML\n\n")
  }

  cat(.fixed_label(x))
  if (!is.null(x$groups))
    cat("Permutations were performed within groups only.\n")
  lines <- .sample_lines(x)
  if (length(lines)) cat(paste0(lines, "\n"), sep = "")

  if (x$permute == "outcome")
    cat("The outcome matrix Y was permuted", format(x$times), "times.\n")
  if (x$permute == "predictor") {
    cat("Significance was estimated using Dekker's\n")
    cat("  'semi-partialling plus' procedure with",
        format(x$times), "permutations.\n")
  }

  if (x$loops) {
    cat("Diagonal values (loops) were used in the estimation.\n")
  } else {
    cat("Diagonal values (loops) were ignored.\n")
  }
  cat("The outcome was treated as",
      format(paste0(.directed_label(x$directed), ".")), "\n")

  if (!is.null(x$r.squared)) {
    # With fixed effects, the R-squared is that of what they leave.
    within <- if (length(x$fixed_effects) > 0) "Within " else ""
    cat(paste0("\n", within, "R-squared:    "), format(round(x$r.squared, 4)))
    cat(paste0("\n", within, "Adj R-squared:"), format(round(x$adj.r.squared, 4)), "\n")
  }

  cat("\nCoefficients:\n")
  if (print_b) {
    cmat <- cbind(format(as.numeric(x$coefficients)),
                  format(x$lower[1, ]),
                  format(x$larger[1, ]),
                  format(x$abs[1, ]))
    colnames(cmat) <- c("Estimate", "Pr(<=b)", "Pr(>=b)", "Pr(>=|b|)")
    rownames(cmat) <- names(x$coefficients)
    if (x$permute == "predictor") cmat[1, 2:4] <- "*"
    print.table(cmat)
    cat("\n--------------\n")
  }

  cmat <- cbind(format(as.numeric(x$coefficients)),
                format(x$lower[2, ]),
                format(x$larger[2, ]),
                format(x$abs[2, ]))
  colnames(cmat) <- c("Estimate", "Pr(<=t)", "Pr(>=t)", "Pr(>=|t|)")
  rownames(cmat) <- names(x$coefficients)
  if (x$permute == "predictor") cmat[1, 2:4] <- "*"
  print.table(cmat)

  if (x$permute == "predictor")
    cat("\n* The intercept has no significance test when predictors are permuted.\n")

  cat("\n--------------\n")

  if (!is.null(x$random.intercepts) && print_random) {
    cmat <- matrix(round(x$random.intercepts, 3),
                   nrow = length(x$random.intercepts), ncol = 1)
    colnames(cmat) <- "Random Intercepts:"
    print.table(cmat)
    cat("\n--------------\n")
  }

  if (!is.null(x$confusion_matrix)) {
    cat("\n")
    print(x$confusion_matrix)
  }
  cat("\n")
}


.print_qap_glm <- function(x, print_b = FALSE, print_random = FALSE) {
  if (is.null(x$random.intercepts)) {
    cat("\nGeneralized Linear Network Model\n")
  } else {
    cat("\nGeneralized Linear Mixed Network Model fit by REML\n")
  }
  if (!is.null(x$theta))
    cat("\nNegative binomial dispersion (theta):", format(round(x$theta, 4)))
  if (!is.null(x$zi_coefficients)) {
    cat("\nZero-inflation coefficients:")
    cat("\n  ", paste(names(x$zi_coefficients),
                      format(round(x$zi_coefficients, 4)),
                      sep = " = ", collapse = ", "))
  }
  if (length(x$fixed_effects) > 0)
    cat("\n", sub("\n$", "", .fixed_label(x)), sep = "")
  if (!is.null(x$groups))
    cat("\nPermutations were performed within groups only.")
  lines <- .sample_lines(x)
  if (length(lines)) cat(paste0("\n", lines), sep = "")

  if (x$permute == "outcome")
    cat("\nThe outcome matrix Y was permuted", format(x$times), "times.")
  if (x$permute == "predictor") {
    cat("\nSignificance was estimated using Dekker's")
    cat("\n  'semi-partialling plus' procedure with",
        format(x$times), "permutations.")
  }

  if (x$loops) {
    cat("\nDiagonal values (loops) were used in the estimation.")
  } else {
    cat("\nDiagonal values (loops) were ignored.")
  }
  cat("\nThe outcome was treated as",
      format(paste0(.directed_label(x$directed), ".")))
  cat("\nModel family:", format(x$family))

  cat("\n\nCoefficients:\n")
  if (print_b) {
    cmat <- matrix(NA, nrow = length(x$coefficients), ncol = 5)
    cmat[, 1] <- format(as.numeric(x$coefficients))
    cmat[, 2] <- format(exp(as.numeric(x$coefficients)))
    cmat[, 3] <- format(x$lower[1, ])
    cmat[, 4] <- format(x$larger[1, ])
    cmat[, 5] <- format(x$abs[1, ])
    if (x$permute == "predictor") cmat[1, 3:5] <- "*"
    colnames(cmat) <- c("Estimate", "Exp(b)", "Pr(<=b)", "Pr(>=b)", "Pr(>=|b|)")
    rownames(cmat) <- names(x$coefficients)
    print.table(cmat)
    cat("--------------\n")
  }

  cmat <- matrix(NA, nrow = length(x$coefficients), ncol = 5)
  cmat[, 1] <- format(as.numeric(x$coefficients))
  cmat[, 2] <- format(exp(as.numeric(x$coefficients)))
  cmat[, 3] <- format(x$lower[2, ])
  cmat[, 4] <- format(x$larger[2, ])
  cmat[, 5] <- format(x$abs[2, ])
  if (x$permute == "predictor") cmat[1, 3:5] <- "*"
  colnames(cmat) <- c("Estimate", "Exp(b)", "Pr(<=t)", "Pr(>=t)", "Pr(>=|t|)")
  rownames(cmat) <- names(x$coefficients)
  print.table(cmat)

  if (x$permute == "predictor")
    cat("\n* The intercept has no significance test when predictors are permuted.\n")

  cat("--------------\n")

  if (!is.null(x$random.intercepts) && print_random) {
    cmat <- matrix(round(x$random.intercepts, 3),
                   nrow = length(x$random.intercepts), ncol = 1)
    colnames(cmat) <- "Random Intercepts:"
    print.table(cmat)
    cat("--------------\n")
  }

  if (!is.null(x$simple_fit)) {
    cat("\nAIC:", format(stats::AIC(x$simple_fit)))
    cat("\nBIC:", format(stats::BIC(x$simple_fit)))
  }

  if (!is.null(x$confusion_matrix)) {
    cat("\n\n")
    print(x$confusion_matrix)
  }
  cat("\n")
}


# ============================================================================
# Formula -> matrix-list front end
# ============================================================================

# The formula is parsed and evaluated in `R/model_formula.R`. This keeps the
# shape the engine and the list path read: the matrices, the formula over their
# column names, and whether the outcome is a cognitive social structure.
#' @keywords internal
#' @noRd
convertToMatrixList <- function(formula, .data, advise = TRUE, pooled = FALSE,
                                previous = NULL) {
  .evaluate_spec(.specify(formula), .data, advise = advise, pooled = pooled,
                 previous = previous)
}

# A longitudinal network of more than one wave is a panel: the same nodes,
# observed again. A list of networks is not, even where it holds the waves of
# one network, since nothing then says that its nodes are the same.
#' @keywords internal
#' @noRd
.is_panel <- function(.data) {
  if (is.null(.data) || .is_list_of_graphs(.data)) return(FALSE)
  tryCatch(isTRUE(manynet::is_longitudinal(.data)) &&
             isTRUE(manynet::net_waves(.data) > 1),
           error = function(e) FALSE)
}

# A panel is fitted on every wave but the first, each against the wave before
# it, which `inertia`, `recip`, and `trans` read. The waves are pooled, so the
# network index of the dyad-level data is the wave, and `QAPengine()` draws one
# permutation of the nodes for every wave.
#' @keywords internal
#' @noRd
.prepare_panel <- function(formula, .data) {
  waves <- manynet::to_waves(.data)
  n_waves <- length(waves)
  mls <- lapply(seq(2, n_waves), function(t) {
    convertToMatrixList(formula, waves[[t]], advise = FALSE, pooled = TRUE,
                        previous = waves[[t - 1]])
  })
  if (any(vapply(mls, function(ml) isTRUE(ml$css), logical(1)))) {
    manynet::snet_abort(
      "A longitudinal cognitive social structure is not yet supported.")
  }
  dep <- names(mls[[1]]$mydata)[1]
  dims <- lapply(mls, function(ml) dim(ml$mydata[[dep]]))
  if (length(unique(dims)) > 1) {
    manynet::snet_abort(
      c("The waves of the network have different numbers of nodes.",
        i = "A panel needs the same nodes in every wave. Mark a node that is absent from a wave as missing there."))
  }
  ref_names <- names(mls[[1]]$mydata)
  matlist <- stats::setNames(lapply(ref_names, function(nm) {
    lapply(mls, function(ml) ml$mydata[[nm]])
  }), ref_names)
  first_graph <- manynet::as_tidygraph(waves[[2]])
  specificationAdvice(.specify(formula), first_graph)
  fitted <- n_waves - 1
  manynet::snet_info(
    "Reading the network as a panel of {n_waves} waves:",
    "each of the last {fitted} wave{?s} is fitted against the wave before it.")
  list(matlist     = matlist,
       formula     = mls[[1]]$formula,
       terms       = mls[[1]]$terms,
       first_graph = first_graph,
       css         = FALSE)
}

# Says what was detected, and warns about the one thing a CSS array cannot say
# for itself. `{manynet}` fills the slice of a node with zeros where that node
# reported no ties, and also where that node was never asked, unless the network
# records the second case as missing: a node whose `na` attribute is TRUE gets a
# slice of NA. A slice of zeros therefore enters the model as a report of no
# ties, which is wrong for someone who did not report.
#' @keywords internal
#' @noRd
.report_css <- function(y) {
  arrays <- if (is.list(y)) y else list(y)
  reporters <- sum(vapply(arrays, function(a) dim(a)[3], integer(1)))
  manynet::snet_info(
    "Reading the outcome as a cognitive social structure, with {reporters} reporter{?s}.")
  empty <- unlist(lapply(arrays, function(a) {
    which(apply(a, 3, function(slice) !anyNA(slice) && all(slice == 0)))
  }))
  if (length(empty) > 0) {
    labels <- names(empty)
    who <- if (!is.null(labels) && all(nzchar(labels))) labels else unname(empty)
    manynet::snet_warn(
      c("{length(empty)} reporter{?s} named no ties: {.val {who}}.",
        i = "If they were not asked, mark them so their reports are dropped rather than read as reporting no ties.",
        i = "For example: {.code manynet::mutate_nodes(net, na = label %in% c(...))}."))
  }
  invisible(NULL)
}

# The layers of a multiplex network, which a formula can name as its outcome or
# as a dyadic covariate. A network with one layer has none to name.
#' @keywords internal
#' @noRd
.layer_names <- function(net) {
  if (!isTRUE(manynet::is_multiplex(net))) return(character(0))
  out <- tryCatch(manynet::layer_names(net), error = function(e) character(0))
  as.character(out)
}

# A predictor of a CSS model needs the outcome's three axes. A nodal term, or a
# layer that is not itself a CSS, is the same for every reporter, so its matrix
# is repeated along the third axis. A CSS layer already has the axis, and only
# has to agree with the outcome.
#' @keywords internal
#' @noRd
.lift_to_css <- function(x, DV) {
  d <- dim(DV)
  if (length(dim(x)) == 3) {
    if (!identical(dim(x), d))
      manynet::snet_abort(
        c("A CSS predictor has dimensions {dim(x)}, but the outcome has {d}.",
          i = "Both must have the same senders, receivers, and reporters."))
    return(x)
  }
  if (!identical(dim(x), d[1:2]))
    manynet::snet_abort(
      "A predictor has dimensions {dim(x)}, but each report of the outcome has {d[1:2]}.")
  array(x, dim = d, dimnames = dimnames(DV))
}

# Columns the engine builds for every dyad, rather than reads off the network.
#' @keywords internal
#' @noRd
.structural_vars <- function() c("sv", "rv", "nv", "bv")


#' @keywords internal
#' @noRd
getDependentName <- function(formula) {
  dep <- list(formula[[2]])
  unlist(lapply(dep, deparse))
}
