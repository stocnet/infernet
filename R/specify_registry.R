# The term registry ------------------------------------------------------------
#
# Each term is one entry in `.formula_terms()`: its arguments, what it reads,
# what data it accepts, how to build it, and its nearest term in each other
# model. A new term is a new entry rather than a new branch, and the list of
# term names lives in one place. The design follows `register_effect()` on
# goldfish's `refactor/rename-functions` branch. Each entry's `build` is in
# specify_builders.R, and the help page of the terms, `terms_mrqap`, is below.

#' Terms of an MRQAP formula
#'
#' @description
#' The terms that the right-hand side of a formula for [estimate_mrqap()] can
#' name, what each one measures, and the equation of the matrix it builds.
#' Each term builds one or more columns of the model, each a matrix with one
#' cell for each dyad of the network.
#' Every term is named here as it is written in a formula, such as `same(x)`;
#' the name of its coefficient is the name of its column, such as `same_dept`.
#'
#' `specify_mrqap(.data = net)` lists the terms that suit a network, written
#' out for its layers and node attributes (see [specify_terms]).
#' A formula written for `{ergm}` or `{goldfish}` names the terms in their
#' own way; the last section gives each term's equivalent there, and
#' [specify_mrqap()] rewrites a formula from one model for another.
#'
#' @section Notation:
#'   A cell \eqn{x_{ij}} of a term's matrix is for the dyad of sender (ego)
#'   \eqn{i} and receiver (alter) \eqn{j}. In a two-mode network, \eqn{i} is a
#'   node of the first mode and \eqn{j} of the second.
#'   \eqn{a_i} is node \eqn{i}'s value of the attribute `x`,
#'   \eqn{t_{ij}} the value of the tie from \eqn{i} to \eqn{j} in the layer
#'   `layer`, and \eqn{y_{ij}} the value of the tie in the outcome.
#'   \eqn{\mathbf{1}[\cdot]}{1[.]} is 1 where the condition holds and 0
#'   otherwise. In a one-mode network, the diagonal \eqn{i = j} is not a
#'   dyad, and is left out of the model.
#'
#' @section Terms of node attributes:
#'   - `ego(x, levels = NULL)` is the sender's value,
#'     \deqn{x_{ij} = a_i.}{x_ij = a_i.}
#'     Its column is `ego_x`.
#'     A categorical attribute gives one 0/1 column for each category
#'     \eqn{c}, `ego_x_c`, with \eqn{x_{ij} = \mathbf{1}[a_i = c]}{x_ij = 1[a_i = c]},
#'     less the first category as the reference. `levels =` chooses the
#'     categories that get a column; the categories it leaves out are then
#'     the reference together.
#'   - `alter(x, levels = NULL)` is the receiver's value,
#'     \eqn{x_{ij} = a_j}{x_ij = a_j}, as `alter_x`, and is dummy-coded as
#'     `ego()` is.
#'   - `same(x, diff = FALSE, levels = NULL)` is 1 where the sender and the
#'     receiver share a category,
#'     \deqn{x_{ij} = \mathbf{1}[a_i = a_j],}{x_ij = 1[a_i = a_j],}
#'     as `same_x`. With `levels =`, only a category in \eqn{L} counts:
#'     \eqn{x_{ij} = \mathbf{1}[a_i = a_j \in L]}{x_ij = 1[a_i = a_j in L]}.
#'     With `diff = TRUE`, as in ergm's `nodematch()`, it gives one column for
#'     each category, `same_x_c`, with
#'     \eqn{x_{ij} = \mathbf{1}[a_i = a_j = c]}{x_ij = 1[a_i = a_j = c]};
#'     a dyad of two different categories is in none of them, and is the
#'     reference.
#'     In a two-mode network, no tie joins two nodes of one mode, so `same()`
#'     is the share of the sender's other ties in the outcome that go to
#'     nodes like the receiver, where the attribute is only on the second
#'     mode,
#'     \deqn{x_{ij} = \frac{\sum_{k \neq j} y_{ik} \mathbf{1}[a_k = a_j]}{\sum_{k \neq j} y_{ik}},}{x_ij = sum_(k != j) y_ik 1[a_k = a_j] / sum_(k != j) y_ik,}
#'     and otherwise the share of the receiver's other ties that come from
#'     nodes like the sender,
#'     \deqn{x_{ij} = \frac{\sum_{k \neq i} y_{kj} \mathbf{1}[a_k = a_i]}{\sum_{k \neq i} y_{kj}},}{x_ij = sum_(k != i) y_kj 1[a_k = a_i] / sum_(k != i) y_kj,}
#'     or 0 where there is no other tie. This reads the outcome's ties.
#'   - `mix(x, levels = NULL)` gives one 0/1 column for each pair of a
#'     sender's category \eqn{c} and a receiver's category \eqn{d},
#'     `mix_x_c_d`, with
#'     \deqn{x_{ij} = \mathbf{1}[a_i = c,\ a_j = d],}{x_ij = 1[a_i = c, a_j = d],}
#'     less the first pair as the reference. In an undirected network the pair
#'     is unordered, and \eqn{x_{ij}} is also 1 where \eqn{a_i = d} and
#'     \eqn{a_j = c}. The pairs are in ergm's order for `nodemix()`, the
#'     sender's category changing fastest, and `levels =` names a pair as
#'     `"c -> d"`, or `"c -- d"` where it is unordered.
#'   - `diff(x, transformer_fn = abs)` is the transformed difference between
#'     the sender's and the receiver's values,
#'     \deqn{x_{ij} = f(a_i - a_j),}{x_ij = f(a_i - a_j),}
#'     as `diff_x`: the absolute difference \eqn{|a_i - a_j|} by default, and
#'     the signed difference with `transformer_fn = identity`.
#'   - `sim(x)` is the similarity of the two values, as a share of the largest
#'     difference between any sender and receiver,
#'     \deqn{x_{ij} = 1 - \frac{|a_i - a_j|}{\max_{kl} |a_k - a_l|},}{x_ij = 1 - |a_i - a_j| / max_kl |a_k - a_l|,}
#'     as `sim_x`. In a one-mode network, the largest difference is the range
#'     of the attribute.
#'
#'   `same()`, `same(diff = TRUE)`, and `mix()` of one attribute measure its
#'   homophily in different ways, as `diff()` and `sim()` do, so a model takes
#'   one of each. `diff()` and `sim()` need a numeric attribute.
#'   `match(x)` is a deprecated alias of `same(x, diff = TRUE)`, and `dist(x)`
#'   of `diff(x)`.
#'
#' @section Terms of other layers:
#'   These read another layer of a multiplex network, or a tie attribute, and
#'   refuse the outcome's own layer, since they would predict the outcome's
#'   ties from those same ties.
#'   - `tie(layer, weighted = FALSE)` is 1 where the layer has a tie,
#'     \deqn{x_{ij} = \mathbf{1}[t_{ij} > 0],}{x_ij = 1[t_ij > 0],}
#'     or its value, \eqn{x_{ij} = t_{ij}}{x_ij = t_ij}, with
#'     `weighted = TRUE`, as `tie_layer`. A layer named on its own enters with
#'     its values as stored, as `layer`.
#'   - `exchange(layer, weighted = NULL)` is the reverse tie,
#'     \deqn{x_{ij} = t_{ji},}{x_ij = t_ji,}
#'     as `exchange_layer`, with its values as stored;
#'     \eqn{\mathbf{1}[t_{ji} > 0]}{1[t_ji > 0]} with `weighted = FALSE`.
#'     `recip(layer)` is the same, as `recip_layer`.
#'   - `indeg(layer, node = "alter", weighted = FALSE)` is the receiver's
#'     number of incoming ties, \eqn{x_{ij} = \sum_{k} w_{kj}}{x_ij = sum_k w_kj},
#'     as `indeg_layer`, or the sender's with `node = "ego"`,
#'     \eqn{x_{ij} = \sum_{k} w_{ki}}{x_ij = sum_k w_ki}, as `ego_indeg_layer`.
#'   - `outdeg(layer, node = "ego", weighted = FALSE)` is the sender's number
#'     of outgoing ties, \eqn{x_{ij} = \sum_{k} w_{ik}}{x_ij = sum_k w_ik}, as
#'     `ego_outdeg_layer`, or the receiver's with `node = "alter"`,
#'     \eqn{x_{ij} = \sum_{k} w_{jk}}{x_ij = sum_k w_jk}, as `outdeg_layer`.
#'
#'   In the degrees, \eqn{w_{kl} = \mathbf{1}[t_{kl} > 0]}{w_kl = 1[t_kl > 0]},
#'   or \eqn{t_{kl}} with `weighted = TRUE`; a loop \eqn{k = l} and an
#'   unobserved tie are not counted. In a two-mode network only the second
#'   mode receives and only the first sends, so `indeg(node = "ego")` and
#'   `outdeg(node = "alter")` abort. `type =` is a deprecated alias of
#'   `node =`, and goldfish's name for it. goldfish reads the receiver for
#'   both, so a formula written for goldfish is read as goldfish reads it, and
#'   goldfish's `indeg` or `outdeg` on its own, the outcome's own degree, is
#'   dropped.
#'
#' @section Terms of others' attributes:
#'   - `tertius(layer, x, summarizer_fn = "mean")` is the mean value of the
#'     receiver's other in-neighbours in the layer: the nodes \eqn{k},
#'     other than the sender, with a tie to the receiver,
#'     \deqn{x_{ij} = \frac{1}{|N_{ij}|} \sum_{k \in N_{ij}} a_k, \quad N_{ij} = \{k \neq i : t_{kj} > 0\},}{x_ij = mean of a_k over N_ij = {k != i : t_kj > 0},}
#'     as `tertius_layer_x` (Haunss and Hollway 2023). A node \eqn{k} with
#'     no value is left out of \eqn{N_{ij}}, and where \eqn{N_{ij}} is empty,
#'     \eqn{x_{ij}} is the mean of the other cells. With
#'     `summarizer_fn = "sum"`, it is the sum weighted by the tie's value,
#'     \eqn{x_{ij} = \sum_{k \neq i} t_{kj} a_k}{x_ij = sum_(k != i) t_kj a_k}.
#'   - `tertius_diff(layer, x, summarizer_fn = "mean")` is how far the
#'     sender's value lies from that mean,
#'     \deqn{x_{ij} = \left|a_i - \frac{1}{|N_{ij}|} \sum_{k \in N_{ij}} a_k\right|,}{x_ij = |a_i - mean of a_k over N_ij|,}
#'     as `tertius_diff_layer_x`, and is filled in the same way.
#'
#'   Both need a numeric attribute. `tertius(x)` without a layer reads the
#'   outcome's own ties, and warns.
#'
#' @section Terms of the outcome's previous wave:
#'   On a longitudinal network, these read the outcome in the wave before,
#'   \eqn{y^{(w-1)}}, as goldfish and RSiena read the network before each
#'   change. On a network of one wave, they could only read the outcome's own
#'   ties, so `inertia` and `trans` are dropped, and `recip` warns.
#'   - `inertia` is 1 where the tie was there in the wave before,
#'     \deqn{x_{ij} = \mathbf{1}[y^{(w-1)}_{ij} > 0],}{x_ij = 1[y(w-1)_ij > 0],}
#'     or its value with `inertia(weighted = TRUE)`.
#'   - `recip` is the reverse tie in the wave before,
#'     \eqn{x_{ij} = y^{(w-1)}_{ji}}{x_ij = y(w-1)_ji}.
#'   - `trans` is the number of two-paths from the sender to the receiver in
#'     the wave before,
#'     \deqn{x_{ij} = \sum_{k} b_{ik} b_{kj}, \quad b_{kl} = \mathbf{1}[y^{(w-1)}_{kl} > 0],}{x_ij = sum_k b_ik b_kj, b_kl = 1[y(w-1)_kl > 0],}
#'     where an unobserved tie counts as 0.
#'
#' @section Terms of a list of networks:
#'   - `global(x)` is a variable \eqn{g_m} of the whole network \eqn{m}, from
#'     its `globals`, for every dyad of that network:
#'     \eqn{x_{ij} = g_m}{x_ij = g_m}, as `global_x`. On one network it is the
#'     intercept, so it needs a list of networks.
#'
#' @section Terms of the reporter of a cognitive social structure:
#'   A cognitive social structure (CSS) holds each reporter \eqn{r}'s report
#'   of the network, so each cell is \eqn{x_{ijr}}, for the dyad as \eqn{r}
#'   reports it (Krackhardt 1987).
#'   - `reporter(x, levels = NULL)` is the reporter's value,
#'     \eqn{x_{ijr} = a_r}{x_ijr = a_r}, as `reporter_x`, and is dummy-coded
#'     as `ego()` is.
#'   - `reporter_ego` is 1 where the reporter is the sender,
#'     \eqn{x_{ijr} = \mathbf{1}[r = i]}{x_ijr = 1[r = i]}, and
#'     `reporter_alter` where the reporter is the receiver,
#'     \eqn{x_{ijr} = \mathbf{1}[r = j]}{x_ijr = 1[r = j]}: a report of the
#'     reporter's own ties.
#'   - `reporter_same(x, node = "ego")` is 1 where the reporter shares the
#'     sender's category, \eqn{x_{ijr} = \mathbf{1}[a_r = a_i]}{x_ijr = 1[a_r = a_i]},
#'     as `reporter_same_ego_x`, or the receiver's with `node = "alter"`,
#'     \eqn{x_{ijr} = \mathbf{1}[a_r = a_j]}{x_ijr = 1[a_r = a_j]}.
#'
#'   See the section on cognitive social structures in [estimate_mrqap()].
#'
#' @section Choosing categories with levels =:
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
#' @section Interactions:
#'   `:` and `*` give interactions of any order. An interaction is the
#'   product of its terms' cells, \eqn{x_{ij} = x^{(1)}_{ij} x^{(2)}_{ij}}{x_ij = x1_ij x2_ij},
#'   with one column for each pair of their columns, named by joining their
#'   names with `:`, such as `ego_age:same_office`.
#'
#' @section Equivalents in other models:
#'   A formula written for `{ergm}` or `{goldfish}` can be used as it is, and
#'   [specify_mrqap()] and [specify_ergm()] rewrite a formula for each other's
#'   model. The nearest term of each is:
#'
#'   | infernet | ergm | goldfish | RSiena |
#'   |---|---|---|---|
#'   | `ego(x)` | `nodeocov("x")`, `nodeofactor("x")`; `b1cov("x")` in a two-mode network; `nodecov("x")` in an undirected one | `ego(x)` | `egoX` |
#'   | `alter(x)` | `nodeicov("x")`, `nodeifactor("x")`; `b2cov("x")` in a two-mode network | `alter(x)` | `altX` |
#'   | `same(x)` | `nodematch("x")` | `same(x)` | `sameX` |
#'   | `same(x, diff = TRUE)` | `nodematch("x", diff = TRUE)` | | |
#'   | `mix(x)` | `nodemix("x")` | | |
#'   | `ego(x, levels = )`, `same(x, levels = )` | `nodeofactor("x", levels = )`, `nodematch("x", levels = )` | | |
#'   | `mix(x, levels = )` | `nodemix("x", levels2 = )` | | |
#'   | `diff(x)` | `absdiff("x")` | `diff(x)` | `absDiffX` |
#'   | `diff(x, transformer_fn = identity)` | `diff("x")` | `diff(x, transformer_fn = identity)` | `diffX` |
#'   | `sim(x)` | `edgecov()` | `sim(x)`, as minus the absolute difference | `simX`, centred |
#'   | a layer, or `tie(layer)` | `edgecov()` | `tie(layer)` | `X` |
#'   | `exchange(layer)` | `edgecov()` of the transpose | `recip(layer)` | |
#'   | `indeg(layer)`, `outdeg(layer)` | `edgecov()` | `indeg(layer)`, `outdeg(layer, type = "ego")` | |
#'   | `tertius(layer, x)`, `tertius_diff(layer, x)` | `edgecov()` | the same, with the sender among the in-neighbours | |
#'   | `global(x)` | | `global(x)` | |
#'   | `recip` | `mutual` | `recip` | `recip` |
#'   | `inertia` | | `inertia` | |
#'   | `trans` | `ttriple` | `trans` | `transTrip` |
#'
#'   `sim()` is on another scale in goldfish, which changes its coefficient
#'   but not its test, and goldfish's `tertius()` counts the sender among the
#'   in-neighbours, so [specify_mrqap()] names both where it reads a goldfish
#'   formula. ergm's `edges` is the intercept, which an MRQAP always has.
#'   The reporter terms have no equivalent, since no other model has a
#'   reporter. ergm's terms of the outcome's own structure, such as `gwesp()`,
#'   and goldfish's endogenous effects, such as `cycle`, have no MRQAP term,
#'   and an MRQAP drops them (see [specify]).
#'
#' @references
#'   Haunss, Sebastian, and James Hollway. 2023.
#'   "Multimodal mechanisms of political discourse dynamics and the case of
#'   Germany's nuclear energy phase-out."
#'   _Network Science_ 11(2): 205-223.
#'   \doi{10.1017/nws.2022.31}.
#'
#'   Krackhardt, David. 1987.
#'   "Cognitive Social Structures."
#'   _Social Networks_ 9(2): 109-134.
#'   \doi{10.1016/0378-8733(87)90009-8}.
#'
#'   Morris, Martina, Mark S. Handcock, and David R. Hunter. 2008.
#'   "Specification of Exponential-Family Random Graph Models: Terms and
#'   Computational Aspects."
#'   _Journal of Statistical Software_ 24(4): 1-24.
#'   \doi{10.18637/jss.v024.i04}.
#' @seealso [estimate_mrqap()] fits a formula of these terms; [specify_terms]
#'   lists the terms that suit a network.
#' @name terms_mrqap
#' @family models
#' @examples
#' # Every term that suits a network, and the columns of some of them
#' specify_mrqap(advice ~ ., manynet::ison_lawfirm)
#' tidy(specify_mrqap(advice ~ ego(age) + same(office) + sim(age)),
#'      manynet::ison_lawfirm)
NULL

# ---- the term registry -------------------------------------------------------

# `formals` is the signature a term is matched against, as for a function.
# `kind` says what its first argument names: a node attribute, a layer, or a
# global variable. `twomode` is FALSE where a two-mode network has no meaning
# for the term, and `hint` then says what to use instead. `outcome` says what
# happens where a layer term names the outcome: "refuse", or "warn" where the
# term is defined on the outcome but is then not permuted with it. `bare` is
# TRUE where the term can be named on its own, as goldfish's endogenous effects
# are: `~ recip` is then `recip()` of the outcome. A bare term whose `outcome`
# is "refuse", such as `~ indeg`, is dropped rather than refused, since it is
# goldfish's default rather than a layer the formula names. `panel` is TRUE where the
# term reads the outcome's previous wave on a longitudinal network, as goldfish
# and RSiena read the state of the network before each change. It then fits
# there without a warning, and `build` reads `ctx$previous`.
#
# The rest say what each model makes of the term, so that one formula can be
# read by several (see "Formula conventions" in CONTRIBUTING.md). `mrqap` is
# what `estimate_mrqap()` does with it: "fits", "warns" where it fits but the
# user should know something, "drops" where the rest of the model can still be
# fitted without it, or "refuses". A term that warns or is dropped has a
# `reason`. These are the statuses of `.statuses()`, which `specify_*()`
# records for each term. The other models are named for the model rather than
# the package: `dynam` (a DyNAM or REM in goldfish), `saom` (RSiena), and
# `ergm` name the equivalent term in each, or the nearest, and are NA where
# there is none. `to_ergm` converts the term for `specify_ergm()`,
# in specify_ergm.R: to the named ergm term, or to `edgecov()` of the
# matrix `build` makes. A term with an `ergm` name and no `to_ergm` is not yet
# converted.
#' @keywords internal
#' @noRd
.formula_terms <- function() {
  c(list(
    ego = list(
      title = "the sender's value of the attribute",
      formals = alist(x = , levels = NULL), kind = "attribute", build = .build_ego,
      mrqap = "fits", dynam = "ego", saom = "egoX",
      ergm = "nodeocov, or nodeofactor for a categorical attribute",
      to_ergm = .ergm_ego),
    alter = list(
      title = "the receiver's value of the attribute",
      formals = alist(x = , levels = NULL), kind = "attribute", build = .build_alter,
      mrqap = "fits", dynam = "alter", saom = "altX",
      ergm = "nodeicov, or nodeifactor for a categorical attribute",
      to_ergm = .ergm_alter),
    same = list(
      title = "whether the sender and the receiver share a category",
      formals = alist(x = , diff = FALSE, levels = NULL), kind = "attribute",
      build = .build_same, mrqap = "fits",
      dynam = "same, without diff = TRUE", saom = "sameX, without diff = TRUE",
      ergm = "nodematch, with the same diff",
      to_ergm = .ergm_same),
    diff = list(
      title = "the difference between the sender's and the receiver's values",
      formals = alist(x = , transformer_fn = abs), kind = "attribute",
      build = .build_diff, mrqap = "fits", dynam = "diff",
      saom = "absDiffX, or diffX with transformer_fn = identity",
      ergm = "absdiff, or diff with transformer_fn = identity",
      to_ergm = .ergm_diff),
    dist = list(
      title = "the difference between the sender's and the receiver's values",
      formals = alist(x = ), kind = "attribute", build = .build_dist,
      mrqap = "warns", reason = "is a deprecated alias of diff()",
      dynam = "diff", saom = "absDiffX", ergm = "absdiff",
      to_ergm = .ergm_dist),
    sim = list(
      title = "the similarity of the sender's and the receiver's values",
      formals = alist(x = ), kind = "attribute", build = .build_sim,
      mrqap = "fits", dynam = "sim, as a negative difference",
      saom = "simX, centred", ergm = "edgecov",
      to_ergm = .ergm_built),
    match = list(
      title = "whether the sender and the receiver share each category",
      formals = alist(x = ), kind = "attribute", build = .build_match,
      twomode = FALSE, hint = "Use {.fn mix} for a two-mode network.",
      mrqap = "warns", reason = "is a deprecated alias of same(x, diff = TRUE)",
      dynam = NA_character_, saom = NA_character_,
      ergm = "nodematch with diff = TRUE",
      to_ergm = .ergm_match),
    mix = list(
      title = "the pair of the sender's and the receiver's categories",
      formals = alist(x = , levels = NULL), kind = "attribute", build = .build_mix,
      mrqap = "fits", dynam = NA_character_, saom = NA_character_,
      ergm = "nodemix",
      to_ergm = .ergm_mix),
    tie = list(
      title = "the tie in another layer",
      formals = alist(layer = , weighted = FALSE), kind = "layer",
      build = .build_tie, outcome = "refuse",
      mrqap = "fits", dynam = "tie", saom = "X", ergm = "edgecov",
      to_ergm = .ergm_built),
    recip = list(
      title = "the reverse tie",
      formals = alist(layer = , weighted = NULL), kind = "layer", build = .build_recip,
      twomode = FALSE, bare = TRUE, panel = TRUE,
      hint = "A two-mode tie has no reverse tie of the same shape.",
      outcome = "warn", mrqap = "warns", reason = .outcome_reason(),
      dynam = "recip", saom = "recip",
      ergm = "mutual, or edgecov of the transpose",
      to_ergm = .ergm_recip),
    exchange = list(
      title = "the reverse tie in another layer",
      formals = alist(layer = , weighted = NULL), kind = "layer", build = .build_recip,
      twomode = FALSE,
      hint = "A two-mode tie has no reverse tie of the same shape.",
      outcome = "warn", mrqap = "fits", dynam = "recip with a network",
      saom = NA_character_, ergm = "edgecov of the transpose",
      to_ergm = .ergm_recip),
    indeg = list(
      title = "the number of incoming ties in a layer",
      formals = alist(layer = , node = "alter", weighted = FALSE, type = NULL),
      kind = "layer", build = .build_indeg, outcome = "refuse", bare = TRUE,
      mrqap = "fits", dynam = "indeg", saom = NA_character_,
      ergm = "edgecov, or nodeicov of the degree",
      to_ergm = .ergm_built),
    outdeg = list(
      title = "the number of outgoing ties in a layer",
      formals = alist(layer = , node = "ego", weighted = FALSE, type = NULL),
      kind = "layer", build = .build_outdeg, outcome = "refuse", bare = TRUE,
      mrqap = "fits", dynam = "outdeg", saom = NA_character_,
      ergm = "edgecov, or nodeocov of the degree",
      to_ergm = .ergm_built),
    tertius = list(
      title = "the attribute of the receiver's other in-neighbours",
      formals = alist(layer = , x = , summarizer_fn = "mean"),
      kind = "tertius", build = .build_tertius, outcome = "warn",
      mrqap = "fits", dynam = "tertius, with ego", saom = NA_character_,
      ergm = "edgecov",
      to_ergm = .ergm_built),
    tertius_diff = list(
      title = "the difference from the receiver's other in-neighbours",
      formals = alist(layer = , x = , summarizer_fn = "mean"),
      kind = "tertius", build = .build_tertius_diff, outcome = "warn",
      mrqap = "fits", dynam = "tertius_diff, with ego", saom = NA_character_,
      ergm = "edgecov",
      to_ergm = .ergm_built),
    global = list(
      title = "a value of the whole network",
      formals = alist(x = ), kind = "global", build = .build_global,
      mrqap = "fits", dynam = "global", saom = NA_character_,
      ergm = NA_character_),
    # A cognitive social structure's terms, named for its reporter. No other
    # model has a reporter, so `css` marks them for the other models' readers.
    reporter = list(
      title = "the reporter's value of the attribute",
      formals = alist(x = , levels = NULL), kind = "attribute",
      build = .build_reporter, css = TRUE,
      mrqap = "fits", dynam = NA_character_, saom = NA_character_,
      ergm = NA_character_),
    reporter_ego = list(
      title = "whether the reporter is the sender",
      formals = alist(), kind = "reporter", build = .build_reporter_ego,
      bare = TRUE, css = TRUE,
      mrqap = "fits", dynam = NA_character_, saom = NA_character_,
      ergm = NA_character_),
    reporter_alter = list(
      title = "whether the reporter is the receiver",
      formals = alist(), kind = "reporter", build = .build_reporter_alter,
      bare = TRUE, css = TRUE,
      mrqap = "fits", dynam = NA_character_, saom = NA_character_,
      ergm = NA_character_),
    reporter_same = list(
      title = "whether the reporter shares the category",
      formals = alist(x = , node = "ego"), kind = "attribute",
      build = .build_reporter_same, css = TRUE, twomode = FALSE,
      hint = "Use {.fn reporter} with {.fn ego} or {.fn alter} on the attribute instead.",
      mrqap = "fits", dynam = NA_character_, saom = NA_character_,
      ergm = NA_character_)
  ),
  .dynam_terms())
}

# goldfish's effects that `estimate_mrqap()` cannot build, so that a formula
# written for goldfish still fits here without them (stocnet/goldfish#122 names
# them). Most read the dependent events by default: a statistic of the
# outcome's own ties, which permuting the outcome would not permute with it.
# The others need a pair of layers, a group, or a time order that a single
# MRQAP does not have. `specify_mrqap()` drops them, and every argument
# is accepted, since none is read.
#' @keywords internal
#' @noRd
.dynam_terms <- function() {
  endogenous <- function(saom, ergm, to_ergm = NULL, kind = "endogenous") {
    list(formals = alist(layer = , ... = ), kind = kind, bare = TRUE,
         mrqap = "drops", reason = .endogenous_reason(),
         saom = saom, ergm = ergm, to_ergm = to_ergm)
  }
  mixed <- function(ergm = NA_character_) {
    list(formals = alist(layer = , ... = ), kind = "mixed",
         mrqap = "drops", reason = "closes a path through two layers, which is not yet built here",
         saom = NA_character_, ergm = ergm)
  }
  groups <- function() {
    list(formals = alist(... = ), kind = "groups", bare = TRUE,
         mrqap = "drops", reason = "is defined on the groups of a DyNAM-i",
         saom = NA_character_, ergm = NA_character_)
  }
  # Fitted on the previous wave of a longitudinal network, and dropped from a
  # cross-sectional one, where they could only read the outcome's own ties.
  lagged <- function(entry, build, formals = entry$formals) {
    entry$panel <- TRUE
    entry$build <- build
    entry$formals <- formals
    entry$twomode <- FALSE
    entry$hint <- "A two-mode network has no two-paths or repeated ties of this kind."
    entry$reason <- "needs a longitudinal network, where it reads the outcome's previous wave"
    entry
  }
  terms <- list(
    inertia         = lagged(endogenous(NA_character_, NA_character_),
                             .build_inertia,
                             formals = alist(layer = , weighted = FALSE, ... = )),
    trans           = lagged(endogenous("transTrip", "ttriple, or dgwesp(type = \"OTP\")",
                                        .ergm_endogenous("ttriple", "triangle")),
                             .build_trans),
    cycle           = endogenous("cycle3", "ctriple, or dgwesp(type = \"ITP\")",
                                 .ergm_endogenous("ctriple")),
    common_sender   = endogenous("gwespBF, nearest", "dgwesp(type = \"ISP\")"),
    common_receiver = endogenous("gwespFB, nearest", "dgwesp(type = \"OSP\")"),
    four            = endogenous("cycle4", "cycle(4)",
                                 .ergm_endogenous("cycle(4)", "cycle(4)")),
    triangle        = endogenous(NA_character_, "triangle",
                                 .ergm_endogenous("triangle", "triangle")),
    node_trans      = endogenous(NA_character_, NA_character_),
    degree          = endogenous(NA_character_, "degree"),
    mixed_trans           = mixed(),
    mixed_cycle           = mixed(),
    mixed_common_sender   = mixed(),
    mixed_common_receiver = mixed(),
    ego_alter_interaction = list(
      formals = alist(x = , ... = ), kind = "interaction",
      mrqap = "drops", reason = "is written {.code ego(x):alter(y)} here",
      saom = "egoXaltX", ergm = "nodeocov(x):nodeicov(y)"),
    alterdeg  = groups(), alterpop = groups(), egodeg = groups(),
    egopop    = groups(), dyad     = groups(), dyadXdiff = groups(),
    dyadXego  = groups(), size     = groups(), sizeXdiff = groups(),
    sizeXego  = groups(), intercept = groups()
  )
  terms$inertia$title <- "the tie in the previous wave"
  terms$trans$title <- "the two-paths in the previous wave"
  for (nm in names(terms)) terms[[nm]]$dynam <- nm
  terms
}
