# Formula -> the formula of another model ------------------------------------------
#
# A formula is read once, whichever model it was written for, into infernet's
# own terms, which are goldfish's where the two share them. Each `specify_*()`
# then writes those terms for its model: it rewrites a term that has an
# equivalent there, names a term that means something different there, and
# drops a term the model cannot fit. One reader and one writer for each model
# keeps the number of translations at twice the number of models, rather than
# one for each pair of them.

#' Specify a model from a formula written for many models
#'
#' @description
#' These functions read a formula, whichever model it was written for, and
#' return the formula that another model fits from it:
#'
#' - `specify_mrqap()` for [estimate_mrqap()], which calls it on every formula;
#' - `specify_dynam()` for a DyNAM or REM in `{goldfish}`;
#' - `specify_ergm()` for `ergm::ergm()`.
#'
#' A term with an equivalent in the model is rewritten as that term.
#' A term that means something different in the model is kept and named.
#' A term the model cannot fit is dropped and named, so that the rest of the
#' model still fits. Rewritten and differently interpreted terms are reported
#' with `manynet::snet_info()`, which is silent unless
#' `options(snet_verbosity = "verbose")`, and dropped terms are reported in
#' one warning.
#' A formula aborts only where no term is left to fit.
#' A name that no model knows is left for the fitting function, which then
#' aborts and suggests the nearest name.
#'
#' @section What each model makes of each term:
#'   `tidy()` of the result has one row for each term of the formula, and
#'   fits nothing. Its `status` is what the model makes of the term:
#'
#'   - `"fits"`: the model fits the term as it is written;
#'   - `"adapted"`: the term is rewritten as its equivalent in the model, in `as`;
#'   - `"interpreted"`: the model has a term of the same name that it defines
#'     differently, as `reason` says;
#'   - `"warns"`: the model fits the term, but with a warning, as `reason` says;
#'   - `"drops"`: the model cannot fit the term, so it is left out, and the rest
#'     is fitted;
#'   - `"refuses"`: the model aborts, as `reason` says.
#'
#'   An MRQAP warns for a term that reads the outcome's own ties, such as
#'   `recip`, and refuses `tie()`, `indeg()`, and `outdeg()` of the outcome
#'   layer. It drops an offset, such as goldfish's `offset(recip, coef = 0)`,
#'   since it estimates every coefficient; goldfish keeps a goldfish offset,
#'   and ergm an ergm one. For an MRQAP, `tidy()` with `.data` also builds each term and names
#'   its `columns`, as [estimate_mrqap()] names them.
#'
#' @section Reading the model a formula was written for:
#'   The model a formula was written for is read, in order, from `input`,
#'   from the class of the formula, such as `"ergm_formula"`, or from the
#'   names in the formula:
#'
#'   - `"ergm"` where a term is named only in ergm, such as `edges`,
#'     `mutual`, or `nodematch("x")`;
#'   - `"dynam"` where a term is named only in goldfish, such as `inertia` or
#'     `trans`;
#'   - `"mrqap"` otherwise.
#'
#'   The model that was read is reported. Where a formula mixes ergm's names
#'   with goldfish's, it is read as an ergm formula, and goldfish's names in it
#'   are read as they are in goldfish.
#'   The result of each function carries its model as its class, so that
#'   `specify_ergm(specify_mrqap(f))` reads its input without a guess.
#'
#' @section ergm and infernet:
#'   ergm names an attribute with a string, and infernet with a bare name.
#'   Both read `levels =` as names or positions. infernet does not read ergm's
#'   functions for `levels =`, such as `LARGEST`, or `nodemix()`'s `levels =`,
#'   which changes the positions of `levels2 =`: such a term is left to ergm,
#'   and an MRQAP drops it.
#'
#'   | infernet | ergm |
#'   |---|---|
#'   | `ego(x)` | `nodeocov("x")`, `nodeofactor("x")`; `b1cov("x")` in a two-mode network; `nodecov("x")` in an undirected one |
#'   | `alter(x)` | `nodeicov("x")`, `nodeifactor("x")`; `b2cov("x")` in a two-mode network |
#'   | `same(x)` | `nodematch("x")` |
#'   | `same(x, diff = TRUE)` | `nodematch("x", diff = TRUE)` |
#'   | `mix(x)` | `nodemix("x")` |
#'   | `ego(x, levels = )`, `same(x, levels = )` | `nodeofactor("x", levels = )`, `nodematch("x", levels = )` |
#'   | `mix(x, levels = )` | `nodemix("x", levels2 = )` |
#'   | `diff(x)` | `absdiff("x")` |
#'   | `diff(x, transformer_fn = identity)` | `diff("x")` |
#'   | a layer, or `tie(layer)` | `edgecov()` |
#'   | `recip` | `mutual` |
#'   | `trans`, `cycle`, `four`, `triangle` | `ttriple`, `ctriple`, `cycle(4)`, `triangle` |
#'
#'   Read from ergm, `nodecov("x")` becomes `ego(x) + alter(x)`, and `edges`
#'   and `density` are the intercept, which an MRQAP always has.
#'   ergm's terms of the outcome's own structure, such as `gwesp()`, and its
#'   node fixed effects, `sender`, `receiver`, and `sociality`, are kept for
#'   ergm and dropped for an MRQAP, where `(1 | sender)` adds a random
#'   intercept instead.
#'   The left-hand side of an ergm formula names the network rather than a
#'   layer, so where it names no layer or tie attribute of `.data`, the
#'   outcome is read as `.`.
#'
#'   A formula written for ergm is returned as it is.
#'   Written for ergm, every other term, such as `sim()`, `indeg()`, or an
#'   interaction, is `edgecov()` of the matrix infernet builds, which needs
#'   `.data`. With `.data`, the formula's environment holds those matrices,
#'   and the outcome as a `network` object under its own name, so
#'   `ergm::ergm(specify_ergm(f, .data))` fits as it is.
#'   `edges` is always added, as ergm's intercept.
#'   `(1 | sender)` and `(1 | receiver)` become ergm's fixed effects `sender`
#'   and `receiver`, which are the nearest ergm has, and the fixed effects
#'   `| sender + receiver` become the same terms.
#'   A cognitive social structure or a list of networks aborts, since ergm
#'   fits one report of one network.
#'
#' @section goldfish and infernet:
#'   goldfish's terms are infernet's where the two packages share them.
#'   `sim()` is 1 - |d| / range in infernet and -|d| in goldfish,
#'   which changes the scale of its coefficient but not its test, and
#'   `tertius()` and `tertius_diff()` leave out the sender in infernet and
#'   include it in goldfish. Both are kept and named.
#'
#'   For an MRQAP, goldfish's endogenous effects, such as `cycle`,
#'   `common_sender`, `common_receiver`, and `four`, read the outcome's own
#'   ties, which permuting the outcome would not permute with them, so they are
#'   dropped. So are goldfish's closures through two layers and the DyNAM-i
#'   statistics of groups. `inertia`, `recip`, and `trans` read the outcome's
#'   previous wave where `.data` is a longitudinal network, and fit there; on a
#'   cross-sectional network, `inertia` and `trans` are dropped too.
#'
#'   goldfish names the side of `indeg()` and `outdeg()` with `type =`, and
#'   reads the receiver for both by default, where infernet's `outdeg()` reads
#'   the sender. A goldfish formula is read as goldfish reads it:
#'   `outdeg(friends)` becomes `outdeg(friends, node = "alter")`, with a
#'   warning, and `type =` becomes `node =`. `indeg` or `outdeg` on its own
#'   reads the outcome's own degree in goldfish, and is dropped for an MRQAP
#'   and for ergm. goldfish's older formulas name a node attribute with its
#'   table of nodes, as in `ego(actors$floor)`, which is read as `ego(floor)`,
#'   since the network holds its nodes' attributes.
#'
#'   For goldfish, a layer named on its own becomes `tie(layer, weighted = TRUE)`,
#'   `exchange(layer)` becomes `recip(layer)`, `dist(x)` becomes `diff(x)`,
#'   and `node = "ego"` becomes `type = "ego"`, so that `outdeg(friends)`
#'   becomes `outdeg(friends, type = "ego")`.
#'   `same(x, diff = TRUE)`, `match()`, `mix()`, random intercepts, and fixed effects have no goldfish
#'   term, and are dropped.
#'
#' @param formula A formula written for an MRQAP, goldfish, or ergm.
#' @param .data Optionally, the network the formula is for.
#'   With it, a bare name that is a layer of the network stays a layer.
#'   In `tidy()` of an MRQAP formula, the network to build the columns from.
#'   `specify_ergm()` needs it to write the terms that ergm reads as
#'   `edgecov()` of a matrix, and drops them without it.
#' @param input The model the formula was written for: `"mrqap"`, `"dynam"`,
#'   `"rem"`, or `"ergm"`. `NULL`, the default, reads it from the class of the
#'   formula, or else from the names in the formula.
#' @return A formula of class `c("mrqap_formula", "formula")`,
#'   `c("dynam_formula", "formula")`, or `c("ergm_formula", "formula")`.
#'   Its `input` attribute names the model it was read from, and its `record`
#'   attribute holds what the model makes of each term.
#'   Printing the formula lists them, and `tidy()` returns them as a tibble,
#'   with the columns `term`, `status`, `as`, and `reason`, and for an MRQAP
#'   also `columns`.
#' @name specify
#' @family models
NULL

#' @rdname specify
#' @examples
#' # An ergm formula, read for an MRQAP
#' specify_mrqap(advice ~ edges + mutual + nodematch("dept") + edgecov("friends"))
#' # A goldfish formula, read for an MRQAP
#' f <- suppressWarnings(
#'   specify_mrqap(advice ~ inertia + recip + trans + ego(age)))
#' f
#' @export
specify_mrqap <- function(formula, .data = NULL, input = NULL) {
  if (inherits(formula, "mrqap_formula")) return(formula)
  read <- .read_node_tables(.read_dynam_degree(.read_formula(formula, .data, input)))
  record <- rbind(read$record, .offset_rows(read, "mrqap"))
  labels <- read$labels

  native <- labels %in% read$native
  record <- rbind(record, .record_row(
    labels[native], rep("", sum(native)), rep("drops", sum(native)),
    vapply(labels[native], .ergm_native_reason, "", USE.NAMES = FALSE)))
  labels <- labels[!native]

  lhs <- .network_lhs(read)
  if (!identical(lhs, read$lhs)) {
    record <- rbind(record, .record_row(
      .deparsed(read$lhs), ".", "adapted",
      "names the network, so the outcome is the network's focal layer or its ties"))
  }
  record <- rbind(record, .interpreted_dynam(labels, read$input, "mrqap"))
  dropped <- .drop_labels(labels, read$ctx)
  # A term rewritten on reading, and then dropped, is recorded once, as written.
  gone <- dropped$record$term %in% names(read$renamed)
  dropped$record$term[gone] <- read$renamed[dropped$record$term[gone]]
  record <- record[!(record$term %in% dropped$record$term), , drop = FALSE]
  record <- rbind(record, dropped$record)
  record <- rbind(record, .fixed_rows(read$fixed, "mrqap"))
  .finish_formula(lhs, dropped$labels, read$bars, record, read, "mrqap",
                  fixed = read$fixed)
}

#' @rdname specify
#' @examples
#' # An MRQAP formula, written for goldfish
#' specify_dynam(advice ~ ego(age) + same(dept) + friends + exchange(friends))
#' @export
specify_dynam <- function(formula, .data = NULL, input = NULL) {
  if (inherits(formula, "dynam_formula")) return(formula)
  read <- .read_formula(formula, .data, input)
  record <- read$record
  registry <- .formula_terms()
  out <- character(0)
  for (lab in read$labels) {
    if (lab %in% read$native) {
      new <- .dynam_from_ergm(lab)
      if (is.null(new)) {
        record <- rbind(record, .record_row(lab, "", "drops", "has no goldfish term"))
      } else {
        record <- rbind(record, .record_row(lab, new, "adapted", ""))
        out <- c(out, new)
      }
      next
    }
    ops <- lapply(.flatten_interaction(str2lang(lab)), .dynam_operand,
                  registry = registry, ctx = read$ctx, input = read$input)
    reason <- unlist(lapply(ops, `[[`, "reason"))
    if (length(reason) > 0) {
      record <- rbind(record, .record_row(lab, "", "drops", reason[1]))
      next
    }
    new <- .deparsed(Reduce(function(a, b) call(":", a, b), lapply(ops, `[[`, "expr")))
    note <- c(unlist(lapply(ops, `[[`, "note")), "")[1]
    if (!identical(new, lab)) record <- rbind(record, .record_row(lab, new, "adapted", note))
    out <- c(out, new)
  }
  for (bar in read$bars) {
    record <- rbind(record, .record_row(paste0("(", bar, ")"), "", "drops",
                                      "is a random intercept, which goldfish does not fit"))
  }
  record <- rbind(record, .fixed_rows(read$fixed, "dynam"))
  record <- rbind(record, .interpreted_dynam(out, read$input, "dynam"))
  # goldfish fixes a coefficient with `offset(effect, coef = )`.
  keep <- read$input %in% c("dynam", "rem")
  record <- rbind(record, .offset_rows(read, "dynam", keep = keep))
  if (keep) out <- c(out, read$offsets)
  .finish_formula(read$lhs, unique(out), character(0), record, read, "dynam")
}

#' @rdname specify
#' @examples
#' # An MRQAP formula, written for ergm; with `.data`, ergm::ergm() fits it
#' specify_ergm(advice ~ ego(age) + same(dept) + recip + trans)
#' @export
specify_ergm <- function(formula, .data = NULL, input = NULL) {
  if (inherits(formula, "ergm_formula")) return(formula)
  if (.is_list_of_graphs(.data)) {
    manynet::snet_abort(
      c("{.pkg ergm} fits one network, and {.arg .data} is a list of {length(.data)} networks.",
        i = "Specify the formula for each network on its own."))
  }
  read <- .read_node_tables(.read_dynam_degree(.read_formula(formula, .data, input)))
  # A formula written for ergm is already one.
  if (read$input == "ergm") {
    return(structure(formula, class = c("ergm_formula", "formula"),
                     input = "ergm", record = .complete_record(.no_record(), read, "ergm")))
  }
  record <- rbind(read$record, .offset_rows(read, "ergm"))
  native <- intersect(read$labels, read$native)
  labels <- setdiff(read$labels, native)
  record <- rbind(record, .interpreted_dynam(labels, read$input, "ergm"))

  lhs <- .network_lhs(read)
  ctx <- NULL
  if (!is.null(.data)) {
    ctx <- .spec_context(list(outcome = .deparsed(lhs), env = read$env), .data)
    if (ctx$css) {
      manynet::snet_abort(
        c("{.pkg ergm} has no form for the cognitive social structure {.val {ctx$out$label}}.",
          i = "Each reporter reports its ties, and ergm fits one report of a network."))
    }
  }

  terms <- character(0)
  covariates <- list()
  if (length(labels) > 0) {
    spec_formula <- eval(call("~", lhs, str2lang(paste(labels, collapse = " + "))))
    environment(spec_formula) <- read$env
    spec <- .specify(spec_formula)
    if (is.null(ctx)) {
      spec <- .resolve_bare_terms(spec, read$ctx)
      converted <- lapply(spec$terms, .ergm_term_nodata)
      sides <- vapply(spec$terms, function(t) {
        any(vapply(t$operands, function(op) op$fn %in% c("ego", "alter"), logical(1)))
      }, logical(1))
      if (any(sides)) {
        manynet::snet_info(
          "Without {.arg .data}, {.fn ego} and {.fn alter} are written for a directed network and a numeric attribute.")
      }
    } else {
      spec <- .resolve_bare_terms(spec, ctx)
      .check_formula_vars(spec, ctx)
      converted <- lapply(spec$terms, .ergm_term, ctx = ctx)
    }
    for (x in converted) {
      # A term rewritten on reading is recorded once, as written.
      term <- x$label
      if (term %in% names(read$renamed)) {
        term <- read$renamed[[term]]
        record <- record[record$term != term, , drop = FALSE]
      }
      if (!is.null(x$reason)) {
        record <- rbind(record, .record_row(term, "", "drops", x$reason))
        next
      }
      new <- paste(x$terms, collapse = " + ")
      if (!identical(new, term)) {
        record <- rbind(record, .record_row(term, new, "adapted", ""))
      }
      terms <- c(terms, x$terms)
      covariates <- c(covariates, x$covariates)
    }
  }

  for (bar in read$bars) {
    group <- trimws(sub(".*\\|", "", bar))
    if (group %in% c("sender", "receiver")) {
      record <- rbind(record, .record_row(
        paste0("(", bar, ")"), group, "adapted",
        "a fixed effect for each node, the nearest ergm has to a random intercept"))
      terms <- c(terms, group)
    } else {
      record <- rbind(record, .record_row(paste0("(", bar, ")"), "", "drops",
                                        "is a random intercept, which ergm does not fit"))
    }
  }

  record <- rbind(record, .fixed_rows(read$fixed, "ergm"))
  terms <- c(terms, intersect(read$fixed, c("sender", "receiver")))

  repeated <- unique(terms[duplicated(terms)])
  if (length(repeated) > 0) {
    manynet::snet_info("Kept one copy of {.code {repeated}}, which several terms give on an undirected network.")
  }
  if (!any(c("edges", "density") %in% record$term)) {
    manynet::snet_info("Adding {.code edges}, ergm's intercept.")
  }

  env <- read$env
  if (!is.null(ctx)) {
    if (!all(ctx$DV %in% c(0, 1, NA))) {
      manynet::snet_info("{.pkg ergm} reads the valued ties of {.val {ctx$out$label}} as present or absent, unless {.fn ergm} is given a {.arg response}.")
    }
    env <- new.env(parent = read$env)
    for (nm in names(covariates)) assign(nm, covariates[[nm]], envir = env)
    # Through igraph: manynet 2.4.4's `as_network()` of a tidygraph drops the
    # first node attribute where the nodes have no names.
    assign(ctx$out$label, manynet::as_network(manynet::as_igraph(ctx$out$net)),
           envir = env)
    lhs <- as.name(ctx$out$label)
  }
  .finish_formula(lhs, c("edges", unique(terms), native), character(0), record,
                  read, "ergm", env = env)
}

#' @export
print.mrqap_formula <- function(x, ...) .print_specified(x)

#' @export
print.dynam_formula <- function(x, ...) .print_specified(x)

#' @export
print.ergm_formula <- function(x, ...) .print_specified(x)

#' @rdname specify
#' @param x A formula returned by a `specify_*()` function.
#' @param ... Not used.
#' @examples
#' # Each term, and what the model makes of it
#' tidy(f)
#' g <- specify_mrqap(advice ~ ego(age) + same(dept) + recip(friends), ison_hightech)
#' tidy(g, ison_hightech)
#' @export
tidy.mrqap_formula <- function(x, .data = NULL, ...) {
  out <- .tidy_specified(x)
  out$columns <- rep(list(character(0)), nrow(out))
  if (is.null(.data)) return(out)
  .record_columns(x, out, .data)
}

#' @rdname specify
#' @export
tidy.dynam_formula <- function(x, ...) .tidy_specified(x)

#' @rdname specify
#' @export
tidy.ergm_formula <- function(x, ...) .tidy_specified(x)

#' @keywords internal
#' @noRd
.tidy_specified <- function(x) {
  record <- attr(x, "record")
  if (is.null(record)) record <- .no_record()
  blank <- function(v) ifelse(nzchar(v), v, NA_character_)
  tibble::tibble(term = record$term, status = record$status,
                 as = blank(record$as), reason = blank(record$reason))
}

# The columns that an MRQAP builds for each term the formula keeps, named as
# `estimate_mrqap()` names them. A term whose matrix cannot be built, such as
# `tie()` of the outcome layer, is refused, with the reason the builder gives.
#' @keywords internal
#' @noRd
.record_columns <- function(x, out, .data) {
  spec <- .specify(x)
  # A panel's columns are those of its second wave, read against its first.
  ctx <- if (.is_panel(.data)) {
    waves <- manynet::to_waves(.data)
    .spec_context(spec, waves[[2]], previous = waves[[1]])
  } else {
    .spec_context(spec, .data)
  }
  spec <- .resolve_bare_terms(spec, ctx)
  .check_formula_vars(spec, ctx)
  built <- lapply(spec$terms, function(term) {
    tryCatch(suppressWarnings(suppressMessages(
      names(Reduce(.interact, lapply(term$operands, .build_operand, ctx = ctx))))),
      error = function(e) e)
  })
  names(built) <- vapply(spec$terms, `[[`, "", "label")
  for (k in seq_len(nrow(out))) {
    if (out$status[k] == "drops" || is.na(out$as[k])) next
    labels <- strsplit(out$as[k], " + ", fixed = TRUE)[[1]]
    for (lab in intersect(labels, names(built))) {
      if (inherits(built[[lab]], "error")) {
        out$status[k] <- "refuses"
        msg <- gsub("\033\\[[0-9;]*m", "", conditionMessage(built[[lab]]))
        out$reason[k] <- strsplit(msg, "\n", fixed = TRUE)[[1]][1]
      } else {
        out$columns[[k]] <- c(out$columns[[k]], built[[lab]])
      }
    }
  }
  out
}

#' @keywords internal
#' @noRd
.print_specified <- function(x) {
  record <- attr(x, "record")
  f <- x
  attr(f, "record") <- NULL
  attr(f, "input") <- NULL
  class(f) <- "formula"
  print(f, showEnv = FALSE)
  if (!is.null(record) && nrow(record) > 0) {
    cat("Read from a formula for", attr(x, "input"), "\n")
    for (k in seq_len(nrow(record))) {
      as <- if (nzchar(record$as[k]) && record$as[k] != record$term[k])
        paste0(" -> ", record$as[k]) else ""
      why <- if (nzchar(record$reason[k])) paste0(": ", record$reason[k]) else ""
      cat("  ", record$status[k], " ", record$term[k], as, why, "\n", sep = "")
    }
  }
  invisible(x)
}

# ---- reading --------------------------------------------------------------------------

# Reads a formula into infernet's terms. An ergm term with an infernet
# equivalent is rewritten as it; one without, such as `gwesp()`, is kept as it
# is and listed in `native`, so that `specify_ergm()` can keep it and the
# other writers can drop it.
#' @keywords internal
#' @noRd
.read_formula <- function(formula, .data, input) {
  if (!inherits(formula, "formula") || length(formula) != 3) {
    manynet::snet_abort(
      c("The formula needs an outcome on its left-hand side.",
        i = "For example: {.code weight ~ ego(age)}."))
  }
  # A list of networks is read as independent networks, not as a panel, even
  # where it holds the waves of one network.
  panel <- .is_panel(.data)
  if (.is_list_of_graphs(.data)) .data <- .data[[1]]
  # The groups after a bare bar are read before the terms, since `sender` and
  # `receiver` are also ergm's names for its node fixed effects.
  split <- .split_fixed(formula)
  formula <- split$formula
  ctx <- .specify_context(.data, .deparsed(formula[[2]]))
  ctx$panel <- panel
  input <- .read_input(formula, input, ctx)
  tt <- stats::terms(formula)
  # An offset fixes a term's coefficient, as `offset(recip, coef = 0)` does in
  # goldfish. It is read apart from the terms, and each writer keeps or drops it.
  offsets <- vapply(as.list(attr(tt, "variables"))[-1][attr(tt, "offset")],
                    .deparsed, "")
  labels <- attr(tt, "term.labels")
  bars <- labels[grepl("|", labels, fixed = TRUE)]
  labels <- labels[!grepl("|", labels, fixed = TRUE)]
  read <- list(labels = labels, native = character(0), record = .no_record())
  if (input == "ergm") read <- .read_ergm_terms(labels, ctx)
  env <- environment(formula)
  c(read, list(lhs = formula[[2]], bars = bars, fixed = split$fixed,
               offsets = offsets, input = input, ctx = ctx,
               data = .data, env = if (is.null(env)) globalenv() else env,
               original = c(attr(tt, "term.labels"), offsets)))
}

# The record of the offsets: a model that fixes coefficients in the same form
# keeps them, and the others drop them. An MRQAP estimates every coefficient,
# so its test of a term would no longer be of the model the formula describes.
#' @keywords internal
#' @noRd
.offset_rows <- function(read, target, keep = FALSE) {
  n <- length(read$offsets)
  if (n == 0 || keep) return(.no_record())
  reason <- switch(target,
    mrqap = "fixes a coefficient, which an MRQAP does not do",
    paste0("is an offset for ", .model_label(if (read$input == "rem") "dynam" else read$input),
           ", which is not yet written for ", .model_label(target)))
  .record_row(read$offsets, rep("", n), rep("drops", n), rep(reason, n))
}

# goldfish's older formulas name a node attribute with its table of nodes, as
# in `ego(actors$floor)`. The network holds its nodes' attributes, so the
# argument `x`, which names a node attribute in every term, is read as the
# column alone. `renamed` maps each rewritten label back to the label as written.
#' @keywords internal
#' @noRd
.read_node_tables <- function(read) {
  if (is.null(read$renamed)) read$renamed <- character(0)
  registry <- .formula_terms()
  for (k in seq_along(read$labels)) {
    lab <- read$labels[k]
    ops <- lapply(.flatten_interaction(str2lang(lab)), function(expr) {
      if (!is.call(expr) || !is.name(expr[[1]])) return(expr)
      entry <- registry[[as.character(expr[[1]])]]
      if (is.null(entry) || !"x" %in% names(entry$formals)) return(expr)
      matched <- tryCatch(match.call(as.function(c(entry$formals, list(NULL))), expr),
                          error = function(e) NULL)
      x <- if (!is.null(matched)) matched$x
      if (!(is.call(x) && identical(x[[1]], as.name("$")))) return(expr)
      for (i in seq_along(expr)[-1]) {
        if (identical(expr[[i]], x)) expr[[i]] <- x[[3]]
      }
      expr
    })
    new <- .deparsed(Reduce(function(a, b) call(":", a, b), ops))
    if (!identical(new, lab)) {
      read$labels[k] <- new
      written <- if (lab %in% names(read$renamed)) read$renamed[[lab]] else lab
      read$renamed[[new]] <- written
      read$record <- read$record[read$record$term != written, , drop = FALSE]
      read$record <- rbind(read$record, .record_row(
        written, new, "adapted", "since the network holds its nodes' attributes"))
    }
  }
  read
}

# goldfish's `indeg()` and `outdeg()` name the side with `type =`, and both
# read the receiver by default, where infernet's `outdeg()` reads the sender.
# A formula written for goldfish is read as goldfish reads it, with `node =`
# made explicit, and a warning where `outdeg()` relies on goldfish's default.
# `renamed` maps each rewritten label back to the label as written.
#' @keywords internal
#' @noRd
.read_dynam_degree <- function(read) {
  read$renamed <- character(0)
  if (!read$input %in% c("dynam", "rem")) return(read)
  defaulted <- character(0)
  for (k in seq_along(read$labels)) {
    lab <- read$labels[k]
    ops <- lapply(.flatten_interaction(str2lang(lab)), function(expr) {
      if (!is.call(expr) || !as.character(expr[[1]]) %in% c("indeg", "outdeg")) {
        return(expr)
      }
      op <- .specify_operand(expr)
      if (is.null(op$args$layer)) return(expr)
      node <- if ("node" %in% op$supplied) op$args$node
        else if ("type" %in% op$supplied) op$args$type
        else "alter"
      if (op$fn == "outdeg" && !any(c("node", "type") %in% op$supplied)) {
        defaulted <<- c(defaulted, .deparsed(expr))
      }
      args <- list(op$args$layer, node = .arg_name(node))
      if ("weighted" %in% op$supplied) args$weighted <- op$args$weighted
      as.call(c(as.name(op$fn), args))
    })
    new <- .deparsed(Reduce(function(a, b) call(":", a, b), ops))
    if (!identical(new, lab)) {
      read$labels[k] <- new
      read$renamed[[new]] <- lab
      read$record <- rbind(read$record, .record_row(lab, new, "adapted", ""))
    }
  }
  defaulted <- unique(defaulted)
  if (length(defaulted) > 0) {
    manynet::snet_warn(
      c("{.code {defaulted}} read{?s/} the receiver's outdegree, as goldfish's {.fn outdeg} does.",
        i = "infernet's {.fn outdeg} reads the sender's outdegree by default; write {.code node = \"ego\"} for it."))
  }
  read
}

# The names a bare name could be, and the outcome's own layer. Without a
# network, every bare name that is a term is read as that term.
#' @keywords internal
#' @noRd
.specify_context <- function(.data, lhs) {
  if (is.null(.data)) {
    return(list(tie_names = character(0), outcome_layer = NA_character_,
                panel = FALSE))
  }
  net <- manynet::as_tidygraph(.data)
  layers <- .layer_names(net)
  layer <- NA_character_
  if (lhs %in% layers) {
    layer <- lhs
  } else if (lhs == ".") {
    focal <- tryCatch(manynet::as_infolist(net)$focal, error = function(e) NULL)
    if (length(focal) == 1 && !is.na(focal) && focal %in% layers) layer <- focal
  }
  list(tie_names = c(manynet::net_tie_attributes(net), layers),
       outcome_layer = .outcome_ties(layer, lhs, net))
}

#' @keywords internal
#' @noRd
.formula_inputs <- function() c("mrqap", "dynam", "rem", "ergm")

# The model a formula was written for: as stated, from its class, or from a
# name that only one model has. A name that is also a layer is the layer.
#' @keywords internal
#' @noRd
.read_input <- function(formula, input, ctx) {
  inputs <- .formula_inputs()
  if (!is.null(input)) {
    if (!(length(input) == 1 && input %in% inputs)) {
      manynet::snet_abort("{.arg input} must be one of {.val {inputs}}.")
    }
    return(input)
  }
  classed <- sub("_formula$", "", grep("_formula$", class(formula), value = TRUE))
  classed <- intersect(classed, inputs)
  if (length(classed) > 0) return(classed[1])
  used <- setdiff(.formula_names(formula[[3]]), ctx$tie_names)
  ergm_only <- intersect(used, setdiff(names(.ergm_terms()), names(.formula_terms())))
  dynam_only <- intersect(used, names(.dynam_terms()))
  # On a panel, a term that reads the previous wave is an MRQAP term too.
  if (isTRUE(ctx$panel)) {
    lagged <- names(Filter(function(e) isTRUE(e$panel), .formula_terms()))
    dynam_only <- setdiff(dynam_only, lagged)
  }
  input <- if (length(ergm_only)) "ergm" else if (length(dynam_only)) "dynam" else "mrqap"
  if (input != "mrqap") {
    clue <- c(ergm_only, dynam_only)[1]
    manynet::snet_info(
      "Reading the formula as one written for {.val {input}}, from {.code {clue}}.")
  }
  input
}

# The names of the functions and bare names on a right-hand side, other than
# the operators and the arguments of a term.
#' @keywords internal
#' @noRd
.formula_names <- function(expr) {
  if (is.name(expr)) return(as.character(expr))
  if (!is.call(expr)) return(character(0))
  fn <- as.character(expr[[1]])
  if (fn %in% c("+", ":", "*", "(", "-")) {
    return(unlist(lapply(as.list(expr)[-1], .formula_names)))
  }
  if (fn == "|") return(character(0))
  fn
}

# The left-hand side of an ergm formula names the network rather than a layer.
# With the network, a name that is no layer or tie attribute of it becomes `.`.
#' @keywords internal
#' @noRd
.network_lhs <- function(read) {
  lhs_name <- .deparsed(read$lhs)
  if (read$input == "ergm" && !is.null(read$data) && lhs_name != "." &&
      !(lhs_name %in% read$ctx$tie_names)) {
    return(as.name("."))
  }
  read$lhs
}

#' @keywords internal
#' @noRd
.deparsed <- function(expr) paste(deparse(expr, width.cutoff = 500L), collapse = "")

# ---- the record of each term ----------------------------------------------------------

#' @keywords internal
#' @noRd
.no_record <- function() {
  data.frame(term = character(0), as = character(0), status = character(0),
             reason = character(0))
}

#' @keywords internal
#' @noRd
.record_row <- function(term, as, status, reason) {
  data.frame(term = term, as = as, status = status, reason = reason)
}

#' @keywords internal
#' @noRd
.model_label <- function(target) {
  c(mrqap = "an MRQAP", dynam = "goldfish", ergm = "ergm")[[target]]
}

# Every term of the formula has a row in the record. A term that was not
# rewritten, interpreted differently, or dropped is kept as it is, and fits;
# for an MRQAP it warns or is refused where it reads the outcome's own ties.
# The rows follow the order of the formula, with the outcome first.
#' @keywords internal
#' @noRd
.complete_record <- function(record, read, target) {
  original <- read$original
  bars <- grepl("|", original, fixed = TRUE)
  original[bars] <- paste0("(", original[bars], ")")
  kept <- setdiff(original, record$term)
  rows <- lapply(kept, function(lab) {
    if (target == "mrqap" && !startsWith(lab, "(")) .mrqap_status(lab, read)
    else list(status = "fits", reason = "")
  })
  record <- rbind(record, .record_row(
    kept, kept, vapply(rows, `[[`, "", "status"), vapply(rows, `[[`, "", "reason")))
  lhs <- .deparsed(read$lhs)
  key <- vapply(record$term, function(term) {
    if (identical(term, lhs)) return(0)
    hit <- which(original == term)
    if (length(hit) == 0) hit <- which(grepl(term, original, fixed = TRUE))
    if (length(hit) > 0) hit[1] else length(original) + 1
  }, numeric(1), USE.NAMES = FALSE)
  record <- record[order(key), , drop = FALSE]
  rownames(record) <- NULL
  record
}

# What an MRQAP makes of a term it keeps. A layer term that reads the outcome,
# by naming it or by standing alone as `recip` does, warns or is refused as the
# registry's `outcome` says; another term warns where its registry status does.
#' @keywords internal
#' @noRd
.mrqap_status <- function(label, read) {
  registry <- .formula_terms()
  outcome <- c(read$ctx$outcome_layer, .deparsed(read$lhs))
  outcome <- outcome[!is.na(outcome)]
  out <- list(status = "fits", reason = "")
  for (expr in .flatten_interaction(str2lang(label))) {
    fn <- if (is.call(expr)) as.character(expr[[1]]) else as.character(expr)
    entry <- registry[[fn]]
    if (is.null(entry)) next
    if (is.name(expr) && (fn %in% read$ctx$tie_names || !isTRUE(entry$bare))) next
    if (!is.null(entry$outcome)) {
      layer <- if (is.call(expr) && length(expr) > 1) {
        .arg_name(.specify_operand(expr)$args[[1]])
      } else NA_character_
      reads_outcome <- if (is.na(layer)) isTRUE(entry$bare) else layer %in% outcome
      # On a panel, a bare term reads the previous wave, not the outcome itself.
      if (reads_outcome && is.na(layer) && isTRUE(entry$panel) &&
          isTRUE(read$ctx$panel)) next
      if (reads_outcome && entry$outcome == "refuse") {
        return(list(status = "refuses",
                    reason = "would predict the outcome's ties from those same ties"))
      }
      if (reads_outcome) out <- list(status = "warns", reason = .outcome_reason())
      next
    }
    if (identical(entry$mrqap, "warns")) out <- list(status = "warns", reason = entry$reason)
  }
  out
}

# Aborts where nothing is left to fit, reports each change, and returns the
# formula with its model as its class.
#' @keywords internal
#' @noRd
.finish_formula <- function(lhs, labels, bars, record, read, target,
                            env = read$env, fixed = character(0)) {
  model <- .model_label(target)
  record <- .complete_record(record, read, target)
  if (length(labels) == 0) {
    lost <- record[record$status == "drops" |
                      (record$status == "adapted" & !nzchar(record$as)), ]
    manynet::snet_abort(
      c("No term of the formula can be fitted by {model}.",
        .dropped_bullets(data.frame(label = lost$term, reason = lost$reason), "x"),
        i = "Add a term that {model} fits, such as {.code ego(age)}."))
  }
  .report_record(record, read$input, model)
  rhs <- str2lang(paste(c(labels, if (length(bars)) paste0("(", bars, ")")),
                        collapse = " + "))
  if (length(fixed) > 0) {
    rhs <- call("|", rhs, str2lang(paste(fixed, collapse = " + ")))
  }
  out <- eval(call("~", lhs, rhs))
  environment(out) <- env
  structure(out, class = c(paste0(target, "_formula"), "formula"),
            input = read$input, record = record)
}

# The record of the fixed effects after a bare bar, one row for each group.
# An MRQAP fits them. goldfish has none. ergm's `sender` and `receiver` terms
# are fixed effects for each node, so the two are the same model there; ergm
# has no reporter or network to absorb.
#' @keywords internal
#' @noRd
.fixed_rows <- function(fixed, target) {
  if (length(fixed) == 0) return(.no_record())
  term <- paste("|", fixed)
  switch(target,
    mrqap = .record_row(term, term, rep("fits", length(fixed)), rep("", length(fixed))),
    dynam = .record_row(term, "", rep("drops", length(fixed)),
                        rep("is a fixed effect, which goldfish does not fit", length(fixed))),
    ergm = {
      node <- fixed %in% c("sender", "receiver")
      .record_row(term, ifelse(node, fixed, ""), ifelse(node, "adapted", "drops"),
                  ifelse(node, "is ergm's fixed effect for each node",
                         "is a fixed effect, which ergm does not fit"))
    })
}

# Rewritten and differently read terms are information, one line each, since
# `snet_info()` gives one line for each call; a dropped term is a warning,
# since the model is then not the one the formula describes.
#' @keywords internal
#' @noRd
.report_record <- function(record, input, model) {
  for (k in seq_len(nrow(record))) {
    term <- .escape(record$term[k])
    as <- .escape(record$as[k])
    reason <- record$reason[k]
    why <- if (nzchar(reason)) paste0(", ", reason) else ""
    switch(record$status[k],
      adapted = if (nzchar(as)) {
        manynet::snet_info(paste0("Writing {.code {term}} as {.code {as}} for {model}", why, "."))
      } else {
        manynet::snet_info("Leaving out {.code {term}}, which {reason}.")
      },
      interpreted = manynet::snet_info("{.code {term}} {reason}."))
  }
  dropped <- record[record$status == "drops", ]
  .warn_dropped(data.frame(label = dropped$term, reason = dropped$reason), model)
}

#' @keywords internal
#' @noRd
.escape <- function(x) gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)

# The terms both packages have, but define differently, named where a formula
# moves between goldfish and the others.
#' @keywords internal
#' @noRd
.interpreted_dynam <- function(labels, input, target) {
  if ((input %in% c("dynam", "rem")) == (target == "dynam")) return(.no_record())
  differs <- c(
    sim          = "is 1 - |d| / range in infernet, and -|d| in goldfish, so its coefficient is on another scale, though its test is the same",
    tertius      = "leaves out the sender in infernet, and includes it in goldfish",
    tertius_diff = "leaves out the sender in infernet, and includes it in goldfish")
  out <- .no_record()
  for (lab in labels) {
    for (expr in .flatten_interaction(str2lang(lab))) {
      fn <- if (is.call(expr)) as.character(expr[[1]]) else ""
      if (fn %in% names(differs)) {
        term <- .deparsed(expr)
        out <- rbind(out, .record_row(term, term, "interpreted", differs[[fn]]))
      }
    }
  }
  unique(out)
}

# ---- writing for an MRQAP -------------------------------------------------------------

# A term that another model fits, but an MRQAP cannot, is dropped, and so is an
# interaction with one. goldfish reads the dependent events where no layer is
# named, and the named layer otherwise, which is then not the outcome's ties.
#' @keywords internal
#' @noRd
.drop_labels <- function(labels, ctx) {
  registry <- .formula_terms()
  reasons <- vapply(labels, function(lab) {
    for (expr in .flatten_interaction(str2lang(lab))) {
      fn <- if (is.call(expr)) as.character(expr[[1]]) else as.character(expr)
      entry <- registry[[fn]]
      if (is.null(entry)) next
      if (is.name(expr) && (fn %in% ctx$tie_names || !isTRUE(entry$bare))) next
      # goldfish's `~ indeg` reads the outcome's own degree. Naming the outcome,
      # `indeg(calls)`, is refused, but goldfish's default is dropped.
      if (identical(entry$outcome, "refuse") && isTRUE(entry$bare) &&
          (is.name(expr) || length(expr) == 1)) {
        return(.endogenous_reason())
      }
      if (entry$mrqap != "drops") next
      if (identical(entry$kind, "endogenous") && is.call(expr) && length(expr) > 1) {
        layer <- .arg_name(.specify_operand(expr)$args[[1]])
        if (!is.na(layer) && !identical(layer, ctx$outcome_layer)) {
          return("reads another layer, which is not yet built here")
        }
      }
      if (isTRUE(entry$panel) && isTRUE(ctx$panel)) next
      return(entry$reason)
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)
  dropped <- !is.na(reasons)
  list(labels = labels[!dropped],
       record = .record_row(labels[dropped], rep("", sum(dropped)),
                         rep("drops", sum(dropped)), reasons[dropped]))
}

# ---- writing for goldfish ----------------------------------------------------------------

# One operand written for goldfish: as it is where goldfish has the term, and
# otherwise its goldfish equivalent, or the reason it has none. A bare name
# that is no term is a layer, which goldfish names with `tie()`.
#' @keywords internal
#' @noRd
.dynam_operand <- function(expr, registry, ctx, input = "mrqap") {
  if (is.name(expr)) {
    name <- as.character(expr)
    entry <- registry[[name]]
    if (name %in% ctx$tie_names || is.null(entry) || !isTRUE(entry$bare)) {
      return(list(expr = call("tie", expr, weighted = TRUE)))
    }
    if (isTRUE(entry$css)) return(.dynam_operand(call(name), registry, ctx, input))
    return(list(expr = expr))
  }
  fn <- as.character(expr[[1]])
  entry <- registry[[fn]]
  if (is.null(entry)) return(list(expr = expr))
  switch(fn,
    indeg    = ,
    outdeg   = .dynam_degree(expr, input),
    exchange = list(expr = as.call(c(as.name("recip"), as.list(expr)[-1]))),
    dist     = list(expr = as.call(c(as.name("diff"), as.list(expr)[-1]))),
    same     = if (.same_by_category(.specify_operand(expr),
                                     if (is.null(ctx$env)) globalenv() else ctx$env)) {
      list(reason = "has no goldfish term with {.code diff = TRUE}")
    } else if (!is.null(.specify_operand(expr)$args$levels)) {
      list(reason = "has no goldfish term with {.code levels =} yet (stocnet/goldfish#26)")
    } else {
      list(expr = expr)
    },
    ego      = ,
    alter    = if (!is.null(.specify_operand(expr)$args$levels)) {
      list(reason = "has no goldfish term with {.code levels =} yet (stocnet/goldfish#26)")
    } else {
      list(expr = expr)
    },
    if (isTRUE(entry$css)) {
      list(reason = "describes the reporter of a cognitive social structure, which goldfish does not have")
    } else if (is.na(entry$dynam)) {
      list(reason = "has no goldfish term")
    } else {
      list(expr = expr)
    })
}

# goldfish names the side of `indeg()` and `outdeg()` with `type =`, and reads
# the receiver by default, so only the sender's side is written. A formula
# written for goldfish keeps its terms as they are.
#' @keywords internal
#' @noRd
.dynam_degree <- function(expr, input) {
  op <- .specify_operand(expr)
  if (input %in% c("dynam", "rem") || is.null(op$args$layer)) return(list(expr = expr))
  node <- .degree_node(op)
  args <- list(op$args$layer)
  if (node == "ego") args$type <- "ego"
  if ("weighted" %in% op$supplied) args$weighted <- op$args$weighted
  note <- if (op$fn == "outdeg" && node == "ego" && !any(c("node", "type") %in% op$supplied)) {
    "since goldfish's outdeg() reads the receiver by default"
  }
  list(expr = as.call(c(as.name(op$fn), args)), note = note)
}

# ergm's structural terms that goldfish has, by goldfish's name.
#' @keywords internal
#' @noRd
.dynam_from_ergm <- function(label) {
  expr <- str2lang(label)
  if (is.name(expr)) {
    map <- c(ttriple = "trans", ctriple = "cycle", triangle = "triangle")
    name <- as.character(expr)
    return(if (name %in% names(map)) map[[name]] else NULL)
  }
  if (is.call(expr) && identical(expr[[1]], as.name("cycle")) &&
      length(expr) == 2 && isTRUE(expr[[2]] == 4)) {
    return("four")
  }
  NULL
}

# ---- writing for ergm --------------------------------------------------------------------

# Without the network, each term ergm names is written as its ergm term, for a
# directed network and a numeric attribute; a term that ergm reads as
# `edgecov()` of a matrix is dropped, since the matrix cannot be built.
#' @keywords internal
#' @noRd
.ergm_term_nodata <- function(term) {
  for (op in term$operands) {
    reason <- if (!is.na(op$fn)) .ergm_reason(op)
    if (!is.null(reason)) return(list(label = term$label, reason = reason))
  }
  one <- function(op) {
    if (is.na(op$fn)) return(.ergm_call("edgecov", op$name))
    # `levels =` is written as it is given, and ergm reads names and positions
    # as infernet does. Only `mix()` needs the data to read a pair's name.
    lv <- if (!is.null(op$args$levels)) paste("=", .deparsed(op$args$levels))
    named <- !is.null(lv) && is.character(tryCatch(eval(op$args$levels, globalenv()),
                                                   error = function(e) NULL))
    switch(op$fn,
      ego   = if (is.null(lv)) .ergm_call("nodeocov", op$name)
              else .ergm_call("nodeofactor", op$name, paste0("levels ", lv)),
      alter = if (is.null(lv)) .ergm_call("nodeicov", op$name)
              else .ergm_call("nodeifactor", op$name, paste0("levels ", lv)),
      same  = .ergm_call("nodematch", op$name,
                         if (.same_by_category(op, globalenv())) "diff = TRUE",
                         if (!is.null(lv)) paste0("levels ", lv)),
      match = .ergm_call("nodematch", op$name, "diff = TRUE"),
      mix   = if (is.null(lv)) .ergm_call("nodemix", op$name)
              else if (!named) .ergm_call("nodemix", op$name, paste0("levels2 ", lv)),
      dist  = .ergm_call("absdiff", op$name),
      diff  = switch(.arg_name(op$args$transformer_fn),
                     abs = .ergm_call("absdiff", op$name),
                     identity = .ergm_call("diff", op$name),
                     NULL),
      tie   = .ergm_call("edgecov", op$name),
      recip = if (is.na(op$name)) "mutual",
      trans = if (is.na(op$name)) "ttriple",
      cycle = if (is.na(op$name)) "ctriple",
      four  = if (is.na(op$name)) "cycle(4)",
      triangle = if (is.na(op$name)) "triangle",
      NULL)
  }
  parts <- lapply(term$operands, one)
  if (any(vapply(parts, is.null, logical(1)))) {
    return(list(label = term$label,
                reason = "needs .data to build its matrix for edgecov()"))
  }
  structural <- c("mutual", "ttriple", "ctriple", "cycle(4)", "triangle")
  if (length(parts) > 1 && any(unlist(parts) %in% structural)) {
    return(list(label = term$label,
                reason = "reads the outcome's own ties, so it has no fixed matrix to interact"))
  }
  list(label = term$label, terms = paste(unlist(parts), collapse = ":"))
}

# ---- reading ergm ------------------------------------------------------------------------

# Each ergm term that has an infernet equivalent: a function of the term's
# call that returns the infernet terms it becomes, or an empty list for the
# intercept. A term without one, such as `gwesp()`, gives the reason an MRQAP
# cannot fit it, and is kept for ergm. ergm names an attribute with a string,
# and infernet with a bare name.
#' @keywords internal
#' @noRd
.ergm_terms <- function() {
  attr_of <- function(expr) {
    as.name(.arg_name(if (is.call(expr) && length(expr) > 1) expr[[2]]))
  }
  as_term <- function(fn) function(expr) list(call(fn, attr_of(expr)))
  # ergm's `levels =` chooses the categories that get a statistic, as infernet's
  # does. infernet reads names and positions, and not ergm's functions, such as
  # `LARGEST`, so a term that uses one is ergm's alone.
  levels_reason <- function(expr, from) {
    lv <- if (is.call(expr)) expr[[from]]
    if (is.null(lv)) return(NULL)
    value <- tryCatch(eval(lv, baseenv()), error = function(e) NULL)
    if ((is.character(value) || is.numeric(value)) && length(value) > 0) return(NULL)
    paste0("chooses its levels with {.code ", .escape_braces(.deparsed(lv)),
           "}, and infernet reads only names and positions")
  }
  with_levels <- function(new, expr, from = "levels") {
    lv <- if (is.call(expr)) expr[[from]]
    if (!is.null(lv)) new$levels <- lv
    new
  }
  factor_term <- function(fns) function(expr) {
    reason <- levels_reason(expr, "levels")
    if (!is.null(reason)) return(reason)
    lapply(fns, function(fn) with_levels(call(fn, attr_of(expr)), expr))
  }
  both_sides <- function(expr) list(call("ego", attr_of(expr)), call("alter", attr_of(expr)))
  intercept <- function(expr) list()
  structural <- c(
    "asymmetric", "balance", "b1degree", "b2degree", "b1star", "b2star",
    "concurrent", "ctriple", "cycle", "cyclicalties", "ddsp", "degree",
    "desp", "dgwdsp", "dgwesp", "dgwnsp", "dnsp", "dsp", "esp", "gwb1degree",
    "gwb2degree", "gwdegree", "gwdsp", "gwesp", "gwidegree", "gwnsp",
    "gwodegree", "idegree", "intransitive", "isolates", "istar", "kstar",
    "localtriangle", "m2star", "meandeg", "nsp", "odegree", "ostar",
    "transitive", "transitiveties", "triadcensus", "triangle", "ttriple",
    "twopath")
  fixed <- c("sender", "receiver", "sociality")
  read <- list(
    edges       = intercept,
    density     = intercept,
    mutual      = function(expr) list(quote(recip)),
    nodeocov    = as_term("ego"),
    nodeofactor = factor_term("ego"),
    b1cov       = as_term("ego"),
    b1factor    = factor_term("ego"),
    nodeicov    = as_term("alter"),
    nodeifactor = factor_term("alter"),
    b2cov       = as_term("alter"),
    b2factor    = factor_term("alter"),
    nodecov     = both_sides,
    nodefactor  = factor_term(c("ego", "alter")),
    nodematch   = function(expr) {
      reason <- levels_reason(expr, "levels")
      if (!is.null(reason)) return(reason)
      diff <- is.call(expr) && !is.null(expr$diff) && isTRUE(eval(expr$diff))
      new <- if (diff) call("same", attr_of(expr), diff = TRUE)
        else call("same", attr_of(expr))
      list(with_levels(new, expr))
    },
    # `levels2 =` chooses pairs by their position, as infernet's `levels =`
    # does for `mix()`. ergm's `levels =` first chooses the categories, which
    # changes those positions, so infernet leaves that to ergm.
    nodemix     = function(expr) {
      if (is.call(expr) && !is.null(expr[["levels"]])) {
        return("chooses its categories with {.code levels =}; choose pairs with {.code levels2 =} instead")
      }
      reason <- levels_reason(expr, "levels2")
      if (!is.null(reason)) return(reason)
      list(with_levels(call("mix", attr_of(expr)), expr, "levels2"))
    },
    absdiff     = as_term("diff"),
    diff        = function(expr) {
      list(call("diff", attr_of(expr), transformer_fn = quote(identity)))
    },
    edgecov     = function(expr) list(attr_of(expr))
  )
  c(read,
    stats::setNames(rep(list(.endogenous_reason()), length(structural)),
                    structural),
    stats::setNames(rep(list("is a fixed effect for each node; (1 | sender) adds a random intercept instead"),
                        length(fixed)), fixed))
}

# Why an MRQAP cannot fit an ergm term that has no infernet equivalent.
#' @keywords internal
#' @noRd
.ergm_native_reason <- function(label) {
  ergm <- .ergm_terms()
  for (expr in .flatten_interaction(str2lang(label))) {
    fn <- if (is.call(expr)) as.character(expr[[1]]) else as.character(expr)
    reader <- ergm[[fn]]
    if (is.function(reader)) reader <- reader(expr)
    if (is.character(reader)) return(reader)
  }
  "has no MRQAP equivalent"
}

# Rewrites each ergm term as the infernet terms it becomes. A term that becomes
# two, such as `nodecov()`, can stand alone but not in an interaction. A term
# without an equivalent stays as it is, and is listed in `native`. A name
# that ergm does not have is left for the writer to read.
#' @keywords internal
#' @noRd
.read_ergm_terms <- function(labels, ctx) {
  ergm <- .ergm_terms()
  out <- character(0)
  native <- character(0)
  record <- .no_record()
  for (lab in labels) {
    ops <- .flatten_interaction(str2lang(lab))
    parts <- list()
    is_native <- FALSE
    for (expr in ops) {
      fn <- if (is.call(expr)) as.character(expr[[1]]) else as.character(expr)
      reader <- ergm[[fn]]
      if (is.null(reader) || (is.name(expr) && fn %in% ctx$tie_names)) {
        parts <- c(parts, list(list(expr)))
      } else {
        # A reader gives the reason, rather than terms, where this call of an
        # ergm term has no equivalent, as with `nodematch(levels = LARGEST)`.
        read <- if (is.character(reader)) reader else reader(expr)
        if (is.character(read)) {
          is_native <- TRUE
          break
        }
        parts <- c(parts, list(read))
      }
    }
    if (is_native) {
      native <- c(native, lab)
      out <- c(out, lab)
      next
    }
    sizes <- lengths(parts)
    reason <- if (any(sizes == 0) && length(ops) > 1) {
      "interacts with the intercept"
    } else if (any(sizes > 1) && length(ops) > 1) {
      "becomes two terms, which cannot be interacted"
    } else NA_character_
    if (!is.na(reason)) {
      record <- rbind(record, .record_row(lab, "", "drops", reason))
      next
    }
    if (any(sizes == 0)) {
      record <- rbind(record, .record_row(lab, "", "adapted",
                                        "is the intercept that the model always has"))
      next
    }
    new <- if (length(ops) == 1) {
      vapply(parts[[1]], .deparsed, "")
    } else {
      .deparsed(Reduce(function(a, b) call(":", a, b), lapply(parts, `[[`, 1)))
    }
    if (!identical(new, lab)) {
      record <- rbind(record, .record_row(lab, paste(new, collapse = " + "), "adapted", ""))
    }
    out <- c(out, new)
  }
  list(labels = unique(out), native = native, record = record)
}
