# Infernet terms -> ergm terms --------------------------------------------------
#
# `specify_ergm()` in model_specify.R writes each term as its named ergm term
# where ergm has one, and otherwise as `edgecov()` of the matrix infernet
# builds. Each registry entry in `.formula_terms()` says how with its `to_ergm`
# function, next to the `ergm` name it documents. A term whose `ergm` is NA has
# no ergm form, and is dropped.

# One term of the specification: its label, and either the ergm terms and the
# matrices they name, or the reason it has no ergm form. An interaction is
# `edgecov()` of the product infernet builds. For dyad-independent terms this
# is what ergm's `:` counts, and it holds for every term infernet builds.
#' @keywords internal
#' @noRd
.ergm_term <- function(term, ctx) {
  registry <- .formula_terms()
  for (op in term$operands) {
    if (is.na(op$fn)) next
    entry <- registry[[op$fn]]
    reason <- if (identical(entry$kind, "global")) {
      "is a network-level variable, the same for every dyad of the one network that ergm fits"
    } else if (is.na(entry$ergm)) {
      "has no ergm term"
    } else if (is.null(entry$to_ergm)) {
      paste0("is not yet converted here; its nearest ergm term is {.code ",
             .escape_braces(entry$ergm), "}")
    } else if (ctx$twomode && identical(entry$twomode, FALSE)) {
      "is not defined for a two-mode network"
    } else NULL
    if (!is.null(reason)) return(list(label = term$label, reason = reason))
  }
  if (length(term$operands) > 1) {
    endogenous <- vapply(term$operands, function(op) {
      !is.na(op$fn) && identical(registry[[op$fn]]$kind, "endogenous")
    }, logical(1))
    if (any(endogenous)) {
      return(list(label = term$label,
                  reason = "reads the outcome's own ties, so it has no fixed matrix to interact"))
    }
    parts <- lapply(term$operands, function(op) .ergm_quietly(.build_operand(op, ctx)))
    out <- .ergm_edgecov(Reduce(.interact, parts))
  } else {
    op <- term$operands[[1]]
    out <- if (is.na(op$fn)) .ergm_edgecov(.build_covariate(op, ctx))
      else registry[[op$fn]]$to_ergm(op, ctx)
  }
  c(list(label = term$label), out)
}

# `edgecov()` of each matrix infernet builds, named as its column so that the
# ergm coefficients can be matched to the MRQAP ones.
#' @keywords internal
#' @noRd
.ergm_edgecov <- function(mats) {
  names(mats) <- vapply(names(mats), .col_name, "")
  list(terms = sprintf("edgecov(%s)", names(mats)), covariates = mats)
}

# The builders say what they leave out, and warn where a matrix reads the
# outcome's ties, which matters to a permutation test and not here.
#' @keywords internal
#' @noRd
.ergm_quietly <- function(expr) suppressWarnings(suppressMessages(expr))

#' @keywords internal
#' @noRd
.ergm_call <- function(fn, x, ...) {
  paste0(fn, "(", paste(c(deparse(x), ...), collapse = ", "), ")")
}

#' @keywords internal
#' @noRd
.escape_braces <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}

# ---- converters, one for each registry entry ------------------------------------

# The sender's or the receiver's value. A two-mode network reads each side on
# its own mode, as ergm's b1 and b2 terms do. An undirected tie has no sender,
# so both sides are `nodecov()`, the sum of the two values.
.ergm_ego <- function(op, ctx) .ergm_side(op, ctx, "ego")
.ergm_alter <- function(op, ctx) .ergm_side(op, ctx, "alter")

#' @keywords internal
#' @noRd
.ergm_side <- function(op, ctx, side) {
  v <- .node_values(ctx, op$name, side)
  prefix <- if (ctx$twomode) c(ego = "b1", alter = "b2")[[side]]
    else if (ctx$directed) c(ego = "nodeo", alter = "nodei")[[side]]
    else "node"
  suffix <- if (is.character(v) || is.factor(v)) "factor" else "cov"
  list(terms = .ergm_call(paste0(prefix, suffix), op$name))
}

# A two-mode `same()` is the share of a node's ties to nodes like the other,
# which no ergm term counts, so it is the matrix infernet builds.
.ergm_same <- function(op, ctx) {
  if (.same_by_category(op, ctx$env)) {
    if (ctx$twomode) return(list(reason = "is not defined for a two-mode network"))
    return(list(terms = .ergm_call("nodematch", op$name, "diff = TRUE")))
  }
  if (ctx$twomode) return(.ergm_edgecov(.ergm_quietly(.build_same(op, ctx))))
  list(terms = .ergm_call("nodematch", op$name))
}

.ergm_match <- function(op, ctx) {
  list(terms = .ergm_call("nodematch", op$name, "diff = TRUE"))
}

.ergm_mix <- function(op, ctx) list(terms = .ergm_call("nodemix", op$name))

# `abs` is ergm's `absdiff()`, and `identity` is its `diff()`: the tail's value
# less the head's, as infernet takes the sender's less the receiver's. Another
# transformer is the matrix infernet builds.
.ergm_diff <- function(op, ctx) {
  .refuse_categorical(ctx, op)
  f <- .arg_fun(op$args$transformer_fn, ctx$env, "transformer_fn")
  if (identical(f, abs)) return(list(terms = .ergm_call("absdiff", op$name)))
  if (identical(f, identity)) return(list(terms = .ergm_call("diff", op$name)))
  .ergm_edgecov(.build_diff(op, ctx))
}

.ergm_dist <- function(op, ctx) {
  .refuse_categorical(ctx, op)
  list(terms = .ergm_call("absdiff", op$name))
}

# The reverse tie in the outcome is ergm's `mutual`, which counts it rather
# than holding it fixed. In another layer, it is the transpose infernet builds.
.ergm_recip <- function(op, ctx) {
  if (identical(op$fn, "recip") &&
      (is.na(op$name) || identical(op$name, ctx$outcome_layer))) {
    if (!ctx$directed) {
      return(list(reason = "is the outcome's reverse tie, and {.code mutual} needs a directed network"))
    }
    return(list(terms = "mutual"))
  }
  .ergm_built(op, ctx)
}

# A term that ergm has no statistic for: `edgecov()` of the matrix infernet
# builds, which ergm holds fixed.
.ergm_built <- function(op, ctx) .ergm_edgecov(.ergm_quietly(.build_operand(op, ctx)))

# goldfish's endogenous effects read the outcome's own ties, as ergm's
# structural terms do. Where they name another layer, ergm has no such term.
#' @keywords internal
#' @noRd
.ergm_endogenous <- function(directed, undirected = NA_character_) {
  function(op, ctx) {
    if (!is.na(op$name) && !identical(op$name, ctx$outcome_layer)) {
      return(list(reason = "reads another layer, which no ergm term counts"))
    }
    term <- if (ctx$directed) directed else undirected
    if (is.na(term)) return(list(reason = "has no ergm term for an undirected network"))
    list(terms = term)
  }
}
