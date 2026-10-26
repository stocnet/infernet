# Fixed effects -----------------------------------------------------------------
#
# Internal. A bar outside parentheses, `y ~ x | sender + receiver`, absorbs a
# fixed effect for each sender and each receiver, as in {fixest}
# (stocnet/infernet#8). Fitting a dummy for each node takes about an hour for a
# thousand permutations of a 200-node network, so the effects are absorbed
# rather than estimated, and two ways do that.
#
# A gaussian model is demeaned once. A node permutation maps senders onto
# senders and receivers onto receivers, and the permutations fit one fixed set
# of dyads (see `.missing_blocks()`), so a matrix whose sender and receiver
# means are zero still has zero means after any draw. By the Frisch-Waugh-Lovell
# theorem, the slopes of a model with the fixed effects are then the slopes of
# a plain model of the demeaned variables, so each permutation fits a plain
# `lm()` and needs no {fixest}.
#
# Logit, Poisson, and negative binomial models have no such shortcut, so each
# permutation is fitted with `fixest::feglm()` or `fixest::fenegbin()`. Double
# semi-partialling residualises a predictor linearly whatever the family, so
# that step still demeans.

# The columns of the dyad-level data that hold each group, as {fixest} reads
# them. In a pooled list a node is a different node in each network, so its
# group is the pair of the two, which {fixest} writes `nv^sv`. The waves of a
# panel are the same nodes observed again, and one draw relabels them the same
# way in every wave, so a node keeps one group across the waves, and the
# network effect is one for each wave.
#' @keywords internal
#' @noRd
.fixed_columns <- function(fixed, large = FALSE, panel = FALSE) {
  cols <- c(sender = "sv", receiver = "rv", reporter = "bv", network = "nv")[fixed]
  if (large && !panel)
    cols[fixed != "network"] <- paste0("nv^", cols[fixed != "network"])
  unname(cols)
}

# Which of the requested fixed effects the model can have, and whether the rest
# of the model allows them. An undirected network drops its sender and receiver
# effects, as it does its random intercepts. A two-mode network keeps them,
# since its rows and its columns are different nodes whether or not it is read
# as directed.
#' @keywords internal
#' @noRd
.check_fixed <- function(fixed, shape, directed, large, has_random, family,
                         use_robust_errors, twomode = FALSE) {
  if (length(fixed) == 0) return(character(0))
  if (!shape$css && "reporter" %in% fixed) {
    label <- shape$label
    manynet::snet_abort(
      c("A {label} has no reporters, so it has no reporter fixed effect.",
        i = "A reporter fixed effect is for a cognitive social structure."))
  }
  if (!large && "network" %in% fixed) {
    manynet::snet_abort(
      c("One network has no network fixed effect: it is the intercept.",
        i = "A network fixed effect is for a list of networks."))
  }
  if (!directed && !twomode && any(c("sender", "receiver") %in% fixed)) {
    manynet::snet_warn(
      c("An undirected network has no senders or receivers.",
        i = "Dropping the sender and receiver fixed effects."))
    fixed <- setdiff(fixed, c("sender", "receiver"))
    if (length(fixed) == 0) return(character(0))
  }
  if (has_random) {
    manynet::snet_abort(
      c("A model cannot yet have both fixed effects and random intercepts.",
        i = "Keep the fixed effects after the bar, or the random intercepts in parentheses."))
  }
  if (use_robust_errors) {
    manynet::snet_abort(
      c("Robust standard errors are not yet available with fixed effects.",
        i = "Set {.code use_robust_errors = FALSE}, or drop the fixed effects."))
  }
  if (family == "zip") {
    manynet::snet_abort(
      c("Fixed effects are not available for the {.val zip} family.",
        i = "Drop the fixed effects, or use {.val poisson}."))
  }
  fixed
}

# Each group as consecutive integers, one vector for each fixed effect.
#' @keywords internal
#' @noRd
.fixed_ids <- function(pred, cols) {
  lapply(strsplit(cols, "^", fixed = TRUE), function(parts) {
    as.integer(interaction(pred[parts], drop = TRUE))
  })
}

# The columns of `v` less their means in every group of every fixed effect, by
# alternating projections: subtract each group's means in turn, until no mean
# is left. One fixed effect takes one pass.
#' @keywords internal
#' @noRd
.demean <- function(v, ids, tol = 1e-10, max_iter = 10000L) {
  v <- as.matrix(v)
  counts <- lapply(ids, tabulate)
  scale <- max(1, abs(v))
  for (iter in seq_len(max_iter)) {
    shift <- 0
    for (k in seq_along(ids)) {
      means <- rowsum(v, ids[[k]], reorder = TRUE) / counts[[k]]
      v <- v - means[ids[[k]], , drop = FALSE]
      shift <- max(shift, abs(means))
    }
    if (length(ids) == 1 || shift < tol * scale) return(v)
  }
  manynet::snet_warn(
    "Demeaning by the fixed effects did not converge after {max_iter} rounds.")
  v
}

# The number of parameters the fixed effects absorb, which the residual degrees
# of freedom lose. Two crossed effects lose one level for each connected set of
# their groups after the first, as a sender and a receiver that never share a
# dyad would. A third effect, as in a cognitive social structure, is taken to
# be connected to the others.
#' @keywords internal
#' @noRd
.fixed_rank <- function(ids) {
  levels <- vapply(ids, max, numeric(1))
  if (length(ids) == 1) return(levels)
  sum(levels) - (length(ids) - 1) - (.components(ids[[1]], ids[[2]]) - 1)
}

# The number of connected sets in the bipartite graph of two fixed effects,
# with an edge between the groups of each observation.
#' @keywords internal
#' @noRd
.components <- function(a, b) {
  b <- b + max(a)
  label <- seq_len(max(b))
  repeat {
    m <- pmin(label[a], label[b])
    new <- label
    low_a <- vapply(split(m, a), min, numeric(1))
    low_b <- vapply(split(m, b), min, numeric(1))
    new[as.integer(names(low_a))] <- pmin(new[as.integer(names(low_a))], low_a)
    new[as.integer(names(low_b))] <- pmin(new[as.integer(names(low_b))], low_b)
    new <- new[new]
    if (identical(new, label)) break
    label <- new
  }
  length(unique(label))
}

# A gaussian model's fixed effects, absorbed once: the outcome and every
# predictor less its group means, in the dyad-level data and in the matrices
# that the permutations draw from.
#' @keywords internal
#' @noRd
.absorb_fixed <- function(shape, matlist, pred, vec, dep, main, fixed,
                          cols_fe, large, directed) {
  ids <- .fixed_ids(pred, cols_fe)
  cols <- c(dep, main)
  numeric_cols <- vapply(pred[cols], is.numeric, logical(1))
  if (!all(numeric_cols)) {
    manynet::snet_abort(
      c("Cannot absorb fixed effects with the non-numeric predictor{?s} {.val {cols[!numeric_cols]}}.",
        i = "Code it as numeric columns, as {.fn ego} and {.fn same} do."))
  }
  dm <- .demean(as.matrix(pred[cols]), ids)
  .check_absorbed(dm[, main, drop = FALSE], pred[main], fixed)
  for (v in cols) {
    pred[[v]] <- dm[, v]
    matlist[[v]] <- shape$unresidualise(dm[, v], matlist[[v]], pred, large,
                                        vec$valid, vec$valid_list, directed)
  }
  list(pred = pred, matlist = matlist, df = .fixed_rank(ids))
}

# A predictor that does not vary within the groups of a fixed effect, such as
# `ego(age)` with a sender effect, is all absorbed: nothing is left of it to
# estimate. It is named, rather than fitted as an `NA`.
#' @keywords internal
#' @noRd
.check_absorbed <- function(dm, raw, fixed) {
  scale <- vapply(raw, function(v) max(1, abs(v)), numeric(1))
  absorbed <- colnames(dm)[apply(abs(dm), 2, max) < 1e-8 * scale]
  if (length(absorbed) > 0) {
    manynet::snet_abort(
      c("The {.val {fixed}} fixed effect{?s} absorb{?s/} {.val {absorbed}}, which does not vary within {?its/their} groups.",
        i = "Leave it out of the formula: the fixed effect already holds it."))
  }
  invisible()
}

# The fixed effects a result absorbed, in words, for printing.
#' @keywords internal
#' @noRd
.fixed_label <- function(x) {
  if (length(x$fixed_effects) == 0) return(NULL)
  paste0("Fixed effects were absorbed for each ",
         paste(x$fixed_effects, collapse = " and "), ".\n")
}
