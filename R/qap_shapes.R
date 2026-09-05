# Data shapes -----------------------------------------------------------------
#
# One engine fits both a dyadic network and a cognitive social structure. The
# two differ in four places and nowhere else: how a network becomes one row per
# observation, how a permutation is drawn, how a residualised predictor is put
# back, and which random intercepts exist. A shape holds those four, so that
# `QAPengine()` can be written once.
#
# Before this, the two were separate 200-line functions that were 55% the same
# code, and every fix had to be made twice.

#' @keywords internal
#' @noRd
.qap_shape <- function(css = FALSE) {
  if (css) .qap_shape_cognitive() else .qap_shape_dyadic()
}

#' @keywords internal
#' @noRd
.qap_shape_dyadic <- function() {
  list(
    css   = FALSE,
    label = "network",
    ndim  = 2L,
    # A sender and a receiver intercept, and one per network in a pooled list.
    rand_slots = c(nets = "nv", sender = "sv", receiver = "rv"),
    # The dyadic path takes the first draw. A degenerate permutation is dropped
    # by the loop's own `tryCatch()`, and `aggregate_perm_results()` counts it.
    max_trials = 1L,
    vectorise = function(y, x, groups, diag, directed, net) {
      pred <- make_qap_data(y = y, x = x, g = groups, diag = diag,
                            directed = directed, net = net)
      list(pred = pred, valid = NULL)
    },
    permute = function(m, groups) RMPerm(m, groups),
    unresidualise = function(xR, original, pred, large, valid, valid_list) {
      residuals_to_matrix(xR, original, pred, large)
    }
  )
}

#' @keywords internal
#' @noRd
.qap_shape_cognitive <- function() {
  list(
    css   = TRUE,
    label = "cognitive social structure",
    ndim  = 3L,
    # A perceiver intercept as well, since each perceiver reports the whole
    # network and their reports are not independent of each other.
    rand_slots = c(nets = "nv", perceiver = "pv", sender = "sv", receiver = "rv"),
    # A CSS array is sparse, so a permutation can leave an outcome with one
    # value and nothing to fit. Redraw rather than discard: discarding biases
    # the null towards the draws that happened to be dense.
    max_trials = 10000L,
    vectorise = function(y, x, groups, diag, directed, net) {
      make_css_data(y = y, x = x, nets = net, diag = diag, directed = directed)
    },
    permute = function(m, groups) RMPerm(m, groups, CSS = TRUE),
    unresidualise = function(xR, original, pred, large, valid, valid_list) {
      residuals_to_array(xR, original, valid, pred, large, valid_list)
    }
  )
}

# Applies a shape's vectoriser to one network or to a pooled list of them.
# Both engines wrote this loop out twice, once for each shape.
#' @keywords internal
#' @noRd
.vectorise_matlist <- function(shape, matlist, dep, data_vars, groups,
                               diag, directed, large) {
  if (!large) {
    x <- lapply(data_vars, function(v) matlist[[v]])
    names(x) <- data_vars
    out <- shape$vectorise(y = matlist[[dep]], x = x, groups = groups,
                           diag = diag, directed = directed, net = 1)
    return(list(pred = out$pred, valid = out$valid, valid_list = NULL))
  }

  n_nets <- length(matlist[[dep]])
  preds  <- vector("list", n_nets)
  valids <- vector("list", n_nets)
  for (net in seq_len(n_nets)) {
    x <- lapply(data_vars, function(v) matlist[[v]][[net]])
    names(x) <- data_vars
    g <- if (!is.null(groups) && is.list(groups)) groups[[net]] else groups
    out <- shape$vectorise(y = matlist[[dep]][[net]], x = x, groups = g,
                           diag = diag, directed = directed, net = net)
    preds[[net]]  <- out$pred
    valids[[net]] <- out$valid
  }
  list(pred = do.call(rbind, preds), valid = NULL, valid_list = valids)
}

# Whether a permuted sample can be fitted at all: the outcome must vary, and so
# must every numeric predictor. A constant column has no slope to estimate.
#' @keywords internal
#' @noRd
.sufficient_data <- function(pred, dep, data_vars) {
  if (length(stats::na.omit(unique(pred[[dep]]))) <= 1) return(FALSE)
  numeric_preds <- pred[, intersect(data_vars, names(pred)), drop = FALSE]
  numeric_preds <- numeric_preds[, vapply(numeric_preds, is.numeric, logical(1)),
                                 drop = FALSE]
  if (ncol(numeric_preds) == 0) return(TRUE)
  all(vapply(numeric_preds, function(col) length(unique(col)) > 1, logical(1)))
}
