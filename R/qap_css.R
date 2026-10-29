# Cognitive social structures --------------------------------------------------
#
# Internal. What a CSS needs that a dyadic network does not: a vectoriser for a
# sender-by-receiver-by-reporter array, and a print method. Everything else it
# shares with the dyadic case, and lives in R/qap_engine.R.

# A CSS array has a row for each sender, a column for each receiver, and a
# slice for each reporter, so it is read along all three of its axes. A one-mode
# CSS from `{manynet}` has as many slices as nodes, and a two-mode CSS has a
# slice for every node of both modes, so the third axis cannot be assumed to
# match the first. Reading all three from `dim()` is what keeps a two-mode CSS,
# or one with fewer reporters than nodes, from indexing past an axis.
#
# Every array is vectorised with `[valid]`, in R's column-major order: slice by
# slice, and column by column within a slice. `residuals_to_array()` writes back
# with `out[valid] <- xR`, so the two orders agree by construction.
#' @keywords internal
#' @noRd
make_css_data <- function(y, x, nets, loops, directed) {
  d <- dim(y)
  nr <- d[1]; nc <- d[2]; np <- d[3]
  square <- nr == nc
  nx <- length(x)

  for (var in seq_len(nx)) {
    if (!identical(dim(x[[var]]), d))
      manynet::snet_abort(
        c("Predictor {.val {names(x)[var]}} has dimensions {dim(x[[var]])}.",
          i = "It must match the outcome, of {d}: sender, receiver, and reporter."))
  }

  slice <- matrix(TRUE, nr, nc)
  # A tie from a node to itself is a loop, so it is dropped unless loops are
  # kept. A two-mode CSS has no diagonal: its rows and columns are different
  # nodes.
  if (!loops && square) diag(slice) <- FALSE
  # Each reporter's view of an undirected network holds every dyad twice, so
  # keep the lower triangle only, as `make_qap_data()` does for one network.
  if (!directed && square) slice[upper.tri(slice)] <- FALSE
  valid <- array(slice, dim = d)

  valid[is.na(y)] <- FALSE
  for (var in seq_len(nx)) valid[is.na(x[[var]])] <- FALSE

  if (!any(valid)) {
    manynet::snet_abort(
      c("No valid observations remain in network {nets} after dropping NA and diagonal cells.",
        i = "Check that the predictors and the outcome are observed for the same reports."))
  }

  pred <- data.frame(yv = y[valid], nv = as.factor(nets))
  pred$sv <- as.factor(slice.index(y, 1)[valid])
  pred$rv <- as.factor(slice.index(y, 2)[valid])
  # `bv` takes its name from the `by` column in which `{manynet}` records who
  # reported each tie.
  pred$bv <- as.factor(slice.index(y, 3)[valid])
  for (var in seq_len(nx)) {
    pred[[names(x)[var]]] <- x[[var]][valid]
  }
  list(pred = pred, valid = valid)
}

# Coefficient-table helper used by print.netreg_css
#' @keywords internal
#' @noRd
glm_tab <- function(x) {
  cat("\n\nCoefficients:\n")

  nc <- length(x$base$coefficients)
  cmat <- matrix(NA, nrow = nc, ncol = 4)
  cmat[, 1] <- format(round(as.numeric(x$base$coefficients), 3))
  cmat[, 2] <- format(x$lower[2, ])
  cmat[, 3] <- format(x$larger[2, ])
  cmat[, 4] <- format(x$abs[2, ])
  if (x$permute == "predictor") cmat[1, 2:4] <- "*"
  colnames(cmat) <- c("Estimate", "Pr(<=t)", "Pr(>=t)", "Pr(>=|t|)")
  rownames(cmat) <- names(x$base$coefficients)
  print.table(cmat)

  if (x$permute == "predictor")
    cat("\n* The intercept has no significance test when predictors are permuted.\n")

  if (!is.null(x$base$base_model)) {
    cat("\nAIC of base model:", format(stats::AIC(x$base$base_model)))
    cat("\nBIC of base model:", format(stats::BIC(x$base$base_model)))
  }
  cat("\n")
}

