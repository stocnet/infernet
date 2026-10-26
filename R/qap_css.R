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
make_css_data <- function(y, x, nets, diag, directed) {
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
  if (!diag && square) diag(slice) <- FALSE
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

# A CSS result prints its own way, whatever its family: it says how many
# reporters it was fitted on, which a reader needs to interpret it.
#' @rdname results
#' @export
print.netreg_css <- function(x, ...) {

  if (!any(x$random)) {
    cat("\nGeneralized Linear Network Model for a cognitive social structure\n\n")
  } else {
    cat("\nGeneralized Linear Mixed Network Model for a cognitive social structure\n\n")
  }

  # A result should say what it was fitted on: how many reporters, and how many
  # of their reports reached the model after missing ones were dropped.
  if (!is.null(x$pred$bv)) {
    reporters <- length(unique(x$pred$bv))
    cat("Fitted on", format(nrow(x$pred)), "reported dyads from",
        reporters, if (reporters == 1) "reporter.\n" else "reporters.\n")
  }
  if (isTRUE(x$random[["reporter"]]))
    cat("Each reporter has a random intercept.\n")
  cat(.fixed_label(x))

  if (!is.null(x$theta))
    cat("Negative binomial dispersion (theta):", format(round(x$theta, 4)), "\n")
  if (!is.null(x$zi_coefficients)) {
    cat("Zero-inflation coefficients:\n")
    cat("  ", paste(names(x$zi_coefficients),
                    format(round(x$zi_coefficients, 4)),
                    sep = " = ", collapse = ", "), "\n")
  }

  if (!is.null(x$groups))
    cat("Permutations were performed within groups only.\n")

  if (x$permute == "outcome")
    cat("The outcome array Y was permuted", format(x$times), "times.\n")
  if (x$permute == "predictor") {
    cat("Significance was estimated using Dekker's\n")
    cat("  'semi-partialling plus' procedure with",
        format(x$times), "permutations.\n")
  }

  if (x$robust_se)
    cat("T-values are based on robust standard errors.\n")

  if (x$diag) {
    cat("Diagonal values (loops) were used in the estimation.\n",
        "  Results may be biased because of that.\n")
  } else {
    cat("Diagonal values (loops) were ignored.\n")
  }
  cat("The outcome was treated as",
      format(paste0(.directed_label(x$directed), ".")), "\n")

  glm_tab(x)

  if (!is.null(x$confusion_matrix)) {
    cat("\n")
    print(x$confusion_matrix)
  }

  cat("\n")
  invisible(x)
}
