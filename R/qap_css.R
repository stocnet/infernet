# Cognitive social structures --------------------------------------------------
#
# Internal. What a CSS needs that a dyadic network does not: a vectoriser for a
# sender-by-receiver-by-perceiver array, and a print method. Everything else it
# shares with the dyadic case, and lives in R/qap_engine.R.

#' @keywords internal
#' @noRd
array_to_vector <- function(ar, directed., diag.) {
  v <- c()
  for (i in 1:nrow(ar)) {
    if (!directed.) {
      v <- c(v, as.vector(ar[, , i][upper.tri(ar[, , i], diag = diag.)]))
    } else {
      v <- c(v, as.vector(ar[, , i]))
    }
  }
  return(v)
}

#' @keywords internal
#' @noRd
make_css_data <- function(y, x, nets, diag, directed) {
  n <- dim(y)[1]
  nx <- length(x)
  valid <- array(TRUE, dim = c(n, n, n))

  if (!diag) {
    for (i in 1:n) {
      diag(y[, , i]) <- NA
      for (var in 1:nx) {
        diag(x[[var]][, , i]) <- NA
      }
    }
  }

  valid[is.na(y)] <- FALSE

  for (var in 1:nx) {
    valid[is.na(x[[var]])] <- FALSE
  }

  if (!directed) {
    for (i in 1:n) {
      y[, , i][lower.tri(y[, , i])] <- NA
      valid[, , i][lower.tri(valid[, , i])] <- FALSE
      for (var in 1:nx) {
        x[[var]][, , i][lower.tri(x[[var]][, , i])] <- NA
      }
    }
  }

  y[!valid] <- NA

  for (var in 1:nx) {
    x[[var]][!valid] <- NA
  }

  vv <- array_to_vector(valid, directed. = directed, diag. = diag)
  yv <- array_to_vector(y, directed. = directed, diag. = diag)[vv]

  pred <- data.frame(yv = yv, nv = nets)

  per <- sen <- rec <- array(NA, dim = c(n, n, n))

  for (i in 1:n) {
    sen[i, , ] <- i
    rec[, i, ] <- i
    per[, , i] <- i
  }

  pred$sv <- as.factor(array_to_vector(sen, directed. = directed, diag. = diag)[vv])
  pred$rv <- as.factor(array_to_vector(rec, directed. = directed, diag. = diag)[vv])
  pred$pv <- as.factor(array_to_vector(per, directed. = directed, diag. = diag)[vv])

  for (var in c(1:nx)) {
    pred[[names(x)[var]]] <- array_to_vector(x[[var]],
                                             directed. = directed, diag. = diag)[vv]
  }
  return(list(pred = pred, valid = valid))
}

# Coefficient-table helper used by print.QAPCSS
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

# Registered so that a CSS fit prints as a model rather than as a list. The
# class is internal, so the method is registered without a help topic.
#' @exportS3Method print QAPCSS
print.QAPCSS <- function(x, ...) {

  if (!any(x$random)) {
    cat("\nGeneralized Linear Network Model for CSS\n\n")
  } else {
    cat("\nGeneralized Linear Mixed Network Model for CSS fit by REML\n\n")
  }

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
