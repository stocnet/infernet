# Reporting a diagnosis of a fitted model ####

# The methods of the result of `diagnose_confusion()`, which
# `R/diagnose_confusion.R` makes; and `fitted()` and `residuals()` of an
# `infernetMrqapFit`, which a diagnosis reads.

#' @export
#' @noRd
print.infernetConfusion <- function(x, ...) {
  .print_title(c(paste("Confusion matrix",
                       if (!is.null(x$model)) paste("of a", .lower_first(x$model))),
                 paste("From", x$n_draws,
                       "draws of the predicted probabilities, with 95% intervals")))
  cell <- function(i) {
    paste0(formatC(x$confusion_matrix[i], format = "f", digits = 1), " [",
           round(x$confusion_matrix_ci_lower[i]), ", ",
           round(x$confusion_matrix_ci_upper[i]), "]")
  }
  mat <- matrix(vapply(1:4, cell, character(1)), 2, 2,
                dimnames = dimnames(x$confusion_matrix))
  print(noquote(mat))
  cat("\n")
  .print_glance(glance.infernetConfusion(x))
  .print_footer(character(0),
                .methods_hint("tidy() for the ROC curve, glance() for the metrics",
                              "and plot() from {autograph}"))
  invisible(x)
}

# A result made by an earlier version has the class of the engine's matrix.
#' @exportS3Method print QAPConfusionMatrix
print.QAPConfusionMatrix <- function(x, ...) print.infernetConfusion(x, ...)

#' @rdname diagnose_confusion
#' @method tidy infernetConfusion
#' @export
tidy.infernetConfusion <- function(x, ...) {
  if (is.null(x$roc)) return(tibble::tibble(threshold = numeric(0),
                                            fpr = numeric(0), tpr = numeric(0)))
  x$roc
}

#' @rdname diagnose_confusion
#' @method glance infernetConfusion
#' @export
glance.infernetConfusion <- function(x, ...) {
  num <- function(v) if (is.null(v)) NA_real_ else as.numeric(v)
  tibble::tibble(accuracy = num(x$accuracy),
                 sensitivity = num(x$sensitivity),
                 specificity = num(x$specificity),
                 precision = num(x$precision),
                 f1 = num(x$f1),
                 auc = num(x$auc),
                 threshold = num(x$threshold %||% 0.5),
                 threshold.accuracy = num(x$threshold_accuracy),
                 nobs = as.integer(num(x$n)),
                 draws = as.integer(num(x$n_draws)))
}

# ---- fitted values and residuals --------------------------------------------

#' The fitted values and residuals of a network regression
#'
#' @description
#'   `fitted()` and `residuals()` give one value for each dyad that a model
#'   from [estimate_mrqap()] used, in the rows of the dyad-level data,
#'   `x$pred`. They are the input of a diagnosis, such as
#'   [diagnose_outliers()], which reads the Pearson residuals.
#'
#'   They read the observed model, which the permutations do not change.
#'   For the predicted values of new dyads, see [predict_mrqap].
#' @section Fixed effects:
#'   A gaussian model absorbs its fixed effects by demeaning.
#'   `fitted()` gives the observed values less the residuals, not the
#'   demeaned fitted values.
#' @param object An `infernetMrqapFit`, from [estimate_mrqap()].
#' @param ... For `residuals()`, passed to the `residuals()` method of the
#'   observed model, such as `type = "pearson"` for a `glm`.
#'   Not used by `fitted()`.
#' @return A numeric vector, with one value for each dyad that the model used.
#' @family models
#' @examples
#' css_model <- estimate_mrqap(advice ~ same(dept) + reports,
#'                             manynet::ison_hightech, times = 5)
#' head(fitted(css_model))
#' head(residuals(css_model, type = "pearson"))
#' @name diagnose_residuals
NULL

#' @rdname diagnose_residuals
#' @exportS3Method stats::fitted
fitted.infernetMrqapFit <- function(object, ...) {
  .observed_model(object)
  stats::predict(object, type = "response")
}

#' @rdname diagnose_residuals
#' @exportS3Method stats::residuals
residuals.infernetMrqapFit <- function(object, ...) {
  unname(stats::residuals(.observed_model(object), ...))
}
