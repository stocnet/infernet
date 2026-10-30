# Reporting a diagnosis of a fitted model ####

# The methods of the result of `diagnose_confusion()`, which
# `R/diagnose_confusion.R` makes.

#' @rdname diagnose_confusion
#' @export
print.diagnose_confusion <- function(x, ...) {
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
  .print_glance(glance.diagnose_confusion(x))
  .print_footer(character(0),
                .methods_hint("tidy() for the ROC curve, glance() for the metrics",
                              "and plot() from {autograph}"))
  invisible(x)
}

# A result made by an earlier version has the class of the engine's matrix.
#' @exportS3Method print QAPConfusionMatrix
print.QAPConfusionMatrix <- function(x, ...) print.diagnose_confusion(x, ...)

#' @rdname diagnose_confusion
#' @method tidy diagnose_confusion
#' @export
tidy.diagnose_confusion <- function(x, ...) {
  if (is.null(x$roc)) return(tibble::tibble(threshold = numeric(0),
                                            fpr = numeric(0), tpr = numeric(0)))
  x$roc
}

#' @rdname diagnose_confusion
#' @method glance diagnose_confusion
#' @export
glance.diagnose_confusion <- function(x, ...) {
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
