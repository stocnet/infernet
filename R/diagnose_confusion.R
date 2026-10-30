# Diagnosing how well a model classifies ties ####

#' Diagnosing how well a model classifies ties
#'
#' @description
#'   `diagnose_confusion()` compares the ties that a model of a binary outcome
#'   predicts with the ties that were observed. It has a method for a
#'   `netreg` from [estimate_mrqap()]: a logistic model, or a linear model of
#'   a 0/1 outcome, read as a linear probability model.
#'
#'   - The probabilistic confusion matrix draws each tie from its predicted
#'     probability, `times` times, and gives the mean count of each cell with
#'     a 95% interval, so it shows how uncertain the classification is.
#'   - The metrics are the means over those draws: the accuracy, the
#'     sensitivity (the share of observed ties that are predicted), the
#'     specificity (the share of observed non-ties that are predicted), the
#'     precision (the share of predicted ties that are observed), and F1.
#'   - The confusion matrix at `threshold` classifies each dyad as a tie
#'     where its predicted probability is at least `threshold`.
#'   - The ROC curve gives the true positive rate against the false positive
#'     rate at every threshold, and the AUC is the area under it: the
#'     probability that an observed tie has a higher predicted probability
#'     than an observed non-tie.
#'
#'   `print()` shows the matrix, `tidy()` gives the ROC curve, and `glance()`
#'   gives the metrics in one row, after `{broom}`. `{autograph}` plots it.
#' @details
#'   The predicted probabilities are those of the observed model, which the
#'   permutations do not change. A linear probability model can predict a
#'   value below 0 or above 1, which is taken as 0 or 1.
#'
#'   [estimate_mrqap()] keeps the result for a binary outcome as
#'   `$confusion_matrix`, from 1000 draws with the seed of the fit.
#' @param x A `netreg` of a binary outcome, from [estimate_mrqap()].
#'   For `print()`, `tidy()`, and `glance()`, the result of
#'   `diagnose_confusion()`.
#' @param threshold The predicted probability at or above which a dyad is
#'   classified as a tie.
#' @param times The number of draws of the probabilistic confusion matrix.
#' @param control A list with a `seed`, as for [control_mrqap()]. Without one,
#'   the seed of the fit is used.
#' @param ... Not used.
#' @return `diagnose_confusion()` returns an object of class
#'   `diagnose_confusion`, a list with the mean `confusion_matrix`, its
#'   `confusion_matrix_sd`, `confusion_matrix_ci_lower`, and
#'   `confusion_matrix_ci_upper`, the metrics, the
#'   `threshold_confusion_matrix`, the `roc` curve, and the `auc`.
#'   `tidy()` returns a tibble of `threshold`, `fpr`, and `tpr`, and
#'   `glance()` a tibble of one row of the metrics.
#'   `print()` returns `x`, invisibly.
#' @family models
#' @examples
#' fit <- estimate_mrqap(. ~ ego(Age) + alter(Age),
#'                       manynet::ison_adolescents |>
#'                         manynet::mutate_nodes(Age = 12:19),
#'                       times = 20, control = list(seed = 1))
#' (conf <- diagnose_confusion(fit))
#' glance(conf)
#' tidy(conf)
#' @export
diagnose_confusion <- function(x, ...) UseMethod("diagnose_confusion")

#' @rdname diagnose_confusion
#' @export
diagnose_confusion.default <- function(x, ...) {
  manynet::snet_abort(
    c("{.fn diagnose_confusion} has no method for a {.cls {class(x)[1]}}.",
      i = "Give it a {.cls netreg} of a binary outcome, from {.fn estimate_mrqap}."))
}

#' @rdname diagnose_confusion
#' @export
diagnose_confusion.netreg_glm <- function(x, threshold = 0.5, times = 1000,
                                          control = list(), ...) {
  if (!identical(x$family, "binomial")) {
    manynet::snet_abort(
      c("{.fn diagnose_confusion} needs a model of a binary outcome, and this one is {.val {x$family}}.",
        i = "Fit a {.val binomial} model, or a {.val gaussian} model of a 0/1 outcome."))
  }
  # From the model rather than from `pred`, since {fixest} leaves out the
  # dyads of a group with one outcome value.
  bm <- .observed_model(x)
  prob <- stats::fitted(bm)
  out <- probabilistic_confusion_matrix(
    actual = prob + stats::residuals(bm, type = "response"),
    predicted_prob = prob, n_draws = times,
    seed = .confusion_seed(x, control), threshold = threshold)
  .confusion_model(out, x)
}

#' @rdname diagnose_confusion
#' @export
diagnose_confusion.netreg_lm <- function(x, threshold = 0.5, times = 1000,
                                         control = list(), ...) {
  if (!.is_binary_outcome(x$pred[[x$dep]])) {
    manynet::snet_abort(
      c("{.fn diagnose_confusion} needs a model of a binary outcome.",
        i = "The outcome {.val {x$dep}} has values other than 0 and 1."))
  }
  out <- .lpm_confusion(x, seed = .confusion_seed(x, control),
                        threshold = threshold, times = times)
  .confusion_model(out, x)
}

#' @keywords internal
#' @noRd
.confusion_seed <- function(x, control) {
  seed <- .resolve_control(control)$seed
  if (is.null(seed)) seed <- x$control$seed
  seed
}

#' @keywords internal
#' @noRd
.confusion_model <- function(out, x) {
  out$model <- paste0(.fit_label(x), " MRQAP of ", x$dep)
  out
}

#' @keywords internal
#' @noRd
.is_binary_outcome <- function(y) {
  vals <- if (is.list(y)) unlist(y, use.names = FALSE) else as.vector(y)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(FALSE)
  all(vals %in% c(0, 1))
}

# A linear probability model: the observed values less the residuals, rather
# than `fitted()`, since a model with fixed effects was fitted to demeaned
# values, and clamped to [0, 1].
#' @keywords internal
#' @noRd
.lpm_confusion <- function(fit, seed = NULL, threshold = 0.5, times = 1000) {
  bm <- fit$simple_fit
  if (is.null(bm)) bm <- fit$base$base_model
  if (is.null(bm)) return(NULL)
  actual <- fit$pred[[fit$dep]]
  predicted <- actual - stats::residuals(bm)
  predicted <- pmin(pmax(predicted, 0), 1)
  probabilistic_confusion_matrix(
    actual = as.integer(actual),
    predicted_prob = predicted,
    n_draws = times,
    seed = seed,
    threshold = threshold
  )
}

# Each dyad's tie is drawn from its predicted probability `n_draws` times,
# and each draw gives one confusion matrix.
#' @keywords internal
#' @noRd
probabilistic_confusion_matrix <- function(actual,
                                           predicted_prob,
                                           n_draws = 1000,
                                           seed = NULL,
                                           threshold = 0.5) {
  if (!is.null(seed)) set.seed(seed)

  n <- length(actual)
  stopifnot(length(predicted_prob) == n)
  stopifnot(all(actual %in% c(0, 1)))
  stopifnot(all(predicted_prob >= 0 & predicted_prob <= 1, na.rm = TRUE))

  draws <- matrix(stats::rbinom(n * n_draws, size = 1, prob = predicted_prob),
                  nrow = n, ncol = n_draws)

  actual_1 <- actual == 1
  actual_0 <- actual == 0
  n_actual_1 <- sum(actual_1)
  n_actual_0 <- sum(actual_0)

  tp <- colSums(draws[actual_1, , drop = FALSE])
  fn <- n_actual_1 - tp
  fp <- colSums(draws[actual_0, , drop = FALSE])
  tn <- n_actual_0 - fp

  make_mat <- function(v_tn, v_fp, v_fn, v_tp, FUN) {
    matrix(c(FUN(v_tn), FUN(v_fp), FUN(v_fn), FUN(v_tp)),
           nrow = 2, ncol = 2,
           dimnames = list(Actual = c("0", "1"),
                           Predicted = c("0", "1")))
  }

  safe_mean <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NA_real_)
    mean(x)
  }

  pred_class <- as.integer(predicted_prob >= threshold)
  result <- list(
    confusion_matrix       = make_mat(tn, fp, fn, tp, mean),
    confusion_matrix_sd    = make_mat(tn, fp, fn, tp, stats::sd),
    confusion_matrix_ci_lower = make_mat(tn, fp, fn, tp,
                                         function(x) stats::quantile(x, 0.025)),
    confusion_matrix_ci_upper = make_mat(tn, fp, fn, tp,
                                         function(x) stats::quantile(x, 0.975)),
    accuracy       = mean((tp + tn) / n),
    accuracy_sd    = stats::sd((tp + tn) / n),
    sensitivity    = safe_mean(tp / (tp + fn)),
    specificity    = safe_mean(tn / (tn + fp)),
    precision      = safe_mean(tp / (tp + fp)),
    f1             = safe_mean(2 * tp / (2 * tp + fp + fn)),
    n_draws        = n_draws,
    n              = n,
    threshold      = threshold,
    threshold_confusion_matrix = table(
      Actual    = factor(actual, levels = c(0, 1)),
      Predicted = factor(pred_class, levels = c(0, 1))),
    threshold_accuracy = mean(pred_class == actual),
    roc            = .roc_curve(actual, predicted_prob),
    auc            = .auc(actual, predicted_prob)
  )
  class(result) <- "diagnose_confusion"
  result
}

# The true and false positive rates at each distinct predicted probability,
# from the highest down, so that a long network is sorted once.
#' @keywords internal
#' @noRd
.roc_curve <- function(actual, prob) {
  o <- order(prob, decreasing = TRUE)
  p <- prob[o]
  a <- actual[o]
  last <- c(p[-1] != p[-length(p)], TRUE)
  n1 <- sum(a == 1)
  n0 <- sum(a == 0)
  tibble::tibble(threshold = c(Inf, p[last]),
                 fpr = c(0, cumsum(a == 0)[last] / n0),
                 tpr = c(0, cumsum(a == 1)[last] / n1))
}

# The Mann-Whitney statistic: the share of pairs of an observed tie and an
# observed non-tie where the tie has the higher probability, a tie counting
# one half.
#' @keywords internal
#' @noRd
.auc <- function(actual, prob) {
  n1 <- sum(actual == 1)
  n0 <- sum(actual == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(prob)
  (sum(r[actual == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
