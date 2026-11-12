# Reporting network tests ####

# The methods of a `network_test`, which every `test_*()` function makes, on
# the help page of `test_measures`.

#' @section Methods:
#'   These methods report a `network_test`, the result of
#'   [test_random()], [test_configuration()], [test_permutation()],
#'   [test_gof()], [test_distributions()], [test_parameter()], and
#'   [test_time()].
#'
#'   - `print()` shows a title that says what was tested, a blank line,
#'     `tidy()` of the result, and a footer that says how the null
#'     distribution was made and the controls that differ from the default.
#'     For a test whose statistic is a distance, from [test_gof()],
#'     [test_distributions()], [test_parameter()], or [test_time()], only the
#'     upper tail is a p-value, and only it is marked.
#'   - `tidy()` and `glance()` give the same as a tibble of one row, after
#'     `{broom}`.
#'   - `summary()` gives the observed statistic, quantiles of the null
#'     distribution, and the two p-values, as a tibble of class
#'     `infernetTestSummary`, with one row for each statistic.
#' @param x A `network_test`.
#' @return
#'   `summary()` gives a tibble with the `statistic`, a column for each
#'   quantile of the null distribution, such as `null.2.5%`, and `p.lower` and
#'   `p.upper`.
#'   `tidy()` and `glance()` give one row with the observed statistic,
#'   the mean and standard deviation of the null distribution,
#'   and the share of the null distribution at or below (`p.lower`)
#'   and at or above (`p.upper`) the observed statistic.
#'   For `test_gof()` and `test_distributions()`, the statistic is a distance,
#'   the null distribution is that of the reference simulations,
#'   and `p.upper` is the p-value.
#' @examples
#' qaptest <- test_permutation(manynet::ison_adolescents,
#'                             netrics::net_by_density, times = 20)
#' qaptest
#' tidy(qaptest)
#' summary(qaptest)
#' @name report_tests
#' @rdname test_measures
NULL

#' @rdname test_measures
#' @method tidy network_test
#' @export
tidy.network_test <- function(x, ...) {
  tibble::tibble(method = x$test,
                 statistic = as.numeric(x$testval),
                 null.mean = mean(x$testdist, na.rm = TRUE),
                 null.sd = stats::sd(x$testdist, na.rm = TRUE),
                 p.lower = x$plteobs,
                 p.upper = x$pgteobs,
                 # {migraph}'s tests call it reps
                 times = if (is.null(x$times)) x$reps else x$times)
}

#' @rdname test_measures
#' @param object A `network_test`.
#' @param probs For `summary()`, the probabilities of the quantiles of the null distribution.
#' @export
summary.network_test <- function(object, probs = c(0.025, 0.5, 0.975), ...) {
  null <- as.matrix(object$testdist)
  stat <- as.numeric(object$testval)
  # A test of one statistic has one column of draws; a test of several, such
  # as of a vector-valued measure, has one column for each.
  if (ncol(null) != length(stat)) null <- matrix(as.numeric(null), ncol = 1)
  q <- t(apply(null, 2, stats::quantile, probs = probs, na.rm = TRUE,
               names = FALSE))
  if (length(probs) == 1) q <- t(q)
  colnames(q) <- paste0("null.", 100 * probs, "%")
  out <- tibble::as_tibble(c(
    list(statistic = stat[seq_len(nrow(q))]),
    as.list(as.data.frame(q, check.names = FALSE)),
    list(p.lower = rep_len(as.numeric(object$plteobs), nrow(q)),
         p.upper = rep_len(as.numeric(object$pgteobs), nrow(q)))))
  structure(out, class = c("infernetTestSummary", class(out)),
            header = .test_header(object),
            times = if (is.null(object$times)) object$reps else object$times,
            draws = sum(!is.na(null[, 1])))
}

#' @export
#' @noRd
print.infernetTestSummary <- function(x, ...) {
  # A summary that lost its attributes, such as by `[`, prints as a tibble.
  if (is.null(attr(x, "header"))) return(NextMethod())
  .print_title(attr(x, "header"))
  print(tibble::as_tibble(unclass(x)[names(x)]))
  .print_footer(paste0("Quantiles of the null distribution, from ",
                       attr(x, "draws"), " draws."))
  invisible(x)
}

#' @rdname test_measures
#' @method glance network_test
#' @export
glance.network_test <- function(x, ...) {
  tidy.network_test(x, ...)
}

#' @export
#' @noRd
print.network_test <- function(x, ...,
                               max.length = 6,
                               digits = 3){
  .print_title(.test_header(x))
  times <- if (is.null(x$times)) x$reps else x$times
  distance <- x$test %in% .distance_tests()
  tab <- tidy.network_test(x)
  tab$method <- NULL
  tab$times <- NULL
  marks <- .print_tidy(tab, p = if (distance) "p.upper" else "none")
  .print_footer(c(.test_null(x, times),
                  if (distance) "The p-value is p.upper: only a larger statistic counts against the null.",
                  if (isTRUE(x$failed > 0))
                    paste(x$failed, "permutations did not converge, and are left out."),
                  .controls_line(x$control),
                  .signif_legend(marks)),
                .methods_hint("tidy() and glance() for a tibble",
                              "and plot() from {autograph} for the null distribution"))
  invisible(x)
}

# The tests whose statistic is a distance, so that only its upper tail is a
# p-value.
#' @keywords internal
#' @noRd
.distance_tests <- function() c("Goodness-of-fit", "Two-sample", "Parameter", "Time")

# What was tested.
#' @keywords internal
#' @noRd
.test_header <- function(x) {
  stat <- x$statistic %||% "a statistic"
  model <- if (!is.null(x$model)) paste(" in", .a_model(x$model)) else ""
  # A time test names a coefficient for each wave, as `recip:wave2`.
  terms <- x$terms
  if (identical(x$test, "Time")) terms <- unique(sub(":wave[0-9]+$", "", terms))
  terms <- paste(terms, collapse = ", ")
  switch(x$test,
         CUG = paste("CUG test of", stat),
         configuration = paste("Configuration test of", stat),
         QAP = paste("QAP test of", stat),
         `Goodness-of-fit` = paste0("Goodness-of-fit test of ", stat,
                                    if (!is.null(x$model)) paste(" for", .a_model(x$model))),
         `Two-sample` = "Two-sample test of two sets of simulated diffusions",
         Parameter = paste0("Parameter test of ", terms, " = ",
                            paste(x$value, collapse = ", "), model),
         Time = paste0("Time test of ", terms, " over the waves", model),
         paste(x$test, "test"))
}

# A model as a title names it: "a logistic MRQAP", or "the simulated
# diffusions" as it is recorded.
#' @keywords internal
#' @noRd
.a_model <- function(model) {
  if (startsWith(model, "the ")) model else paste("a", .lower_first(model))
}

# How the null distribution was made.
#' @keywords internal
#' @noRd
.test_null <- function(x, times) {
  null <- switch(x$test,
                 CUG = "random networks with the same size and density",
                 configuration = "random networks with the same degrees",
                 QAP = "permutations of the node labels",
                 `Goodness-of-fit` = paste0("simulations of the model; ",
                                            times - length(x$testdist),
                                            " estimate the mean and covariance"),
                 `Two-sample` = "permutations of the pooled diffusions",
                 Parameter = , Time = "permutations of the tested predictors, by double semi-partialling",
                 "draws")
  paste0("Null: ", times, " ", null, ".")
}
