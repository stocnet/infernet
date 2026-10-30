# infernet: network_test ####

#' Reporting network test results
#' @name report_tests
#' @family tests
#' @family reports
#' @description
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
#' @param x A `network_test`.
#' @param max.length,digits Not used.
#' @param ... Not used.
#' @return `print()` returns `x`, invisibly.
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
#' print(qaptest)
#' tidy(qaptest)
NULL

#' @rdname report_tests
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

#' @rdname report_tests
#' @method glance network_test
#' @export
glance.network_test <- function(x, ...) {
  tidy.network_test(x, ...)
}

#' @rdname report_tests
#' @export
print.network_test <- function(x, ...,
                               max.length = 6,
                               digits = 3){
  .print_title(.test_header(x))
  times <- if (is.null(x$times)) x$reps else x$times
  distance <- x$test %in% .distance_tests()
  tab <- tidy.network_test(x)
  tab$method <- NULL
  tab$times <- NULL
  marks <- .print_tidy(tab, times, p = if (distance) "p.upper" else "none")
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
