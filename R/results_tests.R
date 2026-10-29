# infernet: network_test ####

#' Reporting network test results
#'
#' @description
#'   These methods report a `network_test`, the result of
#'   [test_random()], [test_configuration()], [test_permutation()],
#'   [test_gof()], [test_distributions()], [test_parameter()], and
#'   [test_time()].
#'
#'   - `print()` shows the observed statistic and the share of the null
#'     distribution at or above and at or below it.
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
#' @family models
#' @examples
#' qaptest <- test_permutation(manynet::ison_adolescents,
#'                             netrics::net_by_density, times = 20)
#' print(qaptest)
#' tidy(qaptest)
#' @name test_results
NULL
#' @rdname test_results
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

#' @rdname test_results
#' @method glance network_test
#' @export
glance.network_test <- function(x, ...) {
  tidy.network_test(x, ...)
}

