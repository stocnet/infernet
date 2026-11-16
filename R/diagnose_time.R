# Diagnosing a model of a sequence ####

#' Diagnosing a model of a sequence over time
#'
#' @description
#'   These generics diagnose a model of a sequence of events or of waves.
#'
#'   - `diagnose_changepoints()` finds where the fit of a model of a sequence
#'     changes.
#'   - `diagnose_onset()` finds when a model of a sequence starts to have
#'     information about its coefficients.
#'
#'   `{infernet}` holds these generics so that `{goldfish}` and `{RSiena}`
#'   can share them.
#'   Each package can then register a method for its own fits,
#'   such as a `{goldfish}` model of relational events or
#'   an `{RSiena}` model of network panel waves,
#'   and a user calls the same function for each model.
#'   The two generics take `{goldfish}`'s signature, `(x, ...)`,
#'   as `diagnose_outliers()` does.
#'
#'   `{infernet}` itself has no method for these generics yet,
#'   so the default method gives an error that names the class of `x`.
#' @param x A fitted model of a sequence.
#' @param ... Arguments for a method.
#' @return The diagnosis that the method for the class of `x` gives.
#'   The default method gives an error.
#' @family evaluating functions
#' @name diagnose_time
NULL

#' @rdname diagnose_time
#' @export
diagnose_changepoints <- function(x, ...) UseMethod("diagnose_changepoints")

#' @rdname diagnose_time
#' @export
diagnose_changepoints.default <- function(x, ...) {
  .no_diagnose_method("diagnose_changepoints", x)
}

#' @rdname diagnose_time
#' @export
diagnose_onset <- function(x, ...) UseMethod("diagnose_onset")

#' @rdname diagnose_time
#' @export
diagnose_onset.default <- function(x, ...) {
  .no_diagnose_method("diagnose_onset", x)
}

#' @keywords internal
#' @noRd
.no_diagnose_method <- function(fn, x,
                                hint = "{.pkg infernet} has no method for this generic yet.") {
  manynet::snet_abort(
    c("{.fn {fn}} has no method for a {.cls {class(x)[1]}}.",
      i = hint))
}
