# Diagnosing a model of a sequence ####

#' Diagnosing a model of a sequence over time
#'
#' @description
#'   These generics diagnose a model of a sequence of events or of waves.
#'   `{infernet}` holds them, so that `{goldfish}` and other packages register
#'   methods for their own fits on the same generics, and a user calls one
#'   function for any model.
#'
#'   - `diagnose_changepoints()` finds where the fit of a model of a sequence
#'     changes.
#'   - `diagnose_onset()` finds when a model of a sequence starts to have
#'     information about its coefficients.
#'
#'   `{infernet}` has no method for them yet. `{goldfish}` has methods for
#'   its fits.
#' @param x A fitted model of a sequence.
#' @param ... Arguments for a method.
#' @return The diagnosis that the method of the model gives.
#' @family models
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
.no_diagnose_method <- function(fn, x) {
  manynet::snet_abort(
    c("{.fn {fn}} has no method for a {.cls {class(x)[1]}}.",
      i = "Load the package that fitted the model, such as {.pkg goldfish}, for its methods."))
}
