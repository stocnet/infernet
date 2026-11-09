#' Declare the coefficients of a model
#'
#' @description
#'   `declare_coefs()` makes a set of coefficients for the terms of a model:
#'   one value for each column that the specification builds on a network.
#'   A set of coefficients gives the initial values of an estimation, or the
#'   model that `simulate()` draws networks from.
#'
#'   It is a generic, with a method for each model:
#'
#'   - for a specification from [specify_mrqap()], the values stated in `...`;
#'   - for an `infernetMrqapFit`, from [estimate_mrqap()], the estimated
#'     coefficients, with the specification, the family, and the residual
#'     standard deviation of the fit.
#'
#'   A coefficient is the estimated or stated value of a model's term.
#'   A value that is not stated is `NA`.
#'
#' @details
#'   The names of the values are the names of the columns that the
#'   specification builds, as `estimate_mrqap()` names its coefficients:
#'   `"(Intercept)"`, and then a column such as `"ego_age"` or `"same_dept"`.
#'   `tidy(specify_mrqap(formula, .data), .data)` lists the columns of each
#'   term in its `columns` column.
#'   With `.data`, `declare_coefs()` checks each name against these columns,
#'   and gives each column without a value an `NA`.
#'   Without it, the names are checked when `simulate()` builds the columns.
#'
#'   [simulate()] reads a set of coefficients of a `"gaussian"`, `"binomial"`,
#'   or `"poisson"` model.
#'   A `"gaussian"` model also needs `sigma`, the standard deviation of its
#'   residuals, also where the family was read from the outcome.
#'   [estimate_mrqap()] uses a set of coefficients as initial values where its
#'   estimator iterates.
#' @param x A specification from [specify_mrqap()], or a fitted model, such
#'   as an `infernetMrqapFit` from [estimate_mrqap()].
#' @param ... For a specification, the values of the coefficients, each named
#'   for its column, such as `"(Intercept)" = -2, ego_age = 0.1`, or as one
#'   named vector, such as `stats::coef(fit)`.
#'   For a fit, not used.
#' @param family The family of the model: `"gaussian"`, `"binomial"`,
#'   `"poisson"`, `"negbin"`, or `"zip"`, as in [control_mrqap()].
#'   `NULL`, the default, reads it from the values of the outcome in
#'   `.data`, as [estimate_mrqap()] does: `"binomial"` for a 0/1 outcome,
#'   `"poisson"` for a count, and `"gaussian"` for any other
#'   (see [control_mrqap()]). Without `.data`, `NULL` aborts.
#' @param sigma For a `"gaussian"` model, the standard deviation of the
#'   residuals. Other families do not use it.
#' @param .data Optionally, the network that the specification builds its
#'   columns on, to check the names of the values.
#' @param object A set of coefficients from `declare_coefs()`, or an
#'   `infernetMrqapFit` from [estimate_mrqap()].
#' @return An object of class `infernetMrqapCoefs`: a named numeric vector of
#'   the coefficients, with `NA` for a value that is not stated.
#'   Its attributes are `spec`, the specification; `family`, the family; and
#'   `sigma`, the standard deviation of the residuals, or `NULL`.
#'   Printing it gives the family, the formula, and the values.
#'   `stats::coef()` gives the values as a plain named vector.
#'   `stats::coef()` of an `infernetMrqapFit` gives its estimated
#'   coefficients the same way, so `coef(declare_coefs(fit))` equals
#'   `coef(fit)`.
#' @seealso [simulate_mrqap] to simulate networks from a set of coefficients,
#'   and [estimate_mrqap()] for the initial values of an estimation.
#' @family specifying functions
#' @examples
#' spec <- specify_mrqap(friends ~ social + tasks)
#' coefs <- declare_coefs(spec, "(Intercept)" = -1.5, social = 0.2,
#'                        tasks = -0.4, family = "poisson",
#'                        .data = ison_algebra)
#' coefs
#' stats::coef(coefs)
#'
#' # The coefficients of a fit
#' fit <- estimate_mrqap(friends ~ social + tasks, ison_algebra, times = 5,
#'                       control = list(family = "poisson"))
#' declare_coefs(fit)
#' @export
declare_coefs <- function(x, ...) UseMethod("declare_coefs")

#' @rdname declare_coefs
#' @export
declare_coefs.default <- function(x, ...) {
  manynet::snet_abort(
    c("{.fn declare_coefs} has no method for a {.cls {class(x)[1]}}.",
      i = "Give it a specification from {.fn specify_mrqap}, or a fit from {.fn estimate_mrqap}."))
}

#' @rdname declare_coefs
#' @export
declare_coefs.mrqap_formula <- function(x, ..., family = NULL, sigma = NULL,
                                        .data = NULL) {
  family <- .check_coef_family(family)
  values <- .coef_values(list(...))
  design <- if (!is.null(.data)) .mrqap_design(x, .data)
  family <- .resolve_coef_family(family, design)
  sigma <- .check_sigma(sigma, family)
  if (!is.null(design)) {
    columns <- colnames(design$X)
    unknown <- setdiff(names(values), columns)
    if (length(unknown) > 0) {
      manynet::snet_abort(
        c("The specification builds no column named {.val {unknown}} on {.arg .data}.",
          i = "Its columns are {.val {columns}}."))
    }
    values <- stats::setNames(values[columns], columns)
  }
  .new_coefs(values, x, family, sigma)
}

#' @rdname declare_coefs
#' @export
declare_coefs.infernetMrqapFit <- function(x, ...) {
  values <- x$coefficients
  sigma <- NULL
  if (identical(x$family, "gaussian") && .simulates_from_coefs(x)) {
    sigma <- .fit_sigma(x)
  }
  .new_coefs(values, .fit_spec(x), x$family, sigma)
}

#' @export
print.infernetMrqapCoefs <- function(x, ...) {
  family <- attr(x, "family")
  sigma <- attr(x, "sigma")
  stated <- sum(!is.na(x))
  .print_title(c(paste("Coefficients of a", family, "MRQAP"),
                 paste(stated, "of", length(x), "coefficients stated")))
  spec <- attr(x, "spec")
  if (!is.null(spec)) print(.plain_formula(spec), showEnv = FALSE)
  cat("\n")
  print(stats::coef(x))
  .print_footer(if (!is.null(sigma))
    paste("The residuals have a standard deviation of", signif(sigma, 4)))
  invisible(x)
}

#' @rdname declare_coefs
#' @export
coef.infernetMrqapCoefs <- function(object, ...) .plain_coefs(object)

#' @rdname declare_coefs
#' @export
coef.infernetMrqapFit <- function(object, ...) .plain_coefs(object$coefficients)

# Coefficients as a plain named vector, without a class or other attributes.
#' @keywords internal
#' @noRd
.plain_coefs <- function(values) {
  out <- as.numeric(values)
  names(out) <- names(values)
  out
}

# ---- helpers -------------------------------------------------------------------

#' @keywords internal
#' @noRd
.new_coefs <- function(values, spec, family, sigma) {
  structure(values, spec = spec, family = family, sigma = sigma,
            class = "infernetMrqapCoefs")
}

# The families of `control_mrqap()`.
#' @keywords internal
#' @noRd
.coef_families <- function() c("gaussian", "binomial", "poisson", "negbin", "zip")

#' @keywords internal
#' @noRd
.check_coef_family <- function(family) {
  if (is.null(family)) return(NULL)
  choices <- .coef_families()
  if (!(is.character(family) && length(family) == 1 && family %in% choices)) {
    manynet::snet_abort("{.arg family} must be one of {.val {choices}}.")
  }
  family
}

#' @keywords internal
#' @noRd
.check_sigma <- function(sigma, family) {
  if (is.null(sigma)) return(NULL)
  if (!identical(family, "gaussian")) {
    manynet::snet_warn(
      "A {.val {family}} model has no {.arg sigma}, so it is not used.")
    return(NULL)
  }
  if (!(is.numeric(sigma) && length(sigma) == 1 && !is.na(sigma) && sigma > 0)) {
    manynet::snet_abort("{.arg sigma} must be one positive number.")
  }
  sigma
}

# `NULL` reads the family from the values of the outcome, as
# `estimate_mrqap()` does, with `.auto_family()`.
# A specification has no outcome values, so this needs the network.
#' @keywords internal
#' @noRd
.resolve_coef_family <- function(family, design = NULL) {
  if (!is.null(family)) return(family)
  if (is.null(design)) {
    choices <- .coef_families()
    manynet::snet_abort(
      c("Without {.arg family}, the family is read from the outcome, so it needs {.arg .data}.",
        i = "Give the network as {.arg .data}, or name the {.arg family}: {.val {choices}}."))
  }
  family <- .auto_family(design$cells$matrix)
  manynet::snet_info(
    "Treating the outcome as {.val {family}}, from its values.")
  family
}

# The values of `...`: named numbers, or one named vector such as `coef(fit)`.
#' @keywords internal
#' @noRd
.coef_values <- function(dots) {
  if (length(dots) == 0) {
    manynet::snet_abort(
      c("{.fn declare_coefs} needs a value for each coefficient.",
        i = "For example: {.code declare_coefs(spec, \"(Intercept)\" = -1, ego_age = 0.1, family = \"poisson\")}."))
  }
  if (length(dots) == 1 && is.null(names(dots))) dots <- as.list(dots[[1]])
  ok <- vapply(dots, function(v) is.numeric(v) && length(v) == 1 ||
                 (length(v) == 1 && is.na(v)), logical(1))
  nms <- names(dots)
  if (!all(ok)) {
    manynet::snet_abort("Each coefficient must be one number, or {.val NA}.")
  }
  if (is.null(nms) || any(!nzchar(nms))) {
    manynet::snet_abort(
      c("Each coefficient must be named for its column.",
        i = "For example: {.code \"(Intercept)\" = -1, ego_age = 0.1}."))
  }
  if (anyDuplicated(nms)) {
    twice <- unique(nms[duplicated(nms)])
    manynet::snet_abort("Each coefficient must be named once; {.val {twice}} is named more than once.")
  }
  stats::setNames(as.numeric(unlist(dots)), nms)
}

# The specification of a fit, as `specify_mrqap()` gave it to the fit.
#' @keywords internal
#' @noRd
.fit_spec <- function(x) {
  structure(x$formula, class = c("mrqap_formula", "formula"),
            input = "mrqap", record = x$record)
}
