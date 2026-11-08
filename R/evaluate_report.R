# Evaluating a fit by its likelihood ####

# `nobs()`, `logLik()`, and `deviance()` of an `infernetMrqapFit`, so that
# `AIC()` and `BIC()` compare fits.

#' Comparing network regressions by their likelihood
#'
#' @description
#'   These methods give what `AIC()` and `BIC()` read, so that two fits from
#'   [estimate_mrqap()] can be compared.
#'
#'   - `nobs()` gives the number of dyads that the model used:
#'     unobserved dyads, fixed ties, and the loops that the model leaves out
#'     are not counted.
#'   - `logLik()` and `deviance()` give the log-likelihood and the deviance
#'     of the observed model.
#'
#'   They read the observed model, which the permutations do not change.
#'   To test whether the terms that a larger model adds explain more than
#'   chance, use `anova()` of the two fits (see [test_parameter]).
#' @section Fixed effects:
#'   A gaussian model absorbs its fixed effects by demeaning. Its
#'   log-likelihood and deviance are those of the model with a dummy for each
#'   group, and `logLik()` counts the absorbed coefficients in its `df`, so
#'   that `AIC()`, `BIC()`, and `glance()` are those of that model too.
#' @param object An `infernetMrqapFit`, from [estimate_mrqap()].
#' @param ... Not used.
#' @return `nobs()` gives an integer, `logLik()` a `logLik` object, and
#'   `deviance()` a number.
#' @family models
#' @examples
#' css_model <- estimate_mrqap(advice ~ same(dept) + reports,
#'                             manynet::ison_hightech, times = 5)
#' nobs(css_model)
#' logLik(css_model)
#' AIC(css_model)
#' @name evaluate_likelihood
NULL

#' @rdname evaluate_likelihood
#' @exportS3Method stats::nobs
nobs.infernetMrqapFit <- function(object, ...) {
  # From the observed model, since {fixest} leaves out the dyads of a group
  # with one outcome value; else every row of the dyad-level data.
  n <- tryCatch(stats::nobs(.observed_model(object)), error = function(e) NULL)
  if (length(n) == 1 && !is.na(n)) return(as.integer(n))
  if (is.null(object$pred)) NA_integer_ else nrow(object$pred)
}

#' @rdname evaluate_likelihood
#' @exportS3Method stats::logLik
logLik.infernetMrqapFit <- function(object, ...) {
  bm <- .observed_model(object)
  ll <- stats::logLik(bm)
  # {fixest} gives no `df`, and counts its absorbed effects in `nparams`.
  if (is.null(attr(ll, "df")) && !is.null(bm$nparams)) {
    attr(ll, "df") <- bm$nparams
  }
  if (!is.null(object$fixed_df)) {
    attr(ll, "df") <- attr(ll, "df") + object$fixed_df
  }
  attr(ll, "nobs") <- nobs.infernetMrqapFit(object)
  class(ll) <- "logLik"
  ll
}

#' @rdname evaluate_likelihood
#' @exportS3Method stats::deviance
deviance.infernetMrqapFit <- function(object, ...) {
  stats::deviance(.observed_model(object))
}
