# Predicting from network regressions ####

#' Predicting from a network regression
#'
#' @description
#'   `predict()` gives the predicted value of the outcome for each dyad.
#'
#'   - For a `netreg`, from `estimate_mrqap()`, it predicts either the dyads
#'     that the model was fitted on, or the dyads of `newdata`.
#'   - For a `netlm` or `netlogit`, from `net_regression()` in `{migraph}`
#'     before 2.0.0, it predicts the rows of a design matrix.
#' @details
#'   Without `newdata`, the prediction for a `netreg` is the fitted value of
#'   each dyad of the model, with any random intercepts and absorbed fixed
#'   effects.
#'
#'   With `newdata`, the prediction comes from the coefficients only.
#'   A random intercept is then zero, as for a dyad of a node the model has
#'   not seen. A fixed effect is absorbed, so it has no coefficient, and a
#'   model with fixed effects predicts only the dyads it was fitted on.
#'   For a zero-inflated Poisson model, the zero-inflation part reads the same
#'   columns as the count part.
#' @param object A `netreg` object, as returned by `estimate_mrqap()`,
#'   or a `netlm` or `netlogit` object.
#' @param newdata The dyads to predict.
#'   For a `netreg`, `NULL` (the default) predicts the dyads that the model
#'   was fitted on. Otherwise, a data frame or a matrix with a column for each
#'   predictor, named as the coefficients are, e.g. `ego_Citations`.
#'   A column for an interaction, e.g. `ego_Age:same_Grp`, is computed from its
#'   parts where it is absent. The model's own dyad-level data, `object$pred`,
#'   has these columns.
#'   A matrix without column names needs a column for each coefficient,
#'   in their order, with the intercept first.
#'   For a `netlm` or `netlogit`, a design matrix with the same columns as the
#'   fitted model.
#' @param type The scale of the predictions: `"link"` (the default), the
#'   scale of the linear predictor, or `"response"`, the scale of the outcome,
#'   e.g. probabilities for a binomial model and expected counts for a count
#'   model. For a gaussian model the two are the same.
#' @param ... Not used.
#' @return A numeric vector of predicted values, one for each dyad or row.
#' @name predict
#' @examples
#' networkers <- manynet::ison_networkers |>
#'   manynet::to_subgraph(Discipline == "Sociology")
#' model1 <- estimate_mrqap(weight ~ ego(Citations) + alter(Citations),
#'                          networkers, times = 5)
#' # The fitted value of each dyad
#' head(predict(model1))
#' # A sender with 10 citations, to a receiver with 50
#' predict(model1, data.frame(ego_Citations = 10, alter_Citations = 50))
NULL

#' @rdname predict
#' @method predict netreg
#' @export
predict.netreg <- function(object, newdata = NULL,
                           type = c("link", "response"), ...) {
  type <- match.arg(type)
  if (is.null(newdata)) {
    return(.predict_fitted(object, type))
  }
  if (length(object$fixed_effects) > 0) {
    manynet::snet_abort(
      c("A model with {.val {object$fixed_effects}} fixed effects cannot predict {.arg newdata}.",
        i = "The fixed effects are absorbed, so a new dyad has no estimate of them.",
        i = "Leave out {.arg newdata} to predict the dyads the model was fitted on."))
  }
  coefs <- object$coefficients
  design <- .predict_design(newdata, names(coefs))
  eta <- drop(design %*% coefs)
  if (type == "link") return(eta)
  switch(object$family,
         gaussian = eta,
         binomial = stats::plogis(eta),
         poisson  = ,
         negbin   = exp(eta),
         zip      = {
           # `pscl::zeroinfl()` reads the same predictors for both parts.
           zero <- drop(design %*% object$zi_coefficients[names(coefs)])
           exp(eta) * (1 - stats::plogis(zero))
         })
}

#' @rdname predict
#' @method predict netlm
#' @export
predict.netlm <- function(object, newdata = NULL, ...) {
  coefs <- stats::coef(object)
  if (is.null(newdata)) {
    if (is.null(object$X)) {
      manynet::snet_abort(
        "Give {.arg newdata}: the result holds no design matrix to predict.")
    }
    newdata <- object$X
  }
  drop(as.matrix(newdata) %*% coefs)
}

#' @rdname predict
#' @method predict netlogit
#' @export
predict.netlogit <- function(object, newdata = NULL,
                             type = c("link", "response"), ...) {
  type <- match.arg(type)
  coefs <- stats::coef(object)
  if (is.null(newdata)) {
    if (is.null(object$X)) {
      manynet::snet_abort(
        "Give {.arg newdata}: the result holds no design matrix to predict.")
    }
    newdata <- object$X
  }
  eta <- drop(as.matrix(newdata) %*% coefs)
  if (type == "link") eta else stats::plogis(eta)
}

# The fitted values of the dyads the model was fitted on, from the baseline
# fit, so that random intercepts and fixed effects are included.
#' @keywords internal
#' @noRd
.predict_fitted <- function(object, type) {
  bm <- object$base$base_model
  if (inherits(bm, "zeroinfl")) {
    out <- if (type == "response") stats::predict(bm, type = "response") else
      log(stats::predict(bm, type = "count"))
  } else if (inherits(bm, "lm") && !inherits(bm, "glm")) {
    if (length(object$fixed_effects) > 0) {
      # A gaussian model absorbs its fixed effects by demeaning, so its fitted
      # values are demeaned too. The observed value less the residual is not.
      out <- object$pred[[object$dep]] - stats::residuals(bm)
    } else {
      out <- stats::fitted(bm)
    }
  } else if (inherits(bm, "lmerMod")) {
    out <- stats::fitted(bm)
  } else {
    # glm, MASS's negbin, lme4's glmerMod, and fixest's feglm and fepois
    out <- stats::predict(bm, type = type)
  }
  unname(out)
}

# A design matrix from newdata, with a column for each coefficient, in their
# order, and the intercept first.
#' @keywords internal
#' @noRd
.predict_design <- function(newdata, coef_names) {
  if (is.matrix(newdata) && is.null(colnames(newdata))) {
    if (ncol(newdata) != length(coef_names)) {
      manynet::snet_abort(
        c("{.arg newdata} has {ncol(newdata)} column{?s} and no names.",
          i = "Name its columns, or give it a column for each of the {length(coef_names)} coefficients, with the intercept first."))
    }
    return(newdata)
  }
  newdata <- as.data.frame(newdata, check.names = FALSE)
  cols <- lapply(coef_names, function(nm) {
    if (nm == "(Intercept)") return(rep(1, nrow(newdata)))
    if (nm %in% names(newdata)) return(as.numeric(newdata[[nm]]))
    parts <- strsplit(nm, ":", fixed = TRUE)[[1]]
    if (length(parts) > 1 && all(parts %in% names(newdata))) {
      return(Reduce(`*`, lapply(newdata[parts], as.numeric)))
    }
    NULL
  })
  absent <- coef_names[vapply(cols, is.null, logical(1))]
  if (length(absent) > 0) {
    manynet::snet_abort(
      c("{.arg newdata} has no column for {.val {absent}}.",
        i = "Name a column for each predictor, as the coefficients are named."))
  }
  out <- do.call(cbind, cols)
  colnames(out) <- coef_names
  out
}
