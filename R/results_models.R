# Reporting the results of network models and tests ####

#' Reporting network regression results
#'
#' @description
#'   These methods report a `netreg`, the result of [estimate_mrqap()],
#'   whether its class is `netreg_lm`, `netreg_glm`, or `netreg_css`.
#'
#'   - `print()` shows the result as a report: how the null distribution was
#'     made, the estimates, and the permutation p-values.
#'   - `summary()` gives the full table of the coefficients: the columns of
#'     `tidy()` and the parts of each term.
#'   - `tidy()` and `glance()` give the results as a tibble, after `{broom}`:
#'     one row for each coefficient, and one row for the whole model.
#'     So an MRQAP result can sit in one table beside an ERGM or a SIENA
#'     one (see [other_results]).
#'   - `nobs()` gives the number of dyads that the model used:
#'     unobserved dyads, fixed ties, and the loops that the model leaves out
#'     are not counted.
#'   - `logLik()` and `deviance()` give the log-likelihood and the deviance
#'     of the observed model, so `AIC()` and `BIC()` work too.
#'   - `fitted()` and `residuals()` give one value for each dyad that the
#'     model used, in the rows of the dyad-level data.
#'   - `formula()` gives the formula that was fitted, after [specify_mrqap()]
#'     read it, and `terms()` gives the terms of the columns that were fitted,
#'     as the coefficients name them.
#'     For the formula of chosen rows of a table of terms, see [specify_terms].
#' @details
#'   The p-values come from the permutations. Every other method reads the
#'   observed model, which the permutations do not change.
#'   A permutation test gives a p-value but no standard error,
#'   so `tidy()` and `summary()` have no `std.error` column.
#'   `p.value` is read from the permutations of the test statistic,
#'   as `print()` shows it.
#'   When the predictors are permuted, the intercept has no test,
#'   and its `p.value` is `NA`.
#'
#'   Two methods of `{stats}` work without a method here:
#'   `coef()` gives the estimates, and `update()` fits the model again,
#'   with a changed formula, `times`, or `control`, from the call that the
#'   result keeps.
#' @section Methods left out:
#'   - `vcov()` and `confint()`: a covariance or an interval from the observed
#'     model would not come from the permutations, and could be read as the
#'     basis of the permutation p-values.
#'   - `weights()`: a network regression uses no weights.
#'   - `[`: a result has no rows to select. Use `$` or `[[` for an element.
#'   - `profile()`: a permutation fit has no likelihood to profile.
#' @section Fixed effects:
#'   A gaussian model absorbs its fixed effects by demeaning. Its
#'   log-likelihood and deviance are those of the model with a dummy for each
#'   group, and `logLik()` counts the absorbed parameters in its `df`, so
#'   that `AIC()`, `BIC()`, and `glance()` are those of that model too.
#'   `fitted()` gives the observed values less the residuals, not the
#'   demeaned fitted values.
#' @param x,object A `netreg`, from [estimate_mrqap()].
#' @param print_b Logical; also print coefficient-based (not t-based)
#'   permutation p-values.
#' @param print_random Logical; print random intercepts when present.
#' @param exponentiate Logical, whether to exponentiate the estimates,
#'   e.g. to give odds ratios for a logistic model.
#' @param alternative Which permutation p-value to report:
#'   `"two.sided"` (the default) for Pr(>=|t|),
#'   `"less"` for Pr(<=t), or `"greater"` for Pr(>=t).
#' @param ... For `residuals()`, passed to the `residuals()` method of the
#'   observed model, such as `type = "pearson"` for a `glm`.
#'   For `summary()`, passed to `tidy()`, such as `alternative = "greater"`.
#'   Otherwise not used.
#' @return `print()` returns `x`, invisibly.
#'   `tidy()`, `summary()`, and `glance()` return a tibble.
#'   `tidy()` gives the columns `term`, `estimate`, `statistic`, and
#'   `p.value`.
#'   `summary()` gives these columns and the columns of the
#'   fit's term table, `fit$terms`: the `effect` (the formula term) each
#'   coefficient comes from, its `layer`, `attribute`, `category`, and
#'   `reference` category, and for an interaction its `operands`.
#'   These group the coefficients without parsing their names.
#'   `glance()` gives the fit statistics that the model keeps.
#'   `nobs()` gives an integer, `logLik()` a `logLik` object,
#'   `deviance()` a number, `fitted()` and `residuals()` a numeric vector,
#'   `formula()` a formula, and `terms()` a `terms` object.
#' @family models
#' @examples
#' css_model <- estimate_mrqap(advice ~ same(dept) + reports,
#'                             manynet::ison_hightech, times = 5)
#' print(css_model, print_b = TRUE)
#' summary(css_model)
#' tidy(css_model)
#' glance(css_model)
#' nobs(css_model)
#' AIC(css_model)
#' formula(css_model)
#' head(fitted(css_model))
#' @name netreg_results
NULL

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

# infernet: netreg ####

#' @rdname netreg_results
#' @method tidy netreg
#' @export
tidy.netreg <- function(x, exponentiate = FALSE,
                        alternative = c("two.sided", "less", "greater"), ...) {
  alternative <- match.arg(alternative)
  p <- switch(alternative,
              two.sided = x$abs, less = x$lower, greater = x$larger)
  estimate <- as.numeric(x$coefficients)
  if (exponentiate) {
    if (identical(x$family, "gaussian"))
      manynet::snet_warn("The estimates are exponentiated, but the model is gaussian.")
    estimate <- exp(estimate)
  }
  tibble::tibble(term = names(x$coefficients),
                 estimate = estimate,
                 statistic = as.numeric(x$obs),
                 p.value = as.numeric(p[2, ]))
}

#' @rdname netreg_results
#' @export
summary.netreg <- function(object, ...) {
  out <- tidy.netreg(object, ...)
  # A fit made before the term table was kept has none to join.
  if (is.null(object$terms)) return(out)
  parts <- object$terms[match(out$term, object$terms$term),
                        setdiff(names(object$terms), "term")]
  tibble::as_tibble(c(out, parts))
}

#' @rdname netreg_results
#' @method glance netreg
#' @export
glance.netreg <- function(x, ...) {
  tibble::tibble(family = x$family,
                permute = x$permute,
                times = x$times,
                nobs = .result_nobs(x))
}

#' @rdname netreg_results
#' @method glance netreg_lm
#' @export
glance.netreg_lm <- function(x, ...) {
  df <- .result_num(.result_df_residual(x))
  tibble::tibble(r.squared = .result_num(x$r.squared),
                adj.r.squared = .result_num(x$adj.r.squared),
                sigma = .result_num(sqrt(sum(stats::residuals(x)^2) / df)),
                logLik = .result_num(as.numeric(stats::logLik(x))),
                AIC = .result_num(stats::AIC(x)),
                BIC = .result_num(stats::BIC(x)),
                df.residual = .result_num(df),
                nobs = .result_nobs(x),
                permute = x$permute,
                times = x$times)
}

#' @rdname netreg_results
#' @method glance netreg_glm
#' @export
glance.netreg_glm <- function(x, ...) {
  fit <- tryCatch(.observed_model(x), error = function(e) NULL)
  out <- tibble::tibble(family = x$family,
                       logLik = .result_num(as.numeric(stats::logLik(x))),
                       AIC = .result_num(stats::AIC(x)),
                       BIC = .result_num(stats::BIC(x)),
                       deviance = .result_num(stats::deviance(x)),
                       null.deviance = .result_num(fit$null.deviance),
                       df.residual = .result_num(.result_df_residual(x)),
                       nobs = .result_nobs(x),
                       permute = x$permute,
                       times = x$times)
  if (!is.null(x$theta)) out$theta <- x$theta
  out
}

#' @rdname netreg_results
#' @method glance netreg_css
#' @export
glance.netreg_css <- function(x, ...) {
  out <- NextMethod()
  if (!is.null(x$pred$bv)) out$reporters <- length(unique(x$pred$bv))
  out
}

# A fit statistic that the model does not keep, or that its fit cannot give,
# is NA rather than an error.
.result_num <- function(expr) {
  out <- tryCatch(suppressWarnings(expr), error = function(e) NULL)
  if (is.null(out) || length(out) != 1) NA_real_ else as.numeric(out)
}

.result_nobs <- function(x) {
  nobs.netreg(x)
}

# The residual degrees of freedom of the observed model, less the parameters
# that a gaussian model absorbed by demeaning.
.result_df_residual <- function(x) {
  df <- stats::df.residual(.observed_model(x))
  if (!is.null(x$fixed_df)) df <- df - x$fixed_df
  df
}

# The observed model, which a result keeps as `simple_fit` and also in
# `base`, so that `less_mem = TRUE` still leaves one to read.
#' @keywords internal
#' @noRd
.observed_model <- function(x) {
  bm <- x$simple_fit
  if (is.null(bm)) bm <- x$base$base_model
  if (is.null(bm)) {
    manynet::snet_abort(
      c("The result keeps no observed model.",
        i = "Fit it again with {.fn estimate_mrqap}."))
  }
  bm
}

#' @rdname netreg_results
#' @exportS3Method stats::nobs
nobs.netreg <- function(object, ...) {
  # From the observed model, since {fixest} leaves out the dyads of a group
  # with one outcome value; else every row of the dyad-level data.
  n <- tryCatch(stats::nobs(.observed_model(object)), error = function(e) NULL)
  if (length(n) == 1 && !is.na(n)) return(as.integer(n))
  if (is.null(object$pred)) NA_integer_ else nrow(object$pred)
}

#' @rdname netreg_results
#' @exportS3Method stats::logLik
logLik.netreg <- function(object, ...) {
  bm <- .observed_model(object)
  ll <- stats::logLik(bm)
  # {fixest} gives no `df`, and counts its absorbed effects in `nparams`.
  if (is.null(attr(ll, "df")) && !is.null(bm$nparams)) {
    attr(ll, "df") <- bm$nparams
  }
  if (!is.null(object$fixed_df)) {
    attr(ll, "df") <- attr(ll, "df") + object$fixed_df
  }
  attr(ll, "nobs") <- nobs.netreg(object)
  class(ll) <- "logLik"
  ll
}

#' @rdname netreg_results
#' @exportS3Method stats::deviance
deviance.netreg <- function(object, ...) {
  stats::deviance(.observed_model(object))
}

#' @rdname netreg_results
#' @exportS3Method stats::fitted
fitted.netreg <- function(object, ...) {
  .observed_model(object)
  stats::predict(object, type = "response")
}

#' @rdname netreg_results
#' @exportS3Method stats::residuals
residuals.netreg <- function(object, ...) {
  unname(stats::residuals(.observed_model(object), ...))
}

#' @rdname netreg_results
#' @exportS3Method stats::formula
formula.netreg <- function(x, ...) {
  if (is.null(x$formula)) .refit_abort("formula")
  x$formula
}

#' @rdname netreg_results
#' @exportS3Method stats::terms
terms.netreg <- function(x, ...) {
  if (is.null(x$model_formula)) .refit_abort("terms")
  stats::terms(x$model_formula)
}

#' @keywords internal
#' @noRd
.refit_abort <- function(fn) {
  manynet::snet_abort(
    c("The result does not keep what {.fn {fn}} reads.",
      i = "It was made by an earlier version: fit it again with {.fn estimate_mrqap}."))
}

# migraph: netlm and netlogit ####

#' Tidying the results of other network models
#'
#' @description
#'   `tidy()` and `glance()` give the results of other packages' network
#'   models as a tibble, after `{broom}`, with the same columns as for a
#'   `netreg` (see [netreg_results]) where they mean the same.
#'   So an MRQAP result can sit in one table beside an ERGM or a SIENA one.
#'   They cover:
#'
#'   - `netlm` and `netlogit`, from `net_regression()` in `{migraph}` before
#'     2.0.0, which also have a `print()` method;
#'   - `ergm`, from `ergm()` in `{ergm}`;
#'   - `sienaFit`, from `siena07()` in `{RSiena}`.
#' @details
#'   For a `{migraph}` result, `conf.int = TRUE` adds the quantiles of the
#'   permuted test statistics. These describe the null distribution,
#'   not an interval around the estimate.
#' @param x A `netlm`, `netlogit`, `ergm`, or `sienaFit`.
#' @param conf.int Logical, whether to add the columns `conf.low` and
#'   `conf.high`.
#' @param conf.level The level of the interval that `conf.int` adds.
#' @param exponentiate Logical, whether to exponentiate the estimates,
#'   e.g. to give odds ratios for a logistic model.
#' @param deviance,mcmc For an `ergm`, whether to add the deviance columns
#'   or the MCMC settings.
#' @param ... Further arguments, passed to `summary()` for an `ergm`
#'   and otherwise not used.
#' @return `tidy()` and `glance()` return a tibble, and `print()` returns
#'   `x`, invisibly.
#' @family models
#' @name other_results
NULL
#' @rdname other_results
#' @method tidy netlm
#' @export
tidy.netlm <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  result <- tibble::tibble(term = x$names,
                           estimate = x$coefficients,
                           statistic = x$tstat,
                           p.value = x$pgreqabs)
  if (conf.int) result <- .null_interval(result, x, conf.level)
  result
}

#' @rdname other_results
#' @method tidy netlogit
#' @export
tidy.netlogit <- function(x, conf.int = FALSE, conf.level = 0.95,
                          exponentiate = FALSE, ...) {
  result <- tibble::tibble(term = x$names,
                           estimate = x$coefficients,
                           statistic = x$tstat,
                           p.value = x$pgreqabs)
  if (conf.int) result <- .null_interval(result, x, conf.level)
  if (exponentiate) result <- .exponentiate(result)
  result
}

.null_interval <- function(result, x, conf.level) {
  probs <- c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2)
  ci <- apply(x$dist, 2, stats::quantile, probs, na.rm = TRUE)
  result$conf.low <- ci[1, ]
  result$conf.high <- ci[2, ]
  result
}

#' @rdname other_results
#' @method glance netlm
#' @export
glance.netlm <- function(x, ...) {
  mss <- sum((stats::fitted(x) - mean(stats::fitted(x)))^2)
  rss <- sum(stats::resid(x)^2)
  qn <- NROW(x$qr$qr)
  df.int <- x$intercept
  rdf <- qn - x$rank
  resvar <- rss / rdf
  fvalue <- (mss / (x$rank - df.int)) / resvar
  numdf <- x$rank - df.int
  r.squared <- mss / (mss + rss)
  tibble::tibble(
    r.squared = r.squared,
    adj.r.squared = 1 - (1 - r.squared) * ((qn - df.int) / rdf),
    sigma = sqrt(resvar),
    statistic = fvalue,
    p.value = stats::pf(fvalue, numdf, rdf, lower.tail = FALSE),
    df = numdf,
    df.residual = stats::df.residual(x),
    nobs = x$n
  )
}

#' @rdname other_results
#' @method glance netlogit
#' @export
glance.netlogit <- function(x, ...) {
  tibble::tibble(
    pseudo.r.squared = (x$null.deviance - x$deviance) /
      (x$null.deviance - x$deviance + x$df.null),
    AIC = x$aic,
    AICc = x$aic + (2 * x$rank^2 + 2 * x$rank) / (x$n - x$rank - 1),
    BIC = x$bic,
    chi.squared = 1 - stats::pchisq(x$null.deviance - x$deviance,
                                    df = x$df.null - x$df.residual),
    deviance = x$deviance,
    null.deviance = x$null.deviance,
    df.residual = stats::df.residual(x),
    nobs = x$n
  )
}

#' @rdname other_results
#' @export
print.netlm <- function(x, ...) {
  cat("# Fitted model results\n")
  print(tidy(x))
  cat("\n# Model summary statistics\n")
  print(glance(x))
  invisible(x)
}

#' @rdname other_results
#' @export
print.netlogit <- function(x, ...) {
  cat("# Fitted model results\n")
  print(tidy(x))
  cat("\n# Model summary statistics\n")
  print(glance(x))
  invisible(x)
}

# ergm ####

#' @rdname other_results
#' @method tidy ergm
#' @export
tidy.ergm <- function(x, conf.int = FALSE, conf.level = 0.95,
                      exponentiate = FALSE, ...) {
  coefs <- summary(x, ...)$coefficients
  out <- tibble::tibble(term = rownames(coefs))
  cols <- c(estimate = "Estimate", std.error = "Std. Error",
            mcmc.error = "MCMC %", statistic = "z value",
            p.value = "Pr(>|z|)")
  for (nm in names(cols)) {
    if (cols[[nm]] %in% colnames(coefs)) out[[nm]] <- unname(coefs[, cols[[nm]]])
  }
  if (conf.int) {
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    out$conf.low <- out$estimate - z * out$std.error
    out$conf.high <- out$estimate + z * out$std.error
  }
  # An ERGM's coefficients are conditional log-odds, so their exponents are
  # odds ratios. `{ergm}` no longer keeps the `glm` this once checked a link on.
  if (exponentiate) out <- .exponentiate(out)
  out
}

#' @rdname other_results
#' @method glance ergm
#' @export
glance.ergm <- function(x, deviance = FALSE, mcmc = FALSE, ...) {
  s <- summary(x, ...)
  out <- tibble::tibble(independence = s$independence,
                        iterations = x$iterations,
                        logLik = as.numeric(stats::logLik(x)))
  if (deviance) {
    if (!requireNamespace("ergm", quietly = TRUE))
      manynet::snet_abort("Install {.pkg ergm} for {.code deviance = TRUE}.")
    dyads <- stats::nobs(x)
    lln <- ergm::logLikNull(x)
    out$null.deviance <- if (is.na(lln)) 0 else -2 * as.numeric(lln)
    out$df.null <- dyads
    out$residual.deviance <- -2 * out$logLik
    out$df.residual <- dyads - length(x$coefficients)
  }
  out$AIC <- stats::AIC(x)
  out$BIC <- stats::BIC(x)
  if (mcmc) {
    if (isTRUE(x$MPLE_is_MLE)) {
      manynet::snet_info(c("Though {.fn glance} was given {.code mcmc = TRUE},
                  the model was not fitted using MCMC,",
                           "i" = "The corresponding columns will be omitted."))
    } else {
      out$MCMC.interval <- x$control$MCMC.interval
      out$MCMC.burnin <- x$control$MCMC.burnin
      out$MCMC.samplesize <- x$control$MCMC.samplesize
    }
  }
  out
}

# RSiena: sienaFit ####

#' @rdname other_results
#' @method tidy sienaFit
#' @export
tidy.sienaFit <- function(x, ...) {
  tibble::tibble(dv = x$effects$name,
                 term = x$effects$effectName,
                 estimate = x$theta,
                 std.error = x$se,
                 statistic = x$theta / x$se)
}

#' @rdname other_results
#' @method glance sienaFit
#' @export
glance.sienaFit <- function(x, ...) {
  tibble::tibble(tmax = x$tmax,
                 tconv.max = x$tconv.max[, 1])
}

.exponentiate <- function(data) {
  for (col in intersect(c("estimate", "conf.low", "conf.high"), names(data))) {
    data[[col]] <- exp(data[[col]])
  }
  data
}

# A CSS result prints its own way, whatever its family: it says how many
# reporters it was fitted on, which a reader needs to interpret it.
#' @rdname netreg_results
#' @order 3
#' @export
print.netreg_css <- function(x, ...) {
  
  if (!any(x$random)) {
    cat("\nGeneralized Linear Network Model for a cognitive social structure\n\n")
  } else {
    cat("\nGeneralized Linear Mixed Network Model for a cognitive social structure\n\n")
  }
  
  # A result should say what it was fitted on: how many reporters, and how many
  # of their reports reached the model after missing ones were dropped.
  if (!is.null(x$pred$bv)) {
    reporters <- length(unique(x$pred$bv))
    cat("Fitted on", format(nrow(x$pred)), "reported dyads from",
        reporters, if (reporters == 1) "reporter.\n" else "reporters.\n")
  }
  if (isTRUE(x$random[["reporter"]]))
    cat("Each reporter has a random intercept.\n")
  cat(.fixed_label(x))
  
  if (!is.null(x$theta))
    cat("Negative binomial dispersion (theta):", format(round(x$theta, 4)), "\n")
  if (!is.null(x$zi_coefficients)) {
    cat("Zero-inflation coefficients:\n")
    cat("  ", paste(names(x$zi_coefficients),
                    format(round(x$zi_coefficients, 4)),
                    sep = " = ", collapse = ", "), "\n")
  }
  
  if (!is.null(x$groups))
    cat("Permutations were performed within groups only.\n")
  
  if (x$permute == "outcome")
    cat("The outcome array Y was permuted", format(x$times), "times.\n")
  if (x$permute == "predictor") {
    cat("Significance was estimated using Dekker's\n")
    cat("  'semi-partialling plus' procedure with",
        format(x$times), "permutations.\n")
  }
  
  if (x$robust_se)
    cat("T-values are based on robust standard errors.\n")
  
  if (x$loops) {
    cat("Diagonal values (loops) were used in the estimation.\n",
        "  Results may be biased because of that.\n")
  } else {
    cat("Diagonal values (loops) were ignored.\n")
  }
  cat("The outcome was treated as",
      format(paste0(.directed_label(x$directed), ".")), "\n")
  
  glm_tab(x)
  
  if (!is.null(x$confusion_matrix)) {
    cat("\n")
    print(x$confusion_matrix)
  }
  
  cat("\n")
  invisible(x)
}
