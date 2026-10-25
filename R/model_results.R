# Reporting the results of network models and tests ####

#' Reporting network regression and test results
#'
#' @description
#'   `print()` shows the result of `estimate_mrqap()` or of a test as a
#'   report: how the null distribution was made, the estimates,
#'   and the permutation p-values.
#'
#'   `tidy()` and `glance()` return the same results as a tibble, after
#'   `{broom}`. `tidy()` gives one row for each term of a model, and
#'   `glance()` gives one row for the whole model. They cover:
#'
#'   - `netreg`, `netreg_lm`, `netreg_glm`, and `netreg_css`,
#'     from `estimate_mrqap()`
#'   - `network_test`, from `test_random()`, `test_configuration()`, and
#'     `test_permutation()`
#'   - `netlm` and `netlogit`, from `net_regression()` in `{migraph}`
#'   - `ergm`, from `ergm()` in `{ergm}`
#'   - `sienaFit`, from `siena07()` in `{RSiena}`
#'
#'   So an MRQAP result can sit in one table beside an ERGM or a SIENA one.
#' @details
#'   A permutation test gives a p-value but no standard error,
#'   so the tidied regression results have no `std.error` column.
#'   For a `netreg`, `p.value` is read from the permutations of the test
#'   statistic, as `print()` shows it.
#'   When the predictors are permuted, the intercept has no test,
#'   and its `p.value` is `NA`.
#'
#'   For a `{migraph}` result, `conf.int = TRUE` adds the quantiles of the
#'   permuted test statistics. These describe the null distribution,
#'   not an interval around the estimate.
#' @param x A model or test result, e.g. a `netreg` object, as returned by
#'   `estimate_mrqap()`, or a `network_test` object, as returned by
#'   `test_random()`, `test_configuration()`, or `test_permutation()`.
#' @param print_b Logical; also print coefficient-based (not t-based)
#'   permutation p-values.
#' @param print_random Logical; print random intercepts when present.
#' @param max.length,digits Not used.
#' @param conf.int Logical, whether to add the columns `conf.low` and
#'   `conf.high`.
#' @param conf.level The level of the interval that `conf.int` adds.
#' @param exponentiate Logical, whether to exponentiate the estimates,
#'   e.g. to give odds ratios for a logistic model.
#' @param alternative For a `netreg`, which permutation p-value
#'   to report: `"two.sided"` (the default) for Pr(>=|t|),
#'   `"less"` for Pr(<=t), or `"greater"` for Pr(>=t).
#' @param deviance,mcmc For an `ergm`, whether to add the deviance columns
#'   or the MCMC settings.
#' @param ... Further arguments, passed to `summary()` for an `ergm`
#'   and otherwise not used.
#' @return `print()` returns `x`, invisibly.
#'   `tidy()` and `glance()` return a tibble.
#'   `tidy()` gives the columns `term`, `estimate`, `statistic`, and
#'   `p.value` for a regression.
#'   For a `netreg`, it also adds the columns of the fit's term table,
#'   `fit$terms`: the `effect` (the formula term) each coefficient comes from,
#'   its `layer`, `attribute`, `category`, and `reference` category,
#'   and for an interaction its `operands`.
#'   These group the coefficients without parsing their names.
#'   and `glance()` gives the fit statistics that the model keeps.
#'   For a `network_test`, both give one row with the observed statistic,
#'   the mean and standard deviation of the null distribution,
#'   and the share of the null distribution at or below (`p.lower`)
#'   and at or above (`p.upper`) the observed statistic.
#' @family models
#' @examples
#' css_model <- estimate_mrqap(advice ~ same(dept) + reports,
#'                             manynet::ison_hightech, times = 5)
#' print(css_model, print_b = TRUE)
#' tidy(css_model)
#' glance(css_model)
#' @name results
NULL

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

# infernet: netreg ####

#' @rdname results
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
  out <- tibble::tibble(term = names(x$coefficients),
                        estimate = estimate,
                        statistic = as.numeric(x$obs),
                        p.value = as.numeric(p[2, ]))
  # A fit made before the term table was kept has none to join.
  if (is.null(x$terms)) return(out)
  parts <- x$terms[match(out$term, x$terms$term), setdiff(names(x$terms), "term")]
  tibble::as_tibble(c(out, parts))
}

#' @rdname results
#' @method glance netreg
#' @export
glance.netreg <- function(x, ...) {
  tibble::tibble(family = x$family,
                permute = x$permute,
                times = x$times,
                nobs = .result_nobs(x))
}

#' @rdname results
#' @method glance netreg_lm
#' @export
glance.netreg_lm <- function(x, ...) {
  fit <- x$simple_fit
  tibble::tibble(r.squared = .result_num(x$r.squared),
                adj.r.squared = .result_num(x$adj.r.squared),
                sigma = .result_num(stats::sigma(fit)),
                logLik = .result_num(as.numeric(stats::logLik(fit))),
                AIC = .result_num(stats::AIC(fit)),
                BIC = .result_num(stats::BIC(fit)),
                df.residual = .result_num(stats::df.residual(fit)),
                nobs = .result_nobs(x),
                permute = x$permute,
                times = x$times)
}

#' @rdname results
#' @method glance netreg_glm
#' @export
glance.netreg_glm <- function(x, ...) {
  fit <- x$simple_fit
  out <- tibble::tibble(family = x$family,
                       logLik = .result_num(as.numeric(stats::logLik(fit))),
                       AIC = .result_num(stats::AIC(fit)),
                       BIC = .result_num(stats::BIC(fit)),
                       deviance = .result_num(stats::deviance(fit)),
                       null.deviance = .result_num(fit$null.deviance),
                       df.residual = .result_num(stats::df.residual(fit)),
                       nobs = .result_nobs(x),
                       permute = x$permute,
                       times = x$times)
  if (!is.null(x$theta)) out$theta <- x$theta
  out
}

#' @rdname results
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
  if (is.null(x$pred)) NA_integer_ else nrow(x$pred)
}

# infernet: network_test ####

#' @rdname results
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

#' @rdname results
#' @method glance network_test
#' @export
glance.network_test <- function(x, ...) {
  tidy.network_test(x, ...)
}

# migraph: netlm and netlogit ####

#' @rdname results
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

#' @rdname results
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

#' @rdname results
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

#' @rdname results
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

#' @rdname results
#' @export
print.netlm <- function(x, ...) {
  cat("# Fitted model results\n")
  print(tidy(x))
  cat("\n# Model summary statistics\n")
  print(glance(x))
  invisible(x)
}

#' @rdname results
#' @export
print.netlogit <- function(x, ...) {
  cat("# Fitted model results\n")
  print(tidy(x))
  cat("\n# Model summary statistics\n")
  print(glance(x))
  invisible(x)
}

# ergm ####

#' @rdname results
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
  if (exponentiate) {
    if (is.null(x$glm) ||
        (x$glm$family$link != "logit" && x$glm$family$link != "log")) {
      manynet::snet_warn("Coefficients will be exponentiated, but the model did not
                use a {.code log} or {.code logit} link.")
    }
    out <- .exponentiate(out)
  }
  out
}

#' @rdname results
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

#' @rdname results
#' @method tidy sienaFit
#' @export
tidy.sienaFit <- function(x, ...) {
  tibble::tibble(dv = x$effects$name,
                term = x$effects$effectName,
                estimate = x$theta,
                std.error = x$se,
                statistic = x$theta / x$se)
}

#' @rdname results
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
