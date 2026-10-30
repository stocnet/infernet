# Reporting the results of network models and tests ####

#' Reporting network regression results
#'
#' @description
#'   These methods report a `netreg`, the result of [estimate_mrqap()],
#'   whether its class is `netreg_lm`, `netreg_glm`, or `netreg_css`.
#'
#'   - `print()` shows the result in three parts: a title that says what
#'     was fitted, `tidy()` of the estimates with significance marks, and a
#'     footer that says how the null distribution was made and the controls
#'     that differ from the default.
#'     At `options(snet_verbosity = "debug")`, the footer also names the
#'     methods that give more.
#'   - `summary()` gives the full table of the coefficients: the columns of
#'     `tidy()`, the lower and upper permutation p-values, a plain `title`
#'     of each effect, and the parts of each term. Its print leaves out
#'     `title` and the parts, which are too wide to read there, and adds the
#'     random intercepts of a mixed model, `glance()`, the unobserved dyads of
#'     each variable, and the terms that the formula dropped or changed, each
#'     after a blank line. `glance()` gives the permutations and the number of
#'     dyads, so the footer does not say them again.
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
#'   as `print()` shows it. Where no permutation reached the observed
#'   statistic, `print()` shows the p-value as below `1/times`, the smallest
#'   that the permutations can give, and marks it as that bound.
#'   For the confusion matrix of a binary outcome, see [diagnose_confusion()].
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
#' @param print_b,print_random Deprecated. `print_b = TRUE` also prints the
#'   p-values of the coefficients, rather than of their test statistics, and
#'   `print_random = TRUE` the random intercepts. Use `summary()`, which gives
#'   both.
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
#'   `tidy()`, `summary()`, and `glance()` return a tibble; the tibble of
#'   `summary()` has the class `summary_netreg` too, for its print.
#'   `tidy()` gives the columns `term`, `estimate`, `statistic`, and
#'   `p.value`.
#'   `summary()` gives these columns, `p.lower` and `p.upper`, a `title` that
#'   defines each effect, and the columns of the
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
#' css_model
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
  out$p.lower <- as.numeric(object$lower[2, ])
  out$p.upper <- as.numeric(object$larger[2, ])
  out <- out[c("term", "estimate", "statistic", "p.lower", "p.upper", "p.value")]
  # A fit made before the term table was kept has none to join.
  if (!is.null(object$terms)) {
    parts <- object$terms[match(out$term, object$terms$term),
                          setdiff(names(object$terms), "term")]
    out <- tibble::as_tibble(c(out, list(title = .term_titles(parts)), parts))
  }
  structure(out,
            class = c("summary_netreg", class(out)),
            header = .netreg_header(object),
            footer = .netreg_footer(object, null = FALSE),
            times = object$times,
            glance = tryCatch(glance(object), error = function(e) NULL),
            random = tryCatch(.random_intercepts(object), error = function(e) NULL),
            unobserved = object$missing$unobserved,
            record = object$record,
            extra = .summary_extra(object),
            fit_note = if (length(object$fixed_effects) > 0 && !is.null(object$r.squared))
              "R-squared measures only the variance within the fixed effects.",
            control = object$control)
}

# A plain definition of each coefficient's effect, from the term registry.
#' @keywords internal
#' @noRd
.term_titles <- function(parts) {
  registry <- .formula_terms()
  titles <- vapply(registry, function(t) t$title %||% NA_character_, character(1))
  effect <- parts$effect
  out <- unname(titles[match(effect, names(titles))])
  out[effect %in% "intercept"] <- "the intercept"
  inter <- effect %in% "interaction"
  if (any(inter) && !is.null(parts$operands)) {
    out[inter] <- vapply(parts$operands[inter], function(o)
      paste("the product of", paste(o, collapse = " and ")), character(1))
  }
  out
}

# What a family adds to its estimates: the dispersion of a negative binomial
# model, and the zero-inflation coefficients of a zero-inflated one.
#' @keywords internal
#' @noRd
.summary_extra <- function(x) {
  out <- character(0)
  if (!is.null(x$theta)) {
    out <- c(out, paste("Negative binomial dispersion (theta):",
                        format(signif(x$theta, 4))))
  }
  if (!is.null(x$zi_coefficients)) {
    out <- c(out, paste("Zero-inflation coefficients:",
                        paste(names(x$zi_coefficients),
                              format(signif(x$zi_coefficients, 4)),
                              sep = " = ", collapse = ", ")))
  }
  out
}

#' @rdname netreg_results
#' @export
print.summary_netreg <- function(x, ...) {
  times <- attr(x, "times")
  # A summary that lost its attributes, such as by `[`, prints as a tibble.
  if (is.null(times)) return(NextMethod())
  .print_title(attr(x, "header"))
  tab <- tibble::as_tibble(unclass(x)[intersect(
    c("term", "estimate", "statistic", "p.lower", "p.upper", "p.value"),
    names(x))])
  # An intercept that a fixed effect absorbs has no estimate to show.
  tab <- tab[!(tab$term == "(Intercept)" & is.na(tab$estimate)), , drop = FALSE]
  marks <- .print_tidy(tab, times)
  extra <- attr(x, "extra")
  if (length(extra)) .print_header(extra)
  random <- attr(x, "random")
  if (!is.null(random)) {
    .print_section("Random intercepts")
    print(random$variance)
    print(random$intercepts)
  }
  if (!is.null(attr(x, "glance"))) {
    .print_section("Fit of the observed model")
    .print_glance(attr(x, "glance"))
    if (length(attr(x, "fit_note"))) .print_header(attr(x, "fit_note"))
  }
  unobserved <- attr(x, "unobserved")
  if (any(unobserved > 0)) {
    .print_section("Unobserved dyads of each variable, left out of the model")
    print(tibble::tibble(variable = names(unobserved),
                         unobserved = as.integer(unobserved)))
  }
  record <- attr(x, "record")
  if (!is.null(record)) record <- record[record$status != "fits", , drop = FALSE]
  if (!is.null(record) && nrow(record) > 0) {
    .print_section("Terms that the formula changed")
    print(tibble::tibble(term = record$term, status = record$status,
                         as = ifelse(nzchar(record$as), record$as, NA_character_),
                         reason = ifelse(nzchar(record$reason), record$reason,
                                         NA_character_)))
  }
  .print_footer(c(attr(x, "footer"), .signif_legend(marks)),
                .methods_hint("tidy() and glance() for tibbles",
                              "diagnose_confusion() for a binary outcome",
                              "and plot() from {autograph} for a figure"))
  invisible(x)
}

# ---- print ------------------------------------------------------------------

# S3 dispatches on the most specific class, so each kind of result has its own
# method. All of them print the same three parts (see `R/report_print.R`):
# what was fitted, `tidy()` of the estimates, and how the null
# distribution was made. `print.netreg()` is the fallback for a `netreg` with
# no more specific class, such as one built by hand.

#' @rdname netreg_results
#' @order 1
#' @export
print.netreg_lm <- function(x, ..., print_b = FALSE, print_random = FALSE) {
  .print_netreg(x, print_b = print_b, print_random = print_random)
}

#' @rdname netreg_results
#' @order 2
#' @export
print.netreg_glm <- function(x, ..., print_b = FALSE, print_random = FALSE) {
  .print_netreg(x, print_b = print_b, print_random = print_random)
}

#' @rdname netreg_results
#' @order 4
#' @export
print.netreg <- function(x, ...) {
  cat("<netreg>\n")
  utils::str(unclass(x), max.level = 1)
  invisible(x)
}

#' @keywords internal
#' @noRd
.print_netreg <- function(x, print_b = FALSE, print_random = FALSE) {
  .print_title(.netreg_header(x))
  marks <- .print_tidy(.netreg_tidy(x), x$times)
  .print_footer(c(.netreg_footer(x), .signif_legend(marks)),
                .methods_hint("summary() for the fit and every p-value"))
  if (print_b) {
    manynet::snet_warn(
      c("{.arg print_b} is deprecated.",
        i = "{.fn summary} gives the p-values of the coefficients."))
    .print_b_table(x)
  }
  if (print_random) {
    manynet::snet_warn(
      c("{.arg print_random} is deprecated.",
        i = "{.fn summary} gives the random intercepts."))
    .print_random_intercepts(x)
  }
  invisible(x)
}

# `tidy()` of a fit as a print shows it: the intercept that a fixed effect
# absorbs has no estimate, and is left out.
#' @keywords internal
#' @noRd
.netreg_tidy <- function(x) {
  tab <- tidy.netreg(x)
  absorbed <- tab$term == "(Intercept)" & is.na(tab$estimate)
  tab[!absorbed, , drop = FALSE]
}

#' @keywords internal
#' @noRd
.fit_label <- function(x) {
  model <- switch(x$family %||% "",
                  gaussian = "Linear", binomial = "Logistic",
                  poisson = "Poisson", negbin = "Negative binomial",
                  zip = "Zero-inflated Poisson", x$family)
  if (any(unlist(x$random))) model <- paste(model, "mixed")
  model
}

# What was fitted.
#' @keywords internal
#' @noRd
.netreg_header <- function(x) {
  what <- paste0(.fit_label(x), " MRQAP of ", x$dep %||% "the outcome")
  parts <- character(0)
  if (length(x$fixed_effects) > 0) {
    parts <- c(parts, paste(paste(x$fixed_effects, collapse = " and "),
                            "fixed effects"))
  }
  random <- names(x$random)[unlist(x$random)]
  if (length(random) > 0) {
    random[random == "nets"] <- "network"
    parts <- c(parts, paste(paste(random, collapse = " and "),
                            "random intercepts"))
  }
  if (length(parts) > 0) what <- paste0(what, ", with ", paste(parts, collapse = " and "))
  what
}

# How the null distribution was made, from what, and with what controls.
#' @keywords internal
#' @noRd
.netreg_footer <- function(x, null = TRUE) {
  null <- if (!null) {
    NULL
  } else if (identical(x$permute, "outcome")) {
    paste0("Null: ", format(x$times), " permutations of the outcome.")
  } else {
    paste0("Null: ", format(x$times),
           " permutations of each predictor, by double semi-partialling.")
  }
  who <- if (inherits(x, "netreg_css")) {
    "senders, receivers, and reporters alike"
  } else if (isTRUE(x$panel)) {
    "the nodes of every wave alike"
  }
  if (!is.null(x$groups)) who <- paste(c(who %||% "the nodes", "within groups"),
                                       collapse = ", ")
  relabel <- if (!is.null(who)) paste0("Permutations relabel ", who, ".")
  said <- c("family", "permute", "directed", "loops", "groups",
            grep("^random_intercept_", names(x$control), value = TRUE))
  c(.sample_lines(x),
    null,
    relabel,
    if (isTRUE(x$robust_se)) "The t values use robust standard errors.",
    .controls_line(x$control, said))
}

# What a result was fitted on: the dyads that reached the model, after
# unobserved dyads, fixed ties, and loops were left out, and the waves of a
# panel. A reader needs both to interpret the estimates.
#' @keywords internal
#' @noRd
.sample_lines <- function(x) {
  m <- x$missing
  out <- NULL
  if (isTRUE(x$loops)) out <- "The model includes loops."
  if (isTRUE(m$fixed_ties > 0)) {
    out <- c(out, paste(format(m$fixed_ties),
                        "fixed ties left out, and kept in place under permutation."))
  }
  if (isTRUE(x$panel)) {
    waves <- length(unique(x$pred$nv))
    out <- c(out, paste0("Panel: ", waves,
                         " waves, each fitted against the wave before."))
  }
  out
}

# The p-values of the coefficients themselves, rather than of their test
# statistics, which `print_b = TRUE` printed before `summary()` gave them.
#' @keywords internal
#' @noRd
.print_b_table <- function(x) {
  tab <- tibble::tibble(term = names(x$coefficients),
                        estimate = as.numeric(x$coefficients),
                        p.lower = as.numeric(x$lower[1, ]),
                        p.upper = as.numeric(x$larger[1, ]),
                        p.value = as.numeric(x$abs[1, ]))
  .print_section("p-values of the coefficients, rather than of their test statistics")
  .print_tidy(tab, x$times)
  invisible()
}

# The names of the nodes, kept on a fit when it is made, so that its random
# intercepts can be named without the network. A list of networks keeps the
# names of its first.
#' @keywords internal
#' @noRd
.node_names <- function(.data) {
  if (.is_list_of_graphs(.data)) return(.node_names(.data[[1]]))
  tryCatch(manynet::node_names(manynet::as_tidygraph(.data)),
           error = function(e) NULL)
}

# The random intercepts of a mixed model, as `lme4::ranef()` gives them: one
# deviation from the fixed intercept for each sender, receiver, network, or
# reporter, named for the node or network.
#' @keywords internal
#' @noRd
.random_intercepts <- function(x) {
  bm <- x$base$base_model
  if (is.null(bm) || !inherits(bm, "merMod")) return(NULL)
  re <- lme4::ranef(bm)
  vc <- as.data.frame(lme4::VarCorr(bm))
  slots <- c(sv = "sender", rv = "receiver", nv = "network", bv = "reporter")
  names_of <- x$node_names
  out <- lapply(names(re), function(nm) {
    idx <- rownames(re[[nm]])
    who <- if (nm %in% names(slots)) slots[[nm]] else nm
    label <- idx
    if (nm != "nv" && !is.null(names_of)) {
      at <- suppressWarnings(as.integer(idx))
      ok <- !is.na(at) & at >= 1 & at <= length(names_of)
      label[ok] <- names_of[at[ok]]
    }
    tibble::tibble(group = who, level = label,
                   intercept = as.numeric(re[[nm]][, 1]))
  })
  vc <- vc[vc$grp != "Residual", c("grp", "vcov", "sdcor")]
  grp <- ifelse(vc$grp %in% names(slots), slots[vc$grp], vc$grp)
  list(intercepts = do.call(rbind, out),
       variance = tibble::tibble(group = unname(grp), variance = vc$vcov,
                                 sd = vc$sdcor))
}

#' @keywords internal
#' @noRd
.print_random_intercepts <- function(x) {
  re <- .random_intercepts(x)
  if (is.null(re)) return(invisible())
  .print_section("Random intercepts")
  print(re$variance)
  print(re$intercepts)
  invisible()
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
#' @examplesIf requireNamespace("ergm", quietly = TRUE)
#' fit <- suppressMessages(
#'   ergm::ergm(manynet::as_network(manynet::ison_adolescents) ~ edges))
#' tidy(fit)
#' glance(fit)
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

# A CSS result prints as any other result, and its header and footer say how
# many reporters it was fitted on, which a reader needs to interpret it.
#' @rdname netreg_results
#' @order 3
#' @export
print.netreg_css <- function(x, ..., print_b = FALSE, print_random = FALSE) {
  .print_netreg(x, print_b = print_b, print_random = print_random)
}

# ---- diffusion models ---------------------------------------------------------

#' @details
#'   `print()` shows a title that says how many diffusions were simulated,
#'   the report of each step, and a footer with the steps until
#'   every node adopted. `summary()` gives those steps for each simulation.
#' @rdname play_diffusions
#' @param object,x A `diffs_model`.
#' @export
summary.diffs_model <- function(object, ...) {
  fin <- (object$I != object$n) * 1
  toa <- tapply(fin, object$sim, sum) + 1
  tibble::tibble(sim = as.integer(names(toa)), toa = as.numeric(toa))
}

#' @rdname play_diffusions
#' @export
print.diffs_model <- function(x, ...) {
  y <- as.data.frame(x)
  y <- y[, colSums(abs(y), na.rm = TRUE) != 0 | names(y) %in% c("sim", "time"),
         drop = FALSE]
  y$I_new <- NULL
  sims <- length(unique(x$sim))
  states <- intersect(c("S", "E", "I", "R"), names(y))
  .print_title(paste0(sims, " simulated ", paste(states, collapse = ""),
                     " diffusion", if (sims != 1) "s"))
  print(tibble::as_tibble(y), ...)
  toa <- summary.diffs_model(x)$toa
  .print_footer(c(paste0("Steps until every node adopted: median ",
                         stats::median(toa), ", from ", min(toa), " to ",
                         max(toa), "."),
                  .controls_line(attr(x, "control"))),
                .methods_hint("summary() for the steps of each simulation",
                              "test_gof() and test_distributions() to compare",
                              "and plot() from {autograph} for the curves"))
  invisible(x)
}
