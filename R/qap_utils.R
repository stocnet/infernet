# QAP utilities ---------------------------------------------------------------
#
# Support functions for the QAPglm / net_regression engine.  Ported from the
# MrQAP package (Robert W. Krause).  All functions are internal.

# ---- formula parsing --------------------------------------------------------

#' Parse a QAP formula into its components
#' @keywords internal
#' @noRd
parse_qap_formula <- function(formula, fixest_se_cluster = NULL) {
  dependent <- all.vars(formula)[1]

  formula_str <- paste(deparse(formula, width.cutoff = 500), collapse = " ")
  has_pipe  <- grepl("\\|", formula_str)
  has_paren <- grepl("\\(", formula_str)

  if (has_pipe && has_paren) {
    main <- all.vars(reformulas::nobars(formula))[-1]
    fixed_effects <- NULL
    use_fixest    <- FALSE
    has_random    <- TRUE
    all_data_vars <- main
  } else if (has_pipe && !has_paren) {
    main          <- all.vars(formula[[3]][[2]])
    fixed_effects <- all.vars(formula[[3]][[3]])
    has_random    <- FALSE
    use_fixest    <- TRUE
    all_data_vars <- c(main, fixed_effects)
  } else {
    main          <- all.vars(formula[-1])
    fixed_effects <- NULL
    has_random    <- FALSE
    use_fixest    <- !is.null(fixest_se_cluster)
    all_data_vars <- main
  }

  if (!is.null(fixest_se_cluster)) {
    use_fixest <- TRUE
    if (!(fixest_se_cluster %in% all_data_vars)) {
      all_data_vars <- c(all_data_vars, fixest_se_cluster)
    }
  }

  list(dependent     = dependent,
       main          = main,
       fixed_effects = fixed_effects,
       has_random    = has_random,
       use_fixest    = use_fixest,
       all_data_vars = all_data_vars)
}


#' Append random-intercept terms to a formula
#' @keywords internal
#' @noRd
build_internal_formula <- function(formula,
                                   rin = FALSE, ris = FALSE,
                                   rir = FALSE, rip = FALSE) {
  mod_str <- paste(deparse(formula, width.cutoff = 500), collapse = " ")
  rand_int <- ""
  if (rin) rand_int <- paste(rand_int, "+ (1|nv)")
  if (ris) rand_int <- paste(rand_int, "+ (1|sv)")
  if (rir) rand_int <- paste(rand_int, "+ (1|rv)")
  if (rip) rand_int <- paste(rand_int, "+ (1|pv)")
  if (nchar(trimws(rand_int)) > 0) {
    stats::as.formula(paste(mod_str, rand_int))
  } else {
    formula
  }
}


# ---- validation -------------------------------------------------------------

#' @keywords internal
#' @noRd
validate_qap_input <- function(matlist, parsed, css = FALSE) {
  dep <- parsed$dependent
  if (!(dep %in% names(matlist))) {
    manynet::snet_abort("Dependent variable {.val {dep}} not found in the data.")
  }
  structural_vars <- c("sv", "rv", "nv", "pv")
  for (v in parsed$all_data_vars) {
    if (v %in% structural_vars) next
    if (!(v %in% names(matlist))) {
      manynet::snet_abort("Predictor {.val {v}} not found in the data.")
    }
  }

  y <- matlist[[dep]]
  large <- is.list(y)

  if (!css) {
    if (!large) {
      if (!is.matrix(y))
        manynet::snet_abort("The dependent variable {.val {dep}} must be a matrix.")
    } else {
      for (i in seq_along(y)) {
        if (!is.matrix(y[[i]]))
          manynet::snet_abort(
            "Network {i} of the dependent variable {.val {dep}} must be a matrix.")
      }
    }
  } else {
    if (!large) {
      if (length(dim(y)) != 3)
        manynet::snet_abort(
          "The dependent variable {.val {dep}} must be a 3-dimensional array.")
    } else {
      for (i in seq_along(y)) {
        if (length(dim(y[[i]])) != 3)
          manynet::snet_abort(
            "Network {i} of the dependent variable {.val {dep}} must be a 3-dimensional array.")
      }
    }
  }
  invisible(TRUE)
}


# ---- future plan + permutation runner ---------------------------------------

#' @keywords internal
#' @noRd
setup_future_plan <- function(strategy = "sequential", ncores = NULL) {
  old_plan <- future::plan()
  old_maxSize <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = +Inf)
  if (!is.null(ncores) && ncores > 1) {
    future::plan(future::multisession, workers = ncores)
  } else {
    future::plan(strategy)
  }
  attr(old_plan, "old_maxSize") <- old_maxSize
  invisible(old_plan)
}


#' @keywords internal
#' @noRd
run_permutations <- function(times, FUN, ...) {
  future.apply::future_lapply(
    seq_len(times),
    FUN,
    ...,
    future.seed = TRUE
  )
}


# ---- matrix permutation -----------------------------------------------------

# One permutation of the node order, respecting a blocking factor where its
# length matches the mode being permuted. A `groups` vector of the wrong length
# cannot describe this mode, so that mode permutes freely rather than silently
# recycling the factor, which is what `split()` did before.
#' @keywords internal
#' @noRd
.perm_order <- function(n, groups = NULL) {
  if (is.null(groups) || length(groups) != n) return(sample(seq_len(n)))
  groups <- as.character(groups)
  unsplit(lapply(split(seq_len(n), groups), FUN = sample), groups)
}

#' @keywords internal
#' @noRd
RMPerm <- function(m, groups = NULL, CSS = FALSE) {

  if (is.list(m)) {
    return(lapply(m, RMPerm, groups = groups))
  }

  if (is.null(groups)) {
    groups <- rep(1, dim(m)[2])
  } else {
    groups <- as.character(groups)
  }

  if (length(dim(m)) == 2) {
    nr <- dim(m)[1]
    nc <- dim(m)[2]
    if (nr == nc) {
      o <- unsplit(lapply(split(1:nr, groups), FUN = sample), groups)
      p <- matrix(data = m[o, o], nrow = nr, ncol = nc)
    } else {
      # A two-mode incidence matrix has two nodesets, so the rows and the
      # columns permute independently. Permuting both by one order, as the
      # square case does, indexes past the shorter side and errors.
      or <- .perm_order(nr, groups)
      oc <- .perm_order(nc, groups)
      p <- matrix(data = m[or, oc], nrow = nr, ncol = nc)
    }
    dimnames(p) <- dimnames(m)
  } else if (CSS) {
    p <- array(dim = c(dim(m)[1], dim(m)[2], dim(m)[3]))
    o <- unsplit(lapply(split(1:dim(m)[2], groups), FUN = sample), groups)
    p[, , ] <- array(m[o, o, o])
  } else {
    p <- array(dim = c(dim(m)[1], dim(m)[2], dim(m)[3]))
    for (i in 1:dim(m)[1]) {
      o <- unsplit(lapply(split(1:dim(m)[2], groups), FUN = sample), groups)
      p[i, , ] <- array(m[i, o, o])
    }
  }
  return(p)
}


# ---- data frame builder -----------------------------------------------------

#' @keywords internal
#' @noRd
make_qap_data <- function(y, x, g = NULL, diag = FALSE, directed = TRUE,
                          net = 1, perm = FALSE, xi = NULL) {
  nx <- length(x)

  if (perm && is.null(xi)) {
    y <- RMPerm(y, g)
  } else if (perm && !is.null(xi)) {
    x[[xi]] <- RMPerm(x[[xi]], g)
  }

  # The dependent matrix is square for a one-mode network and rectangular for a
  # two-mode one. Reading `nc` from the matrix rather than assuming `nr` is what
  # keeps a two-mode network from being read as a square one, which used to
  # wrap past the last column and invent dyads.
  nr <- dim(y)[1]
  nc <- dim(y)[2]
  square <- identical(nr, nc)

  valid <- matrix(TRUE, nr, nc)
  if (!diag && square) diag(valid) <- FALSE
  # An undirected one-mode network holds each dyad twice, once on each side of
  # the diagonal. Keeping both halves doubles the sample and shrinks every
  # standard error, so take the lower triangle only. A two-mode incidence
  # matrix has no such symmetry, and keeps every cell.
  if (!directed && square) valid[upper.tri(valid)] <- FALSE

  for (var in seq_len(nx)) {
    valid[is.na(x[[var]])] <- FALSE
  }
  valid[is.na(y)] <- FALSE
  y[!valid] <- NA

  vv <- as.vector(valid)

  for (var in seq_len(nx)) {
    x[[var]][!valid] <- NA
  }

  if (sum(vv) == 0) {
    manynet::snet_abort(
      c("No valid dyads remain in network {net} after dropping NA and diagonal cells.",
        i = "Check that the predictors and the outcome are observed for the same node pairs."))
  }

  pred <- data.frame(
    location = as.vector(matrix(seq_len(nr * nc), nr, nc))[vv],
    yv       = as.vector(y)[vv]
  )
  pred$nv <- as.factor(net)

  sv <- matrix(seq_len(nr), nr, nc)
  sv[!valid] <- NA
  pred$sv <- as.vector(sv)[vv]

  rv <- matrix(seq_len(nc), nr, nc, byrow = TRUE)
  rv[!valid] <- NA
  pred$rv <- as.vector(rv)[vv]

  for (var in seq_len(nx)) {
    pred[[names(x)[var]]] <- as.vector(x[[var]])[vv]
  }
  return(pred)
}


# ---- robust SE --------------------------------------------------------------

#' @keywords internal
#' @noRd
HC3 <- function(X, e) {
  XO <- cbind(matrix(1, dim(X)[1], 1), X)
  XTXINV <- solve(t(XO) %*% XO)
  om <- c()

  hf <- function(z, XTXINV = XTXINV) {
    h <- z %*% XTXINV %*% z
    return(h)
  }
  h  <- apply(XO, 1, hf, XTXINV = XTXINV)
  om <- e^2 / (1 - h)^2
  # No gc() here: this runs once per permutation, and forcing a collection
  # thousands of times costs far more than the memory it returns.
  sqrt(diag(t(t(XTXINV %*% t(XO)) * om) %*% XO %*% XTXINV))
}


# ---- baseline + perm fitting ------------------------------------------------

# The predictor names carry spaces ("ego Age"), so the model formula quotes them
# and several fitters hand the backticks back in the coefficient names. Double
# semi-partialling then looks a column up by the unquoted name and fails with a
# subscript error. The inner function has many early returns, one per estimator,
# so the names are cleaned here, where every path passes through exactly once.
#' @keywords internal
#' @noRd
fit_qap_model <- function(...) {
  fit <- .fit_qap_model(...)
  for (el in c("coefficients", "t", "zi_coefficients")) {
    if (!is.null(fit[[el]]) && !is.null(names(fit[[el]]))) {
      names(fit[[el]]) <- gsub("`", "", names(fit[[el]]), fixed = TRUE)
    }
  }
  fit
}

#' @keywords internal
#' @noRd
.fit_qap_model <- function(mod, pred, family,
                          estimator = "standard",
                          use_fixest = FALSE,
                          fixest_se_cluster = NULL,
                          use_robust_errors = FALSE,
                          main_vars = NULL,
                          has_random = FALSE,
                          reference = NULL) {
  fit <- list()
  dep_var <- all.vars(mod)[1]
  nx  <- length(main_vars)

  if (family == "multinom") {
    pred[[dep_var]] <- as.factor(pred[[dep_var]])
    if (!is.null(reference)) {
      pred[[dep_var]] <- stats::relevel(pred[[dep_var]], ref = reference)
    }
    thisRequires("nnet", "for multinomial models")
    base_model       <- nnet::multinom(mod, data = pred, trace = FALSE)
    fit$coefficients <- stats::coefficients(base_model)
    fit$t            <- stats::coefficients(base_model) /
                          summary(base_model)$standard.errors
    fit$base_model   <- base_model
    return(fit)
  }

  if (estimator == "gmm") {
    thisRequires("gmm", "for GMM estimation")
    y_vec <- pred[[dep_var]]
    x_mat <- cbind(1, as.matrix(pred[, main_vars, drop = FALSE]))

    gmm_args <- list(
      x = list(y = y_vec, x = x_mat),
      t0 = stats::rnorm(nx + 1),
      wmatrix = "optimal", vcov = "MDS",
      optfct = "nlminb",
      control = list(eval.max = 10000)
    )

    has_extra_param <- FALSE

    if (family == "binomial") {
      gmm_args$g <- logit_moments
      base_model <- do.call(gmm::gmm, gmm_args)
      resid <- logit_resid(base_model)
    } else if (family == "poisson") {
      gmm_args$g <- poisson_moments
      base_model <- do.call(gmm::gmm, gmm_args)
      resid <- poisson_resid(base_model)
    } else if (family == "negbin") {
      gmm_args$g  <- negbin_moments
      gmm_args$t0 <- stats::rnorm(nx + 2)
      base_model   <- do.call(gmm::gmm, gmm_args)
      resid <- negbin_resid(base_model)
      has_extra_param <- TRUE
    } else if (family == "zip") {
      gmm_args$g  <- zip_moments
      gmm_args$t0 <- stats::rnorm(nx + 2)
      base_model   <- do.call(gmm::gmm, gmm_args)
      resid <- zip_resid(base_model)
      has_extra_param <- TRUE
    } else {
      manynet::snet_abort(
        c("The GMM estimator is not available for the {.val {family}} family.",
          i = "It is available for the binomial, poisson, negbin, and zip families."))
    }

    all_coefs <- base_model$coefficients
    if (!use_robust_errors) {
      all_t <- summary(base_model)$coefficients[, 3]
    }

    if (has_extra_param) {
      fit$coefficients <- all_coefs[1:(nx + 1)]
    } else {
      fit$coefficients <- all_coefs
    }
    names(fit$coefficients) <- c("(Intercept)", main_vars)

    if (use_robust_errors) {
      xv <- as.matrix(pred[, main_vars, drop = FALSE])
      fit$t <- fit$coefficients / HC3(xv, resid)
    } else {
      if (has_extra_param) {
        fit$t <- all_t[1:(nx + 1)]
      } else {
        fit$t <- all_t
      }
    }
    names(fit$t) <- names(fit$coefficients)
    fit$base_model <- base_model
    fit$estimator  <- "gmm"
    return(fit)
  }

  if (family == "zip" && estimator == "standard") {
    if (has_random) {
      thisRequires("glmmTMB", "for mixed zero-inflated Poisson models")
      base_model <- glmmTMB::glmmTMB(mod, data = pred,
                                     family = stats::poisson(),
                                     ziformula = ~1)
      fit$coefficients <- glmmTMB::fixef(base_model)$cond
      resid <- stats::residuals(base_model, type = "response")
      fit$t <- summary(base_model)$coefficients$cond[, 3]
      names(fit$t) <- names(fit$coefficients)
      fit$zi_coefficients <- glmmTMB::fixef(base_model)$zi
      fit$random.intercepts <- list()
      re <- glmmTMB::ranef(base_model)$cond
      for (rV in names(re)) {
        fit$random.intercepts[[rV]] <- re[[rV]][, 1]
      }
    } else {
      thisRequires("pscl", "for zero-inflated Poisson models")
      base_model <- pscl::zeroinfl(mod, data = pred, dist = "poisson")
      fit$coefficients <- base_model$coefficients$count
      resid <- stats::residuals(base_model, type = "response")
      if (use_robust_errors) {
        xv <- as.matrix(pred[, main_vars, drop = FALSE])
        fit$t <- fit$coefficients / HC3(xv, resid)
      } else {
        fit$t <- summary(base_model)$coefficients$count[, 3]
      }
      fit$zi_coefficients <- base_model$coefficients$zero
    }
    fit$base_model <- base_model
    return(fit)
  }

  if (!has_random) {
    if (use_fixest) {
      thisRequires("fixest", "for fixed effects and clustered standard errors")
      fe_family <- if (family == "negbin") "negbin" else family
      base_model <- fixest::feglm(mod, data = pred,
                                  family = fe_family,
                                  cluster = fixest_se_cluster)
      # {fixest} reports an intercept where no fixed effect is absorbed, and
      # none where one is. Add the placeholder only in the second case;
      # otherwise the coefficient vector carries two intercepts.
      fe_coefs <- base_model$coefficients
      fit$coefficients <- if ("(Intercept)" %in% names(fe_coefs)) {
        fe_coefs
      } else {
        c("(Intercept)" = NA, fe_coefs)
      }
      resid <- stats::residuals(base_model)

      # `HC3()` and `vcov()` both return one standard error per estimated
      # coefficient, so the placeholder is needed only where the intercept was
      # absorbed and `fit$coefficients` carries an NA for it.
      absorbed <- !("(Intercept)" %in% names(fe_coefs))
      if (use_robust_errors) {
        xv   <- as.matrix(pred[, main_vars, drop = FALSE])
        hc   <- HC3(xv, resid)
        fit$t <- if (absorbed) {
          fit$coefficients / c(NA, hc[-1])
        } else {
          fit$coefficients / hc
        }
      } else {
        fe_se <- sqrt(diag(stats::vcov(base_model)))
        fe_t  <- fe_coefs / fe_se
        fit$t <- if (absorbed) c("(Intercept)" = NA, fe_t) else fe_t
      }
      names(fit$t) <- names(fit$coefficients)

      if (family == "gaussian") {
        r2s <- tryCatch(fixest::r2(base_model), error = function(e) NULL)
        if (!is.null(r2s)) {
          fit$r.squared     <- r2s[["r2"]]
          fit$adj.r.squared <- r2s[["ar2"]]
        }
      }
    } else {
      if (family == "gaussian") {
        base_model        <- stats::lm(mod, data = pred)
        fit$r.squared     <- summary(base_model)$r.squared
        fit$adj.r.squared <- summary(base_model)$adj.r.squared
      } else if (family == "negbin") {
        thisRequires("MASS", "for negative binomial models")
        base_model <- MASS::glm.nb(mod, data = pred)
        fit$theta  <- base_model$theta
      } else {
        base_model <- stats::glm(mod, data = pred, family = family)
      }
      fit$coefficients <- base_model$coefficients
      resid <- stats::residuals(base_model)

      if (use_robust_errors) {
        xv <- as.matrix(pred[, main_vars, drop = FALSE])
        fit$t <- fit$coefficients / HC3(xv, resid)
      } else {
        fit$t <- summary(base_model)$coefficients[, 3]
      }
    }
  } else {
    if (family == "gaussian") {
      thisRequires("lme4", "for random effects")
      base_model <- lme4::lmer(mod, data = pred)
    } else if (family == "negbin") {
      thisRequires("glmmTMB", "for mixed negative binomial models")
      base_model <- glmmTMB::glmmTMB(mod, data = pred,
                                     family = glmmTMB::nbinom2())
      fit$coefficients <- glmmTMB::fixef(base_model)$cond
      resid <- stats::residuals(base_model, type = "response")
      if (use_robust_errors) {
        xv <- as.matrix(pred[, main_vars, drop = FALSE])
        fit$t <- fit$coefficients / HC3(xv, resid)
      } else {
        fit$t <- summary(base_model)$coefficients$cond[, 3]
        names(fit$t) <- names(fit$coefficients)
      }
      fit$theta <- glmmTMB::sigma(base_model)
      fit$random.intercepts <- list()
      re <- glmmTMB::ranef(base_model)$cond
      for (rV in names(re)) {
        fit$random.intercepts[[rV]] <- re[[rV]][, 1]
      }
      fit$base_model <- base_model
      return(fit)
    } else {
      thisRequires("lme4", "for random effects")
      base_model <- lme4::glmer(mod, data = pred, family = family,
                                control = lme4::glmerControl(
                                  calc.derivs = FALSE,
                                  optimizer   = "bobyqa"),
                                nAGQ = 0)
      fit$log_lik <- stats::logLik(base_model)
    }

    fit$coefficients <- summary(base_model)$coefficients[, 1]
    resid <- stats::residuals(base_model)

    if (use_robust_errors) {
      xv <- as.matrix(pred[, main_vars, drop = FALSE])
      fit$t <- fit$coefficients / HC3(xv, resid)
    } else {
      fit$t <- summary(base_model)$coefficients[, 3]
    }

    fit$random.intercepts <- list()
    for (rV in names(stats::coefficients(base_model))) {
      fit$random.intercepts[[rV]] <- stats::coefficients(base_model)[[rV]][, 1]
    }
  }

  fit$base_model <- base_model
  return(fit)
}


# ---- permutation comparison -------------------------------------------------

#' @keywords internal
#' @noRd
compare_perm_to_baseline <- function(perm_coefs, perm_t, base_fit,
                                     xi = NULL) {
  pres <- rbind(perm_coefs, perm_t)
  bres <- rbind(base_fit$coefficients, base_fit$t)

  out <- list()
  if (is.null(xi)) {
    out$lower  <- pres <= bres
    out$larger <- pres >= bres
    out$abs    <- abs(pres) >= abs(bres)
  } else {
    out$lower  <- (pres <= bres)[, xi]
    out$larger <- (pres >= bres)[, xi]
    out$abs    <- (abs(pres) >= abs(bres))[, xi]
  }
  return(out)
}


#' @keywords internal
#' @noRd
aggregate_perm_results <- function(results, times) {
  results <- Filter(Negate(is.null), results)
  n_valid <- length(results)
  if (n_valid == 0)
    manynet::snet_abort(
      c("All {times} permutations failed to converge.",
        i = "Try a simpler model, another {.arg family}, or fewer predictors."))
  if (n_valid < times) {
    manynet::snet_warn(
      "{times - n_valid} of {times} permutation{?s} failed and {?was/were} excluded.")
  }
  resL <- unlist(results, recursive = FALSE)
  list(
    lower  = Reduce("+", resL[names(resL) == "lower"],  0) / n_valid,
    larger = Reduce("+", resL[names(resL) == "larger"], 0) / n_valid,
    abs    = Reduce("+", resL[names(resL) == "abs"],    0) / n_valid
  )
}


# ---- residualisation for permute = "predictor" ---------------------------------------------

#' @keywords internal
#' @noRd
residualise_predictor <- function(xi, pred, main_vars,
                                  has_random = FALSE,
                                  rand_formula = "") {
  bq <- function(v) paste0("`", v, "`")
  others <- setdiff(main_vars, xi)
  modx_str <- paste(bq(xi), "~ 1")
  if (length(others) > 0) {
    modx_str <- paste(modx_str,
                      paste(vapply(others, bq, character(1)),
                            collapse = " + "),
                      sep = " + ")
  }
  if (has_random && nchar(trimws(rand_formula)) > 0) {
    modx_str <- paste(modx_str, rand_formula)
  }
  modx <- stats::as.formula(modx_str)

  if (!has_random) {
    xm <- stats::lm(modx, data = pred)
  } else {
    thisRequires("lme4", "for random effects")
    # Residualising is a step towards the null distribution, not a result the
    # user reads, so a degenerate mixed fit here must not abort the whole run.
    # Crossed sender and receiver intercepts on a predictor are often singular,
    # and `lmer()` then stops with "Downdated VtV is not positive definite".
    xm <- tryCatch(suppressWarnings(lme4::lmer(modx, data = pred)),
                   error = function(e) NULL)
    if (is.null(xm)) {
      manynet::snet_warn(
        c("Could not residualise {.val {xi}} with random intercepts.",
          i = "Residualising it without them instead."))
      xm <- stats::lm(stats::as.formula(
        paste(bq(xi), "~ 1",
              if (length(others) > 0)
                paste("+", paste(vapply(others, bq, character(1)),
                                 collapse = " + ")) else "")),
        data = pred)
    }
  }
  stats::residuals(xm)
}


#' @keywords internal
#' @noRd
residuals_to_matrix <- function(xR, original_matrix, pred, large = FALSE) {
  out <- original_matrix
  if (!large) {
    out[pred$location] <- xR
  } else {
    for (net_id in unique(as.character(pred$nv))) {
      idx <- as.character(pred$nv) == net_id
      net_num <- as.integer(net_id)
      out[[net_num]][pred$location[idx]] <- xR[idx]
    }
  }
  return(out)
}


#' @keywords internal
#' @noRd
residuals_to_array <- function(xR, original_array, valid, pred,
                               large = FALSE, valid_list = NULL) {
  if (!large) {
    n <- dim(original_array)[1]
    out <- array(NA, dim = c(n, n, n))
    out[valid] <- xR
    return(out)
  } else {
    out <- original_array
    for (gr in seq_along(original_array)) {
      out[[gr]] <- array(NA, dim = dim(original_array[[gr]]))
      out[[gr]][valid_list[[gr]]] <- xR[pred$nv == gr]
    }
    return(out)
  }
}
