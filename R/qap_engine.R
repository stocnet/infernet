# QAPglm engine --------------------------------------------------------------
#
# Internal.  The matrix-level engine that drives net_regression(): performs
# the baseline fit and the QAP / QAP-DSP permutation inference on a pre-built
# list of matrices.  Ported from MrQAP::QAPglm().  All parallelism uses the
# `future` framework via `run_permutations()`; no progressr integration.

#' @keywords internal
#' @noRd
QAPglm <- function(formula,
                   data,
                   family    = "gaussian",
                   mode      = "directed",
                   diag      = FALSE,
                   nullhyp   = "qapspp",
                   estimator = "standard",
                   reps      = 1000,
                   seed      = NULL,
                   groups    = NULL,
                   strategy  = "sequential",
                   ncores    = NULL,
                   fixest_se_cluster = NULL,
                   comparison = NULL,
                   reference  = NULL,
                   random_intercept_nets     = FALSE,
                   random_intercept_sender   = FALSE,
                   random_intercept_receiver = FALSE,
                   use_robust_errors = FALSE,
                   less_mem   = FALSE,
                   use_gpu    = FALSE) {

  if (!is.null(seed)) set.seed(seed)

  parsed <- parse_qap_formula(formula, fixest_se_cluster)
  dep        <- parsed$dependent
  main       <- parsed$main
  data_vars  <- intersect(parsed$all_data_vars, names(data))

  validate_qap_input(data, parsed, css = FALSE)
  large <- is.list(data[[dep]])

  mode_internal <- if (mode == "directed") "digraph" else "graph"

  rin <- random_intercept_nets
  ris <- random_intercept_sender
  rir <- random_intercept_receiver

  mod <- build_internal_formula(formula, rin = rin, ris = ris, rir = rir)
  mod_str <- paste(deparse(mod, width.cutoff = 500), collapse = " ")
  has_random <- grepl("\\(", mod_str) || parsed$has_random
  use_fixest <- parsed$use_fixest
  if (has_random && use_fixest) {
    warning("Cannot combine fixest FE and lme4 random effects. ",
            "Using lme4 random effects only.")
    use_fixest <- FALSE
  }

  mod <- stats::as.formula(mod_str)

  if (!large) {
    pred <- make_qap_data(y    = data[[dep]],
                          x    = data[data_vars],
                          g    = groups,
                          diag = diag,
                          mode = mode_internal,
                          net  = 1,
                          perm = FALSE,
                          xi   = NULL)
  } else {
    pred_list <- vector("list", length(data[[dep]]))
    for (net in seq_along(data[[dep]])) {
      x2 <- lapply(data_vars, function(v) data[[v]][[net]])
      names(x2) <- data_vars
      g2 <- if (!is.null(groups)) groups[[net]] else NULL
      pred_list[[net]] <- make_qap_data(y    = data[[dep]][[net]],
                                        x    = x2,
                                        g    = g2,
                                        diag = diag,
                                        mode = mode_internal,
                                        net  = net,
                                        perm = FALSE,
                                        xi   = NULL)
    }
    pred <- do.call(rbind, pred_list)
  }

  names(pred)[names(pred) == "yv"] <- dep

  if (!is.null(comparison) && is.null(reference)) {
    reference <- NULL
  }

  fit <- list()

  rand_part <- ""
  if (rin) rand_part <- paste(rand_part, "+ (1|nv)")
  if (ris) rand_part <- paste(rand_part, "+ (1|sv)")
  if (rir) rand_part <- paste(rand_part, "+ (1|rv)")

  if (is.null(comparison)) {
    fit$base <- fit_qap_model(mod          = mod,
                              pred         = pred,
                              family       = family,
                              estimator    = estimator,
                              use_fixest   = use_fixest,
                              fixest_se_cluster = fixest_se_cluster,
                              use_robust_errors = use_robust_errors,
                              main_vars    = main,
                              has_random   = has_random,
                              reference    = reference)
  } else {
    fit$base <- vector("list", length(comparison))
    names(fit$base) <- names(comparison)
    for (k in seq_along(comparison)) {
      predK <- pred[pred[[dep]] %in% comparison[[k]], ]
      predK[[dep]] <- ifelse(predK[[dep]] == comparison[[k]][1], 0, 1)
      fit$base[[k]] <- fit_qap_model(mod          = mod,
                                     pred         = predK,
                                     family       = family,
                                     estimator    = estimator,
                                     use_fixest   = use_fixest,
                                     fixest_se_cluster = fixest_se_cluster,
                                     use_robust_errors = use_robust_errors,
                                     main_vars    = main,
                                     has_random   = has_random,
                                     reference    = reference)
    }
  }

  if ((nullhyp == "qapspp") && (length(main) == 1)) nullhyp <- "qapy"

  if (use_gpu && family == "gaussian" && !has_random && !use_fixest &&
      is.null(comparison) && !large) {

    if (nullhyp == "qapy") {
      gpu_res <- gpu_batch_ols(data         = data,
                               parsed       = parsed,
                               mode         = mode_internal,
                               diag         = diag,
                               groups       = groups,
                               reps         = reps,
                               baseline_fit = fit$base,
                               perm_var     = NULL)
      fit$lower  <- gpu_res$lower
      fit$larger <- gpu_res$larger
      fit$abs    <- gpu_res$abs

    } else if (nullhyp == "qapspp") {
      n_coefs <- length(fit$base$coefficients)
      fit$lower  <- matrix(NA, nrow = 2, ncol = n_coefs)
      fit$larger <- fit$abs <- fit$lower
      colnames(fit$lower) <- colnames(fit$larger) <-
        colnames(fit$abs)  <- names(fit$base$coefficients)

      for (xi in main) {
        xR <- residualise_predictor(xi, pred, main,
                                    has_random   = has_random,
                                    rand_formula = rand_part)
        data_resid <- data
        data_resid[[xi]] <- residuals_to_matrix(xR, data[[xi]], pred, large)

        gpu_res <- gpu_batch_ols(data         = data_resid,
                                 parsed       = parsed,
                                 mode         = mode_internal,
                                 diag         = diag,
                                 groups       = groups,
                                 reps         = reps,
                                 baseline_fit = fit$base,
                                 perm_var     = xi)
        fit$lower[, xi]  <- gpu_res$lower[, xi]
        fit$larger[, xi] <- gpu_res$larger[, xi]
        fit$abs[, xi]    <- gpu_res$abs[, xi]
      }
    }

  } else {
    old_plan <- setup_future_plan(strategy, ncores)
    on.exit({
      future::plan(old_plan)
      options(future.globals.maxSize = attr(old_plan, "old_maxSize"))
    }, add = TRUE)

    if (nullhyp == "qapy") {
      res <- run_permutations(
        reps, QAPglmPermEst,
        data.     = data,
        perm_var. = NULL,
        mode.     = mode_internal,
        diag.     = diag,
        mod.      = mod,
        groups.   = groups,
        fit.      = if (is.null(comparison)) fit$base else fit$base,
        family.   = family,
        estimator. = estimator,
        use_fixest. = use_fixest,
        fixest_se_cluster. = fixest_se_cluster,
        use_robust_errors. = use_robust_errors,
        has_random. = has_random,
        main_vars. = main,
        data_vars. = data_vars,
        parsed.   = parsed,
        comp.     = comparison,
        reference. = reference
      )

      if (is.null(comparison)) {
        agg <- aggregate_perm_results(res, reps)
        fit$lower  <- agg$lower
        fit$larger <- agg$larger
        fit$abs    <- agg$abs
      } else {
        res_valid <- Filter(Negate(is.null), res)
        n_valid   <- length(res_valid)
        fit$lower <- fit$larger <- fit$abs <-
          vector("list", length(comparison))
        names(fit$lower)  <- names(comparison)
        names(fit$larger) <- names(comparison)
        names(fit$abs)    <- names(comparison)
        resL <- unlist(unlist(res_valid, recursive = FALSE), recursive = FALSE)
        for (k in seq_along(comparison)) {
          cn <- names(comparison)[k]
          fit$lower[[k]]  <- Reduce("+", resL[names(resL) == paste0(cn, ".lower")], 0) / n_valid
          fit$larger[[k]] <- Reduce("+", resL[names(resL) == paste0(cn, ".larger")], 0) / n_valid
          fit$abs[[k]]    <- Reduce("+", resL[names(resL) == paste0(cn, ".abs")], 0) / n_valid
        }
      }

    } else if (nullhyp == "qapspp") {
      if (is.null(comparison)) {
        n_coefs <- length(fit$base$coefficients)
        fit$lower  <- matrix(NA, nrow = 2, ncol = n_coefs)
        fit$larger <- fit$abs <- fit$lower
        colnames(fit$lower) <- colnames(fit$larger) <-
          colnames(fit$abs)  <- names(fit$base$coefficients)
      } else {
        fit$lower <- fit$larger <- fit$abs <-
          vector("list", length(comparison))
        names(fit$lower) <- names(fit$larger) <-
          names(fit$abs)  <- names(comparison)
        for (k in seq_along(comparison)) {
          n_coefs <- length(fit$base[[k]]$coefficients)
          fit$lower[[k]] <- matrix(NA, nrow = 2, ncol = n_coefs)
          fit$larger[[k]] <- fit$abs[[k]] <- fit$lower[[k]]
          colnames(fit$lower[[k]]) <- colnames(fit$larger[[k]]) <-
            colnames(fit$abs[[k]])  <- names(fit$base[[k]]$coefficients)
        }
      }

      for (xi in main) {
        xR <- residualise_predictor(xi, pred, main,
                                    has_random   = has_random,
                                    rand_formula = rand_part)

        data_resid <- data
        data_resid[[xi]] <- residuals_to_matrix(xR, data[[xi]], pred, large)

        res <- run_permutations(
          reps, QAPglmPermEst,
          data.     = data_resid,
          perm_var. = xi,
          mode.     = mode_internal,
          diag.     = diag,
          mod.      = mod,
          groups.   = groups,
          fit.      = if (is.null(comparison)) fit$base else fit$base,
          family.   = family,
          estimator. = estimator,
          use_fixest. = use_fixest,
          fixest_se_cluster. = fixest_se_cluster,
          use_robust_errors. = use_robust_errors,
          has_random. = has_random,
          main_vars. = main,
          data_vars. = data_vars,
          parsed.   = parsed,
          comp.     = comparison,
          reference. = reference
        )

        if (is.null(comparison)) {
          agg <- aggregate_perm_results(res, reps)
          fit$lower[, xi]  <- agg$lower
          fit$larger[, xi] <- agg$larger
          fit$abs[, xi]    <- agg$abs
        } else {
          res_valid <- Filter(Negate(is.null), res)
          n_valid   <- length(res_valid)
          resL <- unlist(unlist(res_valid, recursive = FALSE), recursive = FALSE)
          for (k in seq_along(comparison)) {
            cn <- names(comparison)[k]
            fit$lower[[k]][, xi]  <- Reduce("+", resL[names(resL) == paste0(cn, ".lower")], 0) / n_valid
            fit$larger[[k]][, xi] <- Reduce("+", resL[names(resL) == paste0(cn, ".larger")], 0) / n_valid
            fit$abs[[k]][, xi]    <- Reduce("+", resL[names(resL) == paste0(cn, ".abs")], 0) / n_valid
          }
        }
      }
    }
  }

  if (is.null(comparison)) {
    fit$coefficients <- fit$base$coefficients
    fit$t            <- fit$base$t
    if (!is.null(fit$base$r.squared)) {
      fit$r.squared     <- fit$base$r.squared
      fit$adj.r.squared <- fit$base$adj.r.squared
    }
    if (!is.null(fit$base$random.intercepts))
      fit$random.intercepts <- fit$base$random.intercepts
    if (!is.null(fit$base$theta))
      fit$theta <- fit$base$theta
    if (!is.null(fit$base$zi_coefficients))
      fit$zi_coefficients <- fit$base$zi_coefficients
    if (!less_mem) fit$simple_fit <- fit$base$base_model
  } else {
    if (!less_mem) {
      fit$simple_fits <- lapply(fit$base, `[[`, "base_model")
    }
  }

  if (family == "binomial" && is.null(comparison)) {
    bm <- fit$base$base_model
    if (!inherits(bm, "gmm")) {
      predicted <- stats::fitted(bm)
      actual    <- pred[[dep]]
      fit$confusion_matrix <- probabilistic_confusion_matrix(
        actual = actual, predicted_prob = predicted,
        n_draws = 1000, seed = seed
      )
    }
  }

  fit$nullhyp   <- nullhyp
  fit$diag      <- diag
  fit$family    <- family
  fit$mode      <- mode
  fit$reps      <- reps
  fit$groups    <- unique(unlist(groups))
  fit$robust_se <- use_robust_errors
  fit$estimator <- estimator
  fit$comp      <- comparison
  fit$reference <- reference
  fit$pred      <- pred
  fit$dep       <- dep

  if (family == "gaussian" && is.null(comparison)) {
    class(fit) <- "QAPRegression"
  } else {
    class(fit) <- "QAPGLM"
  }
  return(fit)
}


#' @keywords internal
#' @noRd
QAPglmPermEst <- function(i,
                          data.,
                          perm_var.,
                          mode.,
                          diag.,
                          mod.,
                          groups.,
                          fit.,
                          family.,
                          estimator.,
                          use_fixest.,
                          fixest_se_cluster.,
                          use_robust_errors.,
                          has_random.,
                          main_vars.,
                          data_vars.,
                          parsed.,
                          comp.,
                          reference.) {

  dep   <- parsed.$dependent
  large <- is.list(data.[[dep]])

  d <- data.
  if (is.null(perm_var.)) {
    if (!large) {
      d[[dep]] <- RMPerm(d[[dep]], groups.)
    } else {
      d[[dep]] <- lapply(d[[dep]], RMPerm, groups = groups.)
    }
  } else {
    if (!large) {
      d[[perm_var.]] <- RMPerm(d[[perm_var.]], groups.)
    } else {
      d[[perm_var.]] <- lapply(d[[perm_var.]], RMPerm, groups = groups.)
    }
  }

  if (!large) {
    pred <- make_qap_data(y    = d[[dep]],
                          x    = d[data_vars.],
                          g    = groups.,
                          diag = diag.,
                          mode = mode.,
                          net  = 1,
                          perm = FALSE,
                          xi   = NULL)
  } else {
    pred_list <- vector("list", length(d[[dep]]))
    for (net in seq_along(d[[dep]])) {
      x2 <- lapply(data_vars., function(v) d[[v]][[net]])
      names(x2) <- data_vars.
      g2 <- if (!is.null(groups.)) groups.[[net]] else NULL
      pred_list[[net]] <- make_qap_data(y    = d[[dep]][[net]],
                                        x    = x2,
                                        g    = g2,
                                        diag = diag.,
                                        mode = mode.,
                                        net  = net,
                                        perm = FALSE,
                                        xi   = NULL)
    }
    pred <- do.call(rbind, pred_list)
  }

  names(pred)[names(pred) == "yv"] <- dep

  xi_arg <- if (!is.null(perm_var.)) perm_var. else NULL

  if (is.null(comp.)) {
    perm_fit <- tryCatch(
      fit_qap_model(mod          = mod.,
                    pred         = pred,
                    family       = family.,
                    estimator    = estimator.,
                    use_fixest   = use_fixest.,
                    fixest_se_cluster = fixest_se_cluster.,
                    use_robust_errors = use_robust_errors.,
                    main_vars    = main_vars.,
                    has_random   = has_random.,
                    reference    = reference.),
      error = function(e) NULL
    )
    if (is.null(perm_fit)) return(NULL)

    return(compare_perm_to_baseline(perm_fit$coefficients, perm_fit$t,
                                    fit., xi = xi_arg))
  }

  xresL <- vector("list", length(comp.))
  names(xresL) <- names(comp.)

  for (k in seq_along(comp.)) {
    predK <- pred[pred[[dep]] %in% comp.[[k]], ]
    predK[[dep]] <- ifelse(predK[[dep]] == comp.[[k]][1], 0, 1)

    perm_fit <- tryCatch(
      fit_qap_model(mod          = mod.,
                    pred         = predK,
                    family       = family.,
                    estimator    = estimator.,
                    use_fixest   = use_fixest.,
                    fixest_se_cluster = fixest_se_cluster.,
                    use_robust_errors = use_robust_errors.,
                    main_vars    = main_vars.,
                    has_random   = has_random.,
                    reference    = reference.),
      error = function(e) NULL
    )
    if (is.null(perm_fit)) return(NULL)

    xresL[[k]] <- compare_perm_to_baseline(perm_fit$coefficients, perm_fit$t,
                                           fit.[[k]], xi = xi_arg)
  }

  return(xresL)
}
