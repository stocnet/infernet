# QAPglm engine --------------------------------------------------------------
#
# Internal.  The matrix-level engine that drives net_regression(): performs
# the baseline fit and the QAP / QAP-DSP permutation inference on a pre-built
# list of matrices.  Ported from MrQAP::QAPglm().  All parallelism uses the
# `future` framework via `run_permutations()`; no progressr integration.

#' @keywords internal
#' @noRd
QAPglm <- function(formula,
                   matlist,
                   family    = "gaussian",
                   directed  = TRUE,
                   diag      = FALSE,
                   permute   = "predictor",
                   times      = 1000,
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
                   less_mem   = FALSE) {

  if (!is.null(seed)) set.seed(seed)

  parsed <- parse_qap_formula(formula, fixest_se_cluster)
  dep        <- parsed$dependent
  main       <- parsed$main
  data_vars  <- intersect(parsed$all_data_vars, names(matlist))

  validate_qap_input(matlist, parsed, css = FALSE)
  large <- is.list(matlist[[dep]])


  rin <- random_intercept_nets
  ris <- random_intercept_sender
  rir <- random_intercept_receiver

  mod <- build_internal_formula(formula, rin = rin, ris = ris, rir = rir)
  mod_str <- paste(deparse(mod, width.cutoff = 500), collapse = " ")
  has_random <- grepl("\\(", mod_str) || parsed$has_random
  use_fixest <- parsed$use_fixest
  if (has_random && use_fixest) {
    manynet::snet_warn(
      c("Cannot combine {.pkg fixest} fixed effects with {.pkg lme4} random effects.",
        i = "Using the random effects only."))
    use_fixest <- FALSE
  }

  mod <- stats::as.formula(mod_str)

  if (!large) {
    pred <- make_qap_data(y    = matlist[[dep]],
                          x    = matlist[data_vars],
                          g    = groups,
                          diag = diag,
                          directed = directed,
                          net  = 1,
                          perm = FALSE,
                          xi   = NULL)
  } else {
    pred_list <- vector("list", length(matlist[[dep]]))
    for (net in seq_along(matlist[[dep]])) {
      x2 <- lapply(data_vars, function(v) matlist[[v]][[net]])
      names(x2) <- data_vars
      g2 <- if (!is.null(groups)) groups[[net]] else NULL
      pred_list[[net]] <- make_qap_data(y    = matlist[[dep]][[net]],
                                        x    = x2,
                                        g    = g2,
                                        diag = diag,
                                        directed = directed,
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
                                     use_fixest   = use_fixest,
                                     fixest_se_cluster = fixest_se_cluster,
                                     use_robust_errors = use_robust_errors,
                                     main_vars    = main,
                                     has_random   = has_random,
                                     reference    = reference)
    }
  }

  # Double semi-partialling residualises a predictor against the others, so
  # with one predictor there are none and the scheme reduces to permuting the
  # outcome. Say so: the result would otherwise report a scheme nobody chose.
  if ((permute == "predictor") && (length(main) == 1)) {
    permute <- "outcome"
    # `snet_info()` pastes its arguments, so give it separate strings rather
    # than a named vector: a named vector loses its bullets and runs together.
    manynet::snet_info(
      "Permuting {.val outcome}, not {.val predictor}:",
      "with one predictor there is nothing to residualise it against.")
  }

  old_plan <- setup_future_plan(strategy, ncores)
  on.exit({
    future::plan(old_plan)
    options(future.globals.maxSize = attr(old_plan, "old_maxSize"))
  }, add = TRUE)

  if (permute == "outcome") {
    res <- run_permutations(
      times, QAPglmPermEst,
      matlist.     = matlist,
      perm_var. = NULL,
      directed. = directed,
      diag.     = diag,
      mod.      = mod,
      groups.   = groups,
      fit.      = if (is.null(comparison)) fit$base else fit$base,
      family.   = family,
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
      agg <- aggregate_perm_results(res, times)
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

  } else if (permute == "predictor") {
    if (is.null(comparison)) {
      n_coefs <- length(fit$base$coefficients)
      fit$lower  <- matrix(NA, nrow = 2, ncol = n_coefs,
                           dimnames = list(c("perm_coefs", "perm_t"),
                                           names(fit$base$coefficients)))
      fit$larger <- fit$abs <- fit$lower
    } else {
      fit$lower <- fit$larger <- fit$abs <-
        vector("list", length(comparison))
      names(fit$lower) <- names(fit$larger) <-
        names(fit$abs)  <- names(comparison)
      for (k in seq_along(comparison)) {
        n_coefs <- length(fit$base[[k]]$coefficients)
        fit$lower[[k]] <- matrix(NA, nrow = 2, ncol = n_coefs,
                                 dimnames = list(c("perm_coefs", "perm_t"),
                                                 names(fit$base[[k]]$coefficients)))
        fit$larger[[k]] <- fit$abs[[k]] <- fit$lower[[k]]
      }
    }

    for (xi in main) {
      xR <- residualise_predictor(xi, pred, main,
                                  has_random   = has_random,
                                  rand_formula = rand_part)

      matlist_resid <- matlist
      matlist_resid[[xi]] <- residuals_to_matrix(xR, matlist[[xi]], pred, large)

      res <- run_permutations(
        times, QAPglmPermEst,
        matlist.     = matlist_resid,
        perm_var. = xi,
        directed. = directed,
        diag.     = diag,
        mod.      = mod,
        groups.   = groups,
        fit.      = if (is.null(comparison)) fit$base else fit$base,
        family.   = family,
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
        agg <- aggregate_perm_results(res, times)
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
    fit$confusion_matrix <- probabilistic_confusion_matrix(
      actual = pred[[dep]],
      predicted_prob = stats::fitted(fit$base$base_model),
      n_draws = 1000, seed = seed
    )
  }

  fit$permute   <- permute
  fit$diag      <- diag
  fit$family    <- family
  fit$directed  <- directed
  fit$times      <- times
  fit$groups    <- unique(unlist(groups))
  fit$robust_se <- use_robust_errors
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
                          matlist.,
                          perm_var.,
                          directed.,
                          diag.,
                          mod.,
                          groups.,
                          fit.,
                          family.,
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
  large <- is.list(matlist.[[dep]])

  d <- matlist.
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
                          directed = directed.,
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
                                        directed = directed.,
                                        net  = net,
                                        perm = FALSE,
                                        xi   = NULL)
    }
    pred <- do.call(rbind, pred_list)
  }

  names(pred)[names(pred) == "yv"] <- dep

  xi_arg <- if (!is.null(perm_var.)) perm_var. else NULL

  if (is.null(comp.)) {
    # A fit inside the permutation loop runs `times` times, so a fitter's
    # convergence warning would print once per draw and drown the console.
    # The count of draws that failed outright is reported by
    # `aggregate_perm_results()`, which is the number the user needs.
    perm_fit <- tryCatch(
      suppressWarnings(fit_qap_model(mod          = mod.,
                    pred         = pred,
                    family       = family.,
                    use_fixest   = use_fixest.,
                    fixest_se_cluster = fixest_se_cluster.,
                    use_robust_errors = use_robust_errors.,
                    main_vars    = main_vars.,
                    has_random   = has_random.,
                    reference    = reference.)),
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

    # A fit inside the permutation loop runs `times` times, so a fitter's
    # convergence warning would print once per draw and drown the console.
    # The count of draws that failed outright is reported by
    # `aggregate_perm_results()`, which is the number the user needs.
    perm_fit <- tryCatch(
      suppressWarnings(fit_qap_model(mod          = mod.,
                    pred         = predK,
                    family       = family.,
                    use_fixest   = use_fixest.,
                    fixest_se_cluster = fixest_se_cluster.,
                    use_robust_errors = use_robust_errors.,
                    main_vars    = main_vars.,
                    has_random   = has_random.,
                    reference    = reference.)),
      error = function(e) NULL
    )
    if (is.null(perm_fit)) return(NULL)

    xresL[[k]] <- compare_perm_to_baseline(perm_fit$coefficients, perm_fit$t,
                                           fit.[[k]], xi = xi_arg)
  }

  return(xresL)
}
