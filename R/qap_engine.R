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
                   random_intercept_nets     = FALSE,
                   random_intercept_sender   = FALSE,
                   random_intercept_receiver = FALSE,
                   use_robust_errors = FALSE,
                   less_mem   = FALSE) {

  if (!is.null(seed)) set.seed(seed)

  parsed <- parse_qap_formula(formula)
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

  fit <- list()

  rand_part <- ""
  if (rin) rand_part <- paste(rand_part, "+ (1|nv)")
  if (ris) rand_part <- paste(rand_part, "+ (1|sv)")
  if (rir) rand_part <- paste(rand_part, "+ (1|rv)")

  fit$base <- fit_qap_model(mod          = mod,
                            pred         = pred,
                            family       = family,
                            use_robust_errors = use_robust_errors,
                            main_vars    = main,
                            has_random   = has_random)

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
      fit.      = fit$base,
      family.   = family,
      use_robust_errors. = use_robust_errors,
      has_random. = has_random,
      main_vars. = main,
      data_vars. = data_vars,
      parsed.   = parsed
    )

    agg <- aggregate_perm_results(res, times)
    fit$lower  <- agg$lower
    fit$larger <- agg$larger
    fit$abs    <- agg$abs

  } else if (permute == "predictor") {
    n_coefs <- length(fit$base$coefficients)
    fit$lower  <- matrix(NA, nrow = 2, ncol = n_coefs,
                         dimnames = list(c("perm_coefs", "perm_t"),
                                         names(fit$base$coefficients)))
    fit$larger <- fit$abs <- fit$lower

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
        fit.      = fit$base,
        family.   = family,
        use_robust_errors. = use_robust_errors,
        has_random. = has_random,
        main_vars. = main,
        data_vars. = data_vars,
        parsed.   = parsed
      )

      agg <- aggregate_perm_results(res, times)
      fit$lower[, xi]  <- agg$lower
      fit$larger[, xi] <- agg$larger
      fit$abs[, xi]    <- agg$abs
    }
  }

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

  if (family == "binomial") {
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
  fit$pred      <- pred
  fit$dep       <- dep

  if (family == "gaussian") {
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
                          use_robust_errors.,
                          has_random.,
                          main_vars.,
                          data_vars.,
                          parsed.) {

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

  # A fit inside the permutation loop runs `times` times, so a fitter's
  # convergence warning would print once per draw and drown the console.
  # The count of draws that failed outright is reported by
  # `aggregate_perm_results()`, which is the number the user needs.
  perm_fit <- tryCatch(
    suppressWarnings(fit_qap_model(mod          = mod.,
                  pred         = pred,
                  family       = family.,
                  use_robust_errors = use_robust_errors.,
                  main_vars    = main_vars.,
                  has_random   = has_random.)),
    error = function(e) NULL
  )
  if (is.null(perm_fit)) return(NULL)

  return(compare_perm_to_baseline(perm_fit$coefficients, perm_fit$t,
                                  fit., xi = xi_arg))
}
