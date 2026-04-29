# CSS (Cognitive Social Structure) engine ------------------------------------
#
# 3D-array QAP engine ported from MrQAP.  This engine handles CSS data stored
# as n x n x n arrays (sender × receiver × perceiver).
#
# STATUS: Internal / not yet wired to net_regression().
# The engine will be exposed via control = list(css = TRUE) once manynet
# provides a stable 3D CSS network representation.  Until then all functions
# here are internal and subject to change.

#' @keywords internal
#' @noRd
array_to_vector <- function(ar, mode., diag.) {
  v <- c()
  for (i in 1:nrow(ar)) {
    if (mode. == 'undirected') {
      v <- c(v, as.vector(ar[, , i][upper.tri(ar[, , i], diag = diag.)]))
    } else {
      v <- c(v, as.vector(ar[, , i]))
    }
  }
  return(v)
}


#' @keywords internal
#' @noRd
make_css_data <- function(y, x, nets, diag, mode) {
  n <- dim(y)[1]
  nx <- length(x)
  valid <- array(TRUE, dim = c(n, n, n))

  if (!diag) {
    for (i in 1:n) {
      diag(y[, , i]) <- NA
      for (var in 1:nx) {
        diag(x[[var]][, , i]) <- NA
      }
    }
  }

  valid[is.na(y)] <- FALSE

  for (var in 1:nx) {
    valid[is.na(x[[var]])] <- FALSE
  }

  if (mode == 'undirected') {
    for (i in 1:n) {
      y[, , i][lower.tri(y[, , i])] <- NA
      valid[, , i][lower.tri(valid[, , i])] <- FALSE
      for (var in 1:nx) {
        x[[var]][, , i][lower.tri(x[[var]][, , i])] <- NA
      }
    }
  }

  y[!valid] <- NA

  for (var in 1:nx) {
    x[[var]][!valid] <- NA
  }

  vv <- array_to_vector(valid, mode. = mode, diag. = diag)
  yv <- array_to_vector(y, mode. = mode, diag. = diag)[vv]

  pred <- data.frame(yv = yv, nv = nets)

  per <- sen <- rec <- array(NA, dim = c(n, n, n))

  for (i in 1:n) {
    sen[i, , ] <- i
    rec[, i, ] <- i
    per[, , i] <- i
  }

  pred$sv <- as.factor(array_to_vector(sen, mode. = mode, diag. = diag)[vv])
  pred$rv <- as.factor(array_to_vector(rec, mode. = mode, diag. = diag)[vv])
  pred$pv <- as.factor(array_to_vector(per, mode. = mode, diag. = diag)[vv])

  for (var in c(1:nx)) {
    pred[[names(x)[var]]] <- array_to_vector(x[[var]],
                                             mode. = mode, diag. = diag)[vv]
  }
  return(list(pred = pred, valid = valid))
}


#' @keywords internal
#' @noRd
QAPcssPermEst <- function(i,
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

  y_cat <- stats::na.omit(unique(as.vector(unlist(data.[[dep]]))))

  sufficient_data <- FALSE
  trial <- 0
  max_trials <- 10000

  while (!sufficient_data && trial < max_trials) {
    trial <- trial + 1

    d <- data.
    if (is.null(perm_var.)) {
      if (!large) {
        d[[dep]] <- RMPerm(d[[dep]], groups., CSS = TRUE)
      } else {
        d[[dep]] <- lapply(d[[dep]], RMPerm, groups = groups., CSS = TRUE)
      }
    } else {
      if (!large) {
        d[[perm_var.]] <- RMPerm(d[[perm_var.]], groups., CSS = TRUE)
      } else {
        d[[perm_var.]] <- lapply(d[[perm_var.]], RMPerm,
                                 groups = groups., CSS = TRUE)
      }
    }

    if (!large) {
      x_list <- lapply(data_vars., function(v) d[[v]])
      names(x_list) <- data_vars.
      pred <- make_css_data(y = d[[dep]], x = x_list,
                            nets = 1,
                            diag = diag., mode = mode.)$pred
    } else {
      pred_list <- vector("list", length(d[[dep]]))
      for (gr in seq_along(d[[dep]])) {
        xgr <- lapply(data_vars., function(v) d[[v]][[gr]])
        names(xgr) <- data_vars.
        pred_list[[gr]] <- make_css_data(y = d[[dep]][[gr]], x = xgr,
                                         nets = gr,
                                         diag = diag., mode = mode.)$pred
      }
      pred <- do.call(rbind, pred_list)
    }

    names(pred)[names(pred) == "yv"] <- dep

    if (family. != "multinom" && is.null(comp.)) {
      y_ok <- length(stats::na.omit(unique(pred[[dep]]))) > 1
    } else {
      y2_cat <- stats::na.omit(unique(pred[[dep]]))
      y_present <- all(y_cat %in% y2_cat)
      y_mult <- all(table(pred[[dep]]) > 2)
      y_ok <- y_present && y_mult
    }

    x_ok <- TRUE
    num_preds <- pred[, data_vars.[data_vars. %in% names(pred)], drop = FALSE]
    num_preds <- num_preds[, sapply(num_preds, is.numeric), drop = FALSE]
    if (ncol(num_preds) > 0) {
      x_ok <- all(sapply(num_preds, function(col) length(unique(col)) > 1))
    }

    if (nrow(pred) != 0 && x_ok && y_ok && !is.null(comp.)) {
      for (k in seq_along(comp.)) {
        pred2 <- pred[pred[[dep]] %in% comp.[[k]], ]
        pred2[[dep]] <- ifelse(pred2[[dep]] == comp.[[k]][1], 0, 1)
        check_cols <- c(dep, intersect(main_vars., names(pred2)))
        if (length(check_cols) > 1) {
          cors <- tryCatch(
            stats::cor(pred2[, check_cols, drop = FALSE], use = "complete.obs"),
            error = function(e) NULL
          )
          if (is.null(cors) || any(is.na(cors))) {
            y_ok <- x_ok <- FALSE
          } else {
            diag(cors) <- 0
            if (any(abs(cors) > 0.9999)) y_ok <- x_ok <- FALSE
          }
        }
      }
    }

    sufficient_data <- y_ok && x_ok
  }

  if (trial >= max_trials) {
    stop("Cannot find valid permutation after ", max_trials, " trials.")
  }

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


# Coefficient-table helper used by print.QAPCSS
#' @keywords internal
#' @noRd
glm_tab <- function(x, comp) {
  if (!is.null(comp)) {
    cat("\n\nComparison between",
        x$comp[[comp]][1], "and", x$comp[[comp]][2])
    cat("\n\nCoefficients:\n")

    nc <- length(x$base[[comp]]$coefficients)
    cmat <- matrix(NA, nrow = nc, ncol = 4)
    cmat[, 1] <- format(round(as.numeric(x$base[[comp]]$coefficients), 3))
    cmat[, 2] <- format(x$lower[[comp]][2, ])
    cmat[, 3] <- format(x$larger[[comp]][2, ])
    cmat[, 4] <- format(x$abs[[comp]][2, ])
    if (x$nullhyp == "qapspp") cmat[1, 2:4] <- "*"
    colnames(cmat) <- c("Estimate", "Pr(<=t)", "Pr(>=t)", "Pr(>=|t|)")
    rownames(cmat) <- names(x$base[[comp]]$coefficients)
    print.table(cmat)

    if (x$nullhyp == "qapspp")
      cat("\n* Significance test for the intercept is undefined with qapspp.\n")

    if (!is.null(x$base[[comp]]$base_model)) {
      cat("\nAIC of base model:", format(stats::AIC(x$base[[comp]]$base_model)))
      cat("\nBIC of base model:", format(stats::BIC(x$base[[comp]]$base_model)))
    }
    cat("\n")
  } else {
    cat("\n\nCoefficients:\n")

    nc <- length(x$base$coefficients)
    cmat <- matrix(NA, nrow = nc, ncol = 4)
    cmat[, 1] <- format(round(as.numeric(x$base$coefficients), 3))
    cmat[, 2] <- format(x$lower[2, ])
    cmat[, 3] <- format(x$larger[2, ])
    cmat[, 4] <- format(x$abs[2, ])
    if (x$nullhyp == "qapspp") cmat[1, 2:4] <- "*"
    colnames(cmat) <- c("Estimate", "Pr(<=t)", "Pr(>=t)", "Pr(>=|t|)")
    rownames(cmat) <- names(x$base$coefficients)
    print.table(cmat)

    if (x$nullhyp == "qapspp")
      cat("\n* Significance test for the intercept is undefined with qapspp.\n")

    if (!is.null(x$base$base_model)) {
      cat("\nAIC of base model:", format(stats::AIC(x$base$base_model)))
      cat("\nBIC of base model:", format(stats::BIC(x$base$base_model)))
    }
    cat("\n")
  }
}


#' @keywords internal
#' @noRd
QAPcss <- function(formula,
                   data,
                   mode      = "directed",
                   diag      = FALSE,
                   nullhyp   = "qapy",
                   reps      = 1000,
                   seed      = NULL,
                   strategy  = "sequential",
                   ncores    = NULL,
                   family    = "gaussian",
                   estimator = "standard",
                   groups    = NULL,
                   fixest_se_cluster = NULL,
                   reference  = NULL,
                   comparison = NULL,
                   use_robust_errors = FALSE,
                   random_intercept_nets      = FALSE,
                   random_intercept_sender    = FALSE,
                   random_intercept_receiver  = FALSE,
                   random_intercept_perceiver = FALSE,
                   use_gpu    = FALSE) {

  if (!is.null(seed)) set.seed(seed)

  parsed <- parse_qap_formula(formula, fixest_se_cluster)
  dep       <- parsed$dependent
  main      <- parsed$main
  data_vars <- intersect(parsed$all_data_vars, names(data))
  nx        <- length(main)

  validate_qap_input(data, parsed, css = TRUE)
  large <- is.list(data[[dep]])

  if (!large) {
    y <- data[[dep]]
    if (length(dim(y)) != 3)
      stop("data[['", dep, "']] must be a 3-dimensional array ",
           "[sender, receiver, perceiver].")
  } else {
    for (i in seq_along(data[[dep]])) {
      if (length(dim(data[[dep]][[i]])) != 3)
        stop("data[['", dep, "']][[", i, "]] must be a 3D array.")
    }
  }

  rin <- random_intercept_nets
  rip <- random_intercept_perceiver
  ris <- random_intercept_sender
  rir <- random_intercept_receiver

  mod <- build_internal_formula(formula, rin = rin, ris = ris,
                                rir = rir, rip = rip)
  mod_str <- paste(deparse(mod, width.cutoff = 500), collapse = " ")
  has_random <- grepl("\\(", mod_str) || parsed$has_random
  use_fixest <- parsed$use_fixest
  if (has_random && use_fixest) {
    warning("Cannot combine fixest FE and lme4 random effects. ",
            "Using lme4 only.")
    use_fixest <- FALSE
  }
  mod <- stats::as.formula(mod_str)

  if (has_random && family == "multinom") {
    warning("Random intercepts not implemented for multinomial. ",
            "Using standard nnet::multinom().")
    has_random <- FALSE
  }
  if (!is.null(reference) && !is.character(reference) && family == "multinom")
    reference <- as.character(reference)
  if (use_robust_errors && family == "multinom") {
    warning("Robust SEs not implemented for multinomial.")
    use_robust_errors <- FALSE
  }
  if ((nullhyp == "qapspp") && (nx == 1)) nullhyp <- "qapy"
  if (mode == "undirected" && (ris || rir)) {
    warning("Undirected mode: sender/receiver random intercepts set to FALSE.")
    ris <- rir <- FALSE
  }
  if (diag) warning("Results may not be valid when diagonal is used.")

  rand_part <- ""
  if (rin) rand_part <- paste(rand_part, "+ (1|nv)")
  if (rip) rand_part <- paste(rand_part, "+ (1|pv)")
  if (ris) rand_part <- paste(rand_part, "+ (1|sv)")
  if (rir) rand_part <- paste(rand_part, "+ (1|rv)")

  if (!large) {
    n <- dim(data[[dep]])[1]
    if (!is.null(groups)) {
      if (length(groups) != n)
        stop("groups length (", length(groups), ") != N (", n, ").")
      groups <- as.factor(groups)
    } else {
      groups <- as.factor(rep(1, n))
    }
  }

  valid <- NULL; valid_list <- NULL
  if (!large) {
    x_list <- lapply(data_vars, function(v) data[[v]])
    names(x_list) <- data_vars
    cssd  <- make_css_data(y = data[[dep]], x = x_list,
                           nets = 1,
                           diag = diag, mode = mode)
    pred  <- cssd$pred
    valid <- cssd$valid
  } else {
    pred_list  <- vector("list", length(data[[dep]]))
    valid_list <- vector("list", length(data[[dep]]))
    for (gr in seq_along(data[[dep]])) {
      xgr <- lapply(data_vars, function(v) data[[v]][[gr]])
      names(xgr) <- data_vars
      cssd <- make_css_data(y = data[[dep]][[gr]], x = xgr,
                            nets = gr,
                            diag = diag, mode = mode)
      pred_list[[gr]]  <- cssd$pred
      valid_list[[gr]] <- cssd$valid
    }
    pred <- do.call(rbind, pred_list)
  }

  names(pred)[names(pred) == "yv"] <- dep

  fit <- list()

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

  if (use_gpu && family == "gaussian" && !has_random && !use_fixest &&
      is.null(comparison) && !large) {

    if (nullhyp == "qapy") {
      gpu_res <- gpu_batch_ols_css(data         = data,
                                   parsed       = parsed,
                                   mode         = mode,
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
        test_val <- data[[xi]]
        if (!is.numeric(test_val)) {
          warning("Cannot residualise non-numeric predictor '", xi,
                  "'. Skipping qapspp for this variable.")
          next
        }
        xR <- residualise_predictor(xi, pred, main,
                                    has_random   = has_random,
                                    rand_formula = rand_part)
        data_resid <- data
        data_resid[[xi]] <- residuals_to_array(xR, data[[xi]], valid, pred,
                                               large, valid_list)

        gpu_res <- gpu_batch_ols_css(data         = data_resid,
                                     parsed       = parsed,
                                     mode         = mode,
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
        reps, QAPcssPermEst,
        data.     = data,
        perm_var. = NULL,
        mode.     = mode,
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
        names(fit$lower) <- names(fit$larger) <-
          names(fit$abs) <- names(comparison)
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
        if (family != "multinom") {
          n_coefs <- length(fit$base$coefficients)
          fit$lower  <- matrix(NA, nrow = 2, ncol = n_coefs)
          fit$larger <- fit$abs <- fit$lower
          colnames(fit$lower) <- colnames(fit$larger) <-
            colnames(fit$abs)  <- names(fit$base$coefficients)
        } else {
          ncat <- if (large) {
            length(stats::na.omit(unique(as.vector(unlist(data[[dep]])))))
          } else {
            length(stats::na.omit(unique(as.vector(data[[dep]]))))
          }
          n_coefs <- length(fit$base$coefficients)
          fit$lower  <- matrix(NA, nrow = 2 * (ncat - 1), ncol = n_coefs)
          fit$larger <- fit$abs <- fit$lower
          colnames(fit$lower) <- colnames(fit$larger) <-
            colnames(fit$abs) <- names(fit$base$coefficients)
        }
      } else {
        fit$lower <- fit$larger <- fit$abs <-
          vector("list", length(comparison))
        names(fit$lower) <- names(fit$larger) <-
          names(fit$abs) <- names(comparison)
        for (k in seq_along(comparison)) {
          n_coefs <- length(fit$base[[k]]$coefficients)
          fit$lower[[k]] <- matrix(NA, nrow = 2, ncol = n_coefs)
          fit$larger[[k]] <- fit$abs[[k]] <- fit$lower[[k]]
          colnames(fit$lower[[k]]) <- colnames(fit$larger[[k]]) <-
            colnames(fit$abs[[k]]) <- names(fit$base[[k]]$coefficients)
        }
      }

      for (xi in main) {
        test_val <- if (!large) data[[xi]] else data[[xi]][[1]]
        if (!is.numeric(test_val)) {
          warning("Cannot residualise non-numeric predictor '", xi,
                  "'. Skipping qapspp for this variable.")
          next
        }

        xR <- residualise_predictor(xi, pred, main,
                                    has_random   = has_random,
                                    rand_formula = rand_part)

        data_resid <- data
        data_resid[[xi]] <- residuals_to_array(xR, data[[xi]], valid, pred,
                                               large, valid_list)

        res <- run_permutations(
          reps, QAPcssPermEst,
          data.     = data_resid,
          perm_var. = xi,
          mode.     = mode,
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
  fit$family    <- family
  fit$groups    <- unique(unlist(groups))
  fit$diag      <- diag
  fit$mode      <- mode
  fit$reps      <- reps
  fit$reference <- reference
  fit$comp      <- comparison
  fit$random    <- c(sender    = ris,
                     receiver  = rir,
                     perceiver = rip,
                     nets      = rin)
  fit$robust_se <- use_robust_errors
  fit$estimator <- estimator

  if (is.null(comparison) && !is.null(fit$base$theta))
    fit$theta <- fit$base$theta
  if (is.null(comparison) && !is.null(fit$base$zi_coefficients))
    fit$zi_coefficients <- fit$base$zi_coefficients

  if (family == "multinom")
    names(fit)[names(fit) == "t"] <- "z"

  class(fit) <- "QAPCSS"
  return(fit)
}


#' @keywords internal
#' @noRd
print.QAPCSS <- function(x, ...) {

  if (x$family != "multinom") {
    if (!any(x$random)) {
      cat("\nGeneralized Linear Network Model for CSS\n\n")
    } else {
      cat("\nGeneralized Linear Mixed Network Model for CSS fit by REML\n\n")
    }
  } else {
    cat("\nMultinomial Choice Network Model for CSS\n\n")
    cat("The reference group was", format(paste0(x$reference, ".")), "\n")
  }

  if (!is.null(x$estimator) && x$estimator == "gmm")
    cat("Estimator: Generalized Method-of-Moments.\n")
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

  if (x$nullhyp == "qapy")
    cat("The outcome array Y was permuted", format(x$reps), "times.\n")
  if (x$nullhyp == "qapspp") {
    cat("Significance was estimated using Dekker's\n")
    cat("  'semi-partialling plus' procedure with",
        format(x$reps), "permutations.\n")
  }

  if (x$robust_se)
    cat("T-values are based on robust standard errors.\n")

  if (x$diag) {
    cat("Diagonal values (loops) were used in the estimation.\n",
        "  Results may be biased because of that.\n")
  } else {
    cat("Diagonal values (loops) were ignored.\n")
  }
  cat("The outcome was treated as", format(paste0(x$mode, ".")), "\n")

  if (x$family != "multinom") {
    if (is.null(x$comp)) {
      glm_tab(x, comp = x$comp)
    } else {
      for (mod in seq_along(x$comp)) {
        glm_tab(x, comp = names(x$comp)[[mod]])
      }
    }
  } else {
    cat("\nCoefficients:\n\n")
    for (option in seq_len(nrow(x$base$coefficients))) {
      cat(format(paste0("-- ", rownames(x$base$coefficients)[option], "\n")))

      nc <- ncol(x$base$coefficients)
      cmat <- matrix(NA, nrow = nc, ncol = 4)
      row_idx <- option + nrow(x$base$coefficients)
      cmat[, 1] <- format(as.numeric(x$base$coefficients[option, ]))
      cmat[, 2] <- format(x$lower[row_idx, ])
      cmat[, 3] <- format(x$larger[row_idx, ])
      cmat[, 4] <- format(x$abs[row_idx, ])
      if (x$nullhyp == "qapspp") cmat[1, 2:4] <- "*"
      colnames(cmat) <- c("Estimate", "Pr(<=t)", "Pr(>=t)", "Pr(>=|t|)")
      rownames(cmat) <- colnames(x$base$coefficients)
      print.table(cmat)
      cat("\n\n")
    }

    if (x$nullhyp == "qapspp")
      cat("* Significance test for the intercept is undefined with qapspp.\n")

    cat("\nAIC of base model:", format(stats::AIC(x$base$base_model)))
    cat("\nBIC of base model:", format(stats::BIC(x$base$base_model)))
    cat("\n")
  }

  if (!is.null(x$confusion_matrix)) {
    cat("\n")
    print(x$confusion_matrix)
  }

  cat("\n")
  invisible(x)
}
