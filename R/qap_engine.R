# QAP engine ------------------------------------------------------------------
#
# Internal. The matrix-level engine behind net_regression(): it performs the
# baseline fit and the permutation inference on a pre-built list of matrices,
# for both a dyadic network and a cognitive social structure.
#
# The two used to be `QAPglm()` and `QAPcss()`, two 200-line functions that were
# 55% the same code. Everything they shared is here; everything they did not is
# in R/qap_shapes.R. Ported from MrQAP. All parallelism uses the `future`
# framework via `run_permutations()`.

#' @keywords internal
#' @noRd
QAPengine <- function(formula,
                      matlist,
                      css       = FALSE,
                      family    = "gaussian",
                      directed  = TRUE,
                      diag      = FALSE,
                      permute   = "predictor",
                      times     = 1000,
                      seed      = NULL,
                      groups    = NULL,
                      strategy  = "sequential",
                      ncores    = NULL,
                      random_intercept_nets      = FALSE,
                      random_intercept_sender    = FALSE,
                      random_intercept_receiver  = FALSE,
                      random_intercept_perceiver = FALSE,
                      use_robust_errors = FALSE,
                      less_mem  = FALSE) {

  if (!is.null(seed)) set.seed(seed)

  shape  <- .qap_shape(css)
  parsed <- parse_qap_formula(formula)
  dep       <- parsed$dependent
  main      <- parsed$main
  data_vars <- intersect(parsed$all_data_vars, names(matlist))

  validate_qap_input(matlist, parsed, css = css)
  large <- is.list(matlist[[dep]])

  # ---- random intercepts ----------------------------------------------------

  requested <- c(nets      = random_intercept_nets,
                 sender    = random_intercept_sender,
                 receiver  = random_intercept_receiver,
                 perceiver = random_intercept_perceiver)
  # A slot a shape does not have cannot be asked for. A dyadic network has no
  # perceiver, so a perceiver intercept there is a mistake worth naming.
  unavailable <- names(requested)[requested &
                                    !(names(requested) %in% names(shape$rand_slots))]
  if (length(unavailable) > 0) {
    label <- shape$label
    manynet::snet_abort(
      "A {label} has no {.val {unavailable}} random intercept{?s}.")
  }
  if (!directed && (requested[["sender"]] || requested[["receiver"]])) {
    manynet::snet_warn(
      c("An undirected network has no senders or receivers.",
        i = "Setting the sender and receiver random intercepts to {.val FALSE}."))
    requested[c("sender", "receiver")] <- FALSE
  }

  slots  <- shape$rand_slots
  active <- slots[requested[names(slots)]]
  # `paste0()` folds a zero-length argument into "", so an empty set of
  # intercepts would otherwise produce the unparseable term " + (1|)".
  rand_part <- if (length(active) == 0) {
    ""
  } else {
    paste0(" + (1|", active, ")", collapse = "")
  }

  mod <- stats::as.formula(paste(
    paste(deparse(formula, width.cutoff = 500), collapse = " "), rand_part))
  has_random <- length(active) > 0 || parsed$has_random

  if (diag && css)
    manynet::snet_warn(
      "Results may not be valid where the diagonal is included.")

  # ---- vectorise ------------------------------------------------------------

  if (!large && !is.null(groups)) {
    n <- dim(matlist[[dep]])[1]
    if (length(groups) != n)
      manynet::snet_abort(
        "{.arg groups} is of length {length(groups)}, but the network has {n} nodes.")
    groups <- as.factor(groups)
  }

  vec <- .vectorise_matlist(shape, matlist, dep, data_vars, groups,
                            diag, directed, large)
  pred <- vec$pred
  names(pred)[names(pred) == "yv"] <- dep

  # ---- baseline -------------------------------------------------------------

  fit <- list()
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
    manynet::snet_info(
      "Permuting {.val outcome}, not {.val predictor}:",
      "with one predictor there is nothing to residualise it against.")
  }

  # ---- permute --------------------------------------------------------------

  old_plan <- setup_future_plan(strategy, ncores)
  on.exit({
    future::plan(old_plan)
    options(future.globals.maxSize = attr(old_plan, "old_maxSize"))
  }, add = TRUE)

  perm_args <- list(shape. = shape, directed. = directed, diag. = diag,
                    mod. = mod, groups. = groups, fit. = fit$base,
                    family. = family, use_robust_errors. = use_robust_errors,
                    has_random. = has_random, main_vars. = main,
                    data_vars. = data_vars, parsed. = parsed)

  if (permute == "outcome") {
    res <- do.call(run_permutations,
                   c(list(times, QAPPermEst, matlist. = matlist,
                          perm_var. = NULL), perm_args))
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
      test_val <- if (!large) matlist[[xi]] else matlist[[xi]][[1]]
      if (!is.numeric(test_val)) {
        manynet::snet_warn(
          c("Cannot residualise the non-numeric predictor {.val {xi}}.",
            i = "Skipping double semi-partialling for this predictor."))
        next
      }

      xR <- residualise_predictor(xi, pred, main,
                                  has_random   = has_random,
                                  rand_formula = rand_part)
      matlist_resid <- matlist
      matlist_resid[[xi]] <- shape$unresidualise(xR, matlist[[xi]], pred, large,
                                                 vec$valid, vec$valid_list)

      res <- do.call(run_permutations,
                     c(list(times, QAPPermEst, matlist. = matlist_resid,
                            perm_var. = xi), perm_args))
      agg <- aggregate_perm_results(res, times)
      fit$lower[, xi]  <- agg$lower
      fit$larger[, xi] <- agg$larger
      fit$abs[, xi]    <- agg$abs
    }
  }

  # ---- assemble -------------------------------------------------------------

  # Lift what a reader of the result needs out of the baseline fit, so that
  # `fit$coefficients` works without reaching into `fit$base`.
  fit$coefficients <- fit$base$coefficients
  fit$t            <- fit$base$t
  for (el in c("r.squared", "adj.r.squared", "random.intercepts",
               "theta", "zi_coefficients")) {
    if (!is.null(fit$base[[el]])) fit[[el]] <- fit$base[[el]]
  }
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
  fit$times     <- times
  fit$groups    <- unique(unlist(groups))
  fit$robust_se <- use_robust_errors
  fit$random    <- requested[names(slots)]
  fit$pred      <- pred
  fit$dep       <- dep


  class(fit) <- if (css) {
    "QAPCSS"
  } else if (family == "gaussian") {
    "QAPRegression"
  } else {
    "QAPGLM"
  }
  fit
}


# One permutation: draw, vectorise, refit, and compare against the baseline.
# Returns NULL where the fit fails, which `aggregate_perm_results()` counts.
#' @keywords internal
#' @noRd
QAPPermEst <- function(i,
                       matlist.,
                       perm_var.,
                       shape.,
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

  dep    <- parsed.$dependent
  large  <- is.list(matlist.[[dep]])
  target <- if (is.null(perm_var.)) dep else perm_var.

  trial <- 0L
  repeat {
    trial <- trial + 1L
    d <- matlist.
    d[[target]] <- if (large) {
      lapply(d[[target]], shape.$permute, groups = groups.)
    } else {
      shape.$permute(d[[target]], groups.)
    }

    vec <- .vectorise_matlist(shape., d, dep, data_vars., groups.,
                              diag., directed., large)
    pred <- vec$pred
    names(pred)[names(pred) == "yv"] <- dep

    if (.sufficient_data(pred, dep, data_vars.)) break
    if (trial >= shape.$max_trials) {
      # A shape that takes the first draw lets the fit fail and be counted.
      if (shape.$max_trials == 1L) break
      manynet::snet_abort(
        c("Cannot find a valid permutation after {trial} trials.",
          i = "The network may be too sparse, or too many cells may be missing."))
    }
  }

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

  compare_perm_to_baseline(perm_fit$coefficients, perm_fit$t,
                           fit., xi = perm_var.)
}
