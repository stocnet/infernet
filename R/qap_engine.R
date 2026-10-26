# QAP engine ------------------------------------------------------------------
#
# Internal. The matrix-level engine behind estimate_mrqap(): it performs the
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
                      random_intercept_reporter  = FALSE,
                      use_robust_errors = FALSE,
                      missing   = "available",
                      panel     = FALSE,
                      fixed_effects = character(0),
                      less_mem  = FALSE) {

  if (!is.null(seed)) set.seed(seed)

  shape  <- .qap_shape(css)
  parsed <- parse_qap_formula(formula)
  dep       <- parsed$dependent
  main      <- parsed$main
  data_vars <- intersect(parsed$all_data_vars, names(matlist))

  validate_qap_input(matlist, parsed, css = css)
  large <- is.list(matlist[[dep]])
  if (panel && !large)
    manynet::snet_abort("A panel needs a list of waves for each variable.")

  # ---- random intercepts ----------------------------------------------------

  requested <- c(nets      = random_intercept_nets,
                 sender    = random_intercept_sender,
                 receiver  = random_intercept_receiver,
                 reporter  = random_intercept_reporter)
  # A slot a shape does not have cannot be asked for. A dyadic network has no
  # reporter, so a reporter intercept there is a mistake worth naming.
  unavailable <- names(requested)[requested &
                                    !(names(requested) %in% names(shape$rand_slots))]
  if (length(unavailable) > 0) {
    label <- shape$label
    manynet::snet_abort(
      "A {label} has no {.val {unavailable}} random intercept{?s}.")
  }
  # A two-mode network keeps them: its rows and its columns are different
  # nodes, whether or not it is read as directed.
  y1 <- if (large) matlist[[dep]][[1]] else matlist[[dep]]
  twomode <- dim(y1)[1] != dim(y1)[2]
  if (!directed && !twomode && (requested[["sender"]] || requested[["receiver"]])) {
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

  # ---- fixed effects --------------------------------------------------------

  fixed_effects <- .check_fixed(fixed_effects, shape, directed, large,
                                has_random, family, use_robust_errors,
                                twomode = twomode)
  fe_cols <- .fixed_columns(fixed_effects, large, panel)
  # A gaussian model absorbs them once, below; any other family absorbs them
  # in every fit, with {fixest}.
  absorb_once <- length(fixed_effects) > 0 && family == "gaussian"
  fit_fixed <- if (length(fixed_effects) > 0 && !absorb_once) fe_cols else NULL
  if (!is.null(fit_fixed))
    thisRequires("fixest", "for fixed effects in a model that is not gaussian")
  if (absorb_once) {
    # The intercept is one of the absorbed effects.
    mod <- stats::as.formula(paste(
      paste(deparse(formula, width.cutoff = 500), collapse = " "), "- 1"))
  }

  if (diag && css)
    manynet::snet_warn(
      "Results may not be valid where the diagonal is included.")

  # ---- vectorise ------------------------------------------------------------

  if (!large && !is.null(groups)) {
    # A blocking factor names the nodes of one mode. A one-mode network has a
    # single mode, but a two-mode network has two of different sizes, and either
    # may be the one that is blocked. `.perm_order()` already permutes a mode
    # freely when the factor cannot describe it, so accept a length that matches
    # any side of the outcome and abort only when it matches none.
    ns <- unique(dim(matlist[[dep]]))
    if (!length(groups) %in% ns)
      manynet::snet_abort(
        c("{.arg groups} is of length {length(groups)}.",
          i = "It must match one of the outcome's node sets: {ns}."))
    groups <- as.factor(groups)
  }

  # ---- missing cells ---------------------------------------------------------

  # Counted before the model drops anything, so that "fail" can stop before it
  # does and "available" can say what it dropped.
  # A fixed tie keeps its value, which a predictor built from the network has
  # already read. It is no observation of the process, though, since the
  # design set it, so `exclude` keeps it out of the outcome's rows, and the
  # blocks keep it in place under permutation. It is not missing data, so it is
  # counted apart from the unobserved cells, and "fail" does not stop on it.
  exclude <- if (large) lapply(matlist[[dep]], attr, "fixed")
             else attr(matlist[[dep]], "fixed")
  if (large && all(vapply(exclude, is.null, logical(1)))) exclude <- NULL

  unobserved <- .missing_summary(matlist, dep, data_vars, diag, directed, large,
                                 exclude)
  n_fixed_ties <- unobserved$excluded
  unit <- if (css) "reported dyads" else "dyads"
  missing_vars <- unobserved$by_variable[unobserved$by_variable > 0]
  missing_parts <- paste(missing_vars, "on", names(missing_vars))
  if (identical(missing, "fail") && length(missing_vars) > 0) {
    manynet::snet_abort(
      c("The model has unobserved {unit}: {missing_parts}.",
        i = "Drop them with {.code control = list(missing = \"available\")}.",
        i = "Or remove them first, for example with {.fn manynet::to_no_missing}."))
  }

  vec <- .vectorise_matlist(shape, matlist, dep, data_vars, groups,
                            diag, directed, large, exclude)
  pred <- vec$pred
  names(pred)[names(pred) == "yv"] <- dep

  # Reported only where something was dropped: a complete network uses every
  # cell it could, and a stated model on one should print nothing.
  used <- nrow(pred)
  if (length(missing_vars) > 0) {
    possible <- unobserved$possible
    manynet::snet_info(
      "Using {used} of {possible} {unit}, after dropping the unobserved ones:",
      "{missing_parts}.")
  }
  if (n_fixed_ties > 0) {
    manynet::snet_info(
      "Leaving out {n_fixed_ties} fixed tie{?s}, which the network marks as set by design,",
      "and keeping {?it/them} in place under permutation.")
  }

  # The result keeps the observed values, whichever values are fitted. A
  # gaussian model is fitted to values demeaned once: every permutation fits
  # these dyads, and maps each group onto a group, so the demeaned values keep
  # zero group means in every draw.
  pred_observed <- pred
  fixed_df <- NULL
  if (absorb_once) {
    absorbed <- .absorb_fixed(shape, matlist, pred, vec, dep, main,
                              fixed_effects, fe_cols, large, directed)
    pred     <- absorbed$pred
    matlist  <- absorbed$matlist
    fixed_df <- absorbed$df
  }
  fixed_ids <- NULL
  if (!is.null(fit_fixed)) {
    fixed_ids <- .fixed_ids(pred, fe_cols)
    .check_absorbed(.demean(as.matrix(pred[main]), fixed_ids), pred[main],
                    fixed_effects)
  }

  # ---- baseline -------------------------------------------------------------

  fit <- list()
  fit$base <- fit_qap_model(mod          = mod,
                            pred         = pred,
                            family       = family,
                            use_robust_errors = use_robust_errors,
                            main_vars    = main,
                            has_random   = has_random,
                            fixed_effects = fit_fixed,
                            fixed_df     = fixed_df)
  if (!is.null(fit_fixed)) {
    removed <- nrow(pred) - stats::nobs(fit$base$base_model)
    if (removed > 0)
      manynet::snet_info(
        c("Leaving out {removed} dyad{?s} whose fixed-effect group has only one outcome value.",
          i = "{.pkg fixest} cannot estimate an effect for such a group."))
  }

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
                    data_vars. = data_vars, parsed. = parsed,
                    n_obs. = used, panel. = panel, exclude. = exclude,
                    fixed_effects. = fit_fixed, fixed_df. = fixed_df)
  # Each permutation set exchanges only the nodes whose missing cells match, so
  # that every draw is fitted on the dyads of the observed model.
  fixed <- integer(0)
  .blocks_for <- function(target, label) {
    blocks <- .permutation_blocks(target, groups, diag, times, label, panel,
                                  exclude)
    if (is.null(blocks)) return(groups)
    fixed[[label]] <<- attr(blocks, "fixed")
    blocks
  }

  if (permute == "outcome") {
    res <- do.call(run_permutations,
                   c(list(times, QAPPermEst, matlist. = matlist,
                          perm_var. = NULL,
                          perm_groups. = .blocks_for(matlist[[dep]], dep)),
                     perm_args))
    agg <- aggregate_perm_results(res, times)
    fit$lower    <- agg$lower
    fit$larger   <- agg$larger
    fit$abs      <- agg$abs
    fit$testdist <- .as_testdist(agg$testdist, names(fit$base$coefficients),
                                 times)

  } else if (permute == "predictor") {
    n_coefs <- length(fit$base$coefficients)
    fit$lower  <- matrix(NA, nrow = 2, ncol = n_coefs,
                         dimnames = list(c("perm_coefs", "perm_t"),
                                         names(fit$base$coefficients)))
    fit$larger <- fit$abs <- fit$lower
    # Each predictor is permuted in its own set of draws, so a row pairs the
    # columns only by its position. The intercept is not permuted at all, and
    # its column stays NA, as its p-values do.
    fit$testdist <- .as_testdist(NULL, names(fit$base$coefficients), times)

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
                                  rand_formula = rand_part,
                                  fixed_ids    = fixed_ids)
      matlist_resid <- matlist
      matlist_resid[[xi]] <- shape$unresidualise(xR, matlist[[xi]], pred, large,
                                                 vec$valid, vec$valid_list,
                                                 directed)

      res <- do.call(run_permutations,
                     c(list(times, QAPPermEst, matlist. = matlist_resid,
                            perm_var. = xi,
                            perm_groups. = .blocks_for(matlist_resid[[xi]], xi)),
                       perm_args))
      agg <- aggregate_perm_results(res, times)
      fit$lower[, xi]  <- agg$lower
      fit$larger[, xi] <- agg$larger
      fit$abs[, xi]    <- agg$abs
      draws <- as.vector(agg$testdist)
      fit$testdist[seq_along(draws), xi] <- draws
    }
  }

  if (length(fixed) > 0) {
    n_fixed_nodes <- max(fixed)
    manynet::snet_info(
      "Permuting nodes only among nodes with the same unobserved cells,",
      "so that every permutation uses the same {used} {unit}.")
    if (n_fixed_nodes > 0)
      manynet::snet_info(
        "{n_fixed_nodes} node{?s} with scattered unobserved or fixed cells stay{?s/} in place.")
  }

  # ---- assemble -------------------------------------------------------------

  # Lift what a reader of the result needs out of the baseline fit, so that
  # `fit$coefficients` works without reaching into `fit$base`. The observed
  # test statistics are `obs` here rather than the baseline's `t`, which reads
  # as `t()` or as time.
  fit$coefficients <- fit$base$coefficients
  fit$obs          <- fit$base$t
  for (el in c("r.squared", "adj.r.squared", "random.intercepts",
               "theta", "zi_coefficients")) {
    if (!is.null(fit$base[[el]])) fit[[el]] <- fit$base[[el]]
  }
  if (!less_mem) fit$simple_fit <- fit$base$base_model

  if (family == "binomial") {
    # From the model rather than from `pred`, since {fixest} leaves out the
    # dyads of a group with one outcome value.
    bm <- fit$base$base_model
    fitted_prob <- stats::fitted(bm)
    fit$confusion_matrix <- probabilistic_confusion_matrix(
      actual = fitted_prob + stats::residuals(bm, type = "response"),
      predicted_prob = fitted_prob,
      n_draws = 1000, seed = seed
    )
  }

  fit$permute   <- permute
  fit$panel     <- panel
  fit$diag      <- diag
  fit$family    <- family
  fit$directed  <- directed
  fit$times     <- times
  fit$groups    <- unique(unlist(groups))
  fit$robust_se <- use_robust_errors
  fit$random    <- requested[names(slots)]
  fit$fixed_effects <- fixed_effects
  fit$pred      <- pred_observed
  fit$dep       <- dep
  fit$missing   <- list(treatment   = missing,
                        possible    = unobserved$possible,
                        used        = used,
                        unobserved  = unobserved$by_variable,
                        fixed_ties  = n_fixed_ties,
                        fixed_nodes = fixed)


  # Most specific first, so that a method can be written at any level and S3
  # falls back to the next. Every result is a `netreg`. Its family splits it
  # into linear and generalised, which is where printing differs. A CSS puts
  # `netreg_css` in front, so that a CSS method can take over and still fall
  # back to the family's. The names say what the result is, not the procedure
  # that produced it, as the function names do.
  family_class <- if (family == "gaussian") "netreg_lm" else "netreg_glm"
  class(fit) <- c(if (css) "netreg_css", family_class, "netreg")
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
                       parsed.,
                       perm_groups. = groups.,
                       n_obs. = NULL,
                       panel. = FALSE,
                       exclude. = NULL,
                       fixed_effects. = NULL,
                       fixed_df. = NULL) {

  dep    <- parsed.$dependent
  large  <- is.list(matlist.[[dep]])
  target <- if (is.null(perm_var.)) dep else perm_var.

  trial <- 0L
  repeat {
    trial <- trial + 1L
    d <- matlist.
    # A pooled list has one set of blocks for each network, where its missing
    # cells differ; the user's groups are one factor for all of them.
    # The waves of a panel are the same nodes, observed again, so one draw
    # relabels them the same way in every wave. Permuting each wave on its own
    # would break each node's trajectory under the null.
    d[[target]] <- if (panel.) {
      .permute_panel(d[[target]], perm_groups.)
    } else if (large) {
      per_net <- is.list(perm_groups.) && is.null(names(perm_groups.))
      lapply(seq_along(d[[target]]), function(k) {
        shape.$permute(d[[target]][[k]],
                       if (per_net) perm_groups.[[k]] else perm_groups.)
      })
    } else {
      shape.$permute(d[[target]], perm_groups.)
    }

    vec <- .vectorise_matlist(shape., d, dep, data_vars., groups.,
                              diag., directed., large, exclude.)
    pred <- vec$pred
    names(pred)[names(pred) == "yv"] <- dep
    # The blocks keep the missing cells in place, so a draw on any other number
    # of dyads is a fault in the engine, not in the data.
    if (!is.null(n_obs.) && nrow(pred) != n_obs.)
      manynet::snet_abort(
        c("A permutation used {nrow(pred)} dyads, where the observed model used {n_obs.}.",
          i = "This is a bug in {.pkg infernet}. Please report it."))

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
                                   has_random   = has_random.,
                                   fixed_effects = fixed_effects.,
                                   fixed_df     = fixed_df.)),
    error = function(e) NULL
  )
  if (is.null(perm_fit)) return(NULL)

  compare_perm_to_baseline(perm_fit$coefficients, perm_fit$t,
                           fit., xi = perm_var.)
}


# The permuted test statistics as a matrix of one row per permutation and one
# column per coefficient, named as the coefficients are. The test family keeps
# its null distribution as `testdist` too, so the word means one thing across
# the package. A permutation that failed leaves its row NA rather than shifting
# the others up, so every column has `times` rows.
#' @keywords internal
#' @noRd
.as_testdist <- function(draws, coef_names, times) {
  out <- matrix(NA_real_, nrow = times, ncol = length(coef_names),
                dimnames = list(NULL, coef_names))
  if (!is.null(draws)) {
    draws <- matrix(draws, ncol = length(coef_names))
    out[seq_len(nrow(draws)), ] <- draws
  }
  out
}
