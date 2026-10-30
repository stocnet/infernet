# Tests of parameters ####

#' Tests of a model's parameters
#' @name test_parameter
#' @description
#'   These generics test hypotheses about the parameters of a fitted model:
#'
#'   - `test_parameter()` asks whether one or more parameters have the values
#'   that a hypothesis gives them.
#'   - `test_time()` asks whether parameters are constant over time.
#'
#'   Each has a method for a `netreg`, from `estimate_mrqap()`.
#'   `{infernet}` holds the generics so that other packages can register
#'   methods for their own models on them.
#' @details
#'   For a `netreg`, both tests compare a model in which the tested
#'   coefficients are free with one in which they are fixed.
#'   The statistic is the deviance that freeing them removes:
#'   the residual sum of squares of a gaussian model, and the deviance of a
#'   binomial or poisson one.
#'   The p-value comes from a joint double semi-partialling
#'   (Dekker, Krackhardt and Snijders 2007).
#'   Each tested predictor is residualised on the untested ones,
#'   one permutation of the nodes is applied to all the residuals together,
#'   and the model is fitted again.
#'   The permutations exchange only nodes whose unobserved cells match,
#'   as `estimate_mrqap()` does, so every permutation uses the dyads of the
#'   observed model.
#'   The p-value is
#'   \deqn{p = \frac{1 + \#\{s_{perm} \geq s_{obs}\}}{1 + k}}
#'   where \eqn{k} is the number of permutations that the model could be
#'   fitted to.
#'
#'   With one tested coefficient, this is the test that `estimate_mrqap()`
#'   gives that coefficient under `permute = "predictor"`, with the deviance
#'   in place of the t-statistic.
#'   Its value is that it can test several coefficients together, such as the
#'   levels of a categorical attribute, and test a value other than 0.
#'
#'   `test_parameter()` tests that the coefficients of `terms` equal `value`.
#'   In a gaussian model it subtracts `value` times each predictor from the
#'   outcome, and tests that the coefficients of the result are 0.
#'   In a binomial or poisson model, `value` must be 0, since a value there
#'   would be an offset, and an offset under permutation has no clear meaning.
#'
#'   `test_time()` tests that each coefficient of `terms` is the same in every
#'   fitted wave of a panel.
#'   It adds an interaction of each tested predictor with each fitted wave after
#'   the first, and an intercept for each such wave, and tests that the
#'   interactions are all 0.
#'   The intercepts for the waves are not tested, since the density of a network
#'   often changes from wave to wave.
#'   One permutation relabels the nodes in the same way in every wave.
#' @section Network regressions:
#'   Both methods read the dyads that the model kept, so they do not need the
#'   network again.
#'   They cannot yet test a model of a cognitive social structure,
#'   a model with random intercepts, or a model with fixed effects that is not
#'   gaussian.
#'   A model fitted with `groups` needs the same `groups` in `control`.
#'   `test_time()` needs a panel, a longitudinal network that
#'   `estimate_mrqap()` fits wave by wave.
#'   A list of networks is not a panel, since nothing says that its networks
#'   are in time order.
#' @section Other packages:
#'   `{RSiena}` and `{goldfish}` export `test_parameter()` and `test_time()`
#'   generics with the same signature, for their own fitted models.
#'   If more than one of these packages is attached, the one attached last
#'   masks the others.
#'   Call `infernet::test_parameter()`, `RSiena::test_parameter()`, or
#'   `goldfish::test_parameter()` to choose.
#' @param x A fitted model, such as a `netreg` from `estimate_mrqap()`.
#' @param terms The names of the coefficients to test, as the model prints
#'   them, such as `"ego_Age"`.
#'   A name that is not a coefficient selects every coefficient that starts
#'   with it and an underscore, so `"ego_Grp"` selects the levels
#'   `"ego_Grp_b"` and `"ego_Grp_c"`.
#'   For `test_time()`, `NULL` tests every coefficient but the intercept.
#' @param value The value of the coefficients under the hypothesis:
#'   one number for all of them, or one for each.
#' @param times The number of permutations.
#' @param control A list of controls, or the result of `control_mrqap()`.
#'   The tests read `seed`, `strategy`, `ncores`, and `groups`, and the
#'   controls set with `control_mrqap(..., persist = TRUE)` where these are
#'   not named.
#' @param ... Arguments for a method. Not used by the methods for a `netreg`.
#' @return For a `netreg`, a `network_test`, with the statistic, the
#'   permutation distribution, and the p-value as `pgteobs`.
#'   It also holds the tested coefficients as `terms`, the tested values as
#'   `value`, and the number of permutations left out because the model did
#'   not converge on them as `failed`.
#' @references
#'   Dekker, David, David Krackhardt, and Tom A. B. Snijders. 2007.
#'   "Sensitivity of MRQAP Tests to Collinearity and Autocorrelation Conditions."
#'   _Psychometrika_ 72(4): 563-581.
#'   \doi{10.1007/s11336-007-9016-1}
#' @family models
#' @examples
#' fit <- estimate_mrqap(. ~ ego(Grp) + alter(Age), ison_adolescents |>
#'   mutate_nodes(Grp = rep(c("a", "b", "c"), length.out = 8),
#'                Age = 12:19), times = 50)
#' # Do the groups differ, taking both levels together?
#' test_parameter(fit, "ego_Grp", times = 100, control = list(seed = 1))
#' @export
test_parameter <- function(x, ...) UseMethod("test_parameter")

#' @rdname test_parameter
#' @export
test_parameter.default <- function(x, ...) {
  manynet::snet_abort(
    c("{.fn test_parameter} has no method for a {.cls {class(x)[1]}}.",
      i = "Give it a {.cls netreg} from {.fn estimate_mrqap}."))
}

#' @rdname test_parameter
#' @export
test_parameter.netreg <- function(x, terms, value = 0, ..., times = 1000,
                                  control = list()) {
  .check_joint_fit(x, "test_parameter")
  main <- .main_columns(x)
  if (missing(terms)) {
    manynet::snet_abort(
      c("{.fn test_parameter} needs the {.arg terms} to test.",
        i = "The coefficients of the model are {.val {main}}."))
  }
  tested <- .tested_columns(x, terms)
  if (!is.numeric(value) || anyNA(value) ||
      !length(value) %in% c(1, length(tested))) {
    manynet::snet_abort(
      c("{.arg value} must be one number, or one for each tested coefficient.",
        i = "The tested coefficients are {.val {tested}}."))
  }
  value <- rep_len(value, length(tested))
  if (any(value != 0) && !identical(x$family, "gaussian")) {
    manynet::snet_abort(
      c("{.fn test_parameter} tests a value other than 0 only in a gaussian model.",
        i = "In a {.val {x$family}} model a value is an offset, and an offset under permutation has no clear meaning."))
  }
  .joint_test(x, x$pred, tested, setdiff(main, tested), value,
              times, control, test = "Parameter")
}

#' @rdname test_parameter
#' @export
test_time <- function(x, ...) UseMethod("test_time")

#' @rdname test_parameter
#' @export
test_time.default <- function(x, ...) {
  manynet::snet_abort(
    c("{.fn test_time} has no method for a {.cls {class(x)[1]}}.",
      i = "Give it a {.cls netreg} from {.fn estimate_mrqap}, fitted on a longitudinal network."))
}

#' @rdname test_parameter
#' @export
test_time.netreg <- function(x, terms = NULL, ..., times = 1000,
                             control = list()) {
  .check_joint_fit(x, "test_time")
  if (!isTRUE(x$panel)) {
    pooled <- length(unique(x$pred$nv)) > 1
    manynet::snet_abort(
      c("{.fn test_time} needs a model of a panel: a longitudinal network, fitted wave by wave.",
        i = if (pooled) "A list of networks is not a panel, since nothing says that its networks are in time order."
            else "This model was fitted on one network."))
  }
  main <- .main_columns(x)
  tested <- if (is.null(terms)) main else .tested_columns(x, terms)
  pred <- x$pred
  wave <- as.character(pred$nv)
  waves <- unique(wave)
  if (length(waves) < 2) {
    manynet::snet_abort(
      c("{.fn test_time} needs at least two fitted waves, and the model has one.",
        i = "A panel of three waves fits two: each wave after the first, against the wave before it."))
  }
  # An intercept for each later wave, unless fixed effects already absorb the
  # waves, so that a change in density is not read as a change in a coefficient.
  untested <- main
  if (!"network" %in% x$fixed_effects) {
    for (w in waves[-1]) {
      nm <- paste0("(Intercept):wave", w)
      pred[[nm]] <- as.numeric(wave == w)
      untested <- c(untested, nm)
    }
  }
  inter <- character(0)
  for (v in tested) {
    for (w in waves[-1]) {
      nm <- paste0(v, ":wave", w)
      pred[[nm]] <- pred[[v]] * (wave == w)
      inter <- c(inter, nm)
    }
  }
  .joint_test(x, pred, inter, untested, rep(0, length(inter)),
              times, control, test = "Time")
}

# ---- joint double semi-partialling -------------------------------------------

# What the joint test cannot fit yet, named before anything is computed.
#' @keywords internal
#' @noRd
.check_joint_fit <- function(x, fn) {
  if (inherits(x, "netreg_css")) {
    manynet::snet_abort(
      "{.fn {fn}} cannot yet test a model of a cognitive social structure.")
  }
  if (any(unlist(x$random)) || inherits(x$base$base_model, "merMod")) {
    manynet::snet_abort(
      "{.fn {fn}} cannot yet test a model with random intercepts.")
  }
  if (!x$family %in% c("gaussian", "binomial", "poisson")) {
    manynet::snet_abort(
      c("{.fn {fn}} cannot yet test a {.val {x$family}} model.",
        i = "It tests gaussian, binomial, and poisson models."))
  }
  if (length(x$fixed_effects) > 0 && !identical(x$family, "gaussian")) {
    manynet::snet_abort(
      "{.fn {fn}} cannot yet test a model with fixed effects that is not gaussian.")
  }
  if (is.null(x$pred)) {
    manynet::snet_abort("The model holds no dyads to test.")
  }
  invisible(x)
}

# The predictors of the model, one column of `x$pred` for each coefficient.
#' @keywords internal
#' @noRd
.main_columns <- function(x) {
  intersect(setdiff(names(x$coefficients), "(Intercept)"), names(x$pred))
}

# A name that is a coefficient selects it; any other name selects every
# coefficient that starts with it and an underscore, such as the levels of a
# categorical attribute.
#' @keywords internal
#' @noRd
.tested_columns <- function(x, terms) {
  main <- .main_columns(x)
  if (!is.character(terms) || length(terms) == 0 || anyNA(terms)) {
    manynet::snet_abort(
      c("{.arg terms} must name the coefficients to test.",
        i = "The coefficients of the model are {.val {main}}."))
  }
  if ("(Intercept)" %in% terms) {
    manynet::snet_abort("The intercept cannot be tested by permutation.")
  }
  unique(unlist(lapply(terms, function(term) {
    if (term %in% main) return(term)
    hit <- main[startsWith(main, paste0(term, "_"))]
    if (length(hit) == 0) {
      manynet::snet_abort(
        c("{.val {term}} is not a coefficient of the model.",
          i = "The coefficients of the model are {.val {main}}."))
    }
    hit
  })))
}

# The cells of each network the model used. `location` indexes a matrix by
# column, so its number of rows is read from any dyad past the first column.
# A two-mode network keeps the cells where a row and a column have the same
# index; a one-mode network without loops does not.
#' @keywords internal
#' @noRd
.dyad_layout <- function(pred, loops, directed, panel) {
  net <- as.character(pred$nv)
  ids <- unique(net)
  layout <- lapply(ids, function(k) {
    sel <- which(net == k)
    sv <- pred$sv[sel]
    rv <- pred$rv[sel]
    loc <- pred$location[sel]
    later <- rv > 1
    nr <- if (any(later)) max((loc[later] - sv[later]) %/% (rv[later] - 1)) else max(sv)
    onemode <- isTRUE(loops) || !any(sv == rv)
    nc <- if (onemode) max(nr, rv) else max(rv)
    if (onemode) nr <- nc
    list(sel = sel, sv = sv, rv = rv, nr = nr, nc = nc,
         mirror = onemode && !isTRUE(directed))
  })
  if (panel) {
    nr <- max(vapply(layout, `[[`, numeric(1), "nr"))
    nc <- max(vapply(layout, `[[`, numeric(1), "nc"))
    layout <- lapply(layout, function(l) {
      l$nr <- nr
      l$nc <- nc
      l
    })
  }
  offset <- cumsum(c(0, vapply(layout, function(l) l$nr * l$nc, numeric(1))))
  for (k in seq_along(layout)) layout[[k]]$offset <- offset[k]
  layout
}

# Each tested residual in the cells of its network's matrix, and the cells
# outside the model as NA. An undirected network keeps one half of its cells,
# and holds the same value in the other.
#' @keywords internal
#' @noRd
.residual_cells <- function(layout, resid) {
  total <- sum(vapply(layout, function(l) l$nr * l$nc, numeric(1)))
  cells <- matrix(NA_real_, total, ncol(resid))
  used <- lapply(layout, function(l) {
    m <- matrix(NA_real_, l$nr, l$nc)
    at <- (l$rv - 1) * l$nr + l$sv
    m[at] <- 0
    cells[l$offset + at, ] <<- resid[l$sel, , drop = FALSE]
    if (l$mirror) {
      back <- (l$sv - 1) * l$nr + l$rv
      m[back] <- 0
      cells[l$offset + back, ] <<- resid[l$sel, , drop = FALSE]
    }
    m
  })
  list(cells = cells, used = used)
}

# The deviance of the outcome on a design. Only base and stats functions, so
# that the workers of a parallel plan need nothing from the package.
#' @keywords internal
#' @noRd
.deviance_fun <- function(y, family) {
  fam <- switch(family,
                binomial = stats::binomial(),
                poisson  = stats::poisson(),
                NULL)
  function(X) {
    if (ncol(X) == 0) return(sum(y^2))
    if (is.null(fam)) return(sum(stats::lm.fit(X, y)$residuals^2))
    fit <- tryCatch(suppressWarnings(stats::glm.fit(X, y, family = fam)),
                    error = function(e) NULL)
    if (is.null(fit) || !isTRUE(fit$converged)) NA_real_ else fit$deviance
  }
}

# One permutation: read each tested residual from the cell the permuted nodes
# point to, and refit. Only base functions, as above.
#' @keywords internal
#' @noRd
.joint_draw <- function(layout, cells, untested, reduced, deviance_of) {
  n <- nrow(untested)
  function(o) {
    at <- integer(n)
    for (k in seq_along(layout)) {
      l <- layout[[k]]
      at[l$sel] <- l$offset + (o[[k]]$cols[l$rv] - 1L) * l$nr + o[[k]]$rows[l$sv]
    }
    moved <- cells[at, , drop = FALSE]
    if (anyNA(moved)) {
      stop("A permutation moved a tested value outside the model. ",
           "This is a bug in infernet. Please report it.")
    }
    reduced - deviance_of(cbind(untested, moved))
  }
}

# The joint test of `tested` against `value`, given the `untested` predictors.
#' @keywords internal
#' @noRd
.joint_test <- function(x, pred, tested, untested, value, times, control, test) {
  ctrl <- .resolve_control(control)
  panel <- isTRUE(x$panel)
  y <- pred[[x$dep]]
  XT <- as.matrix(pred[tested])
  XU <- matrix(numeric(0), nrow(pred), 0)
  if (length(untested) > 0) XU <- as.matrix(pred[untested])
  gaussian <- identical(x$family, "gaussian")
  if (gaussian && any(value != 0)) y <- y - drop(XT %*% value)

  if (length(x$fixed_effects) > 0) {
    # By Frisch-Waugh-Lovell the model on the demeaned values is the model with
    # the fixed effects, and a permutation keeps the group means at zero.
    pooled <- length(unique(pred$nv)) > 1
    ids <- .fixed_ids(pred, .fixed_columns(x$fixed_effects, pooled, panel))
    y <- drop(.demean(y, ids))
    XT <- .demean(XT, ids)
    if (ncol(XU) > 0) XU <- .demean(XU, ids)
  } else {
    XU <- cbind(`(Intercept)` = 1, XU)
  }
  resid <- if (ncol(XU) > 0) qr.resid(qr(XU), XT) else XT
  resid <- as.matrix(resid)

  deviance_of <- .deviance_fun(y, x$family)
  reduced <- deviance_of(XU)
  obsd <- reduced - deviance_of(cbind(XU, resid))
  if (is.na(obsd)) {
    manynet::snet_abort("The model does not converge on the observed data.")
  }

  layout <- .dyad_layout(pred, x$loops, x$directed, panel)
  rc <- .residual_cells(layout, resid)
  groups <- ctrl$groups
  target <- if (panel || length(layout) > 1) rc$used else rc$used[[1]]
  blocks <- .permutation_blocks(target, groups, isTRUE(x$loops), times,
                                paste(tested, collapse = ", "), panel)
  perm_groups <- if (is.null(blocks)) groups else blocks
  per_net <- is.list(perm_groups) && is.null(names(perm_groups))

  # The orders are drawn here, before any worker starts, so that a seed gives
  # the same permutations sequentially and in parallel.
  if (!is.null(ctrl$seed)) set.seed(ctrl$seed)
  orders <- lapply(seq_len(times), function(b) {
    if (panel) {
      o <- .perm_orders(c(layout[[1]]$nr, layout[[1]]$nc), perm_groups)
      return(rep(list(o), length(layout)))
    }
    lapply(seq_along(layout), function(k) {
      .perm_orders(c(layout[[k]]$nr, layout[[k]]$nc),
                   if (per_net) perm_groups[[k]] else perm_groups)
    })
  })

  old_plan <- setup_future_plan(ctrl$strategy, ctrl$ncores)
  on.exit(restore_future_plan(old_plan), add = TRUE)
  draw <- .joint_draw(layout, rc$cells, XU, reduced, deviance_of)
  dist <- future.apply::future_vapply(orders, draw, numeric(1),
                                      future.seed = TRUE)

  failed <- sum(is.na(dist))
  if (failed > 0) {
    manynet::snet_warn(
      "The model did not converge on {failed} of the {times} permutations, which are left out.")
  }
  out <- .distance_test(test, obsd, dist[!is.na(dist)], times = times)
  out$terms <- tested
  out$value <- value
  out$failed <- failed
  out$directed <- x$directed
  out$loops <- x$loops
  out$statistic <- "the change in deviance"
  out$model <- paste(.fit_label(x), "MRQAP of", x$dep)
  out$control <- ctrl
  out
}
