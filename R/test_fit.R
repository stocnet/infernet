# Tests of model fit ####

#' Tests of model fit
#' @name test_fit
#' @description
#'   These functions test whether a model makes data like the observed data,
#'   or whether two models make the same data:
#'
#'   - `test_gof()` compares an observed statistic with the same statistic on
#'   data simulated from a model.
#'   It is a generic, with a method for each kind of model:
#'     - a `diffs_model`, from `play_diffusions()`, compares an observed
#'     diffusion with the simulated diffusions;
#'     - a `netreg`, from `estimate_mrqap()`, simulates outcome networks from
#'     the fitted model and compares a network statistic, `FUN`, on each
#'     with the statistic on the observed network.
#'   - `test_distributions()` compares the data simulated from two models.
#'   It is also a generic, with a method for two `diffs_model` objects.
#'
#'   Both give a p-value from simulations, not from a distribution that the
#'   statistic is assumed to follow.
#' @details
#'   `test_gof()` takes the vector of statistics of each simulation, such as
#'   the number of infected nodes in each step of a diffusion, and splits
#'   the simulations into two halves. The first half gives the mean and the
#'   covariance of the statistics. The second half is the reference.
#'   The test statistic is the Mahalanobis distance from that mean, and the
#'   p-value is the share of the reference that is at least as far from the
#'   mean as the observed data:
#'   \deqn{p = \frac{1 + \#\{d_{ref} \geq d_{obs}\}}{1 + k}}
#'   where \eqn{k} is the number of reference simulations.
#'   If the model made the observed data, the observed data and the reference
#'   are exchangeable, so this p-value is exact for any number of simulations.
#'   A reference simulation at the same distance as the observed data counts
#'   against it, so the test is conservative where distances tie.
#'   The reference does not use the simulations that estimate the mean and the
#'   covariance, so it needs no assumption that the statistics are normal.
#'
#'   A small ridge, a thousandth of the mean variance, is added to the
#'   diagonal of the covariance. Statistics such as an adoption curve are
#'   bounded and often constant in some steps, so their covariance is singular.
#'   With the ridge, a difference in a step where the simulations do not vary
#'   makes the distance large, and is not ignored.
#'
#'   `test_distributions()` measures the Mahalanobis distance between the mean
#'   adoption curves of the two models, with their pooled covariance and the
#'   same ridge. It then permutes which model each simulation came from,
#'   `times` times. As the simulations of each model are independent, this
#'   permutation test is exact.
#'
#'   A large distance is evidence against the model, so the p-value is
#'   `Pr(X>=Obs)` in `print()`, and `p.upper` in `tidy()`.
#' @section Diffusions:
#'   The statistic of a diffusion is its curve of infected nodes, `I`,
#'   in each step after the seeds. A diffusion that ends early keeps its last
#'   value until the longest diffusion ends.
#'   The diffusions can come from `play_diffusions()`. Keep `times` large
#'   enough that the reference holds at least 19 simulations,
#'   or no p-value can be below 0.05.
#' @section Network regressions:
#'   The `netreg` method draws each simulated outcome from the fitted model
#'   with `stats::simulate()`, in the dyads that the model used.
#'   The other cells keep their observed values: the diagonal, the fixed ties,
#'   and the cells that the model left out. An unobserved cell is an absent tie
#'   in both the observed and the simulated networks, so they do not differ
#'   there. An undirected network is mirrored, and the simulated network takes
#'   the node attributes of the observed one.
#'
#'   The model needs its `simple_fit`, so a model fitted with
#'   `less_mem = TRUE` cannot be tested. A list of networks, a panel, and a
#'   cognitive social structure cannot be tested yet.
#'   Nor can a `gaussian` model: its draws are continuous, so every dyad of a
#'   draw would hold a tie, and the test would always reject.
#'   Fit a family whose outcomes are ties or counts.
#'
#'   An MRQAP models each dyad apart from the others. A statistic of the
#'   dependence between dyads, such as transitivity or reciprocity, shows
#'   what that misses.
#' @inheritParams test_measures
#' @param object A model to simulate from:
#'   a `diffs_model`, from `play_diffusions()`, or a `netreg`, from
#'   `estimate_mrqap()`.
#'   For `test_distributions()`, `object1` and `object2` are two `diffs_model`
#'   objects.
#' @param observed The observed data.
#'   For a `diffs_model`, a `diff_model` from `manynet::as_diffusion()`,
#'   or a network with a diffusion record from `manynet::play_diffusion()`.
#'   For a `netreg`, the network that the model was fitted on.
#' @param FUN For a `netreg`, a function of a network that returns a
#'   number or a vector of numbers, such as `netrics::net_by_transitivity()`.
#' @param ... For a `netreg`, additional arguments to be passed on to `FUN`.
#'   Other methods do not use them.
#' @param times For a `netreg`, the number of outcomes to simulate.
#'   For `test_distributions()`, the number of permutations.
#'   A `diffs_model` brings its own simulations.
#' @param object1,object2 Two `diffs_model` objects, from `play_diffusions()`.
#' @return A `network_test`, which `print()`, `tidy()`, and `glance()` report.
#'   It holds the observed distance in `testval` and the reference distances
#'   in `testdist`.
#' @section Other packages:
#'   `{RSiena}` and `{goldfish}` export `test_gof()` generics with the same
#'   signature, for their own fitted models.
#'   If more than one of these packages is attached, the one attached last
#'   masks the others.
#'   Call `infernet::test_gof()`, `RSiena::test_gof()`, or
#'   `goldfish::test_gof()` to choose.
#' @references
#'   Hunter, David R., Steven M. Goodreau, and Mark S. Handcock. 2008.
#'   "Goodness of fit of social network models".
#'   _Journal of the American Statistical Association_ 103(481): 248-258.
#'   \doi{10.1198/016214507000000446}
#'
#'   Lospinoso, Josh, and Tom A.B. Snijders. 2019.
#'   "Goodness of fit for stochastic actor-oriented models".
#'   _Methodological Innovations_ 12(3).
#'   \doi{10.1177/2059799119884282}
#' @family models
#' @examples
#' fast <- play_diffusions(ison_adolescents, transmissibility = 0.5,
#'                         times = 40, control = list(seed = 1))
#' slow <- play_diffusions(ison_adolescents, transmissibility = 0.2,
#'                         times = 40, control = list(seed = 1))
#' test_distributions(fast, slow, times = 200)
#' obs <- manynet::as_diffusion(manynet::play_diffusion(ison_adolescents,
#'                                                      transmissibility = 0.2))
#' test_gof(slow, obs)
#'
#' # Does an MRQAP make as many reciprocated ties as the observed network?
#' fit <- estimate_mrqap(friends ~ social + tasks, ison_algebra, times = 20,
#'                       control = list(family = "poisson"))
#' test_gof(fit, ison_algebra, net_by_reciprocity, times = 100,
#'          control = list(seed = 1))
#' @export
test_gof <- function(object, ...) UseMethod("test_gof")

#' @rdname test_fit
#' @export
test_gof.default <- function(object, observed, ...) {
  manynet::snet_abort(
    c("{.fn test_gof} has no method for a {.cls {class(object)[1]}}.",
      i = "Give it a {.cls diffs_model} from {.fn play_diffusions} or a {.cls netreg} from {.fn estimate_mrqap}."))
}

# `{migraph}`'s `test_fit()` took the observed diffusion first. The model now
# comes first, so that `test_gof()` can dispatch on it.
#' @rdname test_fit
#' @export
test_gof.diff_model <- function(object, observed, ...) {
  if (inherits(observed, "diffs_model")) {
    manynet::snet_warn(
      c("{.fn test_gof} takes the model first, and the observed diffusion second.",
        i = "{.pkg migraph}'s {.fn test_fit} took them the other way.",
        i = "Use {.code test_gof(diffusions, observed)} instead."))
    return(test_gof.diffs_model(observed, object, ...))
  }
  manynet::snet_abort(
    c("A single diffusion is not a model to test against.",
      i = "Simulate many diffusions with {.fn play_diffusions}, and give them first."))
}

#' @rdname test_fit
#' @export
test_gof.diffs_model <- function(object, observed, ...) {
  if (!inherits(observed, "diff_model")) {
    if (inherits(observed, "diffs_model")) {
      manynet::snet_abort(
        c("{.arg observed} holds many diffusions.",
          i = "To compare two models, use {.fn test_distributions}."))
    }
    observed <- tryCatch(manynet::as_diffusion(observed), error = function(e) {
      manynet::snet_abort(
        c("{.arg observed} must be a {.cls diff_model}, or a network with a diffusion record.",
          i = "Make one with {.fn manynet::play_diffusion} or {.fn manynet::as_diffusion}."))
    })
  }
  curves <- .align_curves(.adoption_curve(observed), .adoption_curves(object))
  out <- .fit_test(curves$observed, curves$simulated, test = "Goodness-of-fit")
  out$statistic <- "the adoption curve"
  out$model <- "the simulated diffusions"
  out
}

#' @rdname test_fit
#' @export
test_gof.netreg <- function(object, observed, FUN, ...,
                            times = 1000, control = list()) {
  statistic <- .fun_name(substitute(FUN))
  test <- .test_control(control, list(...))
  ctrl <- test$ctrl
  args <- test$args
  measure <- function(g) do.call(FUN, c(list(g), args))
  model <- .netreg_simulator(object, observed)

  obsd <- .as_statistic(measure(model$network(model$matrix)))
  if (!is.null(ctrl$seed)) set.seed(ctrl$seed)
  old_plan <- setup_future_plan(ctrl$strategy, ctrl$ncores)
  on.exit(restore_future_plan(old_plan), add = TRUE)
  # The workers of a parallel plan get the helpers by value, so that they
  # need no internal function of the package.
  as_statistic <- .as_statistic
  simd <- future.apply::future_lapply(seq_len(times), function(i) {
    as_statistic(measure(model$network(model$draw())))
  }, future.seed = TRUE)
  if (any(lengths(simd) != length(obsd))) {
    manynet::snet_abort(
      "{.arg FUN} must return the same number of values on every network.")
  }
  out <- .fit_test(obsd, do.call(rbind, simd), test = "Goodness-of-fit")
  out$directed <- object$directed
  out$loops <- object$loops
  out$statistic <- statistic
  out$model <- paste(.fit_label(object), "MRQAP")
  out$control <- ctrl
  out
}

#' @rdname test_fit
#' @export
test_distributions <- function(object1, object2, ...) {
  UseMethod("test_distributions")
}

#' @rdname test_fit
#' @export
test_distributions.default <- function(object1, object2, ...) {
  manynet::snet_abort(
    c("{.fn test_distributions} has no method for a {.cls {class(object1)[1]}}.",
      i = "Give it two {.cls diffs_model} objects, from {.fn play_diffusions}.",
      i = "To compare one observed diffusion with a model, use {.fn test_gof}."))
}

#' @rdname test_fit
#' @export
test_distributions.diffs_model <- function(object1, object2,
                                           times = 1000, control = list(), ...) {
  if (!inherits(object2, "diffs_model")) {
    manynet::snet_abort(
      c("{.fn test_distributions} compares two {.cls diffs_model} objects.",
        i = "Simulate many diffusions from each model with {.fn play_diffusions}.",
        i = "To compare one observed diffusion with a model, use {.fn test_gof}."))
  }
  ctrl <- .resolve_control(control)
  curves <- .align_curves(.adoption_curves(object1), .adoption_curves(object2))
  # One diffusion gives a curve rather than a matrix of them, so each is made
  # a matrix with a row for each diffusion.
  first <- rbind(curves$observed)
  second <- rbind(curves$simulated)
  n1 <- nrow(first)
  if (n1 < 2 || nrow(second) < 2) {
    manynet::snet_abort("Each model needs at least 2 diffusions.")
  }
  pooled <- rbind(first, second)
  # The workers of a parallel plan get the helpers by value, so that they
  # need no internal function of the package.
  distance <- .two_sample_distance
  ridge <- .ridge
  obsd <- distance(first, second, ridge)

  if (!is.null(ctrl$seed)) set.seed(ctrl$seed)
  old_plan <- setup_future_plan(ctrl$strategy, ctrl$ncores)
  on.exit(restore_future_plan(old_plan), add = TRUE)
  simd <- future.apply::future_vapply(seq_len(times), function(i) {
    idx <- sample.int(nrow(pooled), n1)
    distance(pooled[idx, , drop = FALSE], pooled[-idx, , drop = FALSE], ridge)
  }, numeric(1), future.seed = TRUE)
  out <- .distance_test("Two-sample", obsd, simd, times = times)
  out$statistic <- "the adoption curves"
  out$model <- "the simulated diffusions"
  out$control <- ctrl
  out
}

# The p-value of an observed vector of statistics against simulated ones.
# The first half of the simulations estimates the mean and the covariance,
# and the second half is the reference, so that the observed data and the
# reference are exchangeable under the model.
#' @keywords internal
#' @noRd
.fit_test <- function(observed, simulated, test) {
  simulated <- as.matrix(simulated)
  n <- nrow(simulated)
  half <- n %/% 2
  if (half < 2) {
    manynet::snet_abort(
      c("{.fn test_gof} needs at least 4 simulations, and has {n}.",
        i = "Half of them estimate the mean and the covariance, and half are the reference."))
  }
  if (n - half < 19) {
    manynet::snet_warn(
      c("The reference holds {n - half} simulation{?s}, so no p-value can be below 0.05.",
        i = "Simulate at least 38 times."))
  }
  est <- simulated[seq_len(half), , drop = FALSE]
  ref <- simulated[-seq_len(half), , drop = FALSE]
  center <- colMeans(est)
  covar <- .ridge(stats::cov(est))
  obsd <- stats::mahalanobis(observed, center, covar)
  refd <- stats::mahalanobis(ref, center, covar)
  .distance_test(test, obsd, refd, times = n)
}

# Hotelling's statistic for two samples of curves, with a ridge.
#' @keywords internal
#' @noRd
.two_sample_distance <- function(first, second, ridge) {
  n1 <- nrow(first)
  n2 <- nrow(second)
  pooled <- ((n1 - 1) * stats::cov(first) + (n2 - 1) * stats::cov(second)) /
    (n1 + n2 - 2)
  diff <- colMeans(first) - colMeans(second)
  n1 * n2 / (n1 + n2) *
    stats::mahalanobis(diff, rep(0, length(diff)), ridge(pooled))
}

# A thousandth of the mean variance on the diagonal, so that a constant
# statistic has a variance and a singular covariance can be inverted.
#' @keywords internal
#' @noRd
.ridge <- function(covar) {
  covar <- as.matrix(covar)
  scale <- mean(diag(covar))
  if (!is.finite(scale) || scale <= 0) scale <- 1
  covar + diag(1e-3 * scale, nrow(covar))
}

# The p-values count a tie against the observed data. Two distances that the
# same arithmetic gives for the same curve are equal, but a tolerance keeps a
# rounding difference from breaking a tie.
#' @keywords internal
#' @noRd
.distance_test <- function(test, obsd, refd, times) {
  tol <- 1e-8 * max(1, abs(obsd))
  k <- length(refd)
  out <- list(test = test,
              testval = obsd,
              testdist = refd,
              plteobs = (1 + sum(refd <= obsd + tol)) / (k + 1),
              pgteobs = (1 + sum(refd >= obsd - tol)) / (k + 1),
              times = times)
  class(out) <- "network_test"
  out
}

#' @keywords internal
#' @noRd
.as_statistic <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0 || anyNA(x)) {
    manynet::snet_abort(
      "{.arg FUN} must return at least one number, and no {.val NA}.")
  }
  x
}

# ---- diffusions --------------------------------------------------------------

# `{manynet}` names the time column `t` or `time`, depending on the network.
#' @keywords internal
#' @noRd
.diff_time <- function(x) {
  if ("time" %in% names(x)) x$time else x$t
}

# The infected nodes in each step after the seeds.
#' @keywords internal
#' @noRd
.adoption_curve <- function(x) {
  time <- .diff_time(x)
  infected <- as.numeric(x$I[order(time)])
  infected[sort(time) > 0]
}

#' @keywords internal
#' @noRd
.adoption_curves <- function(x) {
  lapply(split(as.data.frame(x), x$sim), .adoption_curve)
}

# Every curve as long as the longest, keeping its last value.
#' @keywords internal
#' @noRd
.align_curves <- function(observed, simulated) {
  if (!is.list(observed)) observed <- list(observed)
  steps <- max(lengths(c(observed, simulated)), 1)
  pad <- function(curve) {
    if (length(curve) == 0) curve <- 0
    c(curve, rep(curve[length(curve)], steps - length(curve)))
  }
  rows <- function(curves) do.call(rbind, lapply(curves, pad))
  first <- rows(observed)
  list(observed = if (nrow(first) == 1) first[1, ] else first,
       simulated = rows(simulated))
}

# ---- network regressions -----------------------------------------------------

# What the `netreg` method needs to draw an outcome network: the observed
# outcome matrix, a function that fills the model's dyads with a draw from
# the fitted model, and a function that makes a network of a matrix.
#' @keywords internal
#' @noRd
.netreg_simulator <- function(object, observed) {
  fit <- object$simple_fit
  if (is.null(fit)) {
    manynet::snet_abort(
      c("The model has no {.code simple_fit} to simulate from.",
        i = "Fit it again without {.code less_mem = TRUE}."))
  }
  # `as_tidygraph()` makes a tie of every nonzero cell, and a continuous draw
  # has no zero, so every draw would be a complete network.
  if (identical(object$family, "gaussian")) {
    manynet::snet_abort(
      c("{.fn test_gof} cannot simulate networks from a {.val gaussian} model.",
        i = "Its draws are continuous, so every dyad would hold a tie, and the test would always reject.",
        i = "Fit a family whose outcomes are ties or counts, such as {.val binomial} or {.val poisson}."))
  }
  if (inherits(object, "netreg_css") || isTRUE(object$panel) ||
      length(unique(object$pred$nv)) > 1) {
    manynet::snet_abort(
      "{.fn test_gof} cannot yet simulate a list of networks, a panel, or a cognitive social structure.")
  }
  net <- manynet::as_tidygraph(observed)
  layers <- .layer_names(net)
  lhs <- if (object$dep %in% layers) object$dep else "."
  outcome <- .resolve_outcome(lhs, net, layers)
  mat <- outcome$DV
  pred <- object$pred
  dep <- if (object$dep %in% names(pred)) object$dep else "yv"
  if (!is.matrix(mat) || max(pred$location) > length(mat) ||
      (dep %in% names(pred) &&
       !isTRUE(all.equal(as.numeric(mat[pred$location]), as.numeric(pred[[dep]]))))) {
    manynet::snet_abort("{.arg observed} is not the network that the model was fitted on.")
  }
  first <- tryCatch(stats::simulate(fit, nsim = 1)[[1]], error = function(e) {
    manynet::snet_abort(
      c("Cannot simulate from the fitted {.cls {class(fit)[1]}}.",
        x = conditionMessage(e)))
  })
  if (length(first) != nrow(pred)) {
    manynet::snet_abort(
      "The fitted model used {length(first)} of the {nrow(pred)} dyads, so its draws cannot be placed.")
  }

  directed <- object$directed
  square <- nrow(mat) == ncol(mat)
  twomode <- manynet::is_twomode(outcome$net)
  draw <- function() {
    y <- as.numeric(as.matrix(stats::simulate(fit, nsim = 1)[[1]])[, 1])
    out <- mat
    out[pred$location] <- y
    if (!directed && square) out[upper.tri(out)] <- t(out)[upper.tri(out)]
    out
  }
  template <- outcome$net
  network <- function(m) {
    m[is.na(m)] <- 0
    g <- manynet::as_tidygraph(m, twomode = twomode)
    if (!twomode) {
      # A symmetric matrix reads as undirected, and `to_directed()` would keep
      # one arc of each pair, so a directed network is reciprocated.
      if (directed && !manynet::is_directed(g)) g <- manynet::to_reciprocated(g)
      if (!directed && manynet::is_directed(g)) g <- manynet::to_undirected(g)
    }
    manynet::bind_node_attributes(g, template)
  }
  list(matrix = mat, draw = draw, network = network)
}
