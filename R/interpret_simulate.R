#' Simulate networks from an MRQAP
#'
#' @description
#'   `simulate()` draws outcome networks from an MRQAP, conditional on its
#'   covariates. It has three methods, which give the same networks for the
#'   same coefficients, the same network, and the same seed:
#'
#'   - `simulate(fit)`, for an `infernetMrqapFit` from [estimate_mrqap()];
#'   - `simulate(coefs, .data = net)`, for a set of coefficients from
#'     [declare_coefs()];
#'   - `simulate(spec, coefs = , .data = net)`, for a specification from
#'     [specify_mrqap()] and its coefficients.
#'
#'   [test_gof()] draws its simulations of an `infernetMrqapFit` with the same
#'   simulator.
#'
#' @details
#'   Each simulation draws a tie in each dyad that the model used, from the
#'   model's predicted value there: a Bernoulli draw for `"binomial"`, a
#'   Poisson draw for `"poisson"`, and a normal draw with standard deviation
#'   `sigma` for `"gaussian"`. The other cells of the outcome, such as an
#'   unobserved dyad or a fixed tie, keep their value in `.data`, or are empty
#'   without it. An undirected network is mirrored.
#'
#'   A fit draws from its coefficients where its family is `"gaussian"`,
#'   `"binomial"`, or `"poisson"` and it has no random or fixed effects.
#'   Otherwise it draws with `stats::simulate()` of the observed model, which
#'   it keeps with `"model"` in `outputs`.
#'   A set of coefficients and a specification draw only from a model without
#'   random or fixed effects.
#'   A list of networks, a panel, and a cognitive social structure cannot yet
#'   be simulated.
#'
#'   `simulate()` is conditional on the covariates of the network.
#'   `{manynet}`'s `generate_*()` functions are not.
#'
#'   Each simulation has its own random stream, so a seed gives the same
#'   networks whether they are drawn sequentially or in parallel.
#'   The `ncores` and `strategy` set with [set_control_computation()] choose
#'   the plan.
#' @param object An `infernetMrqapFit` from [estimate_mrqap()], a set of
#'   coefficients from [declare_coefs()], or a specification from
#'   [specify_mrqap()].
#' @param nsim The number of networks to simulate.
#' @param seed A seed for the random draws.
#'   By default, the seed set with [set_control_computation()], if any.
#'   The random state of the session is put back after a draw with a seed.
#' @param .data The network that the model builds its columns on.
#'   For a fit, the network that it was fitted on. It is optional for a fit:
#'   without it, the result is a list of `stocnet` networks, in which the
#'   cells outside the model are empty.
#' @param coefs For a specification, the coefficients: a set from
#'   [declare_coefs()], or a named vector such as `stats::coef(fit)`.
#' @param family,sigma For a specification with a plain vector of
#'   coefficients, the family and, for a `"gaussian"` model, the standard
#'   deviation of the residuals, as in [declare_coefs()].
#'   `family = NULL`, the default, takes the family of a set from
#'   [declare_coefs()], or else reads it from the outcome in `.data`, as
#'   [estimate_mrqap()] does.
#' @param ... Not used.
#' @return A list of `nsim` networks, of the class of `.data`, or of class
#'   `stocnet` without it.
#' @seealso [declare_coefs()] for a set of coefficients, and [test_gof()],
#'   which compares the observed network with these simulations.
#' @name simulate_mrqap
#' @family interpreting functions
#' @examples
#' fit <- estimate_mrqap(friends ~ social + tasks, ison_algebra, times = 5,
#'                       control = list(family = "poisson"))
#' nets <- simulate(fit, nsim = 2, seed = 1, .data = ison_algebra)
#' nets[[1]]
#'
#' # The same networks, from the coefficients of the fit
#' coefs <- declare_coefs(fit)
#' same <- simulate(coefs, nsim = 2, seed = 1, .data = ison_algebra)
#'
#' # Or from a specification and coefficients that are stated
#' spec <- specify_mrqap(friends ~ social + tasks)
#' sims <- simulate(spec, nsim = 2, seed = 1, .data = ison_algebra,
#'                  coefs = c("(Intercept)" = -1, social = 0.5, tasks = 0),
#'                  family = "poisson")
#'
#' # The family read from a 0/1 outcome
#' adolescents <- manynet::mutate_nodes(ison_adolescents, age = 11:18)
#' binary <- simulate(specify_mrqap(. ~ ego(age)), seed = 1, .data = adolescents,
#'                    coefs = c("(Intercept)" = -2, ego_age = 0.1))
NULL

#' @rdname simulate_mrqap
#' @importFrom stats simulate
#' @export
simulate.infernetMrqapFit <- function(object, nsim = 1,
                                      seed = getOption("snet_computation")$seed,
                                      .data = NULL, ...) {
  sim <- .mrqap_simulator(object, .data)
  .simulate_networks(sim, nsim, seed, .data)
}

#' @rdname simulate_mrqap
#' @export
simulate.infernetMrqapCoefs <- function(object, nsim = 1,
                                        seed = getOption("snet_computation")$seed,
                                        .data, ...) {
  if (missing(.data) || is.null(.data)) {
    manynet::snet_abort(
      c("{.fn simulate} of a set of coefficients needs {.arg .data}.",
        i = "The columns of the model are built from the network."))
  }
  sim <- .coefs_simulator(object, .data)
  .simulate_networks(sim, nsim, seed, .data)
}

#' @rdname simulate_mrqap
#' @export
simulate.mrqap_formula <- function(object, nsim = 1,
                                   seed = getOption("snet_computation")$seed,
                                   .data, coefs, family = NULL, sigma = NULL,
                                   ...) {
  if (missing(coefs)) {
    manynet::snet_abort(
      c("{.fn simulate} of a specification needs its {.arg coefs}.",
        i = "Give them by name, since the second argument is {.arg nsim}: {.code simulate(spec, coefs = coef(fit), .data = net)}."))
  }
  if (missing(.data) || is.null(.data)) {
    manynet::snet_abort(
      c("{.fn simulate} of a specification needs {.arg .data}.",
        i = "The columns of the model are built from the network."))
  }
  if (inherits(coefs, "infernetMrqapCoefs")) {
    if (is.null(family)) family <- attr(coefs, "family")
    if (is.null(sigma)) sigma <- attr(coefs, "sigma")
    coefs <- stats::coef(coefs)
  }
  family <- .check_coef_family(family)
  if (is.null(family)) {
    family <- .resolve_coef_family(family, .mrqap_design(object, .data))
  }
  coefs <- declare_coefs(object, coefs, family = family, sigma = sigma)
  simulate(coefs, nsim = nsim, seed = seed, .data = .data)
}

# ---- the simulator ---------------------------------------------------------------

# Every route draws through this: `sim$draw()` gives the values of the model's
# dyads, and `sim$network()` places them in the outcome and makes a network of
# it. `test_gof()` reads `sim$matrix`, the observed outcome, too.
# Each simulation gets its own random stream, so the result does not depend on
# the plan.
#' @keywords internal
#' @noRd
.simulate_networks <- function(sim, nsim, seed, .data = NULL) {
  if (!(is.numeric(nsim) && length(nsim) == 1 && !is.na(nsim) &&
        nsim >= 1 && nsim == round(nsim))) {
    manynet::snet_abort("{.arg nsim} must be one whole number of at least 1.")
  }
  if (!is.null(seed)) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
      on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
    } else {
      on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
    }
    set.seed(seed)
  }
  ctrl <- .resolve_control(list())
  old_plan <- setup_future_plan(ctrl$strategy, ctrl$ncores)
  on.exit(restore_future_plan(old_plan), add = TRUE)
  draw <- sim$draw
  network <- sim$network
  out <- future.apply::future_lapply(seq_len(nsim), function(i) network(draw()),
                                     future.seed = TRUE)
  lapply(out, .as_class_of, .data = .data)
}

# A network of the class of `.data`, or a stocnet without it.
#' @keywords internal
#' @noRd
.as_class_of <- function(g, .data = NULL) {
  if (is.null(.data)) return(manynet::as_stocnet(g))
  if (inherits(.data, "tbl_graph")) return(g)
  if (inherits(.data, "stocnet")) return(manynet::as_stocnet(g))
  if (inherits(.data, "igraph")) return(manynet::as_igraph(g))
  if (inherits(.data, "network")) return(manynet::as_network(g))
  if (is.matrix(.data)) return(manynet::as_matrix(g))
  if (is.data.frame(.data)) return(manynet::as_edgelist(g))
  manynet::as_stocnet(g)
}

# What a fit draws from. A fit of a gaussian, binomial, or poisson model
# without random or fixed effects draws from its coefficients, as a set of
# coefficients does, so that the two give the same networks. Any other draws
# with `stats::simulate()` of the observed model.
#' @keywords internal
#' @noRd
.mrqap_simulator <- function(object, observed = NULL) {
  .check_simulable(object$css, object$panel, length(unique(object$pred$nv)) > 1)
  cells <- .fit_cells(object, observed)
  if (.simulates_from_coefs(object)) {
    X <- .fit_design(object)
    values <- .coef_draw(X, object$coefficients, object$family,
                         .fit_sigma(object, X))
  } else {
    values <- .model_draw(object)
  }
  list(matrix = cells$matrix,
       draw = .cell_draw(cells, object$directed, values),
       network = .cell_network(cells, object$directed))
}

# What a set of coefficients draws from: the columns that its specification
# builds on `.data`.
#' @keywords internal
#' @noRd
.coefs_simulator <- function(coefs, .data) {
  spec <- attr(coefs, "spec")
  family <- attr(coefs, "family")
  if (!family %in% .coef_draw_families()) {
    choices <- .coef_draw_families()
    manynet::snet_abort(
      c("{.fn simulate} cannot yet draw from the coefficients of a {.val {family}} model.",
        i = "It draws from a {.val {choices}} model."))
  }
  design <- .mrqap_design(spec, .data)
  X <- design$X
  values <- stats::coef(coefs)
  missing_cols <- setdiff(colnames(X), names(values))
  unknown <- setdiff(names(values), colnames(X))
  if (length(missing_cols) > 0 || length(unknown) > 0) {
    columns <- colnames(X)
    manynet::snet_abort(
      c("The coefficients must name each column that the specification builds on {.arg .data}.",
        x = if (length(missing_cols) > 0) "No value for {.val {missing_cols}}.",
        x = if (length(unknown) > 0) "No column named {.val {unknown}}.",
        i = "The columns are {.val {columns}}."))
  }
  if (anyNA(values[colnames(X)])) {
    unstated <- names(values)[is.na(values)]
    manynet::snet_abort(
      c("{.fn simulate} needs a value for every coefficient.",
        x = "{.val {unstated}} {?is/are} not stated."))
  }
  sigma <- attr(coefs, "sigma")
  if (identical(family, "gaussian") && is.null(sigma)) {
    manynet::snet_abort(
      c("{.fn simulate} of a {.val gaussian} model needs {.arg sigma}.",
        i = "Give the standard deviation of the residuals to {.fn declare_coefs}."))
  }
  list(matrix = design$cells$matrix,
       draw = .cell_draw(design$cells, design$directed,
                         .coef_draw(X, values, family, sigma)),
       network = .cell_network(design$cells, design$directed))
}

#' @keywords internal
#' @noRd
.coef_draw_families <- function() c("gaussian", "binomial", "poisson")

#' @keywords internal
#' @noRd
.check_simulable <- function(css, panel, several) {
  if (isTRUE(css) || isTRUE(panel) || isTRUE(several)) {
    manynet::snet_abort(
      "{.pkg infernet} cannot yet simulate a list of networks, a panel, or a cognitive social structure.")
  }
  invisible(TRUE)
}

#' @keywords internal
#' @noRd
.simulates_from_coefs <- function(object) {
  object$family %in% .coef_draw_families() &&
    length(object$fixed_effects) == 0 && !any(object$random) &&
    !anyNA(object$coefficients)
}

# A draw from the coefficients. Each family draws as `stats::simulate()` of
# the same `lm()` or `glm()` does, with the same calls to the generator, so a
# fit gives the same draws by both routes.
#' @keywords internal
#' @noRd
.coef_draw <- function(X, coefs, family, sigma = NULL) {
  eta <- drop(X %*% coefs[colnames(X)])
  n <- length(eta)
  switch(family,
         gaussian = function() eta + stats::rnorm(n, sd = sigma),
         binomial = {
           mu <- stats::binomial()$linkinv(eta)
           function() stats::rbinom(n, size = 1, prob = mu)
         },
         poisson = {
           mu <- stats::poisson()$linkinv(eta)
           function() stats::rpois(n, mu)
         })
}

# A draw with `stats::simulate()` of the observed model, for a family or an
# effect that the coefficients do not describe alone.
#' @keywords internal
#' @noRd
.model_draw <- function(object) {
  fit <- object$simple_fit
  if (is.null(fit)) {
    manynet::snet_abort(
      c("The model has no {.code simple_fit} to simulate from.",
        i = "Fit it again with {.val model} in {.arg outputs}, as by default."))
  }
  if (identical(object$family, "gaussian") && length(object$fixed_effects) > 0) {
    manynet::snet_abort(
      "Cannot yet simulate a {.val gaussian} model with fixed effects, which was fitted to demeaned values.")
  }
  n <- nrow(object$pred)
  first <- tryCatch(stats::simulate(fit, nsim = 1)[[1]], error = function(e) {
    manynet::snet_abort(
      c("Cannot simulate from the fitted {.cls {class(fit)[1]}}.",
        x = conditionMessage(e)))
  })
  if (length(first) != n) {
    manynet::snet_abort(
      "The fitted model used {length(first)} of the {n} dyads, so its draws cannot be placed.")
  }
  function() as.numeric(as.matrix(stats::simulate(fit, nsim = 1)[[1]])[, 1])
}

# The columns of a fit, from the dyad-level data it keeps.
#' @keywords internal
#' @noRd
.fit_design <- function(object) {
  .design_matrix(object$model_formula, object$pred)
}

#' @keywords internal
#' @noRd
.design_matrix <- function(formula, pred) {
  tt <- stats::delete.response(stats::terms(formula))
  X <- stats::model.matrix(tt, pred)
  colnames(X) <- gsub("`", "", colnames(X), fixed = TRUE)
  attr(X, "assign") <- NULL
  X
}

# The standard deviation of the residuals of a gaussian fit, as `sigma()` of
# its `lm()` gives it.
#' @keywords internal
#' @noRd
.fit_sigma <- function(object, X = .fit_design(object)) {
  y <- object$pred[[object$dep]]
  resid <- y - drop(X %*% object$coefficients[colnames(X)])
  sqrt(sum(resid^2) / (length(y) - ncol(X)))
}

# The cells of a fit's outcome: the observed outcome where `observed` is given,
# or an empty one of the fit's dimensions.
#' @keywords internal
#' @noRd
.fit_cells <- function(object, observed = NULL) {
  pred <- object$pred
  if (!is.null(observed)) {
    net <- manynet::as_tidygraph(observed)
    layers <- .layer_names(net)
    lhs <- if (object$dep %in% layers) object$dep else "."
    outcome <- .resolve_outcome(lhs, net, layers)
    mat <- outcome$DV
    dep <- if (object$dep %in% names(pred)) object$dep else "yv"
    if (!is.matrix(mat) || max(pred$location) > length(mat) ||
        (dep %in% names(pred) &&
         !isTRUE(all.equal(as.numeric(mat[pred$location]), as.numeric(pred[[dep]]))))) {
      manynet::snet_abort("{.arg observed} is not the network that the model was fitted on.")
    }
    return(list(matrix = mat, location = pred$location,
                twomode = manynet::is_twomode(outcome$net),
                template = outcome$net))
  }
  dims <- object$dims
  if (is.null(dims)) {
    manynet::snet_abort(
      c("This fit does not keep the dimensions of its network.",
        i = "Give the network that it was fitted on as {.arg .data}."))
  }
  mat <- matrix(0, dims[1], dims[2])
  labels <- object$node_names
  if (length(labels) == dims[1] + if (isTRUE(object$twomode)) dims[2] else 0) {
    dimnames(mat) <- if (isTRUE(object$twomode))
      list(labels[seq_len(dims[1])], labels[dims[1] + seq_len(dims[2])])
    else list(labels, labels)
  }
  list(matrix = mat, location = pred$location,
       twomode = isTRUE(object$twomode), template = NULL)
}

# The columns that a specification builds on a network, and the cells of its
# outcome, as `estimate_mrqap()` builds them.
#' @keywords internal
#' @noRd
.mrqap_design <- function(spec, .data) {
  if (.is_list_of_graphs(.data) || .is_panel(.data)) .check_simulable(FALSE, TRUE, TRUE)
  parsed_spec <- .specify(spec)
  if (length(parsed_spec$random) > 0 || length(parsed_spec$fixed) > 0) {
    manynet::snet_abort(
      "Cannot yet build the columns of a model with random or fixed effects for a set of coefficients.")
  }
  net <- manynet::as_tidygraph(.data)
  ml <- convertToMatrixList(spec, .data, advise = FALSE)
  .check_simulable(ml$css, FALSE, FALSE)
  matlist <- ml$mydata
  mod <- ml$formula
  parsed <- parse_qap_formula(mod)
  dep <- parsed$dependent
  data_vars <- intersect(parsed$all_data_vars, names(matlist))
  directed <- manynet::is_directed(net)
  loops <- isTRUE(manynet::is_looped(net))
  vec <- .vectorise_matlist(.qap_shape(FALSE), matlist, dep, data_vars, NULL,
                            loops, directed, FALSE,
                            attr(matlist[[dep]], "fixed"))
  pred <- vec$pred
  names(pred)[names(pred) == "yv"] <- dep
  layers <- .layer_names(net)
  lhs <- if (dep %in% layers) dep else "."
  outcome <- .resolve_outcome(lhs, net, layers)
  list(X = .design_matrix(mod, pred), directed = directed,
       cells = list(matrix = matlist[[dep]], location = pred$location,
                    twomode = manynet::is_twomode(outcome$net),
                    template = outcome$net))
}

# Places each draw of the values in the cells of the model. An undirected
# network is mirrored.
#' @keywords internal
#' @noRd
.cell_draw <- function(cells, directed, values) {
  mat <- cells$matrix
  location <- cells$location
  square <- nrow(mat) == ncol(mat)
  function() {
    m <- mat
    m[location] <- values()
    if (!directed && square) m[upper.tri(m)] <- t(m)[upper.tri(m)]
    m
  }
}

# Makes a network of an outcome matrix. A symmetric matrix reads as
# undirected, and `to_directed()` would keep one arc of each pair, so a
# directed network is reciprocated.
#' @keywords internal
#' @noRd
.cell_network <- function(cells, directed) {
  twomode <- cells$twomode
  template <- cells$template
  function(m) {
    m[is.na(m)] <- 0
    g <- manynet::as_tidygraph(m, twomode = twomode)
    if (!twomode) {
      if (directed && !manynet::is_directed(g)) g <- manynet::to_reciprocated(g)
      if (!directed && manynet::is_directed(g)) g <- manynet::to_undirected(g)
    }
    if (!is.null(template)) g <- manynet::bind_node_attributes(g, template)
    g
  }
}
