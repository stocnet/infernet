# Tests of network measures ####

#' Tests of network measures
#' @name test_measures
#' @aliases tests
#' @description
#'   These functions conduct tests of any network-level statistic:
#'   
#'   - `test_random()` performs a conditional uniform graph (CUG) test
#'   of a measure against a distribution of measures on random networks 
#'   of the same dimensions.
#'   - `test_configuration()` performs a CUG test against a distribution of
#'   measures on random networks that preserve the degree sequence
#'   of the original network.
#'   - `test_permutation()` performs a quadratic assignment procedure (QAP) test 
#'   of a measure against a distribution of measures on permutations 
#'   of the original network.
#'   
#' @inheritParams regression
#' @param control The controls of the simulations, as the result of
#'   [control_mrqap()] or a named list of its arguments, such as
#'   `list(ncores = 4, seed = 1)`.
#'   A test reads `ncores`, `strategy`, and `seed`, and leaves the other
#'   controls alone, so that the controls of a model can be given to a test
#'   whole.
#'   `ncores` is generally useful only when `times` > 1000.
#'   See [control_mrqap()] for each control and its default.
#' @family models
#' @param FUN A graph-level statistic function to test.
#' @param ... Additional arguments to be passed on to FUN,
#'   e.g. the name of the attribute.
NULL

#' @rdname test_measures
#' @importFrom manynet generate_random bind_node_attributes is_directed is_complex
#' @examples 
#' marvel_friends <- fict_marvel |> to_uniplex("relationship") |> 
#'   to_unsigned() |> to_giant() |> 
#'   to_subgraph(PowerOrigin == "Human")
#' (cugtest <- test_random(marvel_friends, net_by_heterophily, attribute = "Attractive",
#'    times = 200))
#' # plot(cugtest)
#' @export
test_random <- function(.data, FUN, ..., 
                        times = 1000, 
                        control = list()){
  statistic <- .fun_name(substitute(FUN))
  test <- .test_control(control, list(...))
  ctrl <- test$ctrl
  args <- test$args
  obsd <- do.call(FUN, c(list(.data), args))
  simd <- .simulate_statistic(.data, FUN, args, times, ctrl,
                              draw = manynet::generate_random, bind = TRUE)
  out <- list(test = "CUG",
              testval = obsd,
              testdist = simd,
              directed = manynet::is_directed(.data),
              loops = manynet::is_complex(.data),
              cmode = "edges",
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              times = times,
              statistic = statistic,
              control = ctrl)
  class(out) <- "network_test"
  out
}

#' @rdname test_measures
#' @importFrom manynet generate_configuration
#' @export
test_configuration <- function(.data, FUN, ..., 
                        times = 1000, 
                        control = list()){
  statistic <- .fun_name(substitute(FUN))
  test <- .test_control(control, list(...))
  ctrl <- test$ctrl
  args <- test$args
  obsd <- do.call(FUN, c(list(.data), args))
  simd <- .simulate_statistic(.data, FUN, args, times, ctrl,
                              draw = manynet::generate_configuration, bind = TRUE)
  out <- list(test = "configuration",
              testval = obsd,
              testdist = simd,
              directed = manynet::is_directed(.data),
              loops = manynet::is_complex(.data),
              cmode = "edges",
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              times = times,
              statistic = statistic,
              control = ctrl)
  class(out) <- "network_test"
  out
}

#' @rdname test_measures
#' @examples 
#' # (qaptest <- test_permutation(marvel_friends, 
#' #                 net_by_heterophily, attribute = "Attractive",
#' #                 times = 200))
#' # plot(qaptest)
#' @export
test_permutation <- function(.data, FUN, ..., 
                             times = 1000, 
                             control = list()){
  statistic <- .fun_name(substitute(FUN))
  test <- .test_control(control, list(...))
  ctrl <- test$ctrl
  args <- test$args
  obsd <- do.call(FUN, c(list(.data), args))
  simd <- .simulate_statistic(.data, FUN, args, times, ctrl,
                              draw = manynet::to_permuted, bind = FALSE)
  out <- list(test = "QAP",
              testval = obsd,
              testdist = simd,
              directed = manynet::is_directed(.data),
              loops = manynet::is_complex(.data),
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              times = times,
              statistic = statistic,
              control = ctrl)
  class(out) <- "network_test"
  out
}

# A test reads its controls through `control_mrqap()`, so a list and the
# controls of a model are checked the same way. `strategy` and `ncores` were
# arguments of the tests before 0.5.0; given by name, they would otherwise reach
# `FUN` through `...`, so they are read as controls, with a warning.
#' @keywords internal
#' @noRd
.test_control <- function(control, dots) {
  is_old <- names(dots) %in% .test_formals()
  if (!any(is_old)) return(list(ctrl = .resolve_control(control), args = dots))
  old <- dots[is_old]
  manynet::snet_warn(
    c("{.arg {names(old)}} {?is/are} now {?a control/controls} of the tests.",
      i = "Use {.code control = list({names(old)[1]} = ...)} instead."))
  clash <- intersect(names(old), names(control))
  if (!inherits(control, "mrqap_control") && length(clash) > 0) {
    manynet::snet_abort(
      "{.val {clash}} is given both as an argument and in {.arg control}.")
  }
  control <- utils::modifyList(unclass(control), old)
  list(ctrl = .resolve_control(control), args = dots[!is_old])
}

#' @keywords internal
#' @noRd
.test_formals <- function() c("strategy", "ncores")

# The statistic on each of `times` simulated networks. Each draw gets its own
# stream of random numbers, so a `seed` control, or a seed set before the call,
# reproduces the result sequentially and in parallel alike. A simulated network
# takes the observed node attributes where the statistic needs one.
#' @keywords internal
#' @noRd
.simulate_statistic <- function(.data, FUN, args, times, ctrl, draw, bind) {
  if (!is.null(ctrl$seed)) set.seed(ctrl$seed)
  old_plan <- setup_future_plan(ctrl$strategy, ctrl$ncores)
  on.exit(restore_future_plan(old_plan), add = TRUE)
  # `do.call()` passes each argument by its name, so it need not be the second
  # formal of `FUN`, and the closure needs nothing from the package.
  measure <- function(g) do.call(FUN, c(list(g), args))
  .data <- .draw_from(.data, measure)
  rands <- future.apply::future_lapply(seq_len(times), function(i) draw(.data),
                                       future.seed = TRUE)
  if (bind && length(args) > 0) {
    rands <- lapply(rands, manynet::bind_node_attributes, object2 = .data)
  }
  future.apply::future_vapply(rands, measure, numeric(1), future.seed = TRUE)
}

# `{manynet}` converts a stocnet on every draw from it, which took four fifths
# of each draw of `to_permuted()` (stocnet/manynet#199). A tbl_graph made once
# is drawn from instead, where the statistic gives the same value on it;
# otherwise, such as for a statistic that reads what only a stocnet holds, the
# stocnet is kept.
#' @keywords internal
#' @noRd
.draw_from <- function(.data, measure) {
  if (!inherits(.data, "stocnet")) return(.data)
  tg <- tryCatch(manynet::as_tidygraph(.data), error = function(e) NULL)
  if (is.null(tg)) return(.data)
  same <- tryCatch(isTRUE(all.equal(measure(tg), measure(.data))),
                   error = function(e) FALSE)
  if (same) tg else .data
}

