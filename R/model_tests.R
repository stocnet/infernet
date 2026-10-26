# Tests of network measures ####

#' Tests of network measures
#' @name tests
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
#' @param strategy A `{future}` plan, so that multiple cores can be used
#'   to accelerate the function.
#'   By default `"sequential"`, 
#'   but if multiple cores available,
#'   then `"multisession"` or `"multicore"` may be useful.
#'   Generally this is useful only when `times` > 1000.
#'   See [`{future.apply}`](https://future.apply.futureverse.org) for more.
#' @family models
#' @param FUN A graph-level statistic function to test.
#' @param ... Additional arguments to be passed on to FUN,
#'   e.g. the name of the attribute.
NULL

#' @rdname tests 
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
                        strategy = "sequential"){
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(.data, args)
  } else {
    obsd <- FUN(.data)
  }
  simd <- .simulate_statistic(.data, FUN, args, times, strategy,
                              draw = manynet::generate_random, bind = TRUE)
  out <- list(test = "CUG",
              testval = obsd,
              testdist = simd,
              directed = manynet::is_directed(.data),
              diag = manynet::is_complex(.data),
              cmode = "edges",
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              times = times)
  class(out) <- "network_test"
  out
}

#' @rdname tests 
#' @importFrom manynet generate_configuration
#' @export
test_configuration <- function(.data, FUN, ..., 
                        times = 1000, 
                        strategy = "sequential"){
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(.data, args)
  } else {
    obsd <- FUN(.data)
  }
  simd <- .simulate_statistic(.data, FUN, args, times, strategy,
                              draw = manynet::generate_configuration, bind = TRUE)
  out <- list(test = "configuration",
              testval = obsd,
              testdist = simd,
              directed = manynet::is_directed(.data),
              diag = manynet::is_complex(.data),
              cmode = "edges",
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              times = times)
  class(out) <- "network_test"
  out
}

#' @rdname tests 
#' @examples 
#' # (qaptest <- test_permutation(marvel_friends, 
#' #                 net_by_heterophily, attribute = "Attractive",
#' #                 times = 200))
#' # plot(qaptest)
#' @export
test_permutation <- function(.data, FUN, ..., 
                             times = 1000, 
                             strategy = "sequential"){
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(.data, args)
  } else {
    obsd <- FUN(.data)
  }
  simd <- .simulate_statistic(.data, FUN, args, times, strategy,
                              draw = manynet::to_permuted, bind = FALSE)
  out <- list(test = "QAP",
              testval = obsd,
              testdist = simd,
              directed = manynet::is_directed(.data),
              diag = manynet::is_complex(.data),
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              times = times)
  class(out) <- "network_test"
  out
}

# The statistic on each of `times` simulated networks. Each draw gets its own
# stream of random numbers, so a seed set before the call reproduces the result
# sequentially and in parallel alike. A simulated network takes the observed
# node attributes where the statistic needs one.
#' @keywords internal
#' @noRd
.simulate_statistic <- function(.data, FUN, args, times, strategy, draw, bind) {
  oplan <- future::plan(strategy)
  on.exit(future::plan(oplan), add = TRUE)
  measure <- if (!is.null(args)) function(g) FUN(g, args) else function(g) FUN(g)
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

#' @rdname results
#' @export
print.network_test <- function(x, ...,
                             max.length = 6,
                             digits = 3){
  cat(paste("\n", x$test, "Test Results\n\n"))
  cat("Observed Value:", x$testval, "\n")
  cat("Pr(X>=Obs):", x$pgteobs, "\n")
  cat("Pr(X<=Obs):", x$plteobs, "\n\n")
  invisible(x)
}

