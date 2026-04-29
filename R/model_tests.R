# Tests of network measures ####

#' Tests of network measures
#'
#' @description
#'   These functions conduct tests of any network-level statistic:
#'
#'   - `test_random()` performs a conditional uniform graph (CUG) test
#'   of a measure against a distribution of measures on random networks
#'   of the same dimensions.
#'   - `test_configuration()` performs a CUG test conditioning on the
#'   degree sequence of the observed network.
#'   - `test_permutation()` performs a quadratic assignment procedure (QAP)
#'   test of a measure against a distribution of measures on permutations
#'   of the original network.
#'
#'   `FUN` can be any function from manynet, netrics, migraph, or any other
#'   package that accepts a network as its first argument and returns a
#'   scalar numeric value.
#'
#' @name tests
#' @family models
#' @param .data A manynet-consistent network object (see
#'   [manynet::as_tidygraph()]).
#' @param FUN A graph-level statistic function to test.  The function must
#'   accept a network as its first argument and return a scalar.  Any function
#'   from \pkg{manynet} (e.g. [manynet::net_density()],
#'   [manynet::net_heterophily()]) or \pkg{netrics} works here.
#' @param ... Additional arguments to be passed on to FUN,
#'   e.g. the name of the attribute.
#' @param times Integer.  Number of random networks or permutations to
#'   generate.  Default is 1000; publication-ready work usually needs
#'   1000–10000.
#' @param strategy A \pkg{future} strategy string such as
#'   `"sequential"` (default) or `"multisession"`.
#' @param verbose Logical; show a progress bar during simulation.
#'   Default `FALSE`.
#' @seealso [manynet::generate_random()], [manynet::generate_configuration()],
#'   [manynet::to_permuted()], [net_regression()]
NULL

#' @rdname tests
#' @importFrom manynet generate_random bind_node_attributes is_directed is_complex
#' @examples
#' marvel_friends <- manynet::to_unsigned(manynet::ison_marvel_relationships)
#' marvel_friends <- manynet::to_giant(marvel_friends) |>
#'   manynet::to_subgraph(PowerOrigin == "Human")
#' (cugtest <- test_random(marvel_friends, manynet::net_heterophily,
#'    attribute = "Attractive", times = 200))
#' plot(cugtest)
#' @export
test_random <- function(.data, FUN, ...,
                        times = 1000,
                        strategy = "sequential",
                        verbose = FALSE) {
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(.data, args)
  } else {
    obsd <- FUN(.data)
  }
  oplan <- future::plan(strategy)
  on.exit(future::plan(oplan), add = TRUE)
  rands <- future.apply::future_lapply(
    seq_len(times), manynet::generate_random, n = .data,
    future.seed = TRUE
  )
  if (length(args) > 0) {
    rands <- future.apply::future_lapply(
      rands, manynet::bind_node_attributes, object2 = .data,
      future.seed = TRUE
    )
  }
  if (!is.null(args)) {
    simd <- unlist(future.apply::future_lapply(
      rands, FUN, args, future.seed = TRUE
    ))
  } else {
    simd <- unlist(future.apply::future_lapply(
      rands, FUN, future.seed = TRUE
    ))
  }
  out <- list(test = "CUG",
              testval = obsd,
              testdist = simd,
              mode = manynet::is_directed(.data),
              diag = manynet::is_complex(.data),
              cmode = "edges",
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              reps = times)
  class(out) <- "network_test"
  out
}

#' @rdname tests
#' @importFrom manynet generate_configuration
#' @export
test_configuration <- function(.data, FUN, ...,
                               times = 1000,
                               strategy = "sequential",
                               verbose = FALSE) {
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(.data, args)
  } else {
    obsd <- FUN(.data)
  }
  oplan <- future::plan(strategy)
  on.exit(future::plan(oplan), add = TRUE)
  rands <- future.apply::future_lapply(
    seq_len(times), manynet::generate_configuration, n = .data,
    future.seed = TRUE
  )
  if (length(args) > 0) {
    rands <- future.apply::future_lapply(
      rands, manynet::bind_node_attributes, object2 = .data,
      future.seed = TRUE
    )
  }
  if (!is.null(args)) {
    simd <- unlist(future.apply::future_lapply(
      rands, FUN, args, future.seed = TRUE
    ))
  } else {
    simd <- unlist(future.apply::future_lapply(
      rands, FUN, future.seed = TRUE
    ))
  }
  out <- list(test = "configuration",
              testval = obsd,
              testdist = simd,
              mode = manynet::is_directed(.data),
              diag = manynet::is_complex(.data),
              cmode = "edges",
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              reps = times)
  class(out) <- "network_test"
  out
}

#' @rdname tests
#' @importFrom manynet to_permuted
#' @examples
#' (qaptest <- test_permutation(marvel_friends, manynet::net_heterophily,
#'    attribute = "Attractive", times = 200))
#' plot(qaptest)
#' @export
test_permutation <- function(.data, FUN, ...,
                             times = 1000,
                             strategy = "sequential",
                             verbose = FALSE) {
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(.data, args)
  } else {
    obsd <- FUN(.data)
  }
  oplan <- future::plan(strategy)
  on.exit(future::plan(oplan), add = TRUE)
  rands <- future.apply::future_lapply(
    seq_len(times), function(x) manynet::to_permuted(.data),
    future.seed = TRUE
  )
  if (!is.null(args)) {
    simd <- unlist(future.apply::future_lapply(
      rands, FUN, args, future.seed = TRUE
    ))
  } else {
    simd <- unlist(future.apply::future_lapply(
      rands, FUN, future.seed = TRUE
    ))
  }
  out <- list(test = "QAP",
              testval = obsd,
              testdist = simd,
              mode = manynet::is_directed(.data),
              diag = manynet::is_complex(.data),
              plteobs = mean(simd <= obsd),
              pgteobs = mean(simd >= obsd),
              reps = times)
  class(out) <- "network_test"
  out
}

#' @export
print.network_test <- function(x, ...,
                               max.length = 6,
                               digits = 3) {
  cat(paste("\n", x$test, "Test Results\n\n"))
  cat("Observed Value:", x$testval, "\n")
  cat("Pr(X>=Obs):", x$pgteobs, "\n")
  cat("Pr(X<=Obs):", x$plteobs, "\n\n")
}

#' @rdname tests
#' @param x A `network_test` object returned by `test_random()`,
#'   `test_configuration()`, or `test_permutation()`.
#' @param main Plot title.  Defaults to a description of the test type.
#' @export
plot.network_test <- function(x, ..., main = NULL) {
  if (is.null(main)) {
    main <- paste(x$test, "Test: Permutation Distribution")
  }
  graphics::hist(
    x$testdist,
    main  = main,
    xlab  = "Simulated statistic",
    col   = "lightgrey",
    border = "white",
    ...
  )
  graphics::abline(v = x$testval, col = "red", lwd = 2, lty = 2)
  graphics::legend("topright",
                   legend = paste0("Observed = ", round(x$testval, 4)),
                   col = "red", lty = 2, lwd = 2, bty = "n")
  invisible(x)
}

