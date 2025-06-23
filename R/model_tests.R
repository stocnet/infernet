# Tests of network measures ####

#' Tests of network measures
#' 
#' @description
#'   These functions conduct tests of any network-level statistic:
#'   
#'   - `test_random()` performs a conditional uniform graph (CUG) test
#'   of a measure against a distribution of measures on random networks 
#'   of the same dimensions.
#'   - `test_permutation()` performs a quadratic assignment procedure (QAP) test 
#'   of a measure against a distribution of measures on permutations 
#'   of the original network.
#'   
#' @name tests
#' @inheritParams regression
#' @family models
#' @param FUN A graph-level statistic function to test.
#' @param ... Additional arguments to be passed on to FUN,
#'   e.g. the name of the attribute.
NULL

#' @rdname tests 
#' @importFrom manynet generate_random bind_node_attributes is_directed is_complex
#' @examples 
#' marvel_friends <- to_unsigned(ison_marvel_relationships)
#' marvel_friends <- to_giant(marvel_friends) %>% 
#'   to_subgraph(PowerOrigin == "Human")
#' (cugtest <- test_random(marvel_friends, manynet::net_heterophily, attribute = "Attractive",
#'    times = 200))
#' # plot(cugtest)
#' @export
test_random <- function(.data, FUN, ..., 
                        times = 1000, 
                        strategy = "sequential", 
                        verbose = FALSE){
  args <- unlist(list(...))
  if (!is.null(args)) {
    obsd <- FUN(.data, args)
  } else {
    obsd <- FUN(.data)
  }
  oplan <- future::plan(strategy)
  on.exit(future::plan(oplan), add = TRUE)
  rands <- furrr::future_map(1:times, manynet::generate_random, n = .data, 
                             .progress = verbose, 
                             .options = furrr::furrr_options(seed = T))
  if (length(args) > 0) {
    rands <- furrr::future_map(rands, 
                               manynet::bind_node_attributes, object2 = .data, 
                               .progress = verbose, 
                               .options = furrr::furrr_options(seed = T))
  }
  if (!is.null(args)) {
    simd <- furrr::future_map_dbl(rands,
                  FUN, args)
  } else {
    simd <- furrr::future_map_dbl(rands,
                   FUN)
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
