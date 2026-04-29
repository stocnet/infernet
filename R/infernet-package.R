#' infernet: Inferential Models for Networks
#'
#' @description
#' **infernet** provides inferential tests and regression models for network
#' data.  It implements:
#'
#' - Conditional uniform graph (CUG) tests ([test_random()],
#'   [test_configuration()]) comparing an observed network statistic against
#'   a null distribution of random or configuration-model networks.
#' - Quadratic assignment procedure (QAP) tests ([test_permutation()])
#'   using permutations of the original network.
#' - Multiple regression QAP (MRQAP) via [net_regression()], with Dekker's
#'   double semi-partialling plus or permute-y-only inference, supporting
#'   gaussian, binomial, Poisson, negative binomial, zero-inflated Poisson,
#'   and multinomial families as well as mixed models, fixed effects, and GMM.
#'
#' The test functions accept **any scalar network measure** from
#' \pkg{manynet}, \pkg{netrics}, \pkg{migraph}, or any user-defined function
#' that takes a network as its first argument.
#'
#' @section Silencing advice:
#' When `sim()` or `same()` terms are used in `net_regression()`, a
#' specification reminder is printed.  Set
#' `options(infernet_advice = FALSE)` to suppress it.
#'
#' @section stocnet ecosystem:
#' **infernet** is part of the
#' [stocnet](https://github.com/stocnet/) family of R packages:
#' - \pkg{manynet}: network construction, manipulation, and visualisation.
#' - \pkg{netrics}: network-level statistics.
#' - \pkg{migraph}: node- and tie-level measures and tests.
#' - \pkg{infernet}: inferential tests and regression (this package).
#'
#' @seealso
#' Useful vignette: `vignette("getting-started", package = "infernet")`.
#'
#' @keywords internal
"_PACKAGE"
