# Shared expectations and skips.

# ---- reference data -------------------------------------------------------------

# Rebuilds the dyad-level data frame that the engine fits, so that a baseline
# coefficient can be compared against the equivalent standard fit on identical
# data. Anything this returns comes from the engine's own internals, so a
# comparison against it tests the estimator dispatch, not the vectorisation.
qap_reference_data <- function(formula, .data, directed = NULL, loops = FALSE) {
  ml <- convertToMatrixList(formula, .data, advise = FALSE)
  parsed <- parse_qap_formula(ml$formula)
  g <- manynet::as_tidygraph(.data)
  if (is.null(directed)) directed <- manynet::is_directed(g)
  pred <- make_qap_data(y = ml$mydata[[parsed$dependent]],
                        x = ml$mydata[parsed$main],
                        loops = loops, directed = directed)
  names(pred)[names(pred) == "yv"] <- parsed$dependent
  list(pred = pred, formula = ml$formula, parsed = parsed)
}

# ---- the shape of a result ------------------------------------------------------

# The p-value matrices are the product of the permutation loop, so their shape
# is the contract every estimator has to meet, whatever it fits underneath.
expect_qap_shape <- function(fit, coefs) {
  testthat::expect_s3_class(fit, "netreg")
  testthat::expect_named(fit$coefficients, coefs)
  for (el in c("lower", "larger", "abs")) {
    m <- fit[[el]]
    testthat::expect_equal(dim(m), c(2L, length(coefs)),
                           info = paste("dim of", el))
    testthat::expect_equal(rownames(m), c("perm_coefs", "perm_t"),
                           info = paste("rownames of", el))
    testthat::expect_equal(colnames(m), coefs, info = paste("colnames of", el))
    finite <- m[!is.na(m)]
    testthat::expect_true(all(finite >= 0 & finite <= 1),
                          info = paste(el, "outside [0, 1]"))
  }
  invisible(fit)
}

# ---- messages and warnings -------------------------------------------------------

# `expect_message()` in testthat 3 catches the first message that matches and
# lets every other one through to the console. This checks that one message of
# `object` matches `regexp`, and keeps them all off the console.
expect_any_message <- function(object, regexp, ...) {
  msgs <- testthat::capture_messages(object)
  testthat::expect_true(any(grepl(regexp, msgs, ...)),
                        info = paste(c("Messages:", msgs), collapse = "\n"))
  invisible(msgs)
}

# `manynet::snet_warn()` raises a catchable warning from manynet 2.3.2, and the
# floor is now 2.4.0, so the probe that used to decide whether to assert one is
# gone. The name stays, so that a test says it expects a {manynet} warning.
expect_snet_warning <- function(object, regexp) {
  testthat::expect_warning(object, regexp)
}

# ---- the draw plan and the missing cells ------------------------------------

# A fit with the draw plan, against the same fit with the plan switched off, so
# that each draw permutes the whole matrix and refits. Five draws are enough:
# the two must agree to the last bit in every draw, not on average.
expect_same_draws <- function(formula, .data, ...) {
  ctrl <- list(seed = 1, ...)
  planned <- suppressWarnings(suppressMessages(
    estimate_mrqap(formula, .data, times = 5, control = ctrl)))
  testthat::local_mocked_bindings(.draw_plan = function(...) NULL)
  whole <- suppressWarnings(suppressMessages(
    estimate_mrqap(formula, .data, times = 5, control = ctrl)))
  for (el in c("coefficients", "lower", "larger", "abs", "testdist")) {
    testthat::expect_identical(planned[[el]], whole[[el]], info = el)
  }
}

# Every one of `draws` permutations within `blocks` keeps the missing cells of
# `m` where they are.
expect_missing_fixed <- function(m, blocks, draws = 50, CSS = FALSE) {
  for (i in seq_len(draws)) {
    p <- RMPerm(m, blocks, CSS = CSS)
    expect_identical(is.na(p), is.na(m))
  }
}

# ---- skips ----------------------------------------------------------------------

# A slow test takes seconds rather than a fraction of one: a calibration, which
# checks over many simulated datasets that a test's p-values are uniform under
# its null hypothesis or that it has power against an alternative, and the
# tutorials. They run where `INFERNET_SLOW_TESTS=true`, as CI sets it, and not
# on CRAN or in a plain `devtools::test()`. Set the variable before changing a
# test's statistic or a tutorial:
#   Sys.setenv(INFERNET_SLOW_TESTS = "true"); devtools::test()
skip_unless_slow <- function(what) {
  testthat::skip_if_not(
    isTRUE(as.logical(Sys.getenv("INFERNET_SLOW_TESTS", "false"))),
    paste(what, "is slow: set INFERNET_SLOW_TESTS=true to run it"))
}
