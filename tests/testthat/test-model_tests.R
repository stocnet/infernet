# ============================================================
# Tests for test_random(), test_configuration(), test_permutation()
#
# All calls use `times = 30` for speed; too low for real use.
# ============================================================

# ---- helpers ---------------------------------------------------------------

make_test_graph <- function(n = 12, seed = 7) {
  set.seed(seed)
  m <- matrix(stats::rbinom(n^2, 1, 0.3), n, n)
  diag(m) <- 0
  g <- manynet::as_tidygraph(m)
  g <- manynet::mutate(g,
                       Attr = sample(c("A", "B"), n, replace = TRUE))
  g
}


# ---- test_random -----------------------------------------------------------

test_that("test_random returns a network_test object", {
  g <- make_test_graph()
  out <- test_random(g, manynet::net_density, times = 30)
  expect_s3_class(out, "network_test")
})

test_that("test_random result has required fields", {
  g <- make_test_graph()
  out <- test_random(g, manynet::net_density, times = 30)
  expect_true(!is.null(out$testval))
  expect_true(!is.null(out$testdist))
  expect_length(out$testdist, 30)
  expect_true(!is.null(out$plteobs))
  expect_true(!is.null(out$pgteobs))
  expect_equal(out$reps, 30)
  expect_equal(out$test, "CUG")
})

test_that("test_random: plteobs and pgteobs are in [0, 1]", {
  g <- make_test_graph()
  out <- test_random(g, manynet::net_density, times = 30)
  expect_gte(out$plteobs, 0)
  expect_lte(out$plteobs, 1)
  expect_gte(out$pgteobs, 0)
  expect_lte(out$pgteobs, 1)
})

test_that("test_random: testval matches direct computation", {
  g <- make_test_graph()
  expected <- manynet::net_density(g)
  out <- test_random(g, manynet::net_density, times = 30)
  expect_equal(out$testval, expected)
})

test_that("test_random: testdist contains numeric values", {
  g <- make_test_graph()
  out <- test_random(g, manynet::net_density, times = 30)
  expect_true(is.numeric(out$testdist))
})


# ---- test_permutation ------------------------------------------------------

test_that("test_permutation returns a network_test object", {
  g <- make_test_graph()
  out <- test_permutation(g, manynet::net_density, times = 30)
  expect_s3_class(out, "network_test")
})

test_that("test_permutation result has required fields", {
  g <- make_test_graph()
  out <- test_permutation(g, manynet::net_density, times = 30)
  expect_true(!is.null(out$testval))
  expect_length(out$testdist, 30)
  expect_equal(out$reps, 30)
  expect_equal(out$test, "QAP")
})

test_that("test_permutation: plteobs and pgteobs in [0, 1]", {
  g <- make_test_graph()
  out <- test_permutation(g, manynet::net_density, times = 30)
  expect_gte(out$plteobs, 0)
  expect_lte(out$plteobs, 1)
  expect_gte(out$pgteobs, 0)
  expect_lte(out$pgteobs, 1)
})

test_that("test_permutation: testval equals observed density", {
  g <- make_test_graph()
  expected <- manynet::net_density(g)
  out <- test_permutation(g, manynet::net_density, times = 30)
  expect_equal(out$testval, expected)
})

test_that("test_permutation with attribute argument works", {
  g <- make_test_graph()
  out <- test_permutation(g, manynet::net_heterophily,
                          attribute = "Attr", times = 30)
  expect_s3_class(out, "network_test")
  expect_length(out$testdist, 30)
})


# ---- print.network_test ----------------------------------------------------

test_that("print.network_test produces expected output for CUG test", {
  g <- make_test_graph()
  out <- test_random(g, manynet::net_density, times = 30)
  expect_output(print(out), "CUG Test Results")
  expect_output(print(out), "Observed Value")
  expect_output(print(out), "Pr\\(X>=Obs\\)")
  expect_output(print(out), "Pr\\(X<=Obs\\)")
})

test_that("print.network_test produces expected output for QAP test", {
  g <- make_test_graph()
  out <- test_permutation(g, manynet::net_density, times = 30)
  expect_output(print(out), "QAP Test Results")
})


# ---- plot.network_test -----------------------------------------------------

test_that("plot.network_test runs without error", {
  g <- make_test_graph()
  out <- test_random(g, manynet::net_density, times = 30)
  expect_silent(plot(out))
})

test_that("plot.network_test returns the object invisibly", {
  g <- make_test_graph()
  out <- test_random(g, manynet::net_density, times = 30)
  ret <- plot(out)
  expect_identical(ret, out)
})
