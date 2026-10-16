# ============================================================
# Tests for net_regression()
#
# Every call uses `times = 10` (placeholder for inference; too
# low for real use, fine for API coverage).
# ============================================================

# ---- helpers ---------------------------------------------------------------

make_weighted_net <- function(n = 10, seed = 42) {
  set.seed(seed)
  m <- matrix(stats::rnorm(n^2), n, n)
  diag(m) <- 0
  g <- manynet::as_tidygraph(m)
  g <- manynet::mutate(g, Age = stats::runif(n, 20, 60))
  g <- manynet::mutate(g, Gender = sample(c("F", "M"), n, replace = TRUE))
  g
}

make_binary_net <- function(n = 10, seed = 43) {
  set.seed(seed)
  m <- matrix(stats::rbinom(n^2, 1, 0.3), n, n)
  diag(m) <- 0
  g <- manynet::as_tidygraph(m)
  g <- manynet::mutate(g, Age = stats::runif(n, 20, 60))
  g <- manynet::mutate(g, Gender = sample(c("F", "M"), n, replace = TRUE))
  g
}


# ---- formula terms: ego / alter / same / dist / sim / tertius --------------

test_that("net_regression handles ego / alter / sim on a weighted network", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ ego(Age) + alter(Age) + sim(Age),
                        g, times = 10)
  expect_s3_class(fit, "netreg")
  expect_s3_class(fit, "netreg_lm")
  expect_true(!is.null(fit$coefficients))
  expect_true(!is.null(fit$lower))
})

test_that("net_regression handles same() on a categorical attribute", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ same(Gender), g, times = 10)
  expect_s3_class(fit, "netreg")
  expect_true("same Gender" %in% names(fit$coefficients))
})

test_that("net_regression handles dist() on a numeric attribute", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ dist(Age), g, times = 10)
  expect_s3_class(fit, "netreg")
  expect_true("dist Age" %in% names(fit$coefficients))
})

test_that("net_regression handles tertius() with mean aggregation", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ tertius(Age, mean), g, times = 10)
  expect_s3_class(fit, "netreg")
})


# ---- family = "auto" -------------------------------------------------------

test_that("family 'auto' resolves to gaussian for a weighted network", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ sim(Age), g, times = 10)
  expect_equal(fit$family, "gaussian")
  expect_s3_class(fit, "netreg_lm")
})

test_that("family 'auto' resolves to binomial for a binary network", {
  g <- make_binary_net()
  fit <- net_regression(. ~ sim(Age), g, times = 10)
  expect_equal(fit$family, "binomial")
  expect_s3_class(fit, "netreg_glm")
  expect_s3_class(fit$confusion_matrix, "QAPConfusionMatrix")
})


# ---- gaussian + binary outcome -> LPM confusion matrix ---------------------

test_that("gaussian family on binary outcome adds a clamped LPM CM", {
  g <- make_binary_net()
  fit <- net_regression(. ~ sim(Age), g, times = 10,
                        control = list(family = "gaussian"))
  expect_equal(fit$family, "gaussian")
  expect_s3_class(fit, "netreg_lm")
  expect_s3_class(fit$confusion_matrix, "QAPConfusionMatrix")
})


# ---- list-of-graphs: pooled fit --------------------------------------------

test_that("net_regression fits on a list of graphs", {
  g1 <- make_weighted_net(n = 8, seed = 1)
  g2 <- make_weighted_net(n = 8, seed = 2)
  fit <- net_regression(weight ~ sim(Age), list(g1, g2), times = 10)
  expect_s3_class(fit, "netreg")
  expect_s3_class(fit, "netreg_lm")
})


# ---- list-of-graphs: drop graphs missing a predictor, with warning ---------

test_that("graphs missing a predictor are dropped", {
  g1 <- make_weighted_net(n = 8, seed = 1)
  g2 <- manynet::as_tidygraph(matrix(stats::rnorm(8^2), 8, 8))
  gs <- list(A = g1, B = g2)
  fit <- suppressWarnings(net_regression(weight ~ sim(Age), gs, times = 10))
  expect_s3_class(fit, "netreg")
  expect_length(unique(fit$pred$nv), 1L)
})

test_that("dropping a graph warns", {
  g1 <- make_weighted_net(n = 8, seed = 1)
  g2 <- manynet::as_tidygraph(matrix(stats::rnorm(8^2), 8, 8))
  expect_snet_warning(
    net_regression(weight ~ sim(Age), list(A = g1, B = g2), times = 10),
    "Dropping")
})


# ---- print method ----------------------------------------------------------

test_that("print.netreg_lm runs without error", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ sim(Age), g, times = 10)
  expect_output(print(fit), "OLS Network Model")
})

test_that("print.netreg_glm runs without error", {
  g <- make_binary_net()
  fit <- net_regression(. ~ sim(Age), g, times = 10)
  expect_output(print(fit), "Generalized Linear Network Model")
})


# ---- method control --------------------------------------------------------

test_that("permute = 'outcome' runs and is recorded on the fit", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ ego(Age) + alter(Age),
                        g, times = 10,
                        control = list(permute = "outcome"))
  expect_equal(fit$permute, "outcome")
})


# ---- tertius ---------------------------------------------------------------

test_that("tertius() accepts a quoted and an unquoted summary function", {
  g <- make_weighted_net()
  quoted   <- net_regression(weight ~ tertius(Age, "mean"), g, times = 10)
  unquoted <- net_regression(weight ~ tertius(Age, mean), g, times = 10)
  bare     <- net_regression(weight ~ tertius(Age), g, times = 10)
  expect_equal(quoted$coefficients, unquoted$coefficients)
  expect_equal(quoted$coefficients, bare$coefficients)
})

test_that("tertius() sum differs from mean, and rejects anything else", {
  g <- make_weighted_net()
  mean_fit <- net_regression(weight ~ tertius(Age, "mean"), g, times = 10)
  sum_fit  <- net_regression(weight ~ tertius(Age, "sum"), g, times = 10)
  expect_false(isTRUE(all.equal(mean_fit$coefficients, sum_fit$coefficients)))
  expect_error(net_regression(weight ~ tertius(Age, "median"), g, times = 10),
               "mean")
})


# ---- messaging -------------------------------------------------------------

test_that("a missing attribute names what is available", {
  g <- make_weighted_net()
  expect_error(net_regression(weight ~ ego(Nope), g, times = 10),
               "Age")
})

# The class says what the result is, most specific first, so that a method can
# be written at any level and S3 falls back to the next. `{autograph}` plots a
# `netreg`, and can add a method for a more specific class later.
test_that("a result's classes run from the most specific to netreg", {
  g <- qap_net_gaussian(n = 12)
  lin <- net_regression(weight ~ ego(Age) + alter(Age), g, times = 5,
                        control = list(seed = 1))
  expect_equal(class(lin), c("netreg_lm", "netreg"))

  b <- qap_net_binary(n = 12)
  glm_fit <- net_regression(. ~ ego(Age) + alter(Age), b, times = 5,
                            control = list(seed = 1))
  expect_equal(class(glm_fit), c("netreg_glm", "netreg"))

  css <- suppressMessages(
    net_regression(advice ~ ego(age) + friends, manynet::ison_hightech,
                   times = 5, control = list(seed = 1)))
  expect_equal(class(css), c("netreg_css", "netreg_glm", "netreg"))
})

test_that("each class prints through its own method", {
  g <- qap_net_gaussian(n = 12)
  lin <- net_regression(weight ~ ego(Age) + alter(Age), g, times = 5,
                        control = list(seed = 1))
  expect_output(print(lin), "Coefficients")
  css <- suppressMessages(
    net_regression(advice ~ ego(age) + friends, manynet::ison_hightech,
                   times = 5, control = list(seed = 1)))
  expect_output(print(css), "cognitive social structure")
  # A bare netreg falls back to a structural summary rather than erroring.
  bare <- structure(list(coefficients = 1), class = "netreg")
  expect_output(print(bare), "<netreg>")
})
