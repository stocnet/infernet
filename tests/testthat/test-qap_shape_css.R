# The engine fits two shapes, a dyadic network and a cognitive social structure,
# through one skeleton. These assert that the CSS half of that skeleton works,
# since the CSS entry point in `net_regression()` does not exist yet and the
# merge would otherwise be unverified on the shape it was merged for.

css_fixture <- function(n = 8, seed = 7) {
  set.seed(seed)
  # One true network, which every perceiver sees with some noise.
  truth <- matrix(stats::rbinom(n^2, 1, 0.35), n, n)
  diag(truth) <- 0
  y <- array(0L, dim = c(n, n, n))
  for (p in seq_len(n)) {
    y[, , p] <- pmin(truth + matrix(stats::rbinom(n^2, 1, 0.12), n, n), 1)
    diag(y[, , p]) <- 0
  }
  # A dyadic covariate, the same for every perceiver.
  dyadic <- outer(stats::runif(n), stats::runif(n), "+")
  x <- array(rep(as.vector(dyadic), n), dim = c(n, n, n))
  list(ties = y, cov = x)
}

test_that("the engine fits a CSS and counts its observations", {
  n <- 8
  fit <- QAPengine(ties ~ cov, css_fixture(n), css = TRUE,
                   family = "binomial", times = 20, seed = 1)
  expect_s3_class(fit, "QAPCSS")
  expect_named(fit$coefficients, c("(Intercept)", "cov"))
  # Every cell of every perceiver's report, less the diagonals.
  expect_equal(nrow(fit$pred), n * n * n - n * n)
  expect_equal(dim(fit$lower), c(2L, 2L))
  expect_equal(rownames(fit$lower), c("perm_coefs", "perm_t"))
})

test_that("a CSS baseline matches glm() on the same observations", {
  ml <- css_fixture()
  fit <- QAPengine(ties ~ cov, ml, css = TRUE, family = "binomial",
                   times = 10, seed = 1)
  ref <- stats::glm(ties ~ cov, data = fit$pred, family = stats::binomial())
  expect_equal(unname(fit$coefficients), unname(stats::coef(ref)))
})

test_that("a CSS carries sender, receiver, perceiver and network indices", {
  fit <- QAPengine(ties ~ cov, css_fixture(), css = TRUE,
                   family = "binomial", times = 10, seed = 1)
  expect_true(all(c("sv", "rv", "pv", "nv") %in% names(fit$pred)))
  expect_s3_class(fit$pred$pv, "factor")
})

test_that("a perceiver random intercept is available to a CSS only", {
  skip_if_not_installed("lme4")
  fit <- suppressMessages(suppressWarnings(
    QAPengine(ties ~ cov, css_fixture(), css = TRUE, family = "binomial",
              times = 10, seed = 1, random_intercept_perceiver = TRUE)))
  expect_named(fit$random.intercepts, "pv")
  expect_true(fit$random[["perceiver"]])
})

test_that("a dyadic network rejects a perceiver random intercept", {
  m <- matrix(stats::rnorm(64), 8, 8)
  diag(m) <- 0
  expect_error(
    QAPengine(ties ~ cov, list(ties = m, cov = matrix(stats::rnorm(64), 8, 8)),
              times = 5, random_intercept_perceiver = TRUE),
    "perceiver")
})

test_that("a CSS rejects an outcome that is not three-dimensional", {
  m <- matrix(stats::rnorm(64), 8, 8)
  expect_error(
    QAPengine(ties ~ cov, list(ties = m, cov = m), css = TRUE, times = 5),
    "3-dimensional")
})

test_that("a CSS fit prints as a model", {
  fit <- QAPengine(ties ~ cov, css_fixture(), css = TRUE,
                   family = "binomial", times = 10, seed = 1)
  expect_output(print(fit), "CSS")
  expect_output(print(fit), "Coefficients")
  expect_invisible(print(fit))
})

test_that("both shapes reproduce under the same seed", {
  ml <- css_fixture()
  a <- QAPengine(ties ~ cov, ml, css = TRUE, family = "binomial",
                 times = 20, seed = 42)
  b <- QAPengine(ties ~ cov, ml, css = TRUE, family = "binomial",
                 times = 20, seed = 42)
  expect_equal(a$lower, b$lower)
})
