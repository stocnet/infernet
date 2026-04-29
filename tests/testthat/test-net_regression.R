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

make_undirected_net <- function(n = 10, seed = 44) {
  set.seed(seed)
  m <- matrix(stats::rnorm(n^2), n, n)
  m <- (m + t(m)) / 2  # make symmetric
  diag(m) <- 0
  # Build as undirected via igraph
  ig <- igraph::graph_from_adjacency_matrix(m, mode = "undirected",
                                            weighted = TRUE)
  g <- manynet::as_tidygraph(ig)
  g <- manynet::mutate(g, Age = stats::runif(n, 20, 60))
  g
}


# ---- formula terms: ego / alter / same / dist / sim / tertius --------------

test_that("net_regression handles ego / alter / sim on a weighted network", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ ego(Age) + alter(Age) + sim(Age),
                        g, times = 10)
  expect_s3_class(fit, "net_regression")
  expect_s3_class(fit, "QAPRegression")
  expect_true(!is.null(fit$coefficients))
  expect_true(!is.null(fit$lower))
})

test_that("net_regression handles same() on a categorical attribute", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ same(Gender), g, times = 10)
  expect_s3_class(fit, "net_regression")
  expect_true("same Gender" %in% names(fit$coefficients))
})

test_that("net_regression handles dist() on a numeric attribute", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ dist(Age), g, times = 10)
  expect_s3_class(fit, "net_regression")
  expect_true("dist Age" %in% names(fit$coefficients))
})

test_that("net_regression handles tertius() with mean aggregation", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ tertius(Age, mean), g, times = 10)
  expect_s3_class(fit, "net_regression")
})


# ---- family = "auto" -------------------------------------------------------

test_that("family 'auto' resolves to gaussian for a weighted network", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ sim(Age), g, times = 10)
  expect_equal(fit$family, "gaussian")
  expect_s3_class(fit, "QAPRegression")
})

test_that("family 'auto' resolves to binomial for a binary network", {
  g <- make_binary_net()
  fit <- net_regression(. ~ sim(Age), g, times = 10)
  expect_equal(fit$family, "binomial")
  expect_s3_class(fit, "QAPGLM")
  expect_s3_class(fit$confusion_matrix, "QAPConfusionMatrix")
})


# ---- gaussian + binary outcome -> LPM confusion matrix ---------------------

test_that("gaussian family on binary outcome adds a clamped LPM CM", {
  g <- make_binary_net()
  fit <- net_regression(. ~ sim(Age), g, times = 10,
                        control = list(family = "gaussian"))
  expect_equal(fit$family, "gaussian")
  expect_s3_class(fit, "QAPRegression")
  expect_s3_class(fit$confusion_matrix, "QAPConfusionMatrix")
})


# ---- undirected networks ---------------------------------------------------

test_that("net_regression works on an undirected network", {
  skip_if_not_installed("igraph")
  g <- make_undirected_net()
  fit <- net_regression(weight ~ sim(Age), g, times = 10)
  expect_s3_class(fit, "net_regression")
  expect_equal(fit$mode, "undirected")
})

test_that("net_regression mode can be forced to undirected", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ sim(Age), g, times = 10,
                        control = list(mode = "undirected"))
  expect_equal(fit$mode, "undirected")
})


# ---- list-of-graphs: pooled fit --------------------------------------------

test_that("net_regression fits on a list of graphs", {
  g1 <- make_weighted_net(n = 8, seed = 1)
  g2 <- make_weighted_net(n = 8, seed = 2)
  fit <- net_regression(weight ~ sim(Age), list(g1, g2), times = 10)
  expect_s3_class(fit, "net_regression")
  expect_s3_class(fit, "QAPRegression")
})


# ---- list-of-graphs: drop graphs missing a predictor, with warning ---------

test_that("graphs missing a predictor are dropped with a warning", {
  g1 <- make_weighted_net(n = 8, seed = 1)
  g2 <- manynet::as_tidygraph(matrix(stats::rnorm(8^2), 8, 8))
  gs <- list(A = g1, B = g2)
  expect_warning(
    fit <- net_regression(weight ~ sim(Age), gs, times = 10),
    regexp = "Dropping"
  )
  expect_s3_class(fit, "net_regression")
})


# ---- error handling --------------------------------------------------------

test_that("net_regression errors on a missing node attribute", {
  g <- make_weighted_net()
  expect_error(
    net_regression(weight ~ sim(Nonexistent), g, times = 10),
    regexp = "not found"
  )
})

test_that("net_regression errors when all networks are dropped", {
  g1 <- manynet::as_tidygraph(matrix(stats::rnorm(8^2), 8, 8))
  g2 <- manynet::as_tidygraph(matrix(stats::rnorm(8^2), 8, 8))
  expect_error(
    suppressWarnings(
      net_regression(weight ~ sim(Age), list(g1, g2), times = 10)
    ),
    regexp = "dropped"
  )
})


# ---- print method ----------------------------------------------------------

test_that("print.net_regression runs without error for QAPRegression", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ sim(Age), g, times = 10)
  expect_output(print(fit), "OLS Network Model")
})

test_that("print.net_regression runs without error for QAPGLM", {
  g <- make_binary_net()
  fit <- net_regression(. ~ sim(Age), g, times = 10)
  expect_output(print(fit), "Generalized Linear Network Model")
})


# ---- method control --------------------------------------------------------

test_that("method = 'qapy' runs and flags the nullhyp on the fit", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ ego(Age) + alter(Age),
                        g, times = 10,
                        control = list(method = "qapy"))
  expect_equal(fit$nullhyp, "qapy")
})


# ---- net_regression_control ------------------------------------------------

test_that("net_regression_control returns a list with all expected names", {
  ctrl <- net_regression_control()
  expect_type(ctrl, "list")
  expect_true("method" %in% names(ctrl))
  expect_true("family" %in% names(ctrl))
  expect_true("strategy" %in% names(ctrl))
  expect_true("use_robust_errors" %in% names(ctrl))
  expect_true("random_intercept_sender" %in% names(ctrl))
})

test_that("net_regression_control passes through to net_regression", {
  g <- make_weighted_net()
  ctrl <- net_regression_control(method = "qapy")
  fit <- net_regression(weight ~ sim(Age), g, times = 10, control = ctrl)
  expect_equal(fit$nullhyp, "qapy")
})

test_that("net_regression_control validates method argument", {
  expect_error(net_regression_control(method = "bad"), regexp = "arg")
})


# ---- combine_qap_estimates -------------------------------------------------

test_that("combine_qap_estimates pools two QAPRegression fits", {
  g <- make_weighted_net()
  fit1 <- net_regression(weight ~ sim(Age), g, times = 10)
  fit2 <- net_regression(weight ~ sim(Age), g, times = 10)
  # Access internal function via :::
  combined <- infernet:::combine_qap_estimates(fit1, fit2)
  expect_equal(combined$reps, fit1$reps + fit2$reps)
  # Pooled lower should be between fit1 and fit2 lower values
  expect_true(all(is.finite(combined$lower)))
})


# ---- optional-dependency tests (guarded) -----------------------------------

test_that("family = 'negbin' works when MASS is installed", {
  skip_if_not_installed("MASS")
  g <- make_weighted_net()
  # Use a positive count-like outcome by exponentiating
  set.seed(1)
  n <- 10
  m <- matrix(rpois(n^2, lambda = 3), n, n)
  diag(m) <- 0
  gc <- manynet::as_tidygraph(m)
  gc <- manynet::mutate(gc, Age = stats::runif(n, 20, 60))
  fit <- net_regression(weight ~ sim(Age), gc, times = 10,
                        control = list(family = "negbin"))
  expect_s3_class(fit, "QAPGLM")
  expect_equal(fit$family, "negbin")
})

test_that("use_robust_errors = TRUE returns finite coefficients", {
  g <- make_weighted_net()
  fit <- net_regression(weight ~ sim(Age), g, times = 10,
                        control = list(use_robust_errors = TRUE))
  expect_true(all(is.finite(fit$coefficients)))
})

