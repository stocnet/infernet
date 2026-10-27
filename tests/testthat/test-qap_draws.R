# A draw from a plan permutes the cells the model reads rather than the matrix,
# and a linear model or a glm solves a design built once. Both are shortcuts,
# and a shortcut must not change a published number: for a seed, every network
# shape and every estimator gives the same p-values, coefficients, and null
# distribution, to the last bit, as a draw that permutes the whole matrix and
# refits from the data. Without a plan, `QAPPermEst()` does exactly that.

expect_same_draws <- function(formula, .data, ...) {
  ctrl <- list(seed = 1, ...)
  planned <- suppressWarnings(suppressMessages(
    estimate_mrqap(formula, .data, times = 15, control = ctrl)))
  testthat::local_mocked_bindings(.draw_plan = function(...) NULL)
  whole <- suppressWarnings(suppressMessages(
    estimate_mrqap(formula, .data, times = 15, control = ctrl)))
  for (el in c("coefficients", "lower", "larger", "abs", "testdist")) {
    testthat::expect_identical(planned[[el]], whole[[el]], info = el)
  }
}

test_that("a gaussian model gives the same draws from a plan", {
  g <- qap_net_gaussian(n = 20)
  f <- weight ~ ego(Age) + alter(Age) + sim(Age) + same(Grp)
  expect_same_draws(f, g, permute = "outcome")
  expect_same_draws(f, g, permute = "predictor")
  expect_same_draws(f, g, permute = "outcome", use_robust_errors = TRUE)
  expect_same_draws(f, g, permute = "predictor", use_robust_errors = TRUE)
})

test_that("an interaction gives the same draws from a plan", {
  expect_same_draws(weight ~ ego(Age) * alter(Age), qap_net_gaussian(n = 15),
                    permute = "predictor")
})

test_that("a glm gives the same draws from a plan", {
  f <- . ~ ego(Age) + alter(Age)
  expect_same_draws(f, qap_net_binary(n = 20), permute = "outcome")
  expect_same_draws(f, qap_net_binary(n = 20), permute = "predictor")
  expect_same_draws(f, qap_net_count(n = 20), permute = "predictor")
  expect_same_draws(f, qap_net_binary(n = 20), use_robust_errors = TRUE)
})

test_that("other estimators give the same draws from a plan", {
  skip_if_not_installed("lme4")
  g <- qap_net_gaussian(n = 15)
  expect_same_draws(weight ~ ego(Age) + alter(Age), g,
                    random_intercept_sender = TRUE)
  skip_if_not_installed("MASS")
  expect_same_draws(. ~ ego(Age) + alter(Age), qap_net_count(n = 15),
                    family = "negbin")
})

test_that("fixed effects give the same draws from a plan", {
  g <- qap_net_gaussian(n = 15)
  expect_same_draws(weight ~ diff(Age) + same(Grp) | sender, g)
  expect_same_draws(weight ~ diff(Age) + same(Grp) | sender, g,
                    permute = "outcome")
  skip_if_not_installed("fixest")
  expect_same_draws(. ~ diff(Age) + alter(Age) | sender, qap_net_binary(n = 20))
})

test_that("every shape of network gives the same draws from a plan", {
  expect_same_draws(. ~ ego(Age), qap_net_undirected())
  expect_same_draws(weight ~ same(GONGO) + same(province),
                    qap_net_twomode_wide(), permute = "outcome")
  expect_same_draws(. ~ ego(Att) + alter(Att), qap_net_twomode())
  expect_same_draws(weight ~ ego(Age) + alter(Age), qap_net_gaussian(n = 18),
                    groups = rep(1:3, 6))
  expect_same_draws(advice ~ ego(age) + alter(age) + same(dept) + reports,
                    manynet::ison_hightech, permute = "predictor")
})

test_that("missing and fixed cells give the same draws from a plan", {
  g <- qap_net_gaussian(n = 15)
  m <- manynet::as_matrix(g)
  m[2, ] <- NA
  m[5, 7] <- NA
  holed <- manynet::mutate(manynet::as_tidygraph(m),
                           Age = manynet::node_attribute(g, "Age"))
  expect_same_draws(weight ~ ego(Age) + alter(Age), holed, permute = "outcome")
  expect_same_draws(weight ~ ego(Age) + alter(Age), holed, permute = "predictor")

  b <- qap_net_binary(n = 15, seed = 31)
  set.seed(31)
  fixed <- seq_len(manynet::net_ties(b)) %in% sample(manynet::net_ties(b), 6)
  expect_same_draws(. ~ ego(Age) + alter(Age),
                    manynet::mutate_ties(b, fixed = fixed))
})

test_that("a panel and a list of networks give the same draws from a plan", {
  g <- qap_net_panel(n = 10, waves = 3)
  expect_same_draws(. ~ inertia + ego(age) + alter(age), g)
  expect_same_draws(. ~ ego(age) + alter(age), manynet::to_waves(g),
                    permute = "predictor")
})

test_that("a draw reads the cell that permuting the matrix would put there", {
  set.seed(3)
  for (d in list(c(6, 6), c(4, 9), c(5, 5, 5), c(3, 4, 7))) {
    m <- array(stats::rnorm(prod(d)), dim = d)
    css <- length(d) == 3
    cells <- sort(sample(length(m), 20))
    set.seed(9); o <- .perm_orders(d, css = css)
    set.seed(9); p <- RMPerm(m, CSS = css)
    expect_identical(m[.cell_source(o, .cell_axes(cells, d))], p[cells])
  }
})
