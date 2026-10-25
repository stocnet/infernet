# Every fit keeps its permuted test statistics as `testdist`: one row per
# permutation and one column per coefficient. The p-value shares are summaries of
# this matrix, so the first contract is that the shares can be read back from it.
# The test family keeps its null distribution under the same name.

FORM <- weight ~ ego(Age) + alter(Age) + sim(Age)
COEFS <- c("(Intercept)", "ego_Age", "alter_Age", "sim_Age")

# The share of permutations at or below, at or above, and beyond the observed
# statistic, read from `testdist` rather than from the fit's own summaries.
shares_from_testdist <- function(fit) {
  td <- fit$testdist
  t  <- fit$obs[colnames(td)]
  rbind(
    lower  = vapply(colnames(td), function(j) mean(td[, j] <= t[[j]], na.rm = TRUE), numeric(1)),
    larger = vapply(colnames(td), function(j) mean(td[, j] >= t[[j]], na.rm = TRUE), numeric(1)),
    abs    = vapply(colnames(td), function(j) mean(abs(td[, j]) >= abs(t[[j]]), na.rm = TRUE), numeric(1))
  )
}

test_that("testdist has a row per permutation and a column per coefficient", {
  fit <- estimate_mrqap(FORM, qap_net_gaussian(), times = 30,
                        control = list(seed = 1, permute = "outcome"))
  expect_true(is.matrix(fit$testdist))
  expect_equal(dim(fit$testdist), c(30L, 4L))
  expect_equal(colnames(fit$testdist), COEFS)
  expect_false(anyNA(fit$testdist))
})

test_that("the p-values can be read back from testdist when the outcome is permuted", {
  fit <- estimate_mrqap(FORM, qap_net_gaussian(), times = 40,
                        control = list(seed = 2, permute = "outcome"))
  s <- shares_from_testdist(fit)
  expect_equal(unname(s["lower", ]),  unname(fit$lower["perm_t", ]))
  expect_equal(unname(s["larger", ]), unname(fit$larger["perm_t", ]))
  expect_equal(unname(s["abs", ]),    unname(fit$abs["perm_t", ]))
})

test_that("the p-values can be read back from testdist when predictors are permuted", {
  fit <- estimate_mrqap(FORM, qap_net_gaussian(), times = 40,
                        control = list(seed = 3, permute = "predictor"))
  # The intercept is not permuted, so its column is NA, as its p-values are.
  expect_true(all(is.na(fit$testdist[, "(Intercept)"])))
  expect_false(anyNA(fit$testdist[, -1]))
  s <- shares_from_testdist(fit)
  expect_equal(unname(s["abs", -1]), unname(fit$abs["perm_t", -1]))
  expect_equal(unname(s["lower", -1]), unname(fit$lower["perm_t", -1]))
})

test_that("testdist reproduces under the same seed", {
  a <- estimate_mrqap(FORM, qap_net_gaussian(), times = 20, control = list(seed = 9))
  b <- estimate_mrqap(FORM, qap_net_gaussian(), times = 20, control = list(seed = 9))
  expect_equal(a$testdist, b$testdist)
})

test_that("a CSS fit keeps testdist too", {
  fit <- suppressMessages(
    estimate_mrqap(advice ~ ego(age) + friends, manynet::ison_hightech,
                   times = 15, control = list(seed = 1, permute = "outcome")))
  expect_equal(dim(fit$testdist), c(15L, 3L))
  expect_equal(colnames(fit$testdist), names(fit$coefficients))
})

test_that("a failed permutation leaves its row NA rather than shifting the rest", {
  draws <- matrix(1:6, nrow = 3, dimnames = list(NULL, c("a", "b")))
  out <- .as_testdist(draws, c("a", "b"), times = 5)
  expect_equal(dim(out), c(5L, 2L))
  expect_equal(out[1:3, ], draws)
  expect_true(all(is.na(out[4:5, ])))
})

test_that("the test family and the regression share the word testdist", {
  g <- qap_net_gaussian(n = 15)
  cug <- test_random(g, netrics::net_by_density, times = 10)
  expect_length(cug$testdist, 10)
  fit <- estimate_mrqap(weight ~ ego(Age), g, times = 10, control = list(seed = 1))
  expect_equal(nrow(fit$testdist), 10)
})
