test_that("test_parameter() and test_time() have the signature of RSiena and goldfish", {
  # A method must keep the generic's arguments, so the generics must agree
  # with the ones that other packages register methods on.
  expect_equal(names(formals(test_parameter)), c("x", "..."))
  expect_equal(names(formals(test_time)), c("x", "..."))
})

test_that("test_parameter() and test_time() refuse a class with no method", {
  expect_error(test_parameter(1:3), "no method")
  expect_error(test_time(1:3), "no method")
})

# ---- network regressions -----------------------------------------------------

param_fit <- function(net, formula = . ~ ego(Age) + alter(B), family = "gaussian") {
  suppressWarnings(suppressMessages(
    estimate_mrqap(formula, net, times = 1, control = list(family = family))))
}

test_that("test_parameter() p-values are uniform under the hypothesis", {
  skip_unless_slow("a calibration")
  set.seed(31)
  p <- replicate(100, {
    test_parameter(param_fit(param_net()), "alter_B", times = 39)$pgteobs
  })
  expect_gt(mean(p <= 0.05), 0.01)
  expect_lt(mean(p <= 0.05), 0.12)
  expect_gt(mean(p), 0.4)
  expect_lt(mean(p), 0.6)
})

test_that("test_parameter() rejects a coefficient that is not the value", {
  skip_unless_slow("a calibration")
  set.seed(32)
  p <- replicate(30, {
    test_parameter(param_fit(param_net(b_alter_B = 0.5)), "alter_B",
                   times = 39)$pgteobs
  })
  expect_gt(mean(p <= 0.05), 0.6)
})

test_that("test_parameter() tests a value other than 0 in a gaussian model", {
  # The sender's Age has a coefficient of 0.05 in this network
  fit <- suppressMessages(estimate_mrqap(. ~ ego(Age) + alter(Age),
                                         qap_net_gaussian(), times = 5))
  at <- test_parameter(fit, "ego_Age", value = 0.05, times = 99,
                       control = list(seed = 1))
  zero <- test_parameter(fit, "ego_Age", times = 99, control = list(seed = 1))
  expect_gt(at$pgteobs, 0.05)
  expect_lt(zero$pgteobs, 0.05)
  expect_equal(at$value, 0.05)
  expect_s3_class(at, "network_test")
  expect_equal(tidy(at)$p.upper, at$pgteobs)
})

test_that("test_parameter() tests the levels of an attribute together", {
  set.seed(33)
  fit <- param_fit(param_net(n = 12), . ~ ego(Age) + alter(B) + ego(Grp))
  res <- test_parameter(fit, "ego_Grp", times = 39, control = list(seed = 1))
  expect_equal(res$terms, c("ego_Grp_b", "ego_Grp_c"))
  expect_true(all(res$testdist >= -1e-8))
  both <- test_parameter(fit, c("ego_Age", "alter_B"), times = 39,
                         control = list(seed = 1))
  expect_equal(both$terms, c("ego_Age", "alter_B"))
})

test_that("test_parameter() agrees with estimate_mrqap() on one coefficient", {
  skip_unless_slow("a calibration")
  # With one coefficient, the joint test is the engine's double
  # semi-partialling, with the deviance in place of the t-statistic.
  set.seed(34)
  res <- t(replicate(25, {
    fit <- suppressWarnings(suppressMessages(
      estimate_mrqap(. ~ ego(Age) + alter(B), param_net(), times = 99)))
    c(fit$abs["perm_t", "alter_B"],
      test_parameter(fit, "alter_B", times = 99)$pgteobs)
  }))
  expect_gt(stats::cor(res[, 1], res[, 2]), 0.9)
})

test_that("test_parameter() completes every draw of a binomial model", {
  set.seed(35)
  fit <- param_fit(param_net(n = 12, family = "binomial"), family = "binomial")
  res <- test_parameter(fit, "ego_Age", times = 39, control = list(seed = 1))
  expect_s3_class(res, "network_test")
  expect_equal(res$failed, 0)
})

test_that("test_parameter() keeps the dyads of the model in every shape", {
  u <- suppressMessages(estimate_mrqap(. ~ diff(Age), qap_net_undirected(n = 12),
                                       times = 5))
  expect_s3_class(test_parameter(u, "diff_Age", times = 19), "network_test")
  tw <- suppressMessages(estimate_mrqap(. ~ ego(Att), qap_net_twomode_wide(),
                                        times = 5))
  expect_s3_class(test_parameter(tw, "ego_Att", times = 19), "network_test")
  # A row and a cell unobserved: a draw that moved them would stop with a bug
  g <- qap_net_gaussian(n = 12)
  m <- manynet::as_matrix(g)
  m[2, ] <- NA
  m[5, 7] <- NA
  gm <- manynet::mutate_nodes(manynet::as_tidygraph(m),
                              Age = manynet::node_attribute(g, "Age"))
  mm <- suppressMessages(estimate_mrqap(. ~ ego(Age) + alter(Age), gm, times = 5))
  expect_s3_class(test_parameter(mm, "alter_Age", times = 19), "network_test")
  l <- suppressMessages(estimate_mrqap(. ~ ego(Age), list(qap_net_binary(n = 10),
                                       qap_net_binary(n = 12, seed = 3)), times = 5))
  expect_s3_class(test_parameter(l, "ego_Age", times = 19), "network_test")
  fe <- suppressMessages(estimate_mrqap(. ~ alter(Age) | sender,
                                        qap_net_gaussian(n = 12), times = 5))
  expect_lt(test_parameter(fe, "alter_Age", times = 39,
                           control = list(seed = 1))$pgteobs, 0.05)
})

test_that("test_time() p-values are uniform when the coefficients are constant", {
  skip_unless_slow("a calibration")
  set.seed(36)
  p <- replicate(40, {
    fit <- suppressWarnings(suppressMessages(
      estimate_mrqap(. ~ ego(age) + recip, param_panel(), times = 1)))
    test_time(fit, times = 39)$pgteobs
  })
  expect_lt(mean(p <= 0.05), 0.15)
  expect_gt(mean(p), 0.38)
  expect_lt(mean(p), 0.62)
})

test_that("test_time() finds a coefficient that changes between waves", {
  set.seed(37)
  fit <- suppressMessages(estimate_mrqap(. ~ ego(age) + recip,
                                         param_panel(b = c(0.08, -0.04)), times = 5))
  res <- test_time(fit, "ego_age", times = 99, control = list(seed = 1))
  expect_lt(res$pgteobs, 0.05)
  expect_equal(res$terms, "ego_age:wave2")
  expect_equal(test_time(fit, times = 19)$terms,
               c("ego_age:wave2", "recip:wave2"))
})

# The same on two workers is in test-parallel.R.
test_that("test_parameter() reproduces with a seed", {
  fit <- suppressMessages(estimate_mrqap(. ~ ego(Age) + alter(Age) + ego(Grp),
                                         qap_net_gaussian(n = 12), times = 5))
  one <- test_parameter(fit, "ego_Grp", times = 20, control = list(seed = 3))
  two <- test_parameter(fit, "ego_Grp", times = 20, control = list(seed = 3))
  expect_equal(one, two)
})

test_that("test_parameter() and test_time() refuse what they cannot test", {
  fit <- suppressMessages(estimate_mrqap(. ~ ego(Age) + alter(Age),
                                         qap_net_gaussian(n = 12), times = 5))
  expect_error(test_parameter(fit), "terms")
  expect_error(test_parameter(fit, "Age"), "not a coefficient")
  expect_error(test_parameter(fit, "(Intercept)"), "intercept")
  expect_error(test_parameter(fit, "ego_Age", value = 1:3), "one number")
  expect_error(test_time(fit), "one network")
  bin <- suppressMessages(estimate_mrqap(. ~ ego(Age), qap_net_binary(n = 12),
                                         times = 5))
  expect_error(test_parameter(bin, "ego_Age", value = 1), "only in a gaussian")
  l <- suppressMessages(estimate_mrqap(. ~ ego(Age), list(qap_net_binary(n = 10),
                                       qap_net_binary(n = 12, seed = 3)), times = 5))
  expect_error(test_time(l), "not a panel")
  two <- suppressMessages(estimate_mrqap(. ~ ego(age), qap_net_panel(waves = 2),
                                         times = 5))
  expect_error(test_time(two), "two fitted waves")
  css <- fit
  class(css) <- c("netreg_css", class(fit))
  expect_error(test_parameter(css, "ego_Age"), "cognitive")
  rnd <- fit
  rnd$random <- c(sender = TRUE)
  expect_error(test_parameter(rnd, "ego_Age"), "random intercepts")
  nb <- fit
  nb$family <- "negbin"
  expect_error(test_parameter(nb, "ego_Age"), "negbin")
  fe <- bin
  fe$fixed_effects <- "sender"
  expect_error(test_parameter(fe, "ego_Age"), "fixed effects")
})
