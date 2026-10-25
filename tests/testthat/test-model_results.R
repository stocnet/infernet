# The results are built by hand, so that each field is known and small.
# The fits through estimate_mrqap() are in the last block.

fake_netreg <- function(family = "gaussian", permute = "predictor") {
  y <- c(1.2, 0.4, 2.8, 1.9, 3.1, 0.7, 2.2, 1.5)
  z <- c(0.5, 0.1, 1.6, 0.9, 1.8, 0.2, 1.0, 0.8)
  fit <- stats::lm(y ~ z)
  structure(list(
    coefficients = c("(Intercept)" = 0.1, "ego_x" = 1.5),
    obs = c("(Intercept)" = 0.4, "ego_x" = 3.2),
    lower = rbind(c(NA, 0.9), c(NA, 0.95)),
    larger = rbind(c(NA, 0.1), c(NA, 0.05)),
    abs = rbind(c(NA, 0.2), c(NA, 0.1)),
    r.squared = 0.6, adj.r.squared = 0.55,
    simple_fit = fit, family = family, permute = permute, times = 20,
    pred = data.frame(y = y, z = z)),
    class = c("netreg_lm", "netreg"))
}

test_that("tidy() reads a netreg's t-based p-values", {
  x <- fake_netreg()
  out <- tidy(x)
  expect_named(out, c("term", "estimate", "statistic", "p.value"))
  expect_equal(out$term, c("(Intercept)", "ego_x"))
  expect_equal(out$p.value, c(NA, 0.1))
  expect_equal(tidy(x, alternative = "greater")$p.value, c(NA, 0.05))
  expect_equal(tidy(x, alternative = "less")$p.value, c(NA, 0.95))
})

test_that("tidy() exponentiates a netreg, and warns on a gaussian one", {
  x <- fake_netreg()
  expect_warning(out <- tidy(x, exponentiate = TRUE), "gaussian")
  expect_equal(out$estimate, exp(c(0.1, 1.5)))
  x$family <- "binomial"
  expect_no_warning(tidy(x, exponentiate = TRUE))
})

test_that("glance() gives the fit of a netreg_lm", {
  out <- glance(fake_netreg())
  expect_equal(nrow(out), 1)
  expect_equal(out$r.squared, 0.6)
  expect_equal(out$nobs, 8)
  expect_equal(out$AIC, stats::AIC(fake_netreg()$simple_fit))
})

test_that("glance() gives NA where a netreg keeps no fit", {
  x <- fake_netreg()
  x$simple_fit <- NULL
  out <- glance(x)
  expect_true(is.na(out$AIC))
  expect_true(is.na(out$sigma))
})

test_that("glance() of a netreg_glm reads its glm fit", {
  x <- fake_netreg(family = "binomial")
  y <- c(1, 0, 1, 1, 0, 0, 1, 0)
  z <- c(0.5, 0.1, 1.6, 0.9, 1.8, 0.2, 1.0, 0.8)
  x$simple_fit <- stats::glm(y ~ z, family = stats::binomial)
  class(x) <- c("netreg_glm", "netreg")
  out <- glance(x)
  expect_equal(out$deviance, stats::deviance(x$simple_fit))
  expect_equal(out$family, "binomial")
})

test_that("glance() of a netreg_css counts the reporters", {
  x <- fake_netreg()
  x$pred$bv <- rep(1:4, 2)
  class(x) <- c("netreg_css", "netreg_lm", "netreg")
  out <- glance(x)
  expect_equal(out$reporters, 4)
  expect_equal(out$r.squared, 0.6)
})

test_that("tidy() and glance() give one row for a network_test", {
  null <- c(0.2, 0.3, 0.4, 0.5)
  x <- structure(list(test = "QAP", testval = 0.45, testdist = null,
                      plteobs = 0.75, pgteobs = 0.25, times = 4),
                 class = "network_test")
  out <- tidy(x)
  expect_equal(nrow(out), 1)
  expect_equal(out$null.mean, mean(null))
  expect_equal(out$p.upper, 0.25)
  expect_equal(glance(x), out)
})

test_that("tidy() of a netlm uses conf.level for its null interval", {
  x <- structure(list(names = c("(intercept)", "x"),
                      coefficients = c(1, 2), tstat = c(0.5, 3),
                      pgreqabs = c(0.6, 0.05),
                      dist = cbind(seq(-1, 1, length.out = 101),
                                   seq(-2, 2, length.out = 101))),
                 class = "netlm")
  out <- tidy(x, conf.int = TRUE, conf.level = 0.5)
  expect_equal(out$conf.low, c(-0.5, -1))
  expect_equal(out$conf.high, c(0.5, 1))
})

test_that("tidy() and glance() read the fits of estimate_mrqap()", {
  fit <- estimate_mrqap(weight ~ ego(Age) + alter(Age), qap_net_gaussian(),
                        times = 5, control = list(seed = 1))
  out <- tidy(fit)
  expect_equal(out$estimate, unname(fit$coefficients))
  expect_equal(out$p.value[-1], unname(fit$abs[2, -1]))
  expect_equal(glance(fit)$r.squared, fit$r.squared)
  css <- suppressMessages(
    estimate_mrqap(advice ~ same(dept) + reports, manynet::ison_hightech,
                   times = 5, control = list(seed = 1)))
  expect_equal(glance(css)$reporters, 21)
})
