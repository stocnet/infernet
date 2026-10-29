# The results are built by hand, so that each field is known and small.
# The fits through estimate_mrqap(), and the methods of a netreg, are in the
# last blocks.

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

# ---- the stored null distribution, testdist ---------------------------------------

# Every fit keeps its permuted test statistics as `testdist`: one row per
# permutation and one column per coefficient. The p-value shares are summaries of
# this matrix, so the first contract is that the shares can be read back from it.
# The test family keeps its null distribution under the same name.

FORM_TD <- weight ~ ego(Age) + alter(Age) + sim(Age)
COEFS_TD <- c("(Intercept)", "ego_Age", "alter_Age", "sim_Age")

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
  fit <- estimate_mrqap(FORM_TD, qap_net_gaussian(), times = 30,
                        control = list(seed = 1, permute = "outcome"))
  expect_true(is.matrix(fit$testdist))
  expect_equal(dim(fit$testdist), c(30L, 4L))
  expect_equal(colnames(fit$testdist), COEFS_TD)
  expect_false(anyNA(fit$testdist))
})

test_that("the p-values can be read back from testdist when the outcome is permuted", {
  fit <- estimate_mrqap(FORM_TD, qap_net_gaussian(), times = 40,
                        control = list(seed = 2, permute = "outcome"))
  s <- shares_from_testdist(fit)
  expect_equal(unname(s["lower", ]),  unname(fit$lower["perm_t", ]))
  expect_equal(unname(s["larger", ]), unname(fit$larger["perm_t", ]))
  expect_equal(unname(s["abs", ]),    unname(fit$abs["perm_t", ]))
})

test_that("the p-values can be read back from testdist when predictors are permuted", {
  fit <- estimate_mrqap(FORM_TD, qap_net_gaussian(), times = 40,
                        control = list(seed = 3, permute = "predictor"))
  # The intercept is not permuted, so its column is NA, as its p-values are.
  expect_true(all(is.na(fit$testdist[, "(Intercept)"])))
  expect_false(anyNA(fit$testdist[, -1]))
  s <- shares_from_testdist(fit)
  expect_equal(unname(s["abs", -1]), unname(fit$abs["perm_t", -1]))
  expect_equal(unname(s["lower", -1]), unname(fit$lower["perm_t", -1]))
})

test_that("testdist reproduces under the same seed", {
  a <- estimate_mrqap(FORM_TD, qap_net_gaussian(), times = 20, control = list(seed = 9))
  b <- estimate_mrqap(FORM_TD, qap_net_gaussian(), times = 20, control = list(seed = 9))
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

# ---- the results of other packages ---------------------------------------------

test_that("tidy() and glance() read an ergm fit", {
  skip_if_not_installed("ergm")
  g <- manynet::as_network(manynet::to_uniplex(formula_net(), "a"))
  fit <- suppressMessages(ergm::ergm(g ~ edges + nodematch("grp")))
  out <- tidy(fit, conf.int = TRUE, conf.level = 0.9)
  expect_named(out, c("term", "estimate", "std.error", "mcmc.error",
                      "statistic", "p.value", "conf.low", "conf.high"))
  expect_equal(out$estimate, unname(stats::coef(fit)))
  expect_equal(out$conf.low,
               out$estimate - stats::qnorm(0.95) * out$std.error)
  # An ERGM's coefficients are log-odds, so exponentiating them gives odds
  # ratios, without a warning.
  expect_no_warning(odds <- tidy(fit, exponentiate = TRUE))
  expect_equal(odds$estimate, unname(exp(stats::coef(fit))))
  gl <- glance(fit, deviance = TRUE)
  expect_equal(gl$logLik, as.numeric(stats::logLik(fit)))
  expect_equal(gl$residual.deviance, -2 * gl$logLik)
  expect_equal(gl$df.residual, stats::nobs(fit) - 2)
  # A fit that MPLE found exactly has no MCMC to report.
  expect_false("MCMC.interval" %in% names(suppressMessages(glance(fit, mcmc = TRUE))))
})

test_that("glance() of a netlm gives the fit that summary.lm() gives", {
  set.seed(1)
  d <- data.frame(y = stats::rnorm(30), x = stats::rnorm(30))
  fit <- stats::lm(y ~ x, d)
  s <- summary(fit)
  x <- fit
  x$n <- 30
  x$intercept <- TRUE
  class(x) <- "netlm"
  out <- glance(x)
  expect_equal(out$r.squared, s$r.squared)
  expect_equal(out$adj.r.squared, s$adj.r.squared)
  expect_equal(out$sigma, s$sigma)
  expect_equal(out$statistic, unname(s$fstatistic["value"]))
  expect_equal(out$df.residual, 28L)
  expect_equal(out$nobs, 30)
})

test_that("tidy() of a netlogit gives its null interval and its odds ratios", {
  x <- structure(list(names = c("(intercept)", "x"),
                      coefficients = c(0, log(2)), tstat = c(0.5, 3),
                      pgreqabs = c(0.6, 0.05),
                      dist = cbind(seq(-1, 1, length.out = 101),
                                   seq(-2, 2, length.out = 101))),
                 class = "netlogit")
  out <- tidy(x, conf.int = TRUE, conf.level = 0.5, exponentiate = TRUE)
  expect_equal(out$estimate, c(1, 2))
  expect_equal(out$conf.low, exp(c(-0.5, -1)))
  expect_equal(out$conf.high, exp(c(0.5, 1)))
})

# ---- the methods of a netreg ----------------------------------------------------
#
# Each method reads the observed model, so each must give the value of the
# equivalent standard fit on the same dyad-level data, from
# `qap_reference_data()`.

FORM_M <- weight ~ ego(Age) + alter(Age) + sim(Age)
FORM_MB <- . ~ ego(Age) + alter(Age) + sim(Age)

# ---- gaussian and glm -------------------------------------------------------

test_that("a gaussian fit gives the accessors of lm()", {
  g <- qap_net_gaussian()
  ref <- qap_reference_data(FORM_M, g)
  lm_fit <- stats::lm(ref$formula, data = ref$pred)
  fit <- estimate_mrqap(FORM_M, g, times = 5, control = list(seed = 1))

  expect_equal(unname(stats::coef(fit)), unname(stats::coef(lm_fit)))
  expect_equal(stats::nobs(fit), stats::nobs(lm_fit))
  expect_equal(stats::logLik(fit), stats::logLik(lm_fit), ignore_attr = "class")
  expect_equal(attr(stats::logLik(fit), "df"), attr(stats::logLik(lm_fit), "df"))
  expect_equal(stats::AIC(fit), stats::AIC(lm_fit))
  expect_equal(stats::BIC(fit), stats::BIC(lm_fit))
  expect_equal(stats::deviance(fit), stats::deviance(lm_fit))
  expect_equal(stats::fitted(fit), unname(stats::fitted(lm_fit)))
  expect_equal(stats::residuals(fit), unname(stats::residuals(lm_fit)))
  expect_equal(glance(fit)$sigma, stats::sigma(lm_fit))
})

test_that("a binomial and a poisson fit give the accessors of glm()", {
  cases <- list(binomial = list(g = qap_net_binary(), family = "binomial"),
                poisson  = list(g = qap_net_count(),  family = "poisson"))
  for (case in cases) {
    ref <- qap_reference_data(FORM_MB, case$g)
    glm_fit <- stats::glm(ref$formula, data = ref$pred,
                          family = get(case$family, mode = "function"))
    fit <- estimate_mrqap(FORM_MB, case$g, times = 5,
                          control = list(seed = 1, family = case$family))
    expect_equal(unname(stats::coef(fit)), unname(stats::coef(glm_fit)))
    expect_equal(stats::nobs(fit), stats::nobs(glm_fit))
    expect_equal(stats::logLik(fit), stats::logLik(glm_fit), ignore_attr = "class")
    expect_equal(stats::AIC(fit), stats::AIC(glm_fit))
    expect_equal(stats::deviance(fit), stats::deviance(glm_fit))
    expect_equal(stats::fitted(fit), unname(stats::fitted(glm_fit)))
    expect_equal(stats::residuals(fit), unname(stats::residuals(glm_fit)))
    expect_equal(stats::residuals(fit, type = "pearson"),
                 unname(stats::residuals(glm_fit, type = "pearson")))
  }
})

# ---- fixed effects and fixed ties -------------------------------------------

test_that("gaussian fixed effects give the likelihood of lm() with a dummy for each group", {
  g <- qap_net_gaussian()
  ref <- qap_reference_data(weight ~ diff(Age) + same(Grp), g)
  dummies <- stats::lm(weight ~ diff_Age + same_Grp + factor(sv) + factor(rv),
                       data = ref$pred)
  fit <- estimate_mrqap(weight ~ diff(Age) + same(Grp) | sender + receiver, g,
                        times = 5, control = list(seed = 1))
  expect_equal(stats::nobs(fit), stats::nobs(dummies))
  expect_equal(stats::logLik(fit), stats::logLik(dummies), ignore_attr = "class")
  expect_equal(attr(stats::logLik(fit), "df"), attr(stats::logLik(dummies), "df"))
  expect_equal(stats::AIC(fit), stats::AIC(dummies))
  expect_equal(stats::deviance(fit), stats::deviance(dummies))
  expect_equal(stats::residuals(fit), unname(stats::residuals(dummies)))
  expect_equal(stats::fitted(fit), unname(stats::fitted(dummies)))
  expect_equal(glance(fit)$df.residual, stats::df.residual(dummies))
  expect_equal(glance(fit)$sigma, stats::sigma(dummies))
  expect_equal(glance(fit)$AIC, stats::AIC(dummies))
})

test_that("nobs() leaves out the fixed ties, and glance() agrees", {
  g <- fixed_net()
  fit <- suppressWarnings(estimate_mrqap(. ~ ego(Age), g, times = 5,
                                         control = list(seed = 1)))
  n <- as.numeric(manynet::net_nodes(g))
  expect_equal(fit$missing$fixed_ties, 6)
  expect_equal(stats::nobs(fit), n * (n - 1) - 6)
  expect_equal(stats::nobs(fit), fit$missing$used)
  expect_equal(glance(fit)$nobs, stats::nobs(fit))
})

# ---- less_mem ------------------------------------------------------------------

test_that("the methods read the observed model when less_mem leaves out simple_fit", {
  g <- qap_net_gaussian()
  full <- estimate_mrqap(FORM_M, g, times = 5, control = list(seed = 1))
  lean <- estimate_mrqap(FORM_M, g, times = 5,
                         control = list(seed = 1, less_mem = TRUE))
  expect_null(lean$simple_fit)
  expect_equal(stats::nobs(lean), stats::nobs(full))
  expect_equal(stats::logLik(lean), stats::logLik(full))
  expect_equal(stats::fitted(lean), stats::fitted(full))
  expect_equal(stats::residuals(lean), stats::residuals(full))
})

test_that("a result without an observed model or a formula says how to get one", {
  fit <- estimate_mrqap(FORM_M, qap_net_gaussian(), times = 5,
                        control = list(seed = 1))
  fit$simple_fit <- NULL
  fit$base$base_model <- NULL
  expect_error(stats::logLik(fit), "keeps no observed model")
  expect_true(is.na(stats::nobs(fit)) || stats::nobs(fit) == nrow(fit$pred))
  fit$formula <- NULL
  fit$model_formula <- NULL
  expect_error(stats::formula(fit), "earlier version")
  expect_error(stats::terms(fit), "earlier version")
})

# ---- formula, terms, and update -----------------------------------------------

test_that("formula() gives the formula as read, and terms() the fitted columns", {
  g <- qap_net_gaussian()
  ref <- qap_reference_data(FORM_M, g)
  fit <- estimate_mrqap(FORM_M, g, times = 5, control = list(seed = 1))
  expect_equal(stats::formula(fit), FORM_M, ignore_attr = TRUE)
  expect_s3_class(stats::formula(fit), "formula")
  expect_null(attr(stats::formula(fit), "record"))
  tt <- stats::terms(fit)
  expect_s3_class(tt, "terms")
  expect_equal(attr(tt, "term.labels"), c("ego_Age", "alter_Age", "sim_Age"))
  mf <- stats::model.frame(tt, data = fit$pred)
  expect_equal(nrow(mf), stats::nobs(fit))
  expect_equal(unname(stats::coef(stats::lm(tt, data = fit$pred))),
               unname(stats::coef(fit)))
})

test_that("update() fits the model again with a changed formula or times", {
  g <- qap_net_gaussian()
  fit <- estimate_mrqap(FORM_M, g, times = 5, control = list(seed = 1))
  again <- stats::update(fit)
  expect_equal(again$coefficients, fit$coefficients)
  expect_equal(again$abs, fit$abs)
  smaller <- stats::update(fit, . ~ . - sim(Age))
  expect_named(smaller$coefficients, c("(Intercept)", "ego_Age", "alter_Age"))
  expect_equal(stats::update(fit, times = 7)$times, 7)
})

# ---- tidy and summary -------------------------------------------------------------

test_that("tidy() gives the generics columns, and summary() adds the term table", {
  fit <- suppressMessages(
    estimate_mrqap(a ~ ego(x) + same(grp, diff = TRUE), formula_net(),
                   times = 5, control = list(seed = 1)))
  expect_named(tidy(fit), c("term", "estimate", "statistic", "p.value"))
  out <- summary(fit)
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("term", "estimate", "statistic", "p.value", "effect",
                      "layer", "attribute", "category", "reference", "operands"))
  expect_equal(out$effect, c("intercept", "ego", "same", "same"))
  expect_equal(out$category, c(NA, NA, "p", "q"))
  expect_equal(summary(fit, alternative = "greater")$p.value,
               unname(fit$larger[2, ]))
  # A fit made before the table was kept is summarised without it.
  fit$terms <- NULL
  expect_named(summary(fit), c("term", "estimate", "statistic", "p.value"))
})
