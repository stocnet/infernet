# Every estimator path in fit_qap_model() is selected by a combination of
# `family` and the random and fixed effect flags. This file names each
# combination, so that a path with no test fails the build rather than going
# unnoticed.
#
# Two things are asserted for each. First, the baseline coefficients equal those
# of the equivalent standard fit on the same dyad-level data: the permutation
# inference is the novel part, the point estimates are not. Second, the result
# meets the shape contract in `expect_qap_shape()`.

COEFS3 <- c("(Intercept)", "ego Age", "alter Age", "sim Age")
FORM   <- weight ~ ego(Age) + alter(Age) + sim(Age)
FORM_B <- . ~ ego(Age) + alter(Age) + sim(Age)


# ---- gaussian --------------------------------------------------------------

test_that("gaussian baseline matches lm() on the same dyads", {
  g <- qap_net_gaussian()
  ref <- qap_reference_data(FORM, g)
  lm_fit <- stats::lm(ref$formula, data = ref$pred)

  fit <- net_regression(FORM, g, times = 10, control = list(seed = 1))
  expect_qap_shape(fit, COEFS3)
  expect_equal(unname(fit$coefficients), unname(stats::coef(lm_fit)))
  expect_equal(unname(fit$t), unname(summary(lm_fit)$coefficients[, 3]))
  expect_equal(fit$r.squared, summary(lm_fit)$r.squared)
  expect_equal(fit$adj.r.squared, summary(lm_fit)$adj.r.squared)
  expect_equal(fit$family, "gaussian")
})

test_that("gaussian with HC3 keeps the coefficients and changes the t values", {
  g <- qap_net_gaussian()
  plain  <- net_regression(FORM, g, times = 10, control = list(seed = 1))
  robust <- net_regression(FORM, g, times = 10,
                           control = list(seed = 1, use_robust_errors = TRUE))
  expect_qap_shape(robust, COEFS3)
  expect_equal(robust$coefficients, plain$coefficients)
  expect_false(isTRUE(all.equal(robust$t, plain$t)))
  expect_true(robust$robust_se)
})


# ---- binomial and poisson --------------------------------------------------

test_that("binomial baseline matches glm() on the same dyads", {
  g <- qap_net_binary()
  ref <- qap_reference_data(FORM_B, g)
  glm_fit <- stats::glm(ref$formula, data = ref$pred,
                        family = stats::binomial())

  fit <- net_regression(FORM_B, g, times = 10,
                        control = list(seed = 1, family = "binomial"))
  expect_qap_shape(fit, COEFS3)
  expect_equal(unname(fit$coefficients), unname(stats::coef(glm_fit)))
  expect_equal(unname(fit$t), unname(summary(glm_fit)$coefficients[, 3]))
})

test_that("family = 'auto' picks binomial for a binary outcome", {
  auto  <- net_regression(FORM_B, qap_net_binary(), times = 10,
                          control = list(seed = 1))
  named <- net_regression(FORM_B, qap_net_binary(), times = 10,
                          control = list(seed = 1, family = "binomial"))
  expect_equal(auto$family, "binomial")
  expect_equal(auto$coefficients, named$coefficients)
})

test_that("poisson baseline matches glm() on the same dyads", {
  g <- qap_net_count()
  ref <- qap_reference_data(FORM, g)
  glm_fit <- stats::glm(ref$formula, data = ref$pred, family = stats::poisson())

  fit <- net_regression(FORM, g, times = 10,
                        control = list(seed = 1, family = "poisson"))
  expect_qap_shape(fit, COEFS3)
  expect_equal(unname(fit$coefficients), unname(stats::coef(glm_fit)))
})


# ---- negative binomial and zero-inflated Poisson ---------------------------

test_that("negbin baseline matches MASS::glm.nb() on the same dyads", {
  skip_if_not_installed("MASS")
  g <- qap_net_count()
  ref <- qap_reference_data(FORM, g)
  nb <- suppressWarnings(MASS::glm.nb(ref$formula, data = ref$pred))

  fit <- suppressWarnings(
    net_regression(FORM, g, times = 10,
                   control = list(seed = 1, family = "negbin")))
  expect_qap_shape(fit, COEFS3)
  expect_equal(unname(fit$coefficients), unname(stats::coef(nb)))
  expect_equal(fit$theta, nb$theta)
})

test_that("zip baseline matches pscl::zeroinfl() and names its coefficients", {
  skip_if_not_installed("pscl")
  g <- qap_net_zip()
  ref <- qap_reference_data(FORM, g)
  zi <- pscl::zeroinfl(ref$formula, data = ref$pred, dist = "poisson")

  fit <- net_regression(FORM, g, times = 10,
                        control = list(seed = 1, family = "zip"))
  # A backticked name here used to break double semi-partialling, which looks a
  # column up by the unquoted predictor name.
  expect_qap_shape(fit, COEFS3)
  expect_false(any(grepl("`", names(fit$coefficients), fixed = TRUE)))
  expect_equal(unname(fit$coefficients), unname(zi$coefficients$count))
  expect_equal(unname(fit$zi_coefficients), unname(zi$coefficients$zero))
})


# ---- random effects --------------------------------------------------------

test_that("gaussian random intercepts match lme4::lmer() on the same dyads", {
  skip_if_not_installed("lme4")
  g <- qap_net_gaussian()
  ref <- qap_reference_data(FORM, g)
  mixed <- suppressMessages(suppressWarnings(
    lme4::lmer(build_internal_formula(ref$formula, ris = TRUE),
               data = ref$pred)))

  fit <- suppressMessages(suppressWarnings(
    net_regression(FORM, g, times = 10,
                   control = list(seed = 1, random_intercept_sender = TRUE))))
  expect_qap_shape(fit, COEFS3)
  expect_equal(unname(fit$coefficients),
               unname(summary(mixed)$coefficients[, 1]))
  expect_named(fit$random.intercepts, "sv")
})

test_that("crossed sender and receiver intercepts do not abort the run", {
  skip_if_not_installed("lme4")
  # Residualising a predictor against both intercepts is often singular, and
  # `lmer()` then stops with "Downdated VtV is not positive definite". That is
  # a step towards the null distribution, so it falls back rather than aborting.
  fit <- suppressMessages(suppressWarnings(
    net_regression(FORM, qap_net_gaussian(), times = 10,
                   control = list(seed = 1,
                                  random_intercept_sender = TRUE,
                                  random_intercept_receiver = TRUE))))
  expect_qap_shape(fit, COEFS3)
})

test_that("binomial and poisson random intercepts run", {
  skip_if_not_installed("lme4")
  bin <- suppressMessages(suppressWarnings(
    net_regression(FORM_B, qap_net_binary(), times = 10,
                   control = list(seed = 1, family = "binomial",
                                  random_intercept_sender = TRUE))))
  expect_qap_shape(bin, COEFS3)
  pois <- suppressMessages(suppressWarnings(
    net_regression(FORM, qap_net_count(), times = 10,
                   control = list(seed = 1, family = "poisson",
                                  random_intercept_sender = TRUE))))
  expect_qap_shape(pois, COEFS3)
})


# ---- fixed effects and clustered errors ------------------------------------

test_that("fixest reports one intercept, not two", {
  skip_if_not_installed("fixest")
  # `feglm()` reports an intercept where no fixed effect is absorbed. The engine
  # used to prepend a placeholder regardless, giving two.
  fit <- net_regression(FORM, qap_net_gaussian(), times = 10,
                        control = list(seed = 1, fixest_se_cluster = "sv"))
  expect_qap_shape(fit, COEFS3)
  expect_equal(sum(names(fit$coefficients) == "(Intercept)"), 1L)
  expect_false(anyNA(fit$coefficients))
  expect_equal(length(fit$t), length(fit$coefficients))
})

test_that("fixest coefficients match a direct feglm() fit", {
  skip_if_not_installed("fixest")
  g <- qap_net_gaussian()
  ref <- qap_reference_data(FORM, g)
  fe <- fixest::feglm(ref$formula, data = ref$pred,
                      family = "gaussian", cluster = "sv")

  fit <- net_regression(FORM, g, times = 10,
                        control = list(seed = 1, fixest_se_cluster = "sv"))
  expect_equal(unname(fit$coefficients), unname(fe$coefficients))
})

test_that("fixed effects and random effects together fall back to random", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("lme4")
  both <- suppressMessages(suppressWarnings(
    net_regression(FORM, qap_net_gaussian(), times = 10,
                   control = list(seed = 1, fixest_se_cluster = "sv",
                                  random_intercept_sender = TRUE))))
  random_only <- suppressMessages(suppressWarnings(
    net_regression(FORM, qap_net_gaussian(), times = 10,
                   control = list(seed = 1, random_intercept_sender = TRUE))))
  expect_qap_shape(both, COEFS3)
  # The fixed effects are dropped, so the fit is the random-effects one.
  expect_equal(both$coefficients, random_only$coefficients)
  expect_named(both$random.intercepts, "sv")
})

test_that("combining fixed and random effects warns", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("lme4")
  expect_snet_warning(
    suppressMessages(
      net_regression(FORM, qap_net_gaussian(), times = 10,
                     control = list(seed = 1, fixest_se_cluster = "sv",
                                    random_intercept_sender = TRUE))),
    "random effects")
})

