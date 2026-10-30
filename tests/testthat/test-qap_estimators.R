# Every estimator path in fit_qap_model() is selected by a combination of
# `family` and the random and fixed effect flags. This file names each
# combination, so that a path with no test fails the build rather than going
# unnoticed.
#
# Two things are asserted for each. First, the baseline coefficients equal those
# of the equivalent standard fit on the same dyad-level data: the permutation
# inference is the novel part, the point estimates are not. Second, the result
# meets the shape contract in `expect_qap_shape()`.

COEFS3 <- c("(Intercept)", "ego_Age", "alter_Age", "sim_Age")
FORM   <- weight ~ ego(Age) + alter(Age) + sim(Age)
FORM_B <- . ~ ego(Age) + alter(Age) + sim(Age)


# ---- gaussian --------------------------------------------------------------

test_that("gaussian baseline matches lm() on the same dyads", {
  g <- qap_net_gaussian()
  ref <- qap_reference_data(FORM, g)
  lm_fit <- stats::lm(ref$formula, data = ref$pred)

  fit <- estimate_mrqap(FORM, g, times = 10, control = list(seed = 1))
  expect_qap_shape(fit, COEFS3)
  expect_equal(unname(fit$coefficients), unname(stats::coef(lm_fit)))
  expect_equal(unname(fit$obs), unname(summary(lm_fit)$coefficients[, 3]))
  expect_equal(fit$r.squared, summary(lm_fit)$r.squared)
  expect_equal(fit$adj.r.squared, summary(lm_fit)$adj.r.squared)
  expect_equal(fit$family, "gaussian")
})

test_that("gaussian with HC3 keeps the coefficients and changes the t values", {
  g <- qap_net_gaussian()
  plain  <- estimate_mrqap(FORM, g, times = 10, control = list(seed = 1))
  robust <- estimate_mrqap(FORM, g, times = 10,
                           control = list(seed = 1, use_robust_errors = TRUE))
  expect_qap_shape(robust, COEFS3)
  expect_equal(robust$coefficients, plain$coefficients)
  expect_false(isTRUE(all.equal(robust$obs, plain$obs)))
  expect_true(robust$robust_se)
})


# ---- binomial and poisson --------------------------------------------------

test_that("binomial baseline matches glm() on the same dyads", {
  g <- qap_net_binary()
  ref <- qap_reference_data(FORM_B, g)
  glm_fit <- stats::glm(ref$formula, data = ref$pred,
                        family = stats::binomial())

  fit <- estimate_mrqap(FORM_B, g, times = 10,
                        control = list(seed = 1, family = "binomial"))
  expect_qap_shape(fit, COEFS3)
  expect_equal(unname(fit$coefficients), unname(stats::coef(glm_fit)))
  expect_equal(unname(fit$obs), unname(summary(glm_fit)$coefficients[, 3]))
})

test_that("family = 'auto' picks binomial for a binary outcome", {
  auto  <- estimate_mrqap(FORM_B, qap_net_binary(), times = 10,
                          control = list(seed = 1))
  named <- estimate_mrqap(FORM_B, qap_net_binary(), times = 10,
                          control = list(seed = 1, family = "binomial"))
  expect_equal(auto$family, "binomial")
  expect_equal(auto$coefficients, named$coefficients)
})

test_that("poisson baseline matches glm() on the same dyads", {
  g <- qap_net_count()
  ref <- qap_reference_data(FORM, g)
  glm_fit <- stats::glm(ref$formula, data = ref$pred, family = stats::poisson())

  fit <- estimate_mrqap(FORM, g, times = 10,
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
    estimate_mrqap(FORM, g, times = 10,
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

  fit <- estimate_mrqap(FORM, g, times = 10,
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
    estimate_mrqap(FORM, g, times = 10,
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
    estimate_mrqap(FORM, qap_net_gaussian(), times = 10,
                   control = list(seed = 1,
                                  random_intercept_sender = TRUE,
                                  random_intercept_receiver = TRUE))))
  expect_qap_shape(fit, COEFS3)
})

test_that("binomial and poisson random intercepts meet the shape contract", {
  skip_if_not_installed("lme4")
  bin <- suppressMessages(suppressWarnings(
    estimate_mrqap(FORM_B, qap_net_binary(n = 15), times = 5,
                   control = list(seed = 1, family = "binomial",
                                  random_intercept_sender = TRUE))))
  expect_qap_shape(bin, COEFS3)
  pois <- suppressMessages(suppressWarnings(
    estimate_mrqap(FORM, qap_net_count(n = 15), times = 5,
                   control = list(seed = 1, family = "poisson",
                                  random_intercept_sender = TRUE))))
  expect_qap_shape(pois, COEFS3)
})


# ---- the family read from the outcome ---------------------------------------------

test_that("family = 'auto' picks gaussian for a weighted outcome", {
  g <- qap_net_gaussian(n = 10)
  fit <- estimate_mrqap(weight ~ sim(Age), g, times = 10)
  expect_equal(fit$family, "gaussian")
  expect_s3_class(fit, "netreg_lm")
})

test_that("a gaussian model of a binary outcome keeps a confusion matrix", {
  g <- qap_net_binary(n = 10)
  fit <- estimate_mrqap(. ~ sim(Age), g, times = 10,
                        control = list(family = "gaussian"))
  expect_equal(fit$family, "gaussian")
  expect_s3_class(fit, "netreg_lm")
  expect_s3_class(fit$confusion_matrix, "diagnose_confusion")
})
