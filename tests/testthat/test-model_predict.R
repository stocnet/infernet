# `predict()` of a `netreg` gives, without `newdata`, the fitted values of the
# equivalent standard fit on the same dyads, and with `newdata`, the values
# from the coefficients. Each family is checked against its standard fit.

FORM_P <- weight ~ ego(Age) + alter(Age) + sim(Age)

fit_quietly <- function(formula, .data, ...) {
  suppressMessages(suppressWarnings(
    estimate_mrqap(formula, .data, times = 5, control = list(seed = 1, ...))))
}

test_that("a gaussian model predicts the fitted values of lm()", {
  g <- qap_net_gaussian()
  fit <- fit_quietly(FORM_P, g)
  ref <- qap_reference_data(FORM_P, g)
  lm_fit <- stats::lm(ref$formula, data = ref$pred)
  expect_equal(predict(fit), unname(stats::fitted(lm_fit)))
  expect_equal(predict(fit, type = "response"), predict(fit))
  expect_equal(predict(fit, fit$pred), predict(fit))
})

test_that("a binomial model predicts on both scales, as glm() does", {
  g <- qap_net_binary()
  fit <- fit_quietly(FORM_P, g, family = "binomial")
  ref <- qap_reference_data(FORM_P, g)
  glm_fit <- stats::glm(ref$formula, data = ref$pred, family = stats::binomial())
  expect_equal(predict(fit), unname(stats::predict(glm_fit, type = "link")))
  expect_equal(predict(fit, type = "response"), unname(stats::fitted(glm_fit)))
  expect_equal(predict(fit, fit$pred, type = "response"),
               unname(stats::fitted(glm_fit)))
})

test_that("a Poisson model predicts expected counts, as glm() does", {
  g <- qap_net_count()
  fit <- fit_quietly(FORM_P, g, family = "poisson")
  ref <- qap_reference_data(FORM_P, g)
  glm_fit <- stats::glm(ref$formula, data = ref$pred, family = stats::poisson())
  expect_equal(predict(fit, type = "response"), unname(stats::fitted(glm_fit)))
  expect_equal(predict(fit, fit$pred, type = "response"),
               unname(stats::fitted(glm_fit)))
})

test_that("a negative binomial model predicts as MASS::glm.nb() does", {
  skip_if_not_installed("MASS")
  g <- qap_net_count()
  fit <- fit_quietly(FORM_P, g, family = "negbin")
  ref <- qap_reference_data(FORM_P, g)
  nb <- suppressWarnings(MASS::glm.nb(ref$formula, data = ref$pred))
  expect_equal(predict(fit, type = "response"), unname(stats::fitted(nb)),
               tolerance = 1e-6)
  expect_equal(predict(fit, fit$pred, type = "response"),
               predict(fit, type = "response"), tolerance = 1e-6)
})

test_that("a zero-inflated model predicts as pscl::zeroinfl() does", {
  skip_if_not_installed("pscl")
  g <- qap_net_zip()
  fit <- fit_quietly(FORM_P, g, family = "zip")
  ref <- qap_reference_data(FORM_P, g)
  zi <- pscl::zeroinfl(ref$formula, data = ref$pred, dist = "poisson")
  expected <- unname(stats::predict(zi, type = "response"))
  expect_equal(predict(fit, type = "response"), expected, tolerance = 1e-6)
  expect_equal(predict(fit, fit$pred, type = "response"), expected,
               tolerance = 1e-6)
  expect_equal(predict(fit),
               log(unname(stats::predict(zi, type = "count"))), tolerance = 1e-6)
})

test_that("random intercepts enter the fitted values, and not newdata", {
  skip_if_not_installed("lme4")
  g <- qap_net_gaussian()
  fit <- fit_quietly(FORM_P, g, random_intercept_sender = TRUE)
  bm <- fit$base$base_model
  expect_equal(predict(fit), unname(stats::fitted(bm)))
  expect_equal(predict(fit, fit$pred),
               unname(stats::predict(bm, re.form = NA)))
})

test_that("gaussian fixed effects predict the fitted values of lm() with dummies", {
  g <- qap_net_gaussian()
  form <- weight ~ diff(Age) + same(Grp) | sender + receiver
  fit <- fit_quietly(form, g)
  dummies <- stats::lm(weight ~ diff_Age + same_Grp + factor(sv) + factor(rv),
                       data = fit$pred)
  expect_equal(predict(fit), unname(stats::fitted(dummies)))
  expect_error(predict(fit, fit$pred), "fixed effects")
})

test_that("binomial fixed effects predict as fixest::feglm() does", {
  skip_if_not_installed("fixest")
  g <- qap_net_binary()
  fit <- fit_quietly(. ~ diff(Age) + same(Grp) | sender + receiver, g,
                     family = "binomial")
  fe_form <- stats::as.formula(paste(fit$dep, "~ diff_Age + same_Grp | sv + rv"))
  fe <- fixest::feglm(fe_form, data = fit$pred,
                      family = stats::binomial(), notes = FALSE)
  expect_equal(predict(fit, type = "response"),
               unname(stats::predict(fe, type = "response")), tolerance = 1e-6)
})

test_that("newdata is read by name, by position, or by the parts of an interaction", {
  g <- qap_net_gaussian()
  fit <- fit_quietly(weight ~ ego(Age) * same(Grp), g)
  b <- fit$coefficients
  one <- data.frame(ego_Age = 30, same_Grp = 1)
  expect_equal(unname(predict(fit, one)), unname(b[1] + 30 * b[2] + b[3] + 30 * b[4]))
  expect_equal(predict(fit, matrix(c(1, 30, 1, 30), 1)), predict(fit, one))
  expect_error(predict(fit, matrix(c(1, 30), 1)), "no names")
  expect_error(predict(fit, data.frame(ego_Age = 30)), "same_Grp")
})

test_that("a netlm and a netlogit from migraph predict from their design matrix", {
  b <- c(0.5, 2, -1)
  x <- matrix(c(1, 1, 1, 0, 2, 1), 2)
  lm_res <- structure(list(coefficients = b), class = "netlm")
  logit_res <- structure(list(coefficients = b), class = "netlogit")
  expect_equal(predict(lm_res, x), drop(x %*% b))
  expect_equal(predict(logit_res, x), drop(x %*% b))
  expect_equal(predict(logit_res, x, type = "response"), stats::plogis(drop(x %*% b)))
  expect_error(predict(lm_res), "newdata")
})
