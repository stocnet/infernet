# `formula()` and `terms()` of an MRQAP fit. The print and `tidy()` of a
# specification are in test-specify_record.R and test-report_print.R.

FORM_M <- weight ~ ego(Age) + alter(Age) + sim(Age)

test_that("formula() gives the formula as read, and terms() the fitted columns", {
  g <- qap_net_gaussian()
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

test_that("a result without a formula says how to get one", {
  fit <- estimate_mrqap(FORM_M, qap_net_gaussian(), times = 5,
                        control = list(seed = 1))
  fit$formula <- NULL
  fit$model_formula <- NULL
  expect_error(stats::formula(fit), "earlier version")
  expect_error(stats::terms(fit), "earlier version")
})
