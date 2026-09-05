# `control` is merged over the defaults by name, so a name that is not a control
# would be added silently and the option it was meant to set would keep its
# default, with nothing to say so.

FORM <- weight ~ ego(Age) + alter(Age)

test_that("an unknown control name is rejected, with the nearest match", {
  g <- qap_net_gaussian(n = 15)
  expect_error(
    net_regression(FORM, g, times = 5, control = list(strateggy = "sequential")),
    "strategy")
  expect_error(
    net_regression(FORM, g, times = 5, control = list(nonsense = 1)),
    "nonsense")
})

test_that("an unnamed control entry is rejected", {
  g <- qap_net_gaussian(n = 15)
  expect_error(
    net_regression(FORM, g, times = 5, control = list("sequential")),
    "named")
})

test_that("an empty control list gives the defaults", {
  expect_equal(.resolve_control(list()), .resolve_control())
  expect_equal(.resolve_control()$permute, "predictor")
  expect_equal(.resolve_control()$strategy, "sequential")
  expect_equal(.resolve_control()$family, "auto")
})

test_that("a named control overrides only that default", {
  ctrl <- .resolve_control(list(family = "poisson"))
  expect_equal(ctrl$family, "poisson")
  expect_equal(ctrl$strategy, "sequential")
  expect_null(ctrl$directed)
})

test_that("permute takes only the two spellings it documents", {
  expect_equal(.resolve_control(list(permute = "outcome"))$permute, "outcome")
  expect_error(.resolve_control(list(permute = "qapy")))
})

test_that("permute = 'outcome' is recorded on the fit and gives a full matrix", {
  g <- qap_net_gaussian(n = 20)
  fit <- net_regression(FORM, g, times = 10,
                        control = list(seed = 1, permute = "outcome"))
  expect_equal(fit$permute, "outcome")
  # Permuting y alone tests every coefficient, the intercept included, whereas
  # double semi-partialling residualises one predictor at a time.
  expect_false(anyNA(fit$lower))
  spp <- net_regression(FORM, g, times = 10, control = list(seed = 1))
  expect_equal(spp$permute, "predictor")
  expect_true(all(is.na(spp$lower[, "(Intercept)"])))
})

test_that("a single predictor falls back to permuting the outcome", {
  g <- qap_net_gaussian(n = 20)
  fit <- net_regression(weight ~ ego(Age), g, times = 10,
                        control = list(seed = 1, permute = "predictor"))
  # Double semi-partialling residualises a predictor against the others, and
  # with one predictor there are none.
  expect_equal(fit$permute, "outcome")
})

test_that("directed and diag are read from the network unless set", {
  g <- qap_net_gaussian(n = 15)
  expect_true(net_regression(FORM, g, times = 5,
                             control = list(seed = 1))$directed)
  expect_false(net_regression(FORM, g, times = 5,
                              control = list(seed = 1,
                                             directed = FALSE))$directed)
  loops <- net_regression(FORM, g, times = 5,
                          control = list(seed = 1, diag = TRUE))
  expect_true(loops$diag)
  expect_equal(nrow(loops$pred), 15 * 15)
})
