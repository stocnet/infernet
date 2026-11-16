# `fitted()` and `residuals()` of an MRQAP fit, against the values of the
# equivalent `lm()` or `glm()`. The methods of `diagnose_confusion()` are in
# test-diagnose_confusion.R.

FORM_M <- weight ~ ego(Age) + alter(Age) + sim(Age)
FORM_MB <- . ~ ego(Age) + alter(Age) + sim(Age)

test_that("a gaussian fit gives the fitted values and residuals of lm()", {
  g <- qap_net_gaussian()
  ref <- qap_reference_data(FORM_M, g)
  lm_fit <- stats::lm(ref$formula, data = ref$pred)
  fit <- estimate_mrqap(FORM_M, g, times = 5, control = list(seed = 1))
  expect_equal(stats::fitted(fit), unname(stats::fitted(lm_fit)))
  expect_equal(stats::residuals(fit), unname(stats::residuals(lm_fit)))
})

test_that("a binomial and a poisson fit give the fitted values and residuals of glm()", {
  cases <- list(binomial = list(g = qap_net_binary(), family = "binomial"),
                poisson  = list(g = qap_net_count(),  family = "poisson"))
  for (case in cases) {
    ref <- qap_reference_data(FORM_MB, case$g)
    glm_fit <- stats::glm(ref$formula, data = ref$pred,
                          family = get(case$family, mode = "function"))
    fit <- estimate_mrqap(FORM_MB, case$g, times = 5,
                          control = list(seed = 1, family = case$family))
    expect_equal(stats::fitted(fit), unname(stats::fitted(glm_fit)))
    expect_equal(stats::residuals(fit), unname(stats::residuals(glm_fit)))
    expect_equal(stats::residuals(fit, type = "pearson"),
                 unname(stats::residuals(glm_fit, type = "pearson")))
  }
})

test_that("gaussian fixed effects give the residuals of lm() with a dummy for each group", {
  g <- qap_net_gaussian()
  ref <- qap_reference_data(weight ~ diff(Age) + same(Grp), g)
  dummies <- stats::lm(weight ~ diff_Age + same_Grp + factor(sv) + factor(rv),
                       data = ref$pred)
  fit <- estimate_mrqap(weight ~ diff(Age) + same(Grp) | sender + receiver, g,
                        times = 5, control = list(seed = 1))
  expect_equal(stats::residuals(fit), unname(stats::residuals(dummies)))
  expect_equal(stats::fitted(fit), unname(stats::fitted(dummies)))
})

test_that("the residuals are read from the observed model when outputs leaves out simple_fit", {
  g <- qap_net_gaussian()
  full <- estimate_mrqap(FORM_M, g, times = 5, control = list(seed = 1))
  lean <- estimate_mrqap(FORM_M, g, times = 5, control = list(seed = 1),
                         outputs = character(0))
  expect_equal(stats::fitted(lean), stats::fitted(full))
  expect_equal(stats::residuals(lean), stats::residuals(full))
})
