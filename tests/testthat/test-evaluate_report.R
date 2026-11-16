# `nobs()`, `logLik()`, and `deviance()` of an MRQAP fit, against the values of
# the equivalent `lm()` or `glm()`.

FORM_M <- weight ~ ego(Age) + alter(Age) + sim(Age)
FORM_MB <- . ~ ego(Age) + alter(Age) + sim(Age)

test_that("a gaussian fit gives the likelihood of lm()", {
  g <- qap_net_gaussian()
  ref <- qap_reference_data(FORM_M, g)
  lm_fit <- stats::lm(ref$formula, data = ref$pred)
  fit <- estimate_mrqap(FORM_M, g, times = 5, control = list(seed = 1))
  expect_equal(stats::nobs(fit), stats::nobs(lm_fit))
  expect_equal(stats::logLik(fit), stats::logLik(lm_fit), ignore_attr = "class")
  expect_equal(attr(stats::logLik(fit), "df"), attr(stats::logLik(lm_fit), "df"))
  expect_equal(stats::AIC(fit), stats::AIC(lm_fit))
  expect_equal(stats::BIC(fit), stats::BIC(lm_fit))
  expect_equal(stats::deviance(fit), stats::deviance(lm_fit))
})

test_that("a binomial and a poisson fit give the likelihood of glm()", {
  cases <- list(binomial = list(g = qap_net_binary(), family = "binomial"),
                poisson  = list(g = qap_net_count(),  family = "poisson"))
  for (case in cases) {
    ref <- qap_reference_data(FORM_MB, case$g)
    glm_fit <- stats::glm(ref$formula, data = ref$pred,
                          family = get(case$family, mode = "function"))
    fit <- estimate_mrqap(FORM_MB, case$g, times = 5,
                          control = list(seed = 1, family = case$family))
    expect_equal(stats::nobs(fit), stats::nobs(glm_fit))
    expect_equal(stats::logLik(fit), stats::logLik(glm_fit), ignore_attr = "class")
    expect_equal(stats::AIC(fit), stats::AIC(glm_fit))
    expect_equal(stats::deviance(fit), stats::deviance(glm_fit))
  }
})

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
})

test_that("nobs() leaves out the fixed ties", {
  g <- fixed_net()
  fit <- suppressWarnings(estimate_mrqap(. ~ ego(Age), g, times = 5,
                                         control = list(seed = 1)))
  n <- as.numeric(manynet::net_nodes(g))
  expect_equal(fit$missing$fixed_ties, 6)
  expect_equal(stats::nobs(fit), n * (n - 1) - 6)
  expect_equal(stats::nobs(fit), fit$missing$used)
})

test_that("the likelihood is read from the observed model when outputs leaves out simple_fit", {
  g <- qap_net_gaussian()
  full <- estimate_mrqap(FORM_M, g, times = 5, control = list(seed = 1))
  lean <- estimate_mrqap(FORM_M, g, times = 5, control = list(seed = 1),
                         outputs = character(0))
  expect_null(lean$simple_fit)
  expect_equal(stats::nobs(lean), stats::nobs(full))
  expect_equal(stats::logLik(lean), stats::logLik(full))
})

test_that("a result without an observed model says how to get one", {
  fit <- estimate_mrqap(FORM_M, qap_net_gaussian(), times = 5,
                        control = list(seed = 1))
  fit$simple_fit <- NULL
  fit$base$base_model <- NULL
  expect_error(stats::logLik(fit), "keeps no observed model")
  expect_true(is.na(stats::nobs(fit)) || stats::nobs(fit) == nrow(fit$pred))
})
