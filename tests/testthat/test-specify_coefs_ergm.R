# `declare_coefs()` of an ergm formula or an `ergm` gives coefficients in the
# form that {ergm} reads, and `simulate()` of them draws with {ergm}.
# `diagnose_confusion()` of an `ergm` reads {ergm}'s predicted probabilities.

skip_if_not_installed("ergm")

ergm_net <- function() {
  e <- new.env()
  utils::data("florentine", package = "ergm", envir = e)
  e$flomarriage
}

test_that("declare_coefs() of an ergm fit gives its coefficients, in ergm's order", {
  flomarriage <- ergm_net()
  fit <- suppressMessages(ergm::ergm(flomarriage ~ edges + nodecov("wealth")))
  coefs <- declare_coefs(fit)
  expect_s3_class(coefs, "infernetErgmCoefs")
  expect_equal(stats::coef(coefs), stats::coef(fit))
  expect_output(print(coefs), "Coefficients of an ERGM")
  # The same draw as ergm's own simulate() with the same coefficients
  expect_identical(as.matrix(simulate(coefs, seed = 3)),
                   as.matrix(simulate(fit, seed = 3, coef = stats::coef(fit))))
  expect_s3_class(simulate(coefs, nsim = 2, seed = 1), "network.list")
  # ergm reads them as initial values
  init <- suppressMessages(ergm::ergm(flomarriage ~ edges + nodecov("wealth"),
                                      control = ergm::control.ergm(init = stats::coef(coefs))))
  expect_equal(stats::coef(init), stats::coef(fit), tolerance = 1e-6)
})

test_that("declare_coefs() of an ergm formula checks names, orders values, and fills NA", {
  flomarriage <- ergm_net()
  spec <- specify_ergm(flomarriage ~ edges + nodecov("wealth"))
  coefs <- declare_coefs(spec, nodecov.wealth = 0.01, edges = -3)
  expect_equal(stats::coef(coefs), c(edges = -3, nodecov.wealth = 0.01))
  expect_identical(as.matrix(simulate(coefs, seed = 3)),
                   as.matrix(simulate(spec, coef = c(-3, 0.01), seed = 3)))
  part <- declare_coefs(spec, edges = -3)
  expect_equal(stats::coef(part), c(edges = -3, nodecov.wealth = NA))
  expect_error(simulate(part), "a value for each coefficient")
  expect_error(declare_coefs(spec, edgs = -3), "no coefficient named")
})

test_that("diagnose_confusion() of a dyad-independent ergm equals that of the logistic regression", {
  flomarriage <- ergm_net()
  fit <- suppressMessages(ergm::ergm(flomarriage ~ edges + nodecov("wealth")))
  conf <- diagnose_confusion(fit, times = 50, control = list(seed = 1))
  expect_s3_class(conf, "infernetConfusion")
  m <- as.matrix(flomarriage)
  w <- manynet::node_attribute(flomarriage, "wealth")
  ut <- which(upper.tri(m), arr.ind = TRUE)
  g <- stats::glm(m[ut] ~ I(w[ut[, 1]] + w[ut[, 2]]), family = stats::binomial)
  expect_equal(conf$auc, infernet:::.auc(m[ut], stats::fitted(g)), tolerance = 1e-6)
  expect_equal(sum(conf$confusion_matrix), choose(16, 2))
  expect_equal(sum(conf$threshold_confusion_matrix[2, ]), sum(m[ut]))
  expect_output(print(conf), "dyad-independent ERGM of flomarriage")
})

test_that("diagnose_confusion() of an ergm with dependence terms warns", {
  flomarriage <- ergm_net()
  fit <- suppressWarnings(suppressMessages(
    ergm::ergm(flomarriage ~ edges + gwesp(0.25, fixed = TRUE),
               control = ergm::control.ergm(seed = 1, MCMLE.maxit = 3))))
  expect_snet_warning(diagnose_confusion(fit, times = 20), "dependence terms")
})
