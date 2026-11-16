# simulate(): three routes to the same networks, and the simulator that
# test_gof() shares.

sim_net <- qap_net_binary(n = 12)
sim_spec <- specify_mrqap(. ~ ego(Age))
sim_fit <- estimate_mrqap(sim_spec, sim_net, times = 5)
as_matrices <- function(nets) lapply(nets, manynet::as_matrix)

test_that("simulate(spec, coefs = coef(fit)) gives simulate(fit), for one seed", {
  by_fit <- simulate(sim_fit, nsim = 3, seed = 4, .data = sim_net)
  by_spec <- simulate(sim_spec, nsim = 3, seed = 4, .data = sim_net,
                      coefs = coef(sim_fit), family = "binomial")
  by_coefs <- simulate(declare_coefs(sim_fit), nsim = 3, seed = 4, .data = sim_net)
  expect_identical(as_matrices(by_spec), as_matrices(by_fit))
  expect_identical(as_matrices(by_coefs), as_matrices(by_fit))
  # A different seed gives different networks.
  other <- simulate(sim_fit, nsim = 3, seed = 5, .data = sim_net)
  expect_false(identical(as_matrices(other), as_matrices(by_fit)))
})

test_that("without a family, the specification reads it from the outcome", {
  expect_identical(
    as_matrices(simulate(sim_spec, nsim = 2, seed = 4, .data = sim_net,
                         coefs = coef(sim_fit))),
    as_matrices(simulate(sim_fit, nsim = 2, seed = 4, .data = sim_net)))
  # A set of coefficients keeps its own family.
  count_net <- qap_net_count(n = 12)
  count_fit <- estimate_mrqap(. ~ ego(Age), count_net, times = 5,
                              control = list(family = "poisson"))
  expect_identical(
    as_matrices(simulate(sim_spec, seed = 1, .data = count_net,
                         coefs = declare_coefs(count_fit))),
    as_matrices(simulate(count_fit, seed = 1, .data = count_net)))
  # A count reads as poisson.
  expect_identical(
    as_matrices(simulate(sim_spec, seed = 1, .data = count_net,
                         coefs = coef(count_fit))),
    as_matrices(simulate(count_fit, seed = 1, .data = count_net)))
  # A valued outcome reads as gaussian, which needs sigma.
  expect_error(simulate(sim_spec, .data = qap_net_gaussian(n = 12),
                        coefs = coef(count_fit)), "sigma")
})

test_that("each family gives the same networks by every route", {
  count_net <- qap_net_count(n = 12)
  count_fit <- estimate_mrqap(. ~ ego(Age), count_net, times = 5,
                              control = list(family = "poisson"))
  expect_identical(
    as_matrices(simulate(sim_spec, nsim = 2, seed = 1, .data = count_net,
                         coefs = coef(count_fit), family = "poisson")),
    as_matrices(simulate(count_fit, nsim = 2, seed = 1, .data = count_net)))
  gauss_net <- qap_net_gaussian(n = 12)
  gauss_fit <- suppressMessages(
    estimate_mrqap(. ~ ego(Age), gauss_net, times = 5))
  expect_identical(
    as_matrices(simulate(sim_spec, nsim = 2, seed = 1, .data = gauss_net,
                         coefs = declare_coefs(gauss_fit))),
    as_matrices(simulate(gauss_fit, nsim = 2, seed = 1, .data = gauss_net)))
})

test_that("a draw from the coefficients is the draw of stats::simulate()", {
  sim <- .mrqap_simulator(sim_fit, sim_net)
  set.seed(3)
  ours <- sim$draw()[sim_fit$pred$location]
  set.seed(3)
  theirs <- stats::simulate(sim_fit$simple_fit, nsim = 1)[[1]]
  expect_identical(as.numeric(ours), as.numeric(theirs))
})

test_that("the result is a list of nsim networks, of the class of .data", {
  nets <- simulate(sim_fit, nsim = 3, seed = 1, .data = sim_net)
  expect_type(nets, "list")
  expect_length(nets, 3)
  expect_s3_class(nets[[1]], class(sim_net)[1])
  bare <- simulate(sim_fit, nsim = 2, seed = 1)
  expect_s3_class(bare[[1]], "stocnet")
  # Without the network, the cells outside the model are empty, and on a
  # complete network the draws are the same.
  expect_equal(unname(manynet::as_matrix(bare[[1]])),
               unname(manynet::as_matrix(nets[[1]])))
})

test_that("a tie is drawn only in the dyads of the model", {
  undirected <- qap_net_undirected(n = 12)
  fit <- suppressMessages(estimate_mrqap(. ~ sim(Age), undirected, times = 5))
  m <- manynet::as_matrix(simulate(fit, seed = 2, .data = undirected)[[1]])
  expect_true(isSymmetric(unname(m)))
  expect_true(all(diag(m) == 0))
  nets <- simulate(sim_fit, nsim = 5, seed = 2, .data = sim_net)
  expect_true(all(vapply(nets, function(g) all(diag(manynet::as_matrix(g)) == 0),
                         logical(1))))
})

test_that("a seed puts back the random state of the session", {
  set.seed(1)
  expected <- stats::runif(1)
  set.seed(1)
  simulate(sim_fit, seed = 9, .data = sim_net)
  expect_identical(stats::runif(1), expected)
})

test_that("the default seed is the one set for the session", {
  old <- options(snet_computation = list(seed = 7))
  on.exit(options(old), add = TRUE)
  expect_identical(as_matrices(simulate(sim_fit, nsim = 2, .data = sim_net)),
                   as_matrices(simulate(sim_fit, nsim = 2, seed = 7,
                                        .data = sim_net)))
})

test_that("simulate() names what it needs, and refuses what it cannot draw", {
  coefs <- declare_coefs(sim_fit)
  expect_error(simulate(coefs, seed = 1), "needs .*\\.data")
  expect_error(simulate(sim_spec, 2, .data = sim_net), "coefs")
  expect_error(simulate(sim_fit, nsim = 0), "nsim")
  unstated <- declare_coefs(sim_spec, "(Intercept)" = 0, family = "binomial",
                            .data = sim_net)
  expect_error(simulate(unstated, .data = sim_net), "not stated")
  expect_error(simulate(sim_spec, .data = sim_net, family = "binomial",
                        coefs = c("(Intercept)" = 0, ego_age = 1)),
               "No column named")
  expect_error(simulate(sim_spec, .data = sim_net, family = "gaussian",
                        coefs = coef(sim_fit)), "sigma")
  expect_error(simulate(sim_spec, .data = sim_net, family = "negbin",
                        coefs = coef(sim_fit)), "cannot yet draw")
  expect_error(simulate(sim_spec, .data = list(sim_net, sim_net),
                        family = "binomial", coefs = coef(sim_fit)),
               "cannot yet simulate")
})
