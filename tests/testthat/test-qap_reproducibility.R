# A permutation result that cannot be reproduced cannot be published, so the
# seed is part of the interface rather than an implementation detail.

FORM <- weight ~ ego(Age) + alter(Age) + sim(Age)

test_that("the same seed gives the same p-values", {
  g <- qap_net_gaussian()
  a <- estimate_mrqap(FORM, g, times = 30, control = list(seed = 99))
  b <- estimate_mrqap(FORM, g, times = 30, control = list(seed = 99))
  expect_equal(a$lower, b$lower)
  expect_equal(a$larger, b$larger)
  expect_equal(a$abs, b$abs)
  expect_equal(a$coefficients, b$coefficients)
})

test_that("a different seed gives a different null distribution", {
  # Pure noise, so that the tallies land inside (0, 1) and can differ. A strong
  # effect drives every p-value to 0 or 1 under any seed, which would make this
  # assertion pass for the wrong reason.
  set.seed(77)
  n <- 25
  m <- matrix(stats::rnorm(n^2), n, n)
  diag(m) <- 0
  g <- manynet::mutate(manynet::as_tidygraph(m), Age = stats::runif(n, 20, 60))
  a <- estimate_mrqap(FORM, g, times = 60, control = list(seed = 99))
  b <- estimate_mrqap(FORM, g, times = 60, control = list(seed = 7))
  # The observed model does not depend on the seed; only the null does.
  expect_equal(a$coefficients, b$coefficients)
  expect_false(isTRUE(all.equal(a$lower, b$lower)))
})

test_that("an outer set.seed() reproduces a run with no seed control", {
  g <- qap_net_gaussian()
  set.seed(5); a <- estimate_mrqap(FORM, g, times = 30)
  set.seed(5); b <- estimate_mrqap(FORM, g, times = 30)
  expect_equal(a$lower, b$lower)
})

# That a seed gives the same answer on two workers, and that a run restores the
# plan, is in test-parallel.R, which starts its workers once.

test_that("the test family reproduces under an outer seed", {
  g <- qap_net_gaussian(n = 18)
  set.seed(3); a <- test_random(g, netrics::net_by_density, times = 30)
  set.seed(3); b <- test_random(g, netrics::net_by_density, times = 30)
  expect_equal(a$testdist, b$testdist)
  expect_equal(a$pgteobs, b$pgteobs)
})
