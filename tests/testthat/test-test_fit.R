# Tests of model fit: test_gof() and test_distributions().
#
# The calibration tests draw cheap adoption curves rather than diffusions on a
# network, since one run of `manynet::play_diffusion()` takes about 30 ms and a
# check of uniformity needs some hundreds of p-values.

# A monotone, bounded curve: each of `n` nodes adopts in a step with
# probability `rate`, so it resembles a cumulative adoption curve.
fake_curve <- function(rate = 0.2, n = 20, steps = 8) {
  adopted <- 1
  out <- numeric(steps)
  for (s in seq_len(steps)) {
    adopted <- adopted + stats::rbinom(1, n - adopted, rate)
    out[s] <- adopted
  }
  out
}

# Not `replicate()`, which would pass its index to `fake_curve()` through `...`.
fake_curves <- function(k, ...) {
  do.call(rbind, lapply(seq_len(k), function(i) fake_curve(...)))
}

# A `diffs_model` of fake curves, in the shape that `play_diffusions()` gives.
fake_diffs <- function(k, ...) {
  rows <- lapply(seq_len(k), function(j) {
    curve <- c(1, fake_curve(...))
    data.frame(sim = j, time = seq_along(curve) - 1, n = 20, I = curve)
  })
  out <- tibble::as_tibble(do.call(rbind, rows))
  class(out) <- c("diffs_model", class(out))
  out
}

test_that("test_gof() p-values are uniform under the model", {
  set.seed(11)
  p <- replicate(300, {
    sims <- fake_curves(38)
    infernet:::.fit_test(fake_curve(), sims, "Goodness-of-fit")$pgteobs
  })
  # The reference holds 19 curves, so Pr(p <= 0.05) is 1/20 under the model.
  expect_gt(mean(p <= 0.05), 0.02)
  expect_lt(mean(p <= 0.05), 0.09)
  expect_gt(mean(p), 0.45)
  expect_lt(mean(p), 0.6)
  # The p-values take 20 values, so compare their counts with a uniform one
  counts <- table(factor(round(p * 20), levels = 1:20))
  expect_gt(stats::chisq.test(counts)$p.value, 0.001)
})

test_that("test_gof() rejects a curve from another model", {
  set.seed(12)
  p <- replicate(100, {
    infernet:::.fit_test(fake_curve(rate = 0.6), fake_curves(38),
                         "Goodness-of-fit")$pgteobs
  })
  # With 19 reference curves the power is limited, but well above 0.05
  expect_gt(mean(p <= 0.05), 0.4)
})

test_that("test_distributions() p-values are uniform under one model", {
  skip_unless_slow("a calibration")
  set.seed(13)
  p <- replicate(120, {
    test_distributions(fake_diffs(10), fake_diffs(10), times = 39)$pgteobs
  })
  expect_gt(mean(p <= 0.05), 0.01)
  expect_lt(mean(p <= 0.05), 0.11)
  expect_gt(mean(p), 0.42)
  expect_lt(mean(p), 0.6)
})

test_that("test_distributions() separates two models", {
  set.seed(14)
  res <- test_distributions(fake_diffs(20), fake_diffs(20, rate = 0.35),
                           times = 99)
  expect_s3_class(res, "network_test")
  expect_lt(res$pgteobs, 0.05)
  expect_length(res$testdist, 99)
})

test_that("the p-values count ties against the observed data", {
  sims <- matrix(1, 40, 3)
  res <- infernet:::.fit_test(c(1, 1, 1), sims, "Goodness-of-fit")
  expect_equal(res$testval, 0)
  expect_equal(res$pgteobs, 1)
  expect_equal(res$plteobs, 1)
  far <- infernet:::.fit_test(c(2, 1, 1), sims, "Goodness-of-fit")
  # A step where no simulation varies still counts, through the ridge.
  expect_gt(far$testval, 100)
  expect_equal(far$pgteobs, 1 / 21)
})

test_that("test_gof() checks the number of simulations", {
  expect_error(infernet:::.fit_test(1:3, fake_curves(3), "Goodness-of-fit"),
               "at least 4")
  expect_warning(infernet:::.fit_test(fake_curve(), fake_curves(10),
                                      "Goodness-of-fit"),
                 "0.05")
})

test_that("test_gof() on a diffs_model reads the curves of the diffusions", {
  set.seed(15)
  sims <- fake_diffs(40)
  obs <- sims[sims$sim == 1, ]
  obs$sim <- NULL
  class(obs) <- c("diff_model", "tbl_df", "tbl", "data.frame")
  res <- test_gof(sims, obs)
  expect_s3_class(res, "network_test")
  expect_equal(res$times, 40)
  expect_length(res$testdist, 20)
  expect_equal(tidy(res)$p.upper, res$pgteobs)
  # {migraph}'s order, the observed diffusion first, still works
  expect_warning(old <- test_gof(obs, sims), "model first")
  expect_equal(old, res)
})

test_that("test_gof() and test_distributions() refuse the wrong inputs", {
  sims <- fake_diffs(10)
  one <- sims[sims$sim == 1, ]
  class(one) <- c("diff_model", "tbl_df", "tbl", "data.frame")
  expect_error(test_gof(one, one), "play_diffusions")
  expect_error(test_gof(sims, sims), "test_distributions")
  expect_error(test_gof(1:3, one), "no method")
  expect_error(test_distributions(one, one), "test_gof")
})

test_that("test_gof() aligns curves of different lengths", {
  curves <- infernet:::.align_curves(c(1, 2), list(c(1, 2, 3, 4), 1))
  expect_equal(curves$observed, c(1, 2, 2, 2))
  expect_equal(curves$simulated, rbind(c(1, 2, 3, 4), c(1, 1, 1, 1)))
})

# The same on two workers is in test-parallel.R.
test_that("play_diffusions() and test_distributions() reproduce with a seed", {
  set.seed(16)
  g <- manynet::generate_random(12, 0.25)
  one <- play_diffusions(g, transmissibility = 0.4, times = 8,
                         control = list(seed = 1))
  two <- play_diffusions(g, transmissibility = 0.4, times = 8,
                         control = list(seed = 1))
  expect_equal(one, two)
  a <- test_distributions(one, one, times = 5, control = list(seed = 2))
  b <- test_distributions(one, one, times = 5, control = list(seed = 2))
  expect_equal(a, b)
})

# ---- network regressions -----------------------------------------------------

test_that("test_gof() on a netreg simulates outcomes in the model's dyads", {
  net <- qap_net_binary(n = 15)
  fit <- estimate_mrqap(. ~ ego(Age), net, times = 5, control = list(seed = 1))
  sim <- infernet:::.netreg_simulator(fit, net)
  set.seed(1)
  draws <- replicate(5, sim$draw(), simplify = FALSE)
  expect_true(all(vapply(draws, function(d) all(diag(d) == diag(sim$matrix)),
                         logical(1))))
  expect_true(all(unlist(draws) %in% c(0, 1)))
  expect_false(all(vapply(draws, identical, logical(1), sim$matrix)))
  expect_equal(as.numeric(manynet::net_nodes(sim$network(draws[[1]]))), 15)
  expect_true(manynet::is_directed(sim$network(draws[[1]])))
})

test_that("test_gof() on a netreg finds the dependence that an MRQAP leaves out", {
  set.seed(17)
  m <- matrix(stats::rbinom(225, 1, 0.2), 15, 15)
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  diag(m) <- 0
  # Every tie is reciprocated, and the model has no term for it
  net <- manynet::to_reciprocated(manynet::as_tidygraph(m))
  net <- manynet::mutate_nodes(net, Age = stats::runif(15))
  fit <- estimate_mrqap(. ~ ego(Age), net, times = 5)
  res <- test_gof(fit, net, netrics::net_by_reciprocity, times = 60,
                  control = list(seed = 1))
  expect_s3_class(res, "network_test")
  expect_lt(res$pgteobs, 0.05)
  expect_true(res$directed)
})

test_that("test_gof() on a netreg takes a statistic of several values", {
  net <- qap_net_gaussian(n = 12)
  fit <- estimate_mrqap(. ~ ego(Age) + alter(Age), net, times = 5)
  stat <- function(g) c(mean(manynet::as_matrix(g)), stats::sd(manynet::as_matrix(g)))
  res <- test_gof(fit, net, stat, times = 40, control = list(seed = 1))
  expect_length(res$testdist, 20)
  expect_true(all(res$testdist >= 0))
})

test_that("test_gof() on a netreg mirrors an undirected network", {
  net <- qap_net_undirected(n = 12)
  fit <- estimate_mrqap(. ~ diff(Age), net, times = 5)
  sim <- infernet:::.netreg_simulator(fit, net)
  d <- sim$draw()
  expect_equal(d, t(d))
  expect_false(manynet::is_directed(sim$network(d)))
})

test_that("test_gof() on a netreg reproduces with a seed", {
  net <- qap_net_binary(n = 12)
  fit <- estimate_mrqap(. ~ ego(Age), net, times = 5)
  one <- test_gof(fit, net, netrics::net_by_reciprocity, times = 40,
                  control = list(seed = 3))
  two <- test_gof(fit, net, netrics::net_by_reciprocity, times = 40,
                  control = list(seed = 3))
  expect_equal(one, two)
})

test_that("test_gof() on a netreg refuses what it cannot simulate", {
  net <- qap_net_binary(n = 12)
  lean <- estimate_mrqap(. ~ ego(Age), net, times = 5,
                         control = list(less_mem = TRUE))
  expect_error(test_gof(lean, net, netrics::net_by_reciprocity, times = 5),
               "less_mem")
  fit <- estimate_mrqap(. ~ ego(Age), net, times = 5)
  other <- qap_net_binary(n = 12, seed = 999)
  expect_error(test_gof(fit, other, netrics::net_by_reciprocity, times = 5),
               "not the network")
  expect_error(test_gof(fit, net, function(g) numeric(0), times = 5),
               "at least one number")
})

test_that("test_gof() and test_distributions() name what they need", {
  sims <- fake_diffs(5)
  expect_error(test_gof(sims, 1:3), "diffusion record")
  expect_error(test_distributions(sims, fake_curve()), "two")
  expect_error(test_distributions(fake_diffs(1), sims, times = 5), "at least 2")
})

test_that("test_gof() on a netreg refuses a list, a panel, and a CSS", {
  pooled <- suppressMessages(estimate_mrqap(
    . ~ ego(Age), list(qap_net_binary(n = 10), qap_net_binary(n = 10, seed = 2)),
    times = 2))
  expect_error(test_gof(pooled, qap_net_binary(n = 10), netrics::net_by_reciprocity,
                        times = 5), "cannot yet simulate")
})
