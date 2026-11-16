# `sweep_diffusion()` and `sweep_opinion()` play a model for each setting of
# the arguments in `vary`. These tests check the settings and their labels,
# the measures of each run, and the checks of `vary`.

test_that("a sweep plays the diffusions for each setting", {
  sweep <- sweep_diffusion(manynet::ison_adolescents, times = 2, steps = 5,
                           vary = list(transmissibility = c(0, 1)),
                           control = list(seed = 1))
  expect_s3_class(sweep, "diffusionSweep")
  expect_false(inherits(sweep, "diffs_model"))
  expect_identical(attr(sweep, "control")$seed, 1)
  ends <- summary(sweep)
  expect_equal(nrow(ends), 4)
  expect_equal(names(ends), c("transmissibility", "sim", "infected", "toa"))
  # with no transmission, only the seed is infected
  expect_equal(ends$infected[ends$transmissibility == 0], rep(1/8, 2))
  expect_equal(ends$infected[ends$transmissibility == 1], rep(1, 2))
  expect_output(print(sweep), "4 simulated diffusions over 2 settings")
})

test_that("vary labels a list of seeds by their number, and crosses arguments", {
  group <- manynet::create_filled(20)
  sweep <- sweep_diffusion(group, threshold = 5, times = 1,
                           vary = list(seeds = list(1:4, 1:5)))
  ends <- summary(sweep)
  expect_equal(ends$seeds, c(4L, 5L))
  expect_equal(ends$infected, c(0.2, 1))
  # the run that stops at its seeds never completes
  expect_true(is.na(ends$toa[1]))
  expect_false(is.na(ends$toa[2]))
  both <- sweep_diffusion(manynet::ison_adolescents, times = 1, steps = 3,
                          vary = list(transmissibility = c(0.2, 0.6),
                                      recovery = c(0, 0.3)),
                          control = list(seed = 1))
  expect_equal(nrow(unique(summary(both)[c("transmissibility", "recovery")])), 4)
  named <- sweep_diffusion(manynet::ison_adolescents, times = 1, steps = 3,
                           vary = list(seeds = list(one = 1, two = 1:2)),
                           control = list(seed = 1))
  expect_equal(levels(summary(named)$seeds), c("one", "two"))
  ordered <- sweep_diffusion(manynet::ison_adolescents, times = 1, steps = 3,
                             vary = list(direction = c("out", "in")),
                             control = list(seed = 1))
  expect_equal(levels(summary(ordered)$direction), c("out", "in"))
})

test_that("a sweep checks its settings", {
  expect_error(sweep_diffusion(manynet::ison_adolescents), "needs the settings")
  expect_error(sweep_diffusion(manynet::ison_adolescents, vary = list(1)),
               "named list")
  expect_error(sweep_diffusion(manynet::ison_adolescents, vary = list(foo = 1)),
               "not an argument of `manynet::play_diffusion")
  expect_error(sweep_diffusion(manynet::ison_adolescents, transmissibility = 0.1,
                               vary = list(transmissibility = 1)),
               "both in")
})

test_that("an opinion sweep measures the opinions at the end of each run", {
  path <- manynet::create_explicit(A-B-C)
  # without confidence limits, a path reaches a consensus
  sweep <- sweep_opinion(path, c(0, 0.5, 1), times = 1, steps = 200,
                         vary = list(stubbornness = c(0, 1)))
  expect_s3_class(sweep, "opinionSweep")
  ends <- summary(sweep)
  expect_equal(names(ends), c("stubbornness", "sim", "clusters", "consensus",
                              "spread", "mean", "steps", "settled"))
  expect_equal(ends$clusters, c(1, 3))
  expect_equal(ends$consensus, c(TRUE, FALSE))
  # each node of the path takes the mean, 0.5, in one step;
  # fully stubborn nodes never move, so they settle at once
  expect_equal(ends$steps, c(1, 0))
  expect_output(print(sweep), "2 simulated opinion dynamics over 2 settings")
})

test_that("an opinion sweep crosses its settings, and can vary the opinions", {
  set.seed(1)
  first <- stats::runif(manynet::net_nodes(manynet::ison_networkers))
  hk <- sweep_opinion(manynet::ison_networkers, first, times = 1, steps = 100,
                      rule = "hegselmann_krause",
                      vary = list(confidence = c(0.05, 0.5)))
  ends <- summary(hk)
  # a wider confidence gives fewer clusters
  expect_gt(ends$clusters[1], ends$clusters[2])
  starts <- sweep_opinion(manynet::create_ring(5), times = 1,
                          vary = list(opinions = list(flat = rep(0.5, 5),
                                                      split = c(0, 0, 1, 1, 1))))
  expect_equal(levels(summary(starts)$opinions), c("flat", "split"))
  expect_equal(summary(starts)$spread[1], 0)
  expect_error(sweep_opinion(manynet::ison_networkers, first,
                             vary = list(confidence = 0.1, opinions = list(first))),
               "both in")
})
