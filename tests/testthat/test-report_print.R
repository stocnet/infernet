# The print of each result, as a snapshot. Each result is seeded, on few
# permutations, so that its numbers are stable. What the numbers are is for the
# other files; these check what a print says and how it lays it out.

old_width <- options(width = 80)

test_that("each netreg variant prints its title, its estimates, and its footer", {
  g <- qap_net_gaussian(n = 10)
  b <- qap_net_binary(n = 10)
  expect_snapshot(print(estimate_mrqap(weight ~ ego(Age) + same(Grp), g,
                                       times = 20, control = list(seed = 1))))
  expect_snapshot(print(estimate_mrqap(. ~ ego(Age) + alter(Age), b,
                                       times = 20, control = list(seed = 1))))
  expect_snapshot(print(estimate_mrqap(weight ~ diff(Age) | sender, g,
                                       times = 20, control = list(seed = 1))))
  expect_snapshot(print(suppressMessages(
    estimate_mrqap(advice ~ ego(age) + friends, css_hightech(), times = 5,
                   control = list(seed = 1)))))
  expect_snapshot(print(suppressMessages(
    estimate_mrqap(. ~ inertia + ego(age), qap_net_panel(), times = 5,
                   control = list(seed = 1)))))
})

test_that("a mixed model prints its random intercepts in its summary", {
  skip_if_not_installed("lme4")
  fit <- suppressMessages(suppressWarnings(
    estimate_mrqap(weight ~ ego(Age), qap_net_gaussian(n = 10), times = 5,
                   control = list(seed = 1, random_intercept_sender = TRUE))))
  expect_snapshot(print(fit))
  expect_snapshot(print(summary(fit)))
})

test_that("a summary prints the full table, the fit, and the dropped terms", {
  fit <- suppressWarnings(
    estimate_mrqap(weight ~ ego(Age) + same(Grp) + inertia,
                   qap_net_gaussian(n = 10), times = 20,
                   control = list(seed = 1)))
  expect_snapshot(print(summary(fit)))
})

test_that("each network test prints what it tested and its null", {
  net <- manynet::ison_adolescents
  expect_snapshot(print(test_random(net, netrics::net_by_density, times = 20,
                                    control = list(seed = 1))))
  expect_snapshot(print(test_configuration(net, netrics::net_by_transitivity,
                                           times = 20, control = list(seed = 1))))
  expect_snapshot(print(test_permutation(net, netrics::net_by_density,
                                         times = 20, control = list(seed = 1))))
  fit <- estimate_mrqap(. ~ ego(Age) + alter(Age), qap_net_binary(n = 10),
                        times = 5, control = list(seed = 1))
  expect_snapshot(print(test_parameter(fit, "alter_Age", times = 20,
                                       control = list(seed = 1))))
})

test_that("a diffusion model and its tests print", {
  d <- play_diffusions(manynet::ison_adolescents, transmissibility = 0.3,
                       times = 5, control = list(seed = 1))
  expect_snapshot(print(d))
  f <- play_diffusions(manynet::ison_adolescents, transmissibility = 0.6,
                       times = 5, control = list(seed = 2))
  expect_snapshot(print(test_distributions(f, d, times = 20,
                                           control = list(seed = 1))))
})

test_that("a specification and a control print", {
  expect_snapshot(print(specify_mrqap(advice ~ edges + ego(age) + same(dept))))
  expect_snapshot(print(suppressWarnings(
    specify_ergm(advice ~ ego(age) + same(dept) + recip(friends)))))
  expect_snapshot(print(control_mrqap(seed = 1, family = "binomial")))
})

options(old_width)
