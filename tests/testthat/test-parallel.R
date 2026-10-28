# The parallel path, checked once. Every other file runs sequentially.
#
# A plan of workers takes about half a second to start, and each worker then
# loads the package, which is longer than most tests take. Switching to another
# plan and back starts the workers again. So this file computes its sequential
# references first, then starts two workers once, and every function below
# runs on them through the default `strategy = NULL`, which leaves the plan the
# user set with `future::plan()` as it is.
#
# `future.seed = TRUE` is what makes a seed give the same numbers on any plan.
# Each block asserts that, for one family of functions.

skip_on_cran()

# ---- sequential references -----------------------------------------------------

FORM_PAR <- weight ~ ego(Age) + alter(Age) + sim(Age)
marvel <- manynet::to_uniplex(manynet::fict_marvel, "relationship") |>
  manynet::to_giant() |> manynet::to_unsigned() |>
  manynet::to_subgraph(PowerOrigin == "Human")
set.seed(16)
diffusion_net <- manynet::generate_random(12, 0.25)
binary_net <- qap_net_binary(n = 12)
binary_fit <- estimate_mrqap(. ~ ego(Age), binary_net, times = 5)
param_fit <- suppressMessages(
  estimate_mrqap(. ~ ego(Age) + alter(Age) + ego(Grp),
                 qap_net_gaussian(n = 12), times = 5))

sequential <- list(
  mrqap = estimate_mrqap(FORM_PAR, qap_net_gaussian(), times = 30,
                         control = list(seed = 99)),
  permutation = test_permutation(marvel, netrics::net_by_heterophily,
                                 attribute = "Attractive", times = 20,
                                 control = list(seed = 1)),
  diffusions = play_diffusions(diffusion_net, transmissibility = 0.4,
                               times = 20, control = list(seed = 1)),
  gof = test_gof(binary_fit, binary_net, netrics::net_by_reciprocity,
                 times = 40, control = list(seed = 3)),
  parameter = test_parameter(param_fit, "ego_Grp", times = 20,
                             control = list(seed = 3)))
sequential$distributions <- test_distributions(
  sequential$diffusions, sequential$diffusions, times = 10,
  control = list(seed = 2))

# ---- two workers, for the rest of the file ---------------------------------------

# The last block of this file restores the plan that was set before it.
# `test_that()` catches an error in any block, so the last block still runs.
old_plan <- future::plan(future::multisession, workers = 2)
two_workers <- future::plan()

test_that("the default runs on the workers the user started", {
  old <- setup_future_plan()
  expect_identical(future::plan(), two_workers)
  expect_equal(future::nbrOfWorkers(), 2)
  restore_future_plan(old)
})

test_that("estimate_mrqap() gives the same p-values on two workers", {
  par <- estimate_mrqap(FORM_PAR, qap_net_gaussian(), times = 30,
                        control = list(seed = 99))
  expect_equal(par$lower, sequential$mrqap$lower)
  expect_equal(par$testdist, sequential$mrqap$testdist)
  expect_equal(par$coefficients, sequential$mrqap$coefficients)
  expect_identical(future::plan(), two_workers)
})

test_that("test_permutation() gives the same draws on two workers", {
  par <- test_permutation(marvel, netrics::net_by_heterophily,
                          attribute = "Attractive", times = 20,
                          control = list(seed = 1))
  expect_equal(par$testdist, sequential$permutation$testdist)
  # Two arguments of the statistic reach it by name on the workers too.
  scaled <- test_permutation(marvel, function(.data, loops = FALSE, attribute, scale) {
    scale * netrics::net_by_heterophily(.data, attribute)
  }, attribute = "Attractive", scale = 2, times = 20, control = list(seed = 1))
  expect_equal(scaled$testdist, 2 * sequential$permutation$testdist)
})

test_that("play_diffusions() and test_distributions() agree on two workers", {
  par <- play_diffusions(diffusion_net, transmissibility = 0.4, times = 20,
                         control = list(seed = 1))
  expect_equal(par, sequential$diffusions)
  expect_equal(test_distributions(par, par, times = 10,
                                  control = list(seed = 2)),
               sequential$distributions)
})

test_that("test_gof() on a netreg gives the same p-value on two workers", {
  expect_equal(test_gof(binary_fit, binary_net, netrics::net_by_reciprocity,
                        times = 40, control = list(seed = 3)),
               sequential$gof)
})

test_that("test_parameter() gives the same p-value on two workers", {
  expect_equal(test_parameter(param_fit, "ego_Grp", times = 20,
                              control = list(seed = 3)),
               sequential$parameter)
})

# `ncores` starts its own plan, and so stops the user's workers and starts them
# again afterwards. This is the one block that pays for that.
test_that("ncores starts its own workers, and restores the user's plan", {
  old <- setup_future_plan("sequential", ncores = 2)
  expect_s3_class(future::plan(), "multisession")
  expect_equal(future::nbrOfWorkers(), 2)
  restore_future_plan(old)
  par <- estimate_mrqap(FORM_PAR, qap_net_gaussian(), times = 30,
                        control = list(seed = 99, ncores = 2))
  expect_equal(par$lower, sequential$mrqap$lower)
  expect_identical(future::plan(), two_workers)
})

test_that("the plan set before this file is restored, and its workers stop", {
  future::plan(old_plan)
  expect_s3_class(future::plan(), "sequential")
})
