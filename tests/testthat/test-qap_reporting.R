# Anything the model resolves for itself is reported, so that a user can
# describe the model they actually fitted.
#
# These run with `snet_verbosity = "verbose"`, which is not the default. That
# matters: an informational message is silent in every other test, so a broken
# one stays invisible. A `{cli}` brace expression beginning with a dot is read
# as a style rather than as code, and two of these messages shipped that way
# before this file existed.

verbosely <- function(expr) {
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  force(expr)
}

FORM <- weight ~ ego(Age) + alter(Age) + sim(Age)

test_that("every reporting message renders", {
  g <- qap_net_gaussian(n = 15)
  # A message that cli cannot parse aborts, so reaching the end is the test.
  expect_no_error(verbosely(
    net_regression(FORM, g, times = 5, control = list(seed = 1))))
  expect_no_error(verbosely(
    net_regression(weight ~ ego(Age), g, times = 5, control = list(seed = 1))))
  expect_no_error(verbosely(
    net_regression(FORM, g, times = 5,
                   control = list(seed = 1, use_gpu = TRUE))))
})

test_that("a family resolved from the outcome is reported", {
  expect_message(
    verbosely(net_regression(. ~ ego(Age) + alter(Age), qap_net_binary(n = 15),
                             times = 5, control = list(seed = 1))),
    "binomial")
})

test_that("directedness read from the network is reported", {
  expect_message(
    verbosely(net_regression(weight ~ ego(Age) + alter(Age),
                             qap_net_undirected(n = 14),
                             times = 5, control = list(seed = 1))),
    "undirected")
})

test_that("a stated family and directedness are not reported", {
  g <- qap_net_gaussian(n = 15)
  expect_no_message(
    verbosely(net_regression(FORM, g, times = 5,
                             control = list(seed = 1, family = "gaussian",
                                            directed = TRUE))))
})

test_that("the fallback to permuting the outcome is reported", {
  expect_message(
    verbosely(net_regression(weight ~ ego(Age), qap_net_gaussian(n = 15),
                             times = 5,
                             control = list(seed = 1, permute = "predictor"))),
    "residualise")
})

test_that("the GPU falling back to the CPU is reported", {
  skip_if(gpu_available(), "a CUDA device is present, so there is no fallback")
  expect_message(
    verbosely(net_regression(FORM, qap_net_gaussian(n = 15), times = 5,
                             control = list(seed = 1, use_gpu = TRUE))),
    "CPU")
})

test_that("the model advice on homophily terms is reported", {
  expect_message(
    verbosely(net_regression(weight ~ same(Grp), qap_net_gaussian(n = 15),
                             times = 5, control = list(seed = 1))),
    "ego\\(Grp\\)")
})
