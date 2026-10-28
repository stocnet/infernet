# What `estimate_mrqap()` returns: the classes of a result, and the print method
# of each. The contracts of the engine are in the test-qap_*.R files.

# The class says what the result is, most specific first, so that a method can
# be written at any level and S3 falls back to the next. `{autograph}` plots a
# `netreg`, and can add a method for a more specific class later.
test_that("a result's classes run from the most specific to netreg", {
  g <- qap_net_gaussian(n = 12)
  lin <- estimate_mrqap(weight ~ ego(Age) + alter(Age), g, times = 5,
                        control = list(seed = 1))
  expect_equal(class(lin), c("netreg_lm", "netreg"))

  b <- qap_net_binary(n = 12)
  glm_fit <- estimate_mrqap(. ~ ego(Age) + alter(Age), b, times = 5,
                            control = list(seed = 1))
  expect_equal(class(glm_fit), c("netreg_glm", "netreg"))

  css <- suppressMessages(
    estimate_mrqap(advice ~ ego(age) + friends, css_hightech(),
                   times = 5, control = list(seed = 1)))
  expect_equal(class(css), c("netreg_css", "netreg_glm", "netreg"))
})

test_that("each class prints through its own method", {
  g <- qap_net_gaussian(n = 12)
  lin <- estimate_mrqap(weight ~ ego(Age) + alter(Age), g, times = 5,
                        control = list(seed = 1))
  expect_output(print(lin), "Coefficients")
  css <- suppressMessages(
    estimate_mrqap(advice ~ ego(age) + friends, css_hightech(),
                   times = 5, control = list(seed = 1)))
  expect_output(print(css), "cognitive social structure")
  # A bare netreg falls back to a structural summary rather than erroring.
  bare <- structure(list(coefficients = 1), class = "netreg")
  expect_output(print(bare), "<netreg>")
})

test_that("a netreg_lm prints as an OLS network model", {
  g <- qap_net_gaussian(n = 10)
  fit <- estimate_mrqap(weight ~ sim(Age), g, times = 10)
  expect_output(print(fit), "OLS Network Model")
})

test_that("a netreg_glm prints as a generalised linear network model", {
  g <- qap_net_binary(n = 10)
  fit <- estimate_mrqap(. ~ sim(Age), g, times = 10)
  expect_output(print(fit), "Generalized Linear Network Model")
})

# ---- what each print method reports --------------------------------------------

test_that("a mixed linear model prints its b-based p-values and its intercepts", {
  skip_if_not_installed("lme4")
  fit <- suppressMessages(suppressWarnings(
    estimate_mrqap(weight ~ ego(Age) + alter(Age), qap_net_gaussian(n = 12),
                   times = 5, control = list(seed = 1, loops = TRUE,
                                             groups = rep(1:2, 6),
                                             random_intercept_sender = TRUE))))
  out <- capture.output(print(fit, print_b = TRUE, print_random = TRUE))
  expect_match(out, "Linear Mixed Network Model", all = FALSE)
  expect_match(out, "Pr(<=b)", fixed = TRUE, all = FALSE)
  expect_match(out, "Random intercepts of each sender", all = FALSE)
  expect_match(out, "within groups only", all = FALSE)
  expect_match(out, "loops) were used", fixed = TRUE, all = FALSE)
})

test_that("a mixed glm prints its b-based p-values and its intercepts", {
  skip_if_not_installed("lme4")
  fit <- suppressMessages(suppressWarnings(
    estimate_mrqap(. ~ ego(Age) + alter(Age), qap_net_binary(n = 12),
                   times = 5, control = list(seed = 1, loops = TRUE,
                                             groups = rep(1:2, 6),
                                             random_intercept_sender = TRUE))))
  out <- capture.output(print(fit, print_b = TRUE, print_random = TRUE))
  expect_match(out, "Generalized Linear Mixed Network Model", all = FALSE)
  expect_match(out, "Exp(b)", fixed = TRUE, all = FALSE)
  expect_match(out, "Random intercepts of each sender", all = FALSE)
  expect_match(out, "within groups only", all = FALSE)
  expect_match(out, "loops) were used", fixed = TRUE, all = FALSE)
})

test_that("a negative binomial and a zero-inflated model print their own parts", {
  skip_if_not_installed("MASS")
  nb <- suppressMessages(suppressWarnings(
    estimate_mrqap(. ~ ego(Age), qap_net_count(n = 12), times = 5,
                   control = list(seed = 1, family = "negbin"))))
  expect_output(print(nb), "dispersion \\(theta\\)")
  skip_if_not_installed("pscl")
  zi <- suppressMessages(suppressWarnings(
    estimate_mrqap(. ~ ego(Age), qap_net_zip(n = 12), times = 5,
                   control = list(seed = 1, family = "zip"))))
  expect_output(print(zi), "Zero-inflation coefficients")
})

test_that("a CSS prints its groups, its robust errors, and its loops", {
  fit <- suppressMessages(suppressWarnings(
    estimate_mrqap(advice ~ ego(age) + friends, css_hightech(), times = 5,
                   control = list(seed = 1, loops = TRUE, groups = rep(1:2, 5),
                                  use_robust_errors = TRUE))))
  out <- capture.output(print(fit))
  expect_match(out, "within groups only", all = FALSE)
  expect_match(out, "robust standard errors", all = FALSE)
  expect_match(out, "loops) were used", fixed = TRUE, all = FALSE)
})
