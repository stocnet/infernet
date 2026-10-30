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
  expect_output(print(lin), "Linear MRQAP of weight")
  css <- suppressMessages(
    estimate_mrqap(advice ~ ego(age) + friends, css_hightech(),
                   times = 5, control = list(seed = 1)))
  expect_output(print(css), "reporters alike")
  # A bare netreg falls back to a structural summary rather than erroring.
  bare <- structure(list(coefficients = 1), class = "netreg")
  expect_output(print(bare), "<netreg>")
})

test_that("a print returns its input invisibly", {
  fit <- estimate_mrqap(weight ~ sim(Age), qap_net_gaussian(n = 10), times = 5)
  expect_output(shown <- withVisible(print(fit)))
  expect_false(shown$visible)
  expect_identical(shown$value, fit)
})

test_that("a print names the model and the null", {
  fit <- estimate_mrqap(. ~ sim(Age), qap_net_binary(n = 10), times = 10,
                        control = list(seed = 1))
  out <- capture.output(print(fit))
  # The title, then a blank line; `{manynet}` describes the network.
  expect_equal(out[1:2], c("# Logistic MRQAP of ties", ""))
  expect_false(any(grepl("network of", out)))
  # `glance()` gives the number of dyads.
  expect_false(any(grepl("Fitted on", out)))
  # With one predictor, a permutation of the predictor is one of the outcome.
  expect_match(out, "Null: 10 permutations of the outcome", all = FALSE)
  expect_match(out, "Controls: seed = 1", all = FALSE)
  # The full matrix is for `diagnose_confusion()` and `summary()`.
  expect_false(any(grepl("Confusion", out)))
})

test_that("a print points to more methods only at the most verbose level", {
  fit <- estimate_mrqap(. ~ sim(Age), qap_net_binary(n = 10), times = 10,
                        control = list(seed = 1))
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old))
  expect_false(any(grepl("See summary", capture.output(print(fit)))))
  options(snet_verbosity = "debug")
  expect_match(capture.output(print(fit)), "See summary", all = FALSE)
})

test_that("a p-value of 0 prints as below the smallest that the draws can give", {
  expect_equal(.format_perm_p(c(0, 0.05, NA), times = 20), c("< 0.05", "0.05", NA))
  expect_equal(.signif_marks(c(0, 0.0004, 0.2, NA), times = 1000),
               c("***", "***", "", ""))
  expect_equal(.signif_marks(0, times = 20), "*")
})

# ---- the deprecated arguments, and summary() --------------------------------------

test_that("print_b and print_random are deprecated, and still print their parts", {
  skip_if_not_installed("lme4")
  fit <- suppressMessages(suppressWarnings(
    estimate_mrqap(weight ~ ego(Age) + alter(Age), qap_net_gaussian(n = 12),
                   times = 5, control = list(seed = 1, loops = TRUE,
                                             groups = rep(1:2, 6),
                                             random_intercept_sender = TRUE))))
  expect_warning(out <- capture.output(print(fit, print_b = TRUE)), "print_b")
  expect_match(out, "p-values of the coefficients", all = FALSE)
  expect_warning(out <- capture.output(print(fit, print_random = TRUE)),
                 "print_random")
  expect_match(out, "Random intercepts", all = FALSE)
  out <- capture.output(print(fit))
  expect_match(out, "Linear mixed MRQAP", all = FALSE)
  expect_match(out, "with sender random intercepts", all = FALSE)
  expect_match(out, "within groups", all = FALSE)
  expect_match(out, "includes loops", all = FALSE)
})

test_that("the random intercepts are lme4's, named for each node", {
  skip_if_not_installed("lme4")
  g <- qap_net_gaussian(n = 12)
  fit <- suppressMessages(suppressWarnings(
    estimate_mrqap(weight ~ ego(Age), g, times = 5,
                   control = list(seed = 1, random_intercept_sender = TRUE))))
  re <- .random_intercepts(fit)
  ranef <- lme4::ranef(fit$base$base_model)$sv
  expect_equal(re$intercepts$intercept, unname(ranef[, 1]))
  expect_equal(re$intercepts$level,
               manynet::node_names(g)[as.integer(rownames(ranef))] %||%
                 rownames(ranef))
  expect_equal(re$variance$group, "sender")
})

test_that("a negative binomial and a zero-inflated summary give their own parts", {
  skip_if_not_installed("MASS")
  nb <- suppressMessages(suppressWarnings(
    estimate_mrqap(. ~ ego(Age), qap_net_count(n = 12), times = 5,
                   control = list(seed = 1, family = "negbin"))))
  expect_output(print(nb), "Negative binomial MRQAP")
  expect_output(print(summary(nb)), "dispersion \\(theta\\)")
  skip_if_not_installed("pscl")
  zi <- suppressMessages(suppressWarnings(
    estimate_mrqap(. ~ ego(Age), qap_net_zip(n = 12), times = 5,
                   control = list(seed = 1, family = "zip"))))
  expect_output(print(summary(zi)), "Zero-inflation coefficients")
})

test_that("a CSS prints its reporters, its groups, its robust errors, and its loops", {
  fit <- suppressMessages(suppressWarnings(
    estimate_mrqap(advice ~ ego(age) + friends, css_hightech(), times = 5,
                   control = list(seed = 1, loops = TRUE, groups = rep(1:2, 5),
                                  use_robust_errors = TRUE))))
  out <- capture.output(print(fit))
  expect_match(out, "reporters alike", all = FALSE)
  expect_match(out, "within groups", all = FALSE)
  expect_match(out, "robust standard errors", all = FALSE)
  expect_match(out, "includes loops", all = FALSE)
})

test_that("a summary lists the terms that the formula dropped", {
  fit <- suppressWarnings(
    estimate_mrqap(weight ~ ego(Age) + inertia, qap_net_gaussian(n = 10),
                   times = 5, control = list(seed = 1)))
  expect_equal(fit$record$status[fit$record$term == "inertia"], "drops")
  out <- capture.output(print(summary(fit)))
  expect_match(out, "Terms that the formula changed", all = FALSE)
  expect_match(out, "inertia", all = FALSE)
})
