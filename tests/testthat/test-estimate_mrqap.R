# What `estimate_mrqap()` returns: the classes of a result, and the print method
# of each. The contracts of the engine are in the test-qap_*.R files.

# The class names the model, not the family (D26). The family, a CSS, and a
# panel are fields. `netreg` stays second for `{autograph}` and `{migraph}`.
test_that("a result is an infernetMrqapFit of any family or shape", {
  g <- qap_net_gaussian(n = 12)
  lin <- estimate_mrqap(weight ~ ego(Age) + alter(Age), g, times = 5,
                        control = list(seed = 1))
  expect_equal(class(lin), c("infernetMrqapFit", "netreg"))
  expect_equal(lin$family, "gaussian")
  expect_false(lin$css)

  b <- qap_net_binary(n = 12)
  glm_fit <- estimate_mrqap(. ~ ego(Age) + alter(Age), b, times = 5,
                            control = list(seed = 1))
  expect_equal(class(glm_fit), c("infernetMrqapFit", "netreg"))
  expect_equal(glm_fit$family, "binomial")

  css <- suppressMessages(
    estimate_mrqap(advice ~ ego(age) + friends, css_hightech(),
                   times = 5, control = list(seed = 1)))
  expect_equal(class(css), c("infernetMrqapFit", "netreg"))
  expect_true(css$css)
})

test_that("each family and shape prints through one method", {
  g <- qap_net_gaussian(n = 12)
  lin <- estimate_mrqap(weight ~ ego(Age) + alter(Age), g, times = 5,
                        control = list(seed = 1))
  expect_output(print(lin), "Linear MRQAP of weight")
  css <- suppressMessages(
    estimate_mrqap(advice ~ ego(age) + friends, css_hightech(),
                   times = 5, control = list(seed = 1)))
  expect_output(print(css), "reporters alike")
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

test_that("a permutation p-value counts the observed statistic as a draw", {
  # (1 + b) / (B + 1), so that no p-value is 0 (Phipson and Smyth 2010).
  expect_equal(.perm_p(0, 20), 1 / 21)
  expect_equal(.perm_p(20, 20), 1)
  fit <- estimate_mrqap(. ~ sim(Age), qap_net_binary(n = 10), times = 10,
                        control = list(seed = 1))
  p <- c(fit$lower, fit$larger, fit$abs)
  p <- p[!is.na(p)]
  expect_true(all(p >= 1 / 11 & p <= 1))
  # Density is fixed under the null, so every draw ties with the observed.
  cug <- test_random(manynet::ison_adolescents, netrics::net_by_density,
                     times = 20, control = list(seed = 1))
  expect_equal(c(cug$plteobs, cug$pgteobs), c(1, 1))
})

test_that("a permutation p-value prints as it is, with its marks", {
  expect_equal(.format_perm_p(c(1 / 21, 0.05, NA)), c("0.0476", "0.05", NA))
  expect_equal(.signif_marks(c(1 / 1001, 0.002, 0.2, NA)),
               c("***", "**", "", ""))
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

# ---- outputs -------------------------------------------------------------------

# `outputs =` names the optional parts of a fit only (D23). The default keeps
# a fit as it was before 0.7.0.
test_that("the default outputs keep the observed model and the confusion matrix", {
  b <- qap_net_binary(n = 12)
  fit <- estimate_mrqap(. ~ ego(Age), b, times = 5, control = list(seed = 1))
  expect_equal(fit$outputs, c("model", "confusion"))
  expect_false(is.null(fit$simple_fit))
  expect_s3_class(fit$confusion_matrix, "infernetConfusion")
  expect_null(fit$draws)
})

test_that("outputs leaves out the observed model and the confusion matrix", {
  b <- qap_net_binary(n = 12)
  fit <- estimate_mrqap(. ~ ego(Age), b, times = 5, control = list(seed = 1),
                        outputs = character(0))
  expect_null(fit$simple_fit)
  expect_null(fit$confusion_matrix)
  # What is left out is made again from the baseline fit.
  full <- estimate_mrqap(. ~ ego(Age), b, times = 5, control = list(seed = 1))
  expect_equal(coef(fit), coef(full))
  expect_equal(glance(fit)$deviance, glance(full)$deviance)
  expect_equal(diagnose_confusion(fit, control = list(seed = 1))$auc,
               full$confusion_matrix$auc)
})

test_that("outputs = \"draws\" keeps the permuted coefficients", {
  g <- qap_net_gaussian(n = 12)
  out <- estimate_mrqap(weight ~ ego(Age) + alter(Age), g, times = 8,
                        control = list(seed = 1, permute = "outcome"),
                        outputs = "draws")
  expect_equal(dim(out$draws), dim(out$testdist))
  expect_equal(colnames(out$draws), names(out$coefficients))
  # The share of permuted coefficients at or above the observed one is the
  # upper p-value of the coefficient, as the engine counts it.
  b <- out$coefficients[["ego_Age"]]
  expect_equal(out$larger[1, "ego_Age"],
               (1 + sum(out$draws[, "ego_Age"] >= b)) / (8 + 1))

  pred <- estimate_mrqap(weight ~ ego(Age) + alter(Age), g, times = 8,
                         control = list(seed = 1), outputs = "draws")
  expect_true(all(is.na(pred$draws[, "(Intercept)"])))
  expect_false(anyNA(pred$draws[, "ego_Age"]))
})

test_that("outputs aborts on a value it does not know", {
  g <- qap_net_gaussian(n = 10)
  expect_error(estimate_mrqap(weight ~ ego(Age), g, times = 5,
                              outputs = "chains"),
               "unknown value")
  expect_error(estimate_mrqap(weight ~ ego(Age), g, times = 5, outputs = 1),
               "character vector")
})

test_that("less_mem is deprecated, and leaves out the observed model", {
  warned <- .infernet_state$warned_less_mem
  .infernet_state$warned_less_mem <- NULL
  on.exit(.infernet_state$warned_less_mem <- warned)
  g <- qap_net_gaussian(n = 10)
  expect_warning(
    fit <- estimate_mrqap(weight ~ ego(Age), g, times = 5,
                          control = list(less_mem = TRUE)),
    "deprecated")
  expect_null(fit$simple_fit)
  expect_equal(fit$outputs, "confusion")
})
