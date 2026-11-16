# `diagnose_outliers()`, a diagnostic that `{infernet}` holds for every stocnet model (D22).

test_that("the shared diagnostics have the signature of goldfish", {
  # goldfish registers its methods on these generics, so a method must keep
  # the generic's arguments.
  expect_equal(names(formals(diagnose_outliers)), c("x", "..."))
  expect_equal(names(formals(diagnose_confusion)), c("x", "..."))
})

test_that("the shared diagnostics refuse a class with no method", {
  expect_error(diagnose_outliers(1:3), "no method")
})

test_that("diagnose_outliers() flags the dyad with the largest residual", {
  g <- qap_net_gaussian(n = 12)
  fit <- estimate_mrqap(weight ~ ego(Age) + alter(Age), g, times = 5,
                        control = list(seed = 1))
  out <- diagnose_outliers(fit, method = "Top", threshold = 1)
  expect_s3_class(out, "infernetOutliers")
  expect_equal(nrow(out), stats::nobs(fit))
  expect_equal(sum(out$outlier), 1)
  res <- stats::residuals(fit)
  expect_equal(which(out$outlier), which.max(abs(res)))
  expect_equal(out$.residual, as.numeric(res))
  expect_false(anyNA(out$label[out$outlier]))
  expect_true(all(is.na(out$label[!out$outlier])))
  expect_output(print(out), "1 of")
})

test_that("diagnose_outliers() flags by the Hampel identifier and the IQR", {
  fit <- estimate_mrqap(. ~ ego(Age), qap_net_binary(n = 12), times = 5,
                        control = list(seed = 1))
  res <- stats::residuals(fit, type = "pearson")
  hampel <- diagnose_outliers(fit)
  expect_equal(hampel$outlier,
               abs(res - stats::median(res)) > 3 * stats::mad(res))
  iqr <- diagnose_outliers(fit, method = "IQR", threshold = 2)
  expect_equal(iqr$outlier,
               abs(res - stats::median(res)) > stats::IQR(res))
  expect_error(diagnose_outliers(fit, threshold = -1), "positive")
})

test_that("diagnose_outliers() names the reporter of a CSS dyad", {
  fit <- suppressMessages(
    estimate_mrqap(advice ~ ego(age) + friends, css_hightech(),
                   times = 3, control = list(seed = 1)))
  out <- diagnose_outliers(fit, method = "Top", threshold = 2)
  expect_true("reporter" %in% names(out))
  expect_match(out$label[out$outlier], " by ")
})

test_that("node labels read the factor labels, not their codes", {
  x <- list(node_names = c("a", "b", "c", "d", "e"))
  # Reporters 2 and 5 only: the codes are 1 and 2, the labels 2 and 5.
  expect_equal(.node_label(x, factor(c(2, 5, 5)), "bv"), c("b", "e", "e"))
  expect_equal(.node_label(list(node_names = NULL), factor(c(2, 5)), "sv"), c(2L, 5L))
})

test_that("the receivers of a two-mode fit are named from the second mode", {
  net <- manynet::mutate_nodes(manynet::ison_southern_women,
                               x = seq_len(manynet::net_nodes(manynet::ison_southern_women)))
  fit <- suppressWarnings(suppressMessages(
    estimate_mrqap(. ~ ego(x), net, times = 2, control = list(seed = 1))))
  out <- diagnose_outliers(fit)
  women <- manynet::node_names(net)[!manynet::node_is_mode(net)]
  events <- manynet::node_names(net)[manynet::node_is_mode(net)]
  expect_true(all(out$sender %in% women))
  expect_true(all(out$receiver %in% events))
})
