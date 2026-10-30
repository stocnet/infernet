# `diagnose_confusion()`: its metrics, its ROC curve and AUC, against values
# computed by hand, and the models it refuses.

test_that("the threshold matrix and the AUC match a hand count", {
  actual <- c(0, 0, 1, 1, 1, 0)
  prob <- c(0.1, 0.6, 0.7, 0.4, 0.9, 0.2)
  out <- probabilistic_confusion_matrix(actual, prob, n_draws = 50, seed = 1)
  expect_s3_class(out, "diagnose_confusion")
  # At 0.5: tn = 2, fp = 1, fn = 1, tp = 2.
  expect_equal(as.vector(out$threshold_confusion_matrix), c(2, 1, 1, 2))
  expect_equal(out$threshold_accuracy, 4 / 6)
  # Of the 9 pairs of a tie and a non-tie, the tie is higher in 8.
  expect_equal(out$auc, 8 / 9)
  roc <- tidy(out)
  expect_named(roc, c("threshold", "fpr", "tpr"))
  expect_equal(roc$fpr, c(0, 0, 0, 1, 1, 2, 3) / 3)
  expect_equal(roc$tpr, c(0, 1, 2, 2, 3, 3, 3) / 3)
})

test_that("tied probabilities count one half in the AUC", {
  expect_equal(.auc(c(0, 1), c(0.5, 0.5)), 0.5)
  expect_true(is.na(.auc(c(1, 1), c(0.2, 0.8))))
})

test_that("a logistic and a linear probability model are diagnosed", {
  b <- qap_net_binary(n = 10)
  fit <- estimate_mrqap(. ~ ego(Age) + alter(Age), b, times = 5,
                        control = list(seed = 1))
  out <- diagnose_confusion(fit, times = 50)
  expect_s3_class(out, "diagnose_confusion")
  expect_equal(out$n, nobs(fit))
  # The seed of the fit is used where none is given, so a call reproduces.
  expect_equal(diagnose_confusion(fit, times = 50)$accuracy, out$accuracy)
  expect_named(glance(out), c("accuracy", "sensitivity", "specificity",
                              "precision", "f1", "auc", "threshold",
                              "threshold.accuracy", "nobs", "draws"))
  expect_equal(diagnose_confusion(fit, threshold = 0.9)$threshold, 0.9)
  lpm <- estimate_mrqap(. ~ ego(Age) + alter(Age), b, times = 5,
                        control = list(seed = 1, family = "gaussian"))
  expect_s3_class(diagnose_confusion(lpm, times = 50), "diagnose_confusion")
})

test_that("a model that is not of a binary outcome is refused", {
  g <- qap_net_gaussian(n = 10)
  fit <- estimate_mrqap(weight ~ ego(Age), g, times = 5)
  expect_error(diagnose_confusion(fit), "binary outcome")
  cnt <- estimate_mrqap(. ~ ego(Age), qap_net_count(n = 10), times = 5,
                        control = list(family = "poisson"))
  expect_error(diagnose_confusion(cnt), "poisson")
  expect_error(diagnose_confusion(1:3), "no method")
})

test_that("a confusion matrix prints, also under its old class", {
  old_width <- options(width = 80)
  on.exit(options(old_width))
  fit <- estimate_mrqap(. ~ ego(Age) + alter(Age), qap_net_binary(n = 10),
                        times = 5, control = list(seed = 1))
  out <- diagnose_confusion(fit, times = 50)
  expect_snapshot(print(out))
  old <- out
  class(old) <- "QAPConfusionMatrix"
  expect_output(print(old), "Confusion matrix")
})
