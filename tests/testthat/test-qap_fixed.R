# Fixed effects, written after a bare bar as in {fixest}: `y ~ x | sender`.
#
# A gaussian model absorbs them once, by demeaning, so its slopes and t-values
# must equal those of `lm()` with a dummy for every group. Any other family
# absorbs them in every fit with {fixest}, so its slopes must equal those of
# `fixest::feglm()`. The predictors here are dyadic, since `ego()` and
# `alter()` do not vary within a sender or a receiver.

FE_FORM <- weight ~ diff(Age) + same(Grp)
FE_FORM_SR <- weight ~ diff(Age) + same(Grp) | sender + receiver
FE_COEFS <- c("(Intercept)", "diff_Age", "same_Grp")

# ---- grammar -----------------------------------------------------------------

test_that("a bare bar names fixed effects, and a bracketed bar a random intercept", {
  spec <- .specify(weight ~ ego(Age) + (1 | network) | sender + receiver)
  expect_equal(spec$fixed, c("sender", "receiver"))
  expect_equal(spec$random, "network")
  expect_length(spec$terms, 1)
  expect_length(.specify(weight ~ ego(Age))$fixed, 0)
})

test_that("a bare bar that cannot be read aborts and says why", {
  expect_error(.specify(y ~ x | dept), "has no fixed effect")
  expect_error(.specify(y ~ x | sender | receiver), "more than one bar")
  expect_error(.specify(y ~ x + (1 | sender) | sender),
               "both a fixed effect and a random intercept")
})

test_that("each model's formula says what it makes of a fixed effect", {
  f <- specify_mrqap(advice ~ same(dept) | sender + receiver)
  expect_equal(deparse(f[[3]]), "same(dept) | sender + receiver")
  expect_equal(tidy(f)$status, c("fits", "fits", "fits"))

  d <- suppressWarnings(specify_dynam(advice ~ same(dept) | sender))
  expect_false(grepl("|", deparse(d), fixed = TRUE))
  expect_equal(tidy(d)$status[tidy(d)$term == "| sender"], "drops")

  e <- specify_ergm(advice ~ same(dept) | sender + receiver)
  expect_true(all(c("sender", "receiver") %in% all.vars(e)))
  expect_equal(tidy(e)$as[tidy(e)$term == "| sender"], "sender")
})

# ---- gaussian: absorbed once ---------------------------------------------------

test_that("gaussian fixed effects match lm() with a dummy for each group", {
  g <- qap_net_gaussian()
  ref <- qap_reference_data(FE_FORM, g)
  dummies <- stats::lm(weight ~ diff_Age + same_Grp + factor(sv) + factor(rv),
                       data = ref$pred)
  for (permute in c("predictor", "outcome")) {
    fit <- estimate_mrqap(FE_FORM_SR, g,
                          times = 10, control = list(seed = 1, permute = permute))
    expect_qap_shape(fit, FE_COEFS)
    expect_true(is.na(fit$coefficients[["(Intercept)"]]))
    expect_equal(unname(fit$coefficients[-1]), unname(stats::coef(dummies)[2:3]))
    expect_equal(unname(fit$obs[-1]),
                 unname(summary(dummies)$coefficients[2:3, 3]))
    expect_equal(fit$fixed_effects, c("sender", "receiver"))
    expect_false(anyNA(fit$testdist[, -1]))
  }
})

test_that("the result keeps the observed values, not the demeaned ones", {
  g <- qap_net_gaussian()
  fit <- estimate_mrqap(weight ~ diff(Age) | sender, g, times = 5,
                        control = list(seed = 1))
  ref <- qap_reference_data(weight ~ diff(Age), g)
  expect_equal(fit$pred$weight, ref$pred$weight)
})

test_that("a demeaned matrix keeps zero group means under a permutation", {
  set.seed(7)
  y <- matrix(stats::rnorm(15^2), 15)
  y[2, ] <- NA; y[, 2] <- NA
  pred <- make_qap_data(y, list())
  ids <- .fixed_ids(pred, c("sv", "rv"))
  dm <- .demean(matrix(pred$yv), ids)
  m <- y
  m[pred$location] <- dm
  blocks <- .missing_blocks(y)
  for (i in 1:10) {
    p <- make_qap_data(RMPerm(m, blocks), list())
    expect_identical(p$location, pred$location)
    expect_lt(max(abs(rowsum(p$yv, p$sv))), 1e-8)
    expect_lt(max(abs(rowsum(p$yv, p$rv))), 1e-8)
  }
})

test_that("the absorbed parameters count connected groups once", {
  # Two crossed effects of 3 and 3 groups, all connected, absorb 3 + 3 - 1.
  expect_equal(.fixed_rank(list(c(1, 1, 2, 2, 3), c(1, 2, 2, 3, 3))), 5)
  # Split into two sets that share no observation, they absorb one fewer.
  expect_equal(.fixed_rank(list(c(1, 1, 2, 3, 3), c(1, 2, 2, 3, 3))), 4)
  expect_equal(.fixed_rank(list(c(1, 2, 2))), 2)
})

# ---- other families: absorbed in every fit ---------------------------------------

test_that("binomial fixed effects match fixest::feglm()", {
  skip_if_not_installed("fixest")
  b <- qap_net_binary()
  ref <- qap_reference_data(FE_FORM, b)
  fe <- fixest::feglm(weight ~ diff_Age + same_Grp | sv + rv, data = ref$pred,
                      family = stats::binomial(), notes = FALSE)
  fit <- estimate_mrqap(FE_FORM_SR, b,
                        times = 10, control = list(seed = 1))
  expect_qap_shape(fit, FE_COEFS)
  expect_equal(unname(fit$coefficients[-1]), unname(stats::coef(fe)))
  expect_equal(unname(fit$obs[-1]),
               unname(fixest::coeftable(fe, vcov = "iid")[, 3]))
  expect_false(is.null(fit$confusion_matrix))
})

test_that("Poisson fixed effects on a two-mode network match fixest::fepois()", {
  skip_if_not_installed("fixest")
  g <- qap_net_twomode_wide()
  ref <- qap_reference_data(weight ~ ego(Att), g)
  fe <- fixest::fepois(weight ~ ego_Att | rv, data = ref$pred, notes = FALSE)
  fit <- estimate_mrqap(weight ~ ego(Att) | receiver, g, times = 5,
                        control = list(seed = 1, family = "poisson"))
  expect_equal(unname(fit$coefficients[-1]), unname(stats::coef(fe)))
  expect_equal(fit$fixed_effects, "receiver")
})

test_that("a list of networks nests each node's group in its network", {
  skip_if_not_installed("fixest")
  nets <- list(qap_net_binary(seed = 1), qap_net_binary(seed = 2))
  ref <- do.call(rbind, lapply(1:2, function(i) {
    d <- qap_reference_data(weight ~ same(Grp), nets[[i]])$pred
    d$nv <- i
    d
  }))
  fe <- fixest::feglm(weight ~ same_Grp | nv + nv^sv + nv^rv, data = ref,
                      family = stats::binomial(), notes = FALSE)
  fit <- estimate_mrqap(weight ~ same(Grp) | network + sender + receiver, nets,
                        times = 5, control = list(seed = 1))
  expect_equal(unname(fit$coefficients[-1]), unname(stats::coef(fe)))
})

test_that("a cognitive social structure absorbs a reporter fixed effect", {
  fit <- estimate_mrqap(advice ~ same(dept) + reports | reporter,
                        manynet::ison_hightech, times = 5,
                        control = list(seed = 1, family = "gaussian"))
  expect_s3_class(fit, "netreg_css")
  expect_equal(fit$fixed_effects, "reporter")
  expect_output(print(fit), "Fixed effects were absorbed for each reporter")
})

# ---- what cannot be fitted ------------------------------------------------------

test_that("a predictor that the fixed effects absorb is named", {
  expect_error(
    estimate_mrqap(weight ~ ego(Age) + same(Grp) | sender, qap_net_gaussian(),
                   times = 5),
    "absorbs \"ego_Age\"")
})

test_that("a fixed effect the model cannot have aborts and says why", {
  g <- qap_net_gaussian()
  expect_error(estimate_mrqap(weight ~ diff(Age) | network, g, times = 5),
               "One network has no network fixed effect")
  expect_error(estimate_mrqap(weight ~ diff(Age) | reporter, g, times = 5),
               "no reporters")
  expect_error(estimate_mrqap(weight ~ diff(Age) + (1 | receiver) | sender, g,
                              times = 5),
               "both fixed effects and random intercepts")
  expect_error(estimate_mrqap(weight ~ diff(Age) | sender, g, times = 5,
                              control = list(use_robust_errors = TRUE)),
               "Robust standard errors")
  expect_error(estimate_mrqap(weight ~ diff(Age) | sender, qap_net_zip(),
                              times = 5, control = list(family = "zip")),
               "zip")
})

test_that("an undirected network drops its sender and receiver fixed effects", {
  expect_snet_warning(
    fit <- estimate_mrqap(weight ~ diff(Age) | sender, qap_net_undirected(),
                          times = 5, control = list(seed = 1)),
    "no senders or receivers")
  expect_length(fit$fixed_effects, 0)
  expect_false(is.na(fit$coefficients[["(Intercept)"]]))
})

test_that("a printed result says which fixed effects it absorbed", {
  fit <- estimate_mrqap(weight ~ diff(Age) | sender + receiver,
                        qap_net_gaussian(), times = 5, control = list(seed = 1))
  out <- capture.output(print(fit))
  expect_true(any(grepl("Fixed effects were absorbed for each sender and receiver", out)))
  expect_true(any(grepl("Within R-squared", out)))
})

test_that("a panel keeps each node's group across the waves", {
  g <- qap_net_panel(n = 12, waves = 3)
  fit <- estimate_mrqap(. ~ inertia + recip | sender + receiver, g, times = 5,
                        control = list(seed = 1, family = "gaussian"))
  p <- fit$pred
  names(p)[names(p) == fit$dep] <- "y"
  dummies <- stats::lm(y ~ inertia + recip + factor(sv) + factor(rv), data = p)
  expect_equal(unname(fit$coefficients[-1]), unname(stats::coef(dummies)[2:3]))
  expect_equal(unname(fit$obs[-1]),
               unname(summary(dummies)$coefficients[2:3, 3]))
})
