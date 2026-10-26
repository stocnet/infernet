# A longitudinal network is a panel: the same nodes, observed again. Each wave
# but the first is fitted against the wave before it, `inertia`, `recip`, and
# `trans` read that previous wave, and one permutation relabels the nodes in
# every wave, so that each node keeps its trajectory under the null.

PANEL <- . ~ inertia + recip + trans + ego(age) + alter(age)

test_that("a longitudinal network is read as a panel", {
  g <- qap_net_panel(n = 12, waves = 3)
  expect_true(.is_panel(g))
  fit <- estimate_mrqap(PANEL, g, times = 10, control = list(seed = 1))
  expect_true(fit$panel)
  # Waves 2 and 3, each against the wave before it.
  expect_equal(nrow(fit$pred), 2 * 12 * 11)
  expect_length(unique(fit$pred$nv), 2L)
  expect_named(fit$coefficients,
               c("(Intercept)", "inertia", "recip", "trans", "ego_age", "alter_age"))
  expect_qap_shape(fit, names(fit$coefficients))
})

test_that("inertia, recip, and trans read the previous wave", {
  waves <- manynet::to_waves(qap_net_panel(n = 10))
  ml <- convertToMatrixList(. ~ inertia + recip + trans, waves[[2]],
                            advise = FALSE, previous = waves[[1]])
  prev <- manynet::as_matrix(waves[[1]])
  expect_equal(unname(ml$mydata$inertia), unname((prev > 0) * 1))
  expect_equal(unname(ml$mydata$recip), unname(t(prev)))
  two <- prev %*% prev
  diag(two) <- 0
  expect_equal(unname(ml$mydata$trans), unname(two))
})

test_that("a cross-sectional network drops inertia and trans, with a warning", {
  one <- manynet::to_wave(qap_net_panel(n = 10), 2)
  expect_false(.is_panel(one))
  expect_warning(
    fit <- estimate_mrqap(. ~ inertia + trans + ego(age), one, times = 5,
                          control = list(seed = 1)),
    "needs a longitudinal network")
  expect_named(fit$coefficients, c("(Intercept)", "ego_age"))
  expect_false(fit$panel)
})

test_that("a panel records each lagged term as fitted, and as an MRQAP term", {
  g <- qap_net_panel(n = 10)
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_no_message(f <- specify_mrqap(. ~ inertia + trans + ego(age), g),
                    message = "written for")
  rec <- tidy(f, .data = g)
  expect_equal(rec$status, rep("fits", 3))
  expect_equal(unlist(rec$columns), c("inertia", "trans", "ego_age"))
  expect_equal(attr(f, "input"), "mrqap")
})

test_that("a list of waves is read as independent networks, not as a panel", {
  waves <- manynet::to_waves(qap_net_panel(n = 10))
  expect_false(.is_panel(waves))
  fit <- estimate_mrqap(. ~ ego(age), waves, times = 5, control = list(seed = 1))
  expect_false(fit$panel)
  expect_equal(nrow(fit$pred), 3 * 10 * 9)
})

test_that("one draw relabels every wave the same way", {
  m1 <- matrix(seq_len(36), 6, 6)
  m2 <- m1 + 100
  set.seed(1)
  p <- .permute_panel(list(m1, m2))
  expect_equal(p[[2]] - 100, p[[1]])
  # A two-mode panel relabels its rows and its columns, each the same way.
  r1 <- matrix(seq_len(4 * 7), 4, 7)
  set.seed(2)
  q <- .permute_panel(list(r1, r1 * 10))
  expect_equal(q[[2]], q[[1]] * 10)
  expect_setequal(as.vector(q[[1]]), as.vector(r1))
})

test_that("the blocks of a panel keep every wave's missing cells in place", {
  n <- 8
  w1 <- matrix(stats::rnorm(n^2), n, n)
  w2 <- matrix(stats::rnorm(n^2), n, n)
  diag(w1) <- diag(w2) <- 0
  w1[2, ] <- NA           # did not report in wave 1
  w2[c(2, 5), ] <- NA     # nodes 2 and 5 did not report in wave 2
  w2[7, 3] <- NA          # one scattered missing cell
  b <- .panel_blocks(list(w1, w2))
  expect_false(b$rows[2] == b$rows[5])
  expect_true(all(startsWith(b$rows[c(3, 7)], "fixed")))
  set.seed(3)
  for (i in seq_len(30)) {
    p <- .permute_panel(list(w1, w2), b)
    expect_identical(is.na(p[[1]]), is.na(w1))
    expect_identical(is.na(p[[2]]), is.na(w2))
  }
  expect_null(.panel_blocks(list(matrix(1, 3, 3), matrix(2, 3, 3))))
})

test_that("a panel with missing data uses the observed dyads in every draw", {
  g <- qap_net_panel(n = 12, waves = 3)
  waves <- manynet::to_waves(g)
  ml <- lapply(2:3, function(t) {
    convertToMatrixList(. ~ inertia + ego(age), waves[[t]], advise = FALSE,
                        previous = waves[[t - 1]])$mydata
  })
  matlist <- list(ties    = lapply(ml, `[[`, "ties"),
                  inertia = lapply(ml, `[[`, "inertia"),
                  ego_age = lapply(ml, `[[`, "ego_age"))
  matlist$ties[[1]][3, ] <- NA
  matlist$ties[[2]][c(3, 9), ] <- NA
  for (p in c("outcome", "predictor")) {
    fit <- QAPengine(ties ~ inertia + ego_age, matlist, family = "binomial",
                     permute = p, panel = TRUE, times = 20, seed = 1)
    expect_equal(nrow(fit$pred), 2 * 12 * 11 - 3 * 11)
  }
})
