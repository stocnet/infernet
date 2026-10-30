# Missing data: every permutation must be fitted on the dyads of the observed
# model. Recomputing the dyads after a free permutation moved the missing cells
# of the permuted matrix but not those of the others, so the draws were fitted
# on more dyads than the observed model, and their p-values were biased.
#
# The engine permutes nodes only among nodes with the same unobserved cells.
# These assert that the missing cells map onto themselves under that
# permutation, for each shape of network, and that the engine uses it.

test_that("a free permutation changes the dyads that reach the model", {
  ml <- holed_matlist()
  observed <- nrow(make_qap_data(ml$y, ml[c("ego", "alter")]))
  set.seed(1)
  counts <- vapply(seq_len(50), function(i) {
    nrow(make_qap_data(RMPerm(ml$y), ml[c("ego", "alter")]))
  }, integer(1))
  expect_true(any(counts != observed))
})

test_that("blocks keep the missing cells of a one-mode network in place", {
  ml <- holed_matlist()
  set.seed(2)
  expect_missing_fixed(ml$y, .missing_blocks(ml$y))
  # Residualising leaves NA wherever any variable is unobserved.
  both <- ml$y
  both[is.na(ml$ego)] <- NA
  expect_missing_fixed(both, .missing_blocks(both))
})

test_that("a node with scattered missing cells stays in place", {
  m <- matrix(stats::rnorm(100), 10, 10)
  diag(m) <- 0
  m[3, 7] <- NA
  m[9, 2] <- NA
  b <- .missing_blocks(m)
  expect_setequal(which(startsWith(b$rows, "fixed")), c(2, 3, 7, 9))
  set.seed(3)
  expect_missing_fixed(m, b)
  for (i in seq_len(20)) {
    o <- .perm_order(10, b$rows)
    expect_equal(o[c(2, 3, 7, 9)], c(2, 3, 7, 9))
  }
})

test_that("a block of one node is not read as a range by sample()", {
  set.seed(4)
  for (i in seq_len(20)) {
    o <- .perm_order(6, c("a", "b", "b", "c", "c", "c"))
    expect_equal(o[1], 1L)
    expect_setequal(o, 1:6)
  }
})

test_that("a non-respondent and a node nobody named are separate blocks", {
  m <- matrix(stats::rnorm(64), 8, 8)
  diag(m) <- 0
  m[1:2, ] <- NA          # did not report
  m[, 3] <- NA            # was not named: column only
  b <- .missing_blocks(m)
  expect_equal(length(unique(b$rows[1:2])), 1L)
  expect_false(b$rows[1] == b$rows[3])
  expect_false(b$rows[3] == b$rows[4])
  set.seed(5)
  expect_missing_fixed(m, b)
})

test_that("blocks keep the missing cells of a two-mode network in place", {
  m <- matrix(stats::rnorm(12 * 20), 12, 20)
  m[c(2, 5), ] <- NA
  m[, c(1, 8, 9)] <- NA
  m[11, 15] <- NA
  b <- .missing_blocks(m)
  expect_length(b$rows, 12L)
  expect_length(b$cols, 20L)
  expect_true(startsWith(b$rows[11], "fixed"))
  expect_true(startsWith(b$cols[15], "fixed"))
  set.seed(6)
  expect_missing_fixed(m, b)
})

test_that("blocks keep the missing reports of a CSS in place", {
  n <- 8
  a <- array(stats::rbinom(n^3, 1, 0.3), dim = c(n, n, n))
  a[, , c(2, 6)] <- NA    # two reporters who were not asked
  a[4, 5, 1] <- NA        # one scattered missing report
  b <- .missing_blocks(a)
  expect_equal(b$rows[2], b$rows[6])
  expect_setequal(which(startsWith(b$rows, "fixed")), c(1, 4, 5))
  set.seed(7)
  expect_missing_fixed(a, b, CSS = TRUE)

  # A two-mode CSS has a slice for every node of both modes.
  tm <- array(stats::rbinom(4 * 5 * 9, 1, 0.3), dim = c(4, 5, 9))
  tm[, , c(1, 7)] <- NA
  b2 <- .missing_blocks(tm)
  expect_length(b2$rows, 4L)
  expect_length(b2$cols, 5L)
  set.seed(8)
  expect_missing_fixed(tm, b2, CSS = TRUE)
})

test_that("blocks combine with the user's groups", {
  ml <- holed_matlist()
  grp <- rep(c("a", "b"), 10)
  b <- .missing_blocks(ml$y, grp)
  set.seed(9)
  for (i in seq_len(20)) {
    o <- .perm_order(20, b$rows)
    expect_equal(grp[o], grp)
  }
  expect_missing_fixed(ml$y, b)
})

test_that("a complete network keeps the user's groups unchanged", {
  m <- matrix(stats::rnorm(36), 6, 6)
  diag(m) <- NA           # a loop the model leaves out is not missing
  expect_null(.missing_blocks(m))
  expect_null(.permutation_blocks(m, NULL, FALSE, 10, "y"))
})

test_that("every permutation uses the observed model's dyads", {
  ml <- holed_matlist()
  for (p in c("outcome", "predictor")) {
    fit <- QAPengine(y ~ ego + alter, ml, permute = p, times = 30, seed = 1)
    # 16 observed nodes, five of them unobserved as senders on `ego`.
    expect_equal(nrow(fit$pred), 16 * 15 - 5 * 15)
    expect_equal(fit$missing$used, nrow(fit$pred))
    expect_false(anyNA(fit$lower[, -1]))
    expect_equal(sum(is.na(fit$testdist[, "ego"])), 0L)
  }
})

test_that("a draw on other dyads than the observed model's is caught", {
  ml <- holed_matlist()
  parsed <- parse_qap_formula(y ~ ego + alter)
  pred <- make_qap_data(ml$y, ml[c("ego", "alter")])
  base <- fit_qap_model(y ~ ego + alter, stats::setNames(pred, sub("yv", "y", names(pred))),
                        "gaussian", main_vars = c("ego", "alter"))
  set.seed(10)
  # Without the blocks, some draw moves the outcome's missing cells.
  expect_error(
    for (i in seq_len(30)) {
      QAPPermEst(i, matlist. = ml, perm_var. = NULL, shape. = .qap_shape(),
                 directed. = TRUE, loops. = FALSE, mod. = y ~ ego + alter,
                 groups. = NULL, fit. = base, family. = "gaussian",
                 use_robust_errors. = FALSE, has_random. = FALSE,
                 main_vars. = c("ego", "alter"), data_vars. = c("ego", "alter"),
                 parsed. = parsed, perm_groups. = NULL, n_obs. = nrow(pred))
    },
    "bug in")
})

test_that("a two-mode network with missing rows and columns fits", {
  set.seed(12)
  y <- matrix(stats::rnorm(10 * 16), 10, 16)
  y[c(1, 2), ] <- NA
  y[, c(3, 4, 5)] <- NA
  x <- matrix(stats::rnorm(10 * 16), 10, 16)
  x[7, 9] <- NA
  fit <- QAPengine(y ~ x, list(y = y, x = x), times = 20, seed = 1)
  expect_equal(nrow(fit$pred), 8 * 13 - 1)
  expect_equal(unname(fit$missing$fixed_nodes), 0)
})

test_that("a CSS with reporters who were not asked fits", {
  n <- 8
  set.seed(13)
  y <- array(stats::rbinom(n^3, 1, 0.35), dim = c(n, n, n))
  x <- array(rep(as.vector(outer(stats::runif(n), stats::runif(n), "+")), n),
             dim = c(n, n, n))
  y[, , c(3, 5)] <- NA
  fit <- QAPengine(ties ~ cov, list(ties = y, cov = x), css = TRUE,
                   family = "binomial", times = 20, seed = 1)
  expect_equal(nrow(fit$pred), (n - 2) * (n * n - n))
  expect_equal(fit$missing$used, nrow(fit$pred))
})

test_that("a pooled list keeps each network's missing cells in place", {
  a <- holed_matlist(seed = 1)
  b <- holed_matlist(seed = 2)
  b$y[] <- stats::rnorm(400)   # complete outcome in the second network
  diag(b$y) <- 0
  ml <- list(y = list(a$y, b$y), ego = list(a$ego, b$ego),
             alter = list(a$alter, b$alter))
  fit <- QAPengine(y ~ ego + alter, ml, permute = "outcome", times = 20,
                   seed = 1)
  expect_equal(nrow(fit$pred), (16 * 15 - 5 * 15) + (20 * 19 - 5 * 19))
})

test_that("few distinct permutations warn, and none abort", {
  m <- matrix(stats::rnorm(16), 4, 4)
  diag(m) <- 0
  m[1, 2] <- NA
  # Nodes 1 and 2 are fixed, so only nodes 3 and 4 can swap: two orders.
  expect_snet_warning(.permutation_blocks(m, NULL, FALSE, 10, "y"),
                      "distinct permutation")
  m[3, 4] <- NA
  expect_error(.permutation_blocks(m, NULL, FALSE, 10, "y"),
               "No permutation")
})

test_that("a residualised predictor is NA outside the model, and symmetric", {
  y <- matrix(stats::rnorm(25), 5, 5)
  pred <- make_qap_data(y, list(), directed = FALSE)
  out <- residuals_to_matrix(seq_len(nrow(pred)), matrix(99, 5, 5), pred,
                             directed = FALSE)
  expect_true(all(is.na(diag(out))))
  expect_false(any(out == 99, na.rm = TRUE))
  expect_equal(out[upper.tri(out)], t(out)[upper.tri(out)])
})

test_that("missing = \"fail\" stops on an unobserved dyad, and only then", {
  g <- qap_net_gaussian(n = 15)
  expect_no_error(estimate_mrqap(weight ~ ego(Age), g, times = 5,
                                 control = list(seed = 1, missing = "fail")))
  m <- manynet::as_matrix(g)
  m[2, 3] <- NA
  holed <- manynet::mutate(manynet::as_tidygraph(m),
                           Age = manynet::node_attribute(g, "Age"))
  expect_error(estimate_mrqap(weight ~ ego(Age), holed, times = 5,
                              control = list(seed = 1, missing = "fail")),
               "1 on weight")
  fit <- estimate_mrqap(weight ~ ego(Age), holed, times = 5,
                        control = list(seed = 1))
  expect_equal(fit$missing$treatment, "available")
  expect_equal(fit$missing$used, 15 * 14 - 1)
  expect_equal(fit$missing$unobserved[["weight"]], 1L)
})

test_that("missing takes only the treatments that exist", {
  g <- qap_net_gaussian(n = 10)
  expect_error(estimate_mrqap(weight ~ ego(Age), g, times = 5,
                              control = list(missing = "impute")),
               "not yet available")
  expect_error(estimate_mrqap(weight ~ ego(Age), g, times = 5,
                              control = list(missing = "listwise")),
               "must be one of")
})

test_that("a fixed tie is left out of the model and counted apart", {
  g <- fixed_net()
  expect_equal(sum(.fixed_ties(g)), 6)
  fit <- estimate_mrqap(. ~ ego(Age) + alter(Age), g, times = 10,
                        control = list(seed = 1))
  expect_equal(nrow(fit$pred), 15 * 14 - 6)
  expect_equal(fit$missing$fixed_ties, 6)
  expect_equal(fit$missing$possible, 15 * 14 - 6)
  expect_equal(fit$missing$unobserved[[fit$dep]], 0L)
  expect_output(print(fit), "6 fixed ties left out")
})

test_that("missing = \"fail\" does not stop on a fixed tie", {
  expect_no_error(estimate_mrqap(. ~ ego(Age), fixed_net(), times = 5,
                                 control = list(seed = 1, missing = "fail")))
})

test_that("a network with no fixed tie has no fixed mask", {
  expect_null(.fixed_ties(qap_net_binary(n = 10)))
  g <- manynet::mutate_ties(qap_net_binary(n = 10), fixed = FALSE)
  expect_null(.fixed_ties(g))
})

test_that("a fixed tie keeps its value, and stays among the fixed cells", {
  m <- matrix(seq_len(64) + 0.5, 8, 8)
  diag(m) <- 0
  fixed <- matrix(FALSE, 8, 8)
  fixed[2, 5] <- fixed[6, 1] <- TRUE
  b <- .missing_blocks(m, exclude = fixed)
  expect_setequal(which(startsWith(b$rows, "fixed")), c(1, 2, 5, 6))
  set.seed(11)
  for (i in seq_len(20)) {
    p <- RMPerm(m, b)
    # The fixed cells map onto themselves, so no fixed value enters a free cell.
    expect_setequal(p[fixed], m[fixed])
    expect_false(anyNA(p))
  }
  pred <- make_qap_data(m, list(), exclude = fixed)
  expect_equal(nrow(pred), 8 * 7 - 2)
  s <- .missing_summary(list(y = m), "y", character(0), FALSE, TRUE, FALSE,
                        exclude = fixed)
  expect_equal(s$excluded, 2)
  expect_equal(s$possible, 8 * 7 - 2)
  expect_equal(s$by_variable[["y"]], 0L)
})
