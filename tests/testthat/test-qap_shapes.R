# Properties of the dependent network -- modes, directedness, loops -- must be
# respected in the data the engine fits and in the permutations it draws.
# Each shape below was wrong at some point, so each is asserted by counting the
# dyads that reach the model rather than by checking that the call returns.

test_that("a directed network contributes every ordered dyad", {
  g <- qap_net_gaussian(n = 20)
  fit <- net_regression(weight ~ ego(Age), g, times = 10, control = list(seed = 1))
  expect_equal(fit$mode, "directed")
  expect_equal(nrow(fit$pred), 20 * 19)
})

test_that("an undirected network contributes each dyad once", {
  g <- qap_net_undirected(n = 24)
  expect_false(manynet::is_directed(g))
  fit <- net_regression(weight ~ ego(Age), g, times = 10, control = list(seed = 1))
  # Both halves of a symmetric matrix hold the same dyad. Keeping both doubles
  # the sample and shrinks every standard error by about a factor of root two.
  expect_equal(fit$mode, "undirected")
  expect_equal(nrow(fit$pred), 24 * 23 / 2)
})

test_that("a two-mode network contributes every cell of the incidence matrix", {
  sw <- qap_net_twomode()
  dims <- manynet::net_dims(sw)
  fit <- net_regression(. ~ ego(Att) + alter(Att), sw, times = 10,
                        control = list(seed = 1))
  # An incidence matrix is rectangular, and has no diagonal to drop. Reading it
  # as square wrapped past the last column and invented dyads.
  expect_equal(nrow(fit$pred), dims[1] * dims[2])
  expect_named(fit$coefficients, c("(Intercept)", "ego Att", "alter Att"))
})

test_that("RMPerm() permutes a rectangular matrix without erroring", {
  m <- matrix(seq_len(18 * 14), 18, 14)
  p <- RMPerm(m)
  expect_equal(dim(p), c(18L, 14L))
  # A permutation relabels the nodes, so it moves cells but keeps the multiset.
  expect_setequal(as.vector(p), as.vector(m))
})

test_that("RMPerm() keeps the row and column order aligned for a square matrix", {
  set.seed(4)
  m <- matrix(seq_len(36), 6, 6)
  p <- RMPerm(m)
  expect_equal(dim(p), c(6L, 6L))
  expect_setequal(as.vector(p), as.vector(m))
  # One order for both margins, so the diagonal stays the diagonal.
  expect_setequal(diag(p), diag(m))
})

test_that("dist() and sim() read each mode of a two-mode network separately", {
  sw <- qap_net_twomode()
  ml <- convertToMatrixList(. ~ ego(Att) + alter(Att) + dist(Att) + sim(Att),
                            sw, advise = FALSE)
  ego <- ml$mydata[["ego Att"]]
  alt <- ml$mydata[["alter Att"]]
  expect_equal(ml$mydata[["dist Att"]], abs(ego - alt))
  denom <- max(abs(ego - alt))
  expect_equal(ml$mydata[["sim Att"]], abs(1 - abs(ego - alt) / denom))
})

test_that("a list of networks is pooled, and one missing a predictor is dropped", {
  good <- list(qap_net_gaussian(n = 18, seed = 1),
               qap_net_gaussian(n = 18, seed = 2))
  fit <- net_regression(weight ~ ego(Age), good, times = 10,
                        control = list(seed = 1))
  expect_equal(nrow(fit$pred), 2 * 18 * 17)
  expect_length(unique(fit$pred$nv), 2L)

  bare <- manynet::as_tidygraph(matrix(stats::rnorm(18^2), 18, 18))
  dropped <- suppressWarnings(
    net_regression(weight ~ ego(Age), list(good[[1]], bare, good[[2]]),
                   times = 10, control = list(seed = 1)))
  expect_equal(nrow(dropped$pred), 2 * 18 * 17)
})

test_that("dropping a network from a list warns", {
  good <- qap_net_gaussian(n = 18, seed = 1)
  bare <- manynet::as_tidygraph(matrix(stats::rnorm(18^2), 18, 18))
  expect_snet_warning(
    net_regression(weight ~ ego(Age), list(good, bare), times = 10,
                   control = list(seed = 1)),
    "Dropping")
})

test_that("a missing dyad is dropped from the model", {
  g <- qap_net_gaussian(n = 20)
  m <- manynet::as_matrix(g)
  m[1, 2] <- NA
  holed <- manynet::mutate(manynet::as_tidygraph(m),
                           Age = manynet::node_attribute(g, "Age"))
  fit <- net_regression(weight ~ ego(Age), holed, times = 10,
                        control = list(seed = 1))
  expect_equal(nrow(fit$pred), 20 * 19 - 1)
})
