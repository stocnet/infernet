# Shared fixtures and helpers.
#
# Every fixture is seeded and carries real signal, so that each family converges
# and the reference comparison is not testing noise against noise.
# `times` stays small everywhere: these tests check the estimator, not the
# precision of the null distribution.

# ---- fixtures ---------------------------------------------------------------

qap_net_gaussian <- function(n = 30, seed = 101) {
  set.seed(seed)
  age <- stats::runif(n, 20, 60)
  m <- outer(age, age, function(a, b) 0.05 * a - 0.03 * b) +
    matrix(stats::rnorm(n^2, sd = 0.5), n, n)
  diag(m) <- 0
  manynet::mutate(manynet::as_tidygraph(m),
                  Age = age,
                  Cit = stats::rpois(n, 5),
                  Grp = rep(c("a", "b", "c"), length.out = n))
}

qap_net_binary <- function(n = 30, seed = 102) {
  set.seed(seed)
  age <- stats::runif(n, 20, 60)
  p <- stats::plogis(outer(age, age, function(a, b) 0.06 * (a - 40) - 0.04 * (b - 40)))
  m <- matrix(stats::rbinom(n^2, 1, p), n, n)
  diag(m) <- 0
  manynet::mutate(manynet::as_tidygraph(m),
                  Age = age,
                  Grp = rep(c("a", "b", "c"), length.out = n))
}

qap_net_count <- function(n = 30, seed = 103) {
  set.seed(seed)
  age <- stats::runif(n, 20, 60)
  lambda <- exp(outer(age, age, function(a, b) 0.02 * (a - 40) - 0.01 * (b - 40)))
  m <- matrix(stats::rpois(n^2, lambda), n, n)
  diag(m) <- 0
  manynet::mutate(manynet::as_tidygraph(m), Age = age)
}

qap_net_zip <- function(n = 30, seed = 104) {
  set.seed(seed)
  age <- stats::runif(n, 20, 60)
  lambda <- exp(outer(age, age, function(a, b) 0.02 * (a - 40) - 0.01 * (b - 40)))
  m <- matrix(stats::rpois(n^2, lambda) * stats::rbinom(n^2, 1, 0.7), n, n)
  diag(m) <- 0
  manynet::mutate(manynet::as_tidygraph(m), Age = age)
}

qap_net_undirected <- function(n = 24, seed = 105) {
  set.seed(seed)
  age <- stats::runif(n, 20, 60)
  m <- outer(age, age, function(a, b) 0.02 * (a + b)) +
    matrix(stats::rnorm(n^2, sd = 0.5), n, n)
  m <- (m + t(m)) / 2
  diag(m) <- 0
  manynet::mutate(manynet::as_tidygraph(m, twomode = FALSE), Age = age)
}

qap_net_twomode <- function(seed = 106) {
  set.seed(seed)
  sw <- manynet::ison_southern_women
  manynet::mutate(sw, Att = stats::runif(manynet::net_nodes(sw)))
}

# ---- reference data ---------------------------------------------------------

# Rebuilds the dyad-level data frame that the engine fits, so that a baseline
# coefficient can be compared against the equivalent standard fit on identical
# data. Anything this returns comes from the engine's own internals, so a
# comparison against it tests the estimator dispatch, not the vectorisation.
qap_reference_data <- function(formula, .data, mode = NULL, diag = FALSE) {
  ml <- convertToMatrixList(formula, .data, advise = FALSE)
  parsed <- parse_qap_formula(ml$formula)
  g <- manynet::as_tidygraph(.data)
  if (is.null(mode)) {
    mode <- if (manynet::is_directed(g)) "digraph" else "graph"
  }
  pred <- make_qap_data(y = ml$mydata[[parsed$dependent]],
                        x = ml$mydata[parsed$main],
                        diag = diag, mode = mode)
  names(pred)[names(pred) == "yv"] <- parsed$dependent
  list(pred = pred, formula = ml$formula, parsed = parsed)
}

# ---- expectations -----------------------------------------------------------

# The p-value matrices are the product of the permutation loop, so their shape
# is the contract every estimator has to meet, whatever it fits underneath.
expect_qap_shape <- function(fit, coefs) {
  testthat::expect_s3_class(fit, "net_regression")
  testthat::expect_named(fit$coefficients, coefs)
  for (el in c("lower", "larger", "abs")) {
    m <- fit[[el]]
    testthat::expect_equal(dim(m), c(2L, length(coefs)),
                           info = paste("dim of", el))
    testthat::expect_equal(rownames(m), c("perm_coefs", "perm_t"),
                           info = paste("rownames of", el))
    testthat::expect_equal(colnames(m), coefs, info = paste("colnames of", el))
    finite <- m[!is.na(m)]
    testthat::expect_true(all(finite >= 0 & finite <= 1),
                          info = paste(el, "outside [0, 1]"))
  }
  invisible(fit)
}
