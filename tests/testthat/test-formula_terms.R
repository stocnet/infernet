# Each formula term builds one matrix, or several, for each dyad. These tests
# check each against a value computed by hand or by a direct loop on
# `formula_net()`, whose two layers are small enough to read.

terms_of <- function(formula, .data = formula_net(), ...) {
  convertToMatrixList(formula, .data, advise = FALSE, ...)$mydata
}

layer_of <- function(layer, .data = formula_net()) {
  m <- manynet::as_matrix(manynet::to_uniplex(manynet::as_tidygraph(.data),
                                              layer = layer))
  unname(m)
}

X <- c(3, 1, 4, 1, 5, 9)
GRP <- c("p", "q", "p", "q", "p", "q")

# ---- node attributes ---------------------------------------------------------

test_that("diff() is the absolute difference, and signed with identity", {
  m <- terms_of(a ~ diff(x))
  expect_equal(unname(m[["diff x"]]), abs(outer(X, X, "-")))
  m <- terms_of(a ~ diff(x, transformer_fn = identity))
  expect_equal(unname(m[["diff x"]]), outer(X, X, "-"))
  expect_error(terms_of(a ~ diff(grp)), "same")
})

test_that("dist() is a deprecated alias that keeps its column name", {
  .infernet_state$dist_warned <- NULL
  expect_warning(m <- terms_of(a ~ dist(x)), "deprecated")
  expect_equal(unname(m[["dist x"]]), abs(outer(X, X, "-")))
  # Said once, not once per network of a list.
  expect_no_warning(terms_of(a ~ dist(x)))
})

test_that("match() gives one dummy per category where both hold it", {
  m <- terms_of(a ~ match(grp))
  expect_named(m, c("a", "match grp p", "match grp q"))
  expect_equal(unname(m[["match grp p"]]), outer(GRP == "p", GRP == "p") * 1)
  expect_equal(unname(m[["match grp q"]]), outer(GRP == "q", GRP == "q") * 1)
})

test_that("match() is refused on a two-mode network, pointing to mix()", {
  sw <- manynet::mutate(manynet::ison_southern_women,
                        grp = rep(c("u", "v"), length.out = 32))
  expect_error(terms_of(. ~ match(grp), sw), "mix")
})

test_that("mix() gives one dummy per ordered pair, less the first", {
  m <- suppressMessages(terms_of(a ~ mix(grp)))
  expect_named(m, c("a", "mix grp p->q", "mix grp q->p", "mix grp q->q"))
  expect_equal(unname(m[["mix grp q->p"]]), outer(GRP == "q", GRP == "p") * 1)
  # The pairs and the reference cover every dyad exactly once.
  total <- m[["mix grp p->q"]] + m[["mix grp q->p"]] + m[["mix grp q->q"]] +
    outer(GRP == "p", GRP == "p")
  expect_true(all(total == 1))
})

test_that("mix() names its reference pair", {
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(terms_of(a ~ mix(grp)), "reference")
})

test_that("mix() takes unordered pairs on an undirected network", {
  g <- manynet::mutate(qap_net_undirected(n = 8), grp = rep(c("p", "q"), 4))
  m <- suppressMessages(terms_of(weight ~ mix(grp), g))
  expect_named(m, c("weight", "mix grp p--q", "mix grp q--q"))
  expect_true(isSymmetric(unname(m[["mix grp p--q"]])))
})

test_that("mix() pairs the categories of the two modes of a two-mode network", {
  sw <- manynet::ison_southern_women
  type <- manynet::node_is_mode(sw)
  alt <- (seq_along(type) %% 2) + 1
  sw <- manynet::mutate(sw, grp = ifelse(type, c("e1", "e2")[alt], c("w1", "w2")[alt]))
  m <- suppressMessages(terms_of(. ~ mix(grp), sw))
  expect_named(m, c("ties", "mix grp w1->e2", "mix grp w2->e1", "mix grp w2->e2"))
})

# ---- layers --------------------------------------------------------------------

test_that("a layer gives the same matrix bare or through tie()", {
  m <- terms_of(a ~ b + tie(b))
  expect_equal(unname(m[["b"]]), unname(m[["tie b"]]))
  expect_equal(unname(m[["tie b"]]), layer_of("b"))
})

test_that("tie() gives 0/1 unless weighted", {
  g <- qap_net_gaussian(n = 10)
  g <- manynet::mutate_ties(g, w2 = abs(manynet::tie_attribute(g, "weight")))
  m <- terms_of(weight ~ tie(w2), g)
  expect_true(all(m[["tie w2"]] %in% c(0, 1)))
  m <- terms_of(weight ~ tie(w2, weighted = TRUE), g)
  expect_false(all(m[["tie w2"]] %in% c(0, 1)))
})

test_that("recip() is the transpose of the layer", {
  m <- terms_of(a ~ recip(b))
  expect_equal(unname(m[["recip b"]]), t(layer_of("b")))
})

test_that("indeg() and outdeg() count ties on the side they are asked for", {
  b <- layer_of("b")
  m <- terms_of(a ~ indeg(b) + indeg(b, type = "ego") +
                  outdeg(b) + outdeg(b, type = "ego"))
  n <- nrow(b)
  expect_equal(unname(m[["indeg b"]]), matrix(colSums(b), n, n, byrow = TRUE))
  expect_equal(unname(m[["ego indeg b"]]), matrix(colSums(b), n, n))
  expect_equal(unname(m[["outdeg b"]]), matrix(rowSums(b), n, n, byrow = TRUE))
  expect_equal(unname(m[["ego outdeg b"]]), matrix(rowSums(b), n, n))
})

test_that("indeg() on a two-mode network reads only the side that receives", {
  # Built directly: manynet gives a two-mode network with a second tie
  # attribute a square matrix, so a two-mode covariate layer cannot be made yet.
  g <- manynet::as_tidygraph(qap_net_twomode_wide(nr = 6, nc = 9))
  inc <- manynet::as_matrix(g)
  ctx <- list(net = g, twomode = TRUE, css = FALSE, outcome_layer = NA_character_,
              env = globalenv(), DV = inc)
  op <- .specify(y ~ indeg(weight))$terms[[1]]$operands[[1]]
  expect_equal(unname(.build_degree(op, ctx, "in")[["indeg weight"]]),
               unname(matrix(colSums(inc > 0), 6, 9, byrow = TRUE)))
  op <- .specify(y ~ indeg(weight, type = "ego"))$terms[[1]]$operands[[1]]
  expect_error(.build_degree(op, ctx, "in"), "two-mode")
})

test_that("a layer term refuses the ties of a uniplex outcome too", {
  g <- qap_net_gaussian(n = 10)
  expect_error(terms_of(weight ~ indeg(weight), g), "outcome layer")
})

test_that("a layer term refuses the outcome layer", {
  expect_error(terms_of(a ~ recip(a)), "outcome layer")
  expect_error(terms_of(a ~ indeg(a)), "outcome layer")
  expect_error(terms_of(a ~ tie(a)), "outcome layer")
})

test_that("recip() is refused on a two-mode network", {
  sw <- manynet::mutate_ties(manynet::ison_southern_women, ties = 1)
  expect_error(terms_of(. ~ recip(ties), sw), "two-mode")
})

# ---- others' attributes ----------------------------------------------------------

# A direct loop over the definition: the mean of x over the nodes k other than
# i that have a tie to j, and the mean of the defined cells where there are none.
tertius_by_loop <- function(m, x) {
  n <- nrow(m)
  out <- matrix(NA_real_, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i == j) next
    k <- setdiff(which(m[, j] > 0), c(i, j))
    if (length(k) > 0) out[i, j] <- mean(x[k])
  }
  out[is.na(out) & row(out) != col(out)] <- mean(out, na.rm = TRUE)
  diag(out) <- 0
  out
}

test_that("tertius() is the mean over j's other in-neighbours", {
  m <- terms_of(a ~ tertius(b, x))
  expect_equal(unname(m[["tertius b x"]]), tertius_by_loop(layer_of("b"), X))
  # By hand: A is reached by B (x = 1) and F (x = 9), so for the dyad from C
  # to A the mean is 5, and from F to A only B is left.
  expect_equal(unname(m[["tertius b x"]])[3, 1], 5)
  expect_equal(unname(m[["tertius b x"]])[6, 1], 1)
})

test_that("tertius_diff() is how far ego lies from that mean", {
  m <- terms_of(a ~ tertius_diff(b, x))
  b <- layer_of("b")
  n <- nrow(b)
  agg <- matrix(NA_real_, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    k <- setdiff(which(b[, j] > 0), c(i, j))
    if (i != j && length(k) > 0) agg[i, j] <- abs(X[i] - mean(X[k]))
  }
  agg[is.na(agg) & row(agg) != col(agg)] <- mean(agg, na.rm = TRUE)
  diag(agg) <- 0
  expect_equal(unname(m[["tertius_diff b x"]]), agg)
})

test_that("tertius() without a layer reads the outcome, and warns", {
  expect_warning(m <- terms_of(a ~ tertius(x)), "outcome's own ties")
  expect_equal(unname(m[["tertius x"]]), tertius_by_loop(layer_of("a"), X))
  expect_warning(terms_of(a ~ tertius(a, x)), "outcome's own ties")
})

test_that("tertius() sum is unchanged: the tie-weighted sum over the others", {
  m <- suppressWarnings(terms_of(a ~ tertius(b, x, "sum")))
  b <- layer_of("b")
  expected <- t(vapply(seq_len(6), function(i) colSums((b * X)[-i, , drop = FALSE]),
                       numeric(6)))
  expect_equal(unname(m[["tertius b x"]]), expected)
})

# ---- network-level variables -------------------------------------------------------

test_that("global() gives each pooled network its own value", {
  nets <- list(formula_net(gdp = 5), formula_net(gdp = 8), formula_net(gdp = 2))
  fit <- net_regression(a ~ ego(x) + global(gdp), nets, times = 5,
                        control = list(seed = 1))
  expect_true("global gdp" %in% names(fit$coefficients))
  expect_equal(sort(unique(fit$pred[["global gdp"]])), c(2, 5, 8))
})

test_that("global() on one network is refused, since it is the intercept", {
  expect_error(terms_of(a ~ global(gdp)), "intercept")
})

# ---- fitting -----------------------------------------------------------------

test_that("the layer terms fit a CSS under both permutation schemes", {
  for (permute in c("predictor", "outcome")) {
    fit <- suppressMessages(
      net_regression(advice ~ recip(friends) + indeg(friends) + same(dept),
                     manynet::ison_hightech, times = 5,
                     control = list(seed = 1, permute = permute)))
    expect_s3_class(fit, "netreg_css")
    expect_named(fit$coefficients,
                 c("(Intercept)", "recip friends", "indeg friends", "same dept"))
  }
})

test_that("the new terms fit a dyadic network under both permutation schemes", {
  for (permute in c("predictor", "outcome")) {
    fit <- suppressMessages(
      net_regression(a ~ recip(b) + indeg(b) + match(grp) + tertius(b, x),
                     formula_net(), times = 5,
                     control = list(seed = 1, permute = permute)))
    expect_s3_class(fit, "netreg")
    expect_false(anyNA(fit$coefficients))
  }
})

# ---- the registry and the dictionary -----------------------------------------

test_that("every registered term is in the dictionary in CONTRIBUTING", {
  path <- testthat::test_path("..", "..", ".github", "CONTRIBUTING.md")
  skip_if_not(file.exists(path), "CONTRIBUTING.md is not in the built package")
  guide <- readLines(path)
  start <- grep("^#### The dictionary", guide)
  expect_length(start, 1)
  stop <- start + grep("^#", guide[(start + 1):length(guide)])[1]
  rows <- guide[start:stop]
  for (term in names(.formula_terms())) {
    expect_true(any(grepl(paste0("`", term, "("), rows, fixed = TRUE)),
                info = term)
  }
})
