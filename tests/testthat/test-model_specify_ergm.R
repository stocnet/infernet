# `specify_ergm()` writes an infernet formula for ergm, with its network: each
# term as its named ergm term, or as `edgecov()` of the matrix infernet builds.
# These tests check each mapping on `formula_net()`, a two-mode and an
# undirected network, the terms that are dropped and the data that abort, and
# that ergm fits the result.

rhs_of <- function(f) attr(stats::terms(f), "term.labels")

built_of <- function(formula, .data = formula_net()) {
  convertToMatrixList(formula, .data, advise = FALSE)$mydata
}

layer_matrix <- function(layer, .data = formula_net()) {
  unname(manynet::as_matrix(manynet::to_uniplex(manynet::as_tidygraph(.data),
                                                layer = layer)))
}

X <- c(3, 1, 4, 1, 5, 9)

# ---- one-mode mappings -----------------------------------------------------

test_that("attribute terms become their named ergm terms", {
  f <- specify_ergm(a ~ ego(x) + alter(x) + ego(grp) + alter(grp) +
                          same(grp) + match(grp) + mix(grp), formula_net())
  expect_equal(rhs_of(f),
               c("edges", "nodeocov(\"x\")", "nodeicov(\"x\")",
                 "nodeofactor(\"grp\")", "nodeifactor(\"grp\")",
                 "nodematch(\"grp\")", "nodematch(\"grp\", diff = TRUE)",
                 "nodemix(\"grp\")"))
})

test_that("diff() is absdiff, signed with identity, and otherwise its matrix", {
  f <- specify_ergm(a ~ diff(x) + diff(x, transformer_fn = identity),
                        formula_net())
  expect_equal(rhs_of(f), c("edges", "absdiff(\"x\")", "diff(\"x\")"))
  .infernet_state$dist_warned <- TRUE
  expect_equal(rhs_of(specify_ergm(a ~ dist(x), formula_net())),
               c("edges", "absdiff(\"x\")"))
  sq <- function(d) d^2
  f <- specify_ergm(a ~ diff(x, transformer_fn = sq), formula_net())
  expect_equal(rhs_of(f), c("edges", "edgecov(diff_x)"))
  expect_equal(unname(get("diff_x", environment(f))), outer(X, X, "-")^2)
  expect_error(specify_ergm(a ~ diff(grp), formula_net()), "categorical")
})

test_that("the outcome's reverse tie is mutual, and another layer's is its matrix", {
  expect_equal(rhs_of(specify_ergm(a ~ recip, formula_net())),
               c("edges", "mutual"))
  expect_equal(rhs_of(specify_ergm(a ~ recip(a), formula_net())),
               c("edges", "mutual"))
  f <- specify_ergm(a ~ recip(b) + exchange(b), formula_net())
  expect_equal(rhs_of(f), c("edges", "edgecov(recip_b)", "edgecov(exchange_b)"))
  expect_equal(unname(get("exchange_b", environment(f))), t(layer_matrix("b")))
})

test_that("terms with no ergm statistic are edgecov() of infernet's matrix", {
  f <- specify_ergm(a ~ sim(x) + tie(b) + b + indeg(b) + outdeg(b) +
                          tertius(b, x) + tertius_diff(b, x), formula_net())
  covs <- c("sim_x", "tie_b", "b", "indeg_b", "outdeg_b", "tertius_b_x",
            "tertius_diff_b_x")
  expect_equal(rhs_of(f), c("edges", sprintf("edgecov(%s)", covs)))
  built <- suppressWarnings(built_of(
    a ~ sim(x) + tie(b) + b + indeg(b) + outdeg(b) + tertius(b, x) +
      tertius_diff(b, x)))
  for (nm in covs) {
    expect_equal(get(nm, environment(f)), built[[nm]], info = nm)
  }
})

test_that("an interaction is edgecov() of the product", {
  f <- specify_ergm(a ~ ego(x):alter(x) + match(grp):tie(b), formula_net())
  expect_equal(rhs_of(f),
               c("edges", "edgecov(ego_x_alter_x)",
                 "edgecov(match_grp_p_tie_b)", "edgecov(match_grp_q_tie_b)"))
  expect_equal(unname(get("ego_x_alter_x", environment(f))), outer(X, X))
})

test_that("goldfish's endogenous effects become ergm's structural terms", {
  f <- specify_ergm(a ~ trans + cycle + triangle + four, formula_net())
  expect_equal(rhs_of(f), c("edges", "ttriple", "ctriple", "triangle", "cycle(4)"))
})

test_that("the outcome is a network object under its own name", {
  f <- specify_ergm(a ~ ego(x), formula_net())
  expect_identical(f[[2]], as.name("a"))
  net <- get("a", environment(f))
  expect_s3_class(net, "network")
  expect_equal(unname(as.matrix(net)), layer_matrix("a"))
})

test_that("the conversion reports each change", {
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(specify_ergm(a ~ ego(x), formula_net()), "edges")
  expect_message(specify_ergm(a ~ ego(x), formula_net()), "nodeocov")
  expect_message(specify_ergm(. ~ ego(Age) + alter(Age), qap_net_undirected()),
                 "one copy")
  expect_message(specify_ergm(. ~ ego(Age), qap_net_gaussian()), "response")
})

# ---- two-mode and undirected ---------------------------------------------------

test_that("a two-mode network uses ergm's b1 and b2 terms", {
  g <- manynet::mutate_nodes(qap_net_twomode_wide(),
                             B = c(rep(NA, 12), seq_len(40)),
                             K = rep(c("u", "v"), 26))
  f <- specify_ergm(. ~ ego(Att) + ego(GONGO) + alter(B) + alter(K) +
                          same(GONGO), g)
  expect_equal(rhs_of(f),
               c("edges", "b1cov(\"Att\")", "b1factor(\"GONGO\")",
                 "b2cov(\"B\")", "b2factor(\"K\")", "edgecov(same_GONGO)"))
  expect_true(manynet::is_twomode(get("ties", environment(f))))
  expect_equal(dim(get("same_GONGO", environment(f))), c(12L, 40L))
  expect_warning(specify_ergm(. ~ ego(Att) + match(GONGO) + recip, g),
                 "2 terms.*match\\(GONGO\\).*two-mode.*recip")
})

test_that("an undirected network reads both sides with nodecov()", {
  f <- suppressMessages(
    specify_ergm(. ~ ego(Age) + alter(Age) + diff(Age), qap_net_undirected()))
  expect_equal(rhs_of(f), c("edges", "nodecov(\"Age\")", "absdiff(\"Age\")"))
  expect_warning(suppressMessages(
    specify_ergm(. ~ ego(Age) + recip + cycle, qap_net_undirected())),
    "mutual.*directed.*cycle.*undirected")
})

# ---- aborts ------------------------------------------------------------------

test_that("terms with no ergm form are dropped, naming each", {
  expect_warning(
    f <- suppressMessages(specify_ergm(
      a ~ ego(x) + global(gdp) + inertia + common_sender + trans(b) +
        inertia:ego(x) + (1 | sender), formula_net())),
    paste0("Dropped 5 terms.*global\\(gdp\\).*network-level.*inertia.*no ergm term",
           ".*common_sender.*not yet converted.*dgwesp.*trans\\(b\\).*another layer",
           ".*ego\\(x\\):inertia"))
  # A random intercept for each sender becomes ergm's fixed effect for each.
  expect_equal(rhs_of(f), c("edges", "nodeocov(\"x\")", "sender"))
})

test_that("a list of networks and a cognitive social structure abort", {
  expect_error(specify_ergm(a ~ ego(x), list(formula_net(), formula_net())),
               "list of 2 networks")
  expect_error(specify_ergm(advice ~ recip, manynet::ison_hightech),
               "cognitive social structure")
})

test_that("a typing error is caught as it is for an MRQAP", {
  expect_error(specify_ergm(a ~ ego(age), formula_net()), "not found")
  expect_error(specify_ergm(a ~ egoo(x), formula_net()), "Did you mean")
})

# ---- round trip ----------------------------------------------------------------

test_that("ergm fits the converted formula with the expected terms", {
  skip_if_not_installed("ergm")
  f <- suppressMessages(
    specify_ergm(. ~ ego(Age) + alter(Age) + same(Grp) + sim(Age) + recip,
                     qap_net_binary()))
  fit <- suppressMessages(ergm::ergm(f, estimate = "MPLE"))
  expect_named(stats::coef(fit),
               c("edges", "nodeocov.Age", "nodeicov.Age", "nodematch.Grp",
                 "edgecov.sim_Age", "mutual"))
  expect_true(all(is.finite(stats::coef(fit))))
})
