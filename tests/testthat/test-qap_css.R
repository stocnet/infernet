# The engine fits two shapes, a dyadic network and a cognitive social structure,
# through one skeleton. These assert that the CSS half of that skeleton works at
# the level of the engine, on hand-built arrays. Models fitted through
# `estimate_mrqap()` from a `{manynet}` network are in the second half of this file.

test_that("the engine fits a CSS and counts its observations", {
  n <- 8
  fit <- QAPengine(ties ~ cov, css_fixture(n), css = TRUE,
                   family = "binomial", times = 20, seed = 1)
  expect_s3_class(fit, "netreg_css")
  expect_named(fit$coefficients, c("(Intercept)", "cov"))
  # Every cell of every reporter's report, less the diagonals.
  expect_equal(nrow(fit$pred), n * n * n - n * n)
  expect_equal(dim(fit$lower), c(2L, 2L))
  expect_equal(rownames(fit$lower), c("perm_coefs", "perm_t"))
})

test_that("a CSS baseline matches glm() on the same observations", {
  ml <- css_fixture()
  fit <- QAPengine(ties ~ cov, ml, css = TRUE, family = "binomial",
                   times = 10, seed = 1)
  ref <- stats::glm(ties ~ cov, data = fit$pred, family = stats::binomial())
  expect_equal(unname(fit$coefficients), unname(stats::coef(ref)))
})

test_that("a CSS carries sender, receiver, reporter and network indices", {
  fit <- QAPengine(ties ~ cov, css_fixture(), css = TRUE,
                   family = "binomial", times = 10, seed = 1)
  expect_true(all(c("sv", "rv", "bv", "nv") %in% names(fit$pred)))
  expect_s3_class(fit$pred$bv, "factor")
})

test_that("a reporter random intercept is available to a CSS only", {
  skip_if_not_installed("lme4")
  fit <- suppressMessages(suppressWarnings(
    QAPengine(ties ~ cov, css_fixture(), css = TRUE, family = "binomial",
              times = 10, seed = 1, random_intercept_reporter = TRUE)))
  expect_named(fit$random.intercepts, "bv")
  expect_true(fit$random[["reporter"]])
})

test_that("a dyadic network rejects a reporter random intercept", {
  m <- matrix(stats::rnorm(64), 8, 8)
  diag(m) <- 0
  expect_error(
    QAPengine(ties ~ cov, list(ties = m, cov = matrix(stats::rnorm(64), 8, 8)),
              times = 5, random_intercept_reporter = TRUE),
    "reporter")
})

test_that("a CSS rejects an outcome that is not three-dimensional", {
  m <- matrix(stats::rnorm(64), 8, 8)
  expect_error(
    QAPengine(ties ~ cov, list(ties = m, cov = m), css = TRUE, times = 5),
    "3-dimensional")
})

test_that("a CSS fit prints as a model", {
  fit <- QAPengine(ties ~ cov, css_fixture(), css = TRUE,
                   family = "binomial", times = 10, seed = 1)
  expect_output(shown <- withVisible(print(fit)), "cognitive social structure")
  expect_false(shown$visible)
  expect_output(print(fit), "Coefficients")
})

test_that("both shapes reproduce under the same seed", {
  ml <- css_fixture()
  a <- QAPengine(ties ~ cov, ml, css = TRUE, family = "binomial",
                 times = 20, seed = 42)
  b <- QAPengine(ties ~ cov, ml, css = TRUE, family = "binomial",
                 times = 20, seed = 42)
  expect_equal(a$lower, b$lower)
})

# ---- through estimate_mrqap() -------------------------------------------------

# Cognitive social structures through `estimate_mrqap()`.
#
# A CSS is detected, not declared: `{manynet}` records who reported each tie in
# a `by` column, and `as_matrix()` then returns an array of sender, receiver,
# and reporter. These tests fit models on `manynet::ison_hightech`, a real CSS
# of 21 managers, most of them on its first 10, and on small constructed networks for the shapes and the
# missing reports it does not have.

# ---- fixtures ---------------------------------------------------------------

HT <- css_hightech()
FORM_HT <- advice ~ ego(age) + alter(age) + same(dept) + reports

# ---- detection and shape -----------------------------------------------------

# The one fit of the real network, all 21 managers. The other blocks fit its
# first 10, `css_hightech()`.
test_that("a CSS layer is detected and fitted as one", {
  fit <- suppressMessages(
    estimate_mrqap(FORM_HT, manynet::ison_hightech, times = 10,
                   control = list(seed = 1)))
  expect_s3_class(fit, "netreg")
  expect_s3_class(fit, "netreg_css")
  expect_named(fit$coefficients,
               c("(Intercept)", "ego_age", "alter_age", "same_dept", "reports"))
  # 21 reporters, each describing the 21 x 20 ordered dyads.
  expect_equal(nrow(fit$pred), 21 * 21 * 20)
  expect_equal(length(unique(fit$pred$bv)), 21)
})

test_that("detecting a CSS is reported", {
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_any_message(
    estimate_mrqap(advice ~ ego(age) + friends, HT, times = 5,
                   control = list(seed = 1)),
    "cognitive social structure, with 10 reporters")
})

test_that("a CSS baseline matches glm() on the same reported dyads", {
  fit <- suppressMessages(
    estimate_mrqap(FORM_HT, HT, times = 5, control = list(seed = 1)))
  ref <- stats::glm(stats::reformulate(
    c("ego_age", "alter_age", "same_dept", "reports"), "advice"),
    data = fit$pred, family = stats::binomial())
  expect_equal(unname(fit$coefficients), unname(stats::coef(ref)))
})

test_that("a layer that is not a CSS is the outcome of a dyadic model", {
  fit <- suppressMessages(
    estimate_mrqap(reports ~ ego(age) + alter(age), HT, times = 5,
                   control = list(seed = 1)))
  expect_false(inherits(fit, "netreg_css"))
  expect_equal(nrow(fit$pred), 10 * 9)
})

test_that("a multiplex network asks for a layer to be named as the outcome", {
  expect_error(
    estimate_mrqap(weight ~ ego(age), HT, times = 5),
    "name one as the outcome")
})

# ---- predictors --------------------------------------------------------------

test_that("a CSS layer as a predictor varies by reporter", {
  fit <- suppressMessages(
    estimate_mrqap(advice ~ friends + reports, HT, times = 5,
                   control = list(seed = 1)))
  by_dyad <- split(fit$pred, interaction(fit$pred$sv, fit$pred$rv, drop = TRUE))
  varies <- function(col) any(vapply(by_dyad, function(d) length(unique(d[[col]])) > 1,
                                     logical(1)))
  # Each manager's report of friendship differs, so the predictor does too.
  expect_true(varies("friends"))
  # The formal reporting structure is not a CSS, so every reporter shares it.
  expect_false(varies("reports"))
})

test_that("a nodal predictor is the same for every reporter", {
  fit <- suppressMessages(
    estimate_mrqap(advice ~ ego(age) + friends, HT, times = 5,
                   control = list(seed = 1)))
  per_dyad <- tapply(fit$pred[["ego_age"]],
                     interaction(fit$pred$sv, fit$pred$rv, drop = TRUE),
                     function(v) length(unique(v)))
  expect_true(all(per_dyad == 1))
})

test_that("tertius() is computed within each report", {
  fit <- suppressMessages(
    estimate_mrqap(advice ~ ego(age) + tertius(friends, age), HT, times = 5,
                   control = list(seed = 1)))
  per_dyad <- tapply(fit$pred[["tertius_friends_age"]],
                     interaction(fit$pred$sv, fit$pred$rv, drop = TRUE),
                     function(v) length(unique(v)))
  expect_true(any(per_dyad > 1))
})

test_that("an interaction between a CSS layer and a nodal term fits", {
  fit <- suppressMessages(
    estimate_mrqap(advice ~ friends + ego(age):friends, HT, times = 5,
                   control = list(seed = 1)))
  expect_named(fit$coefficients, c("(Intercept)", "friends", "friends:ego_age"))
})

test_that("a layer named on its own is found as a predictor", {
  # A bare term used to be named "reports NA" and could never be found.
  fit <- suppressMessages(
    estimate_mrqap(advice ~ reports, HT, times = 5, control = list(seed = 1)))
  expect_named(fit$coefficients, c("(Intercept)", "reports"))
})

# ---- reporter terms ----------------------------------------------------------

# The node each row of the fitted data names, as an integer.
node_of <- function(f) as.integer(as.character(f))

test_that("reporter() is the reporter's value, the same over a whole report", {
  fit <- suppressMessages(
    estimate_mrqap(advice ~ reporter(age) + friends, HT, times = 5,
                   control = list(seed = 1)))
  age <- manynet::node_attribute(HT, "age")
  expect_equal(fit$pred[["reporter_age"]], age[node_of(fit$pred$bv)])
  expect_equal(fit$terms$effect[fit$terms$term == "reporter_age"], "reporter")
  expect_equal(fit$terms$attribute[fit$terms$term == "reporter_age"], "age")
})

test_that("a categorical reporter() leaves out a reference, as ego() does", {
  fit <- suppressMessages(
    estimate_mrqap(advice ~ reporter(dept), HT, times = 5,
                   control = list(seed = 1)))
  expect_equal(names(coef(fit))[-1], paste0("reporter_dept_", c("B", "C", "D", "E")))
  expect_equal(unique(fit$terms$reference[-1]), "A")
  fit <- suppressMessages(
    estimate_mrqap(advice ~ reporter(dept, levels = "B"), HT, times = 5,
                   control = list(seed = 1)))
  expect_equal(names(coef(fit))[-1], "reporter_dept_B")
})

test_that("reporter_ego and reporter_alter mark a report about the reporter's own ties", {
  fit <- suppressMessages(
    estimate_mrqap(advice ~ reporter_ego + reporter_alter() + friends, HT,
                   times = 5, control = list(seed = 1)))
  expect_named(coef(fit), c("(Intercept)", "reporter_ego", "reporter_alter", "friends"))
  by <- node_of(fit$pred$bv)
  expect_equal(fit$pred$reporter_ego, (node_of(fit$pred$sv) == by) * 1)
  expect_equal(fit$pred$reporter_alter, (node_of(fit$pred$rv) == by) * 1)
  expect_equal(fit$terms$effect[-1], c("reporter_ego", "reporter_alter", "tie"))
})

test_that("reporter_same() compares the reporter with the sender or the receiver", {
  fit <- suppressMessages(
    estimate_mrqap(advice ~ reporter_same(dept) + reporter_same(dept, node = "alter"),
                   HT, times = 5, control = list(seed = 1)))
  expect_named(coef(fit)[-1], c("reporter_same_ego_dept", "reporter_same_alter_dept"))
  dept <- manynet::node_attribute(HT, "dept")
  by <- node_of(fit$pred$bv)
  expect_equal(fit$pred$reporter_same_ego_dept,
               (dept[node_of(fit$pred$sv)] == dept[by]) * 1)
  expect_equal(fit$pred$reporter_same_alter_dept,
               (dept[node_of(fit$pred$rv)] == dept[by]) * 1)
  expect_error(
    estimate_mrqap(advice ~ reporter_same(dept, node = "both"), HT, times = 5),
    "must be")
})

test_that("a reporter fixed effect absorbs reporter(), and not reporter_ego", {
  expect_error(
    estimate_mrqap(advice ~ reporter(age) + friends | reporter, HT, times = 5),
    "absorbs")
  fit <- suppressMessages(
    estimate_mrqap(advice ~ reporter_ego + friends | reporter, HT, times = 5,
                   control = list(seed = 1)))
  expect_true(is.finite(coef(fit)[["reporter_ego"]]))
})

test_that("a reporter term interacts with a dyadic term", {
  fit <- suppressMessages(
    estimate_mrqap(advice ~ friends * reporter_ego, HT, times = 5,
                   control = list(seed = 1)))
  expect_true("friends:reporter_ego" %in% names(coef(fit)))
})

test_that("a reporter term on a network that is not a CSS is refused", {
  g <- qap_net_gaussian(n = 12)
  expect_error(estimate_mrqap(weight ~ reporter(Age), g, times = 5),
               "cognitive social structure")
  expect_error(estimate_mrqap(weight ~ reporter_ego, g, times = 5),
               "cognitive social structure")
})

test_that("reporter_ego is refused on an undirected CSS", {
  g <- css_net(directed = FALSE)
  expect_error(suppressWarnings(suppressMessages(
    estimate_mrqap(. ~ reporter_ego, g, times = 5))), "directed")
  fit <- suppressWarnings(suppressMessages(
    estimate_mrqap(. ~ reporter(age), g, times = 5, control = list(seed = 1))))
  expect_true("reporter_age" %in% names(coef(fit)))
})

test_that("a two-mode CSS reads the column nodes' reports after the rows'", {
  g <- css_net_twomode(people = 4, events = 3)
  fit <- suppressWarnings(suppressMessages(
    estimate_mrqap(. ~ reporter_ego + reporter_alter, g, times = 5,
                   control = list(seed = 1))))
  by <- node_of(fit$pred$bv)
  expect_equal(fit$pred$reporter_ego, (node_of(fit$pred$sv) == by) * 1)
  expect_equal(fit$pred$reporter_alter, (node_of(fit$pred$rv) + 4 == by) * 1)
  expect_error(
    estimate_mrqap(. ~ reporter_same(age), g, times = 5),
    "two-mode")
})

# ---- random intercepts and permutation --------------------------------------

test_that("each reporter can take a random intercept", {
  skip_if_not_installed("lme4")
  fit <- suppressMessages(suppressWarnings(
    estimate_mrqap(advice ~ ego(age) + friends, HT, times = 5,
                   control = list(seed = 1, random_intercept_reporter = TRUE))))
  expect_named(fit$random.intercepts, "bv")
  expect_length(fit$random.intercepts$bv, 10)
  expect_true(fit$random[["reporter"]])
  expect_output(print(fit), "Each reporter has a random intercept")
})

test_that("a reporter intercept on a network that is not a CSS is refused", {
  expect_error(
    estimate_mrqap(weight ~ ego(Age), qap_net_gaussian(n = 12), times = 5,
                   control = list(random_intercept_reporter = TRUE)),
    "reporter")
})

test_that("a CSS model reproduces under the same seed", {
  a <- suppressMessages(estimate_mrqap(advice ~ ego(age) + friends, HT,
                                       times = 20, control = list(seed = 9)))
  b <- suppressMessages(estimate_mrqap(advice ~ ego(age) + friends, HT,
                                       times = 20, control = list(seed = 9)))
  expect_equal(a$lower, b$lower)
})

test_that(".perm_css() relabels senders, receivers, and reporters together", {
  m <- array(seq_len(5^3), dim = c(5, 5, 5))
  set.seed(1)
  p <- .perm_css(m)
  set.seed(1)
  o <- sample(seq_len(5))
  # The report of the node now called j is the report node j made.
  expect_equal(p, m[o, o, o])
  expect_setequal(as.vector(p), as.vector(m))
})

test_that(".perm_css() orders a two-mode CSS's reporters as rows then columns", {
  m <- array(seq_len(3 * 2 * 5), dim = c(3, 2, 5))
  set.seed(2)
  p <- .perm_css(m)
  set.seed(2)
  or <- sample(seq_len(3))
  oc <- sample(seq_len(2))
  expect_equal(p, m[or, oc, c(or, 3 + oc)])
})

test_that(".perm_css() refuses reporters that are not the nodes", {
  expect_error(.perm_css(array(0, dim = c(4, 4, 2))), "reporters must be its nodes")
})

# ---- missing reports ---------------------------------------------------------

test_that("an unmarked non-reporter is warned about by name", {
  g <- css_net(n = 10, reporting = 8)
  expect_snet_warning(
    estimate_mrqap(. ~ ego(age) + alter(age), g, times = 5,
                   control = list(seed = 1)),
    "2 reporters named no ties")
})

test_that("a non-reporter marked as missing is dropped, not read as zeros", {
  g <- css_net(n = 10, reporting = 8, mark_missing = TRUE)
  fit <- expect_no_warning(suppressMessages(
    estimate_mrqap(. ~ ego(age) + alter(age), g, times = 5,
                   control = list(seed = 1))))
  # Eight reports of 10 x 9 ordered dyads; the two unasked are not in the model.
  expect_equal(nrow(fit$pred), 8 * 10 * 9)
  expect_setequal(as.integer(as.character(unique(fit$pred$bv))), 1:8)
  expect_output(print(fit), "from 8 reporters")
})

# ---- other shapes ------------------------------------------------------------

test_that("an undirected CSS counts each dyad once per report", {
  g <- css_net(n = 10, reporting = 10, directed = FALSE)
  fit <- suppressMessages(suppressWarnings(
    estimate_mrqap(. ~ ego(age) + alter(age), g, times = 5,
                   control = list(seed = 1))))
  expect_false(fit$directed)
  expect_equal(nrow(fit$pred), 10 * (10 * 9 / 2))
})

test_that("a two-mode CSS fits every cell of every report", {
  g <- css_net_twomode(people = 4, events = 3)
  fit <- suppressMessages(suppressWarnings(
    estimate_mrqap(. ~ ego(age), g, times = 5, control = list(seed = 1))))
  # Rows of people, columns of events, and a report from all seven nodes.
  expect_equal(nrow(fit$pred), 4 * 3 * 7)
})

test_that("same() on a two-mode CSS is refused with an alternative", {
  g <- css_net_twomode()
  g <- manynet::mutate_nodes(g, grp = rep(c("x", "y"), length.out = 7))
  expect_error(
    estimate_mrqap(. ~ same(grp), g, times = 5),
    "ego")
})

test_that("a list of CSS networks is pooled", {
  fit <- suppressMessages(
    estimate_mrqap(advice ~ ego(age) + friends, list(HT, HT), times = 5,
                   control = list(seed = 1)))
  expect_s3_class(fit, "netreg_css")
  expect_equal(nrow(fit$pred), 2 * 10 * 10 * 9)
})

test_that("a CSS cannot be pooled with a network that is not one", {
  dyadic <- manynet::to_uniplex(HT, "reports")
  css    <- manynet::to_uniplex(HT, "advice")
  expect_error(
    estimate_mrqap(. ~ ego(age), list(css, dyadic), times = 5),
    "Cannot pool")
})
