# `specify_mrqap()` reads a formula written for any model as an MRQAP reads it.
# These tests hold what each model's terms become, which are dropped, and how
# the model a formula was written for is read.

record_of <- function(f) attr(f, "record")

test_that("the model a formula was written for is read from its names", {
  expect_equal(attr(specify_mrqap(y ~ ego(age) + friends), "input"), "mrqap")
  expect_equal(attr(suppressMessages(suppressWarnings(specify_mrqap(y ~ inertia + ego(age)))), "input"),
               "dynam")
  expect_equal(attr(specify_mrqap(y ~ edges + nodematch("dept")), "input"), "ergm")
  # `recip` is in both packages, so it says nothing about the model.
  expect_equal(attr(suppressWarnings(specify_mrqap(y ~ recip + ego(age))), "input"),
               "mrqap")
})

test_that("the model is read from input, then the class, before the names", {
  f <- structure(y ~ ego(age), class = c("ergm_formula", "formula"))
  expect_equal(attr(specify_mrqap(f), "input"), "ergm")
  expect_equal(attr(specify_mrqap(y ~ ego(age), input = "rem"), "input"), "rem")
  expect_error(specify_mrqap(y ~ ego(age), input = "siena"), "must be one of")
})

test_that("the result is an mrqap_formula, and reading it again changes nothing", {
  f <- specify_mrqap(y ~ edges + nodematch("dept"))
  expect_s3_class(f, c("mrqap_formula", "formula"), exact = TRUE)
  expect_identical(specify_mrqap(f), f)
  expect_output(print(f), "y ~ same\\(dept\\)")
  expect_output(print(f), "adapted nodematch")
})

test_that("ergm terms become their infernet equivalents", {
  f <- specify_mrqap(
    y ~ edges + mutual + nodeocov("age") + nodeicov("age") + nodecov("tenure") +
      nodematch("dept") + nodematch("level", diff = TRUE) + nodemix("dept") +
      absdiff("age") + diff("tenure") + edgecov("friends"))
  expect_equal(deparse(f[[3]], width.cutoff = 500L),
               paste("recip + ego(age) + alter(age) + ego(tenure) + alter(tenure) +",
                     "same(dept) + same(level, diff = TRUE) + mix(dept) + diff(age) +",
                     "diff(tenure, transformer_fn = identity) + friends"))
  ch <- record_of(f)
  expect_equal(ch$as[ch$term == "edges"], "")
  expect_equal(ch$status[ch$term == "edges"], "adapted")
})

test_that("an ergm interaction is read operand by operand", {
  f <- specify_mrqap(y ~ nodematch("dept"):absdiff("age") + nodeocov("age"))
  expect_equal(deparse(f[[3]]), "ego(age) + same(dept):diff(age)")
  # A term that becomes two cannot be interacted, so the interaction is dropped.
  expect_warning(f <- specify_mrqap(y ~ nodecov("age"):nodematch("dept") + ego(x)),
                 "becomes two terms")
  expect_equal(deparse(f[[3]]), "ego(x)")
})

test_that("ergm terms of the outcome's own structure and node fixed effects are dropped", {
  res <- NULL
  expect_warning(res <- specify_mrqap(
    y ~ edges + ttriple + gwesp(0.5, fixed = TRUE) + sender + nodeocov("age")),
    "Dropped 3 terms")
  expect_equal(deparse(res[[3]]), "ego(age)")
  ch <- record_of(res)
  expect_match(ch$reason[ch$term == "sender"], "(1 | sender)", fixed = TRUE)
})

test_that("an ergm left-hand side that names the network is read as `.`", {
  g <- formula_net()
  f <- specify_mrqap(net ~ nodeocov("x") + edgecov("b"), g)
  expect_identical(f[[2]], as.name("."))
  # A layer on the left-hand side stays the outcome.
  f <- specify_mrqap(a ~ nodeocov("x") + edgecov("b"), g)
  expect_identical(f[[2]], as.name("a"))
})

test_that("an ergm formula fits through estimate_mrqap()", {
  g <- formula_net()
  fit <- suppressWarnings(suppressMessages(
    estimate_mrqap(a ~ edges + nodeocov("x") + nodeicov("x") + edgecov("b") + ttriple,
                   g, times = 5, control = list(seed = 1))))
  expect_named(fit$coefficients, c("(Intercept)", "ego_x", "alter_x", "b"))
})

test_that("goldfish terms that read the outcome are dropped, in one warning", {
  expect_warning(f <- specify_mrqap(a ~ inertia + trans(b) + cycle(a) + ego(x),
                                    formula_net()),
                 "Dropped 3 terms")
  expect_equal(deparse(f[[3]]), "ego(x)")
  ch <- record_of(f)
  expect_match(ch$reason[ch$term == "inertia"], "needs a longitudinal network")
  expect_equal(ch$reason[ch$term == "cycle(a)"], "depends on the outcome's own ties")
  expect_match(ch$reason[ch$term == "trans(b)"], "reads another layer")
})

test_that("an interaction with a dropped term is dropped whole", {
  expect_warning(f <- specify_mrqap(a ~ ego(x) + size:ego(x)),
                 "`ego(x):size` is defined on the groups", fixed = TRUE)
  expect_equal(deparse(f[[3]]), "ego(x)")
})

test_that("goldfish terms that mean something different here are named", {
  f <- suppressMessages(suppressWarnings(specify_mrqap(a ~ inertia + sim(x) + tertius(b, x))))
  ch <- record_of(f)
  expect_setequal(ch$term[ch$status == "interpreted"], c("sim(x)", "tertius(b, x)"))
  # Kept as they are.
  expect_equal(deparse(f[[3]]), "sim(x) + tertius(b, x)")
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(suppressWarnings(specify_mrqap(a ~ inertia + sim(x))), "1 - \\|d\\|")
})

test_that("a bare name that is a layer of the network stays a layer", {
  g <- manynet::add_tie_attribute(formula_net(), "size", 1)
  f <- specify_mrqap(a ~ size + ego(x), g)
  expect_equal(deparse(f[[3]]), "size + ego(x)")
  expect_equal(record_of(f)$status, c("fits", "fits"))
})

test_that("a formula with nothing left to fit aborts and names each term", {
  expect_error(specify_mrqap(a ~ inertia + trans + common_sender(a)),
               "No term of the formula can be fitted")
  expect_error(specify_mrqap(a ~ inertia + trans),
               "`inertia` needs a longitudinal network", fixed = TRUE)
  expect_error(specify_mrqap(net ~ edges + ttriple), "`edges` is the intercept")
  # A random intercept is not a term that can stand alone, and it is kept.
  expect_error(specify_mrqap(a ~ inertia + (1 | sender)), "No term of the formula")
  expect_equal(specify_mrqap(a ~ ego(x) + (1 | sender))[[3]],
               quote(ego(x) + (1 | sender)))
})

test_that("a name that no model knows is left for estimate_mrqap() to refuse", {
  expect_equal(deparse(specify_mrqap(a ~ inertai + ego(x))[[3]]), "inertai + ego(x)")
  expect_error(suppressMessages(estimate_mrqap(a ~ inertai + ego(x), formula_net(),
                                               times = 5)), "not found")
  expect_error(suppressMessages(estimate_mrqap(a ~ trnas(b) + ego(x), formula_net(),
                                               times = 5)), "Did you mean")
})

# ---- specify_dynam() -----------------------------------------------------------

test_that("an MRQAP formula is written for goldfish", {
  expect_warning(
    f <- specify_dynam(a ~ ego(x) + same(grp) + b + exchange(b) + dist(x) +
                         same(grp, diff = TRUE) + (1 | sender), formula_net()),
    "Dropped 2 terms that goldfish cannot fit")
  expect_s3_class(f, c("dynam_formula", "formula"), exact = TRUE)
  expect_equal(.deparsed(f[[3]]),
               paste("ego(x) + same(grp) + tie(b, weighted = TRUE) + recip(b) +",
                     "diff(x)"))
  ch <- record_of(f)
  expect_setequal(ch$term[ch$status == "drops"], c("same(grp, diff = TRUE)", "(1 | sender)"))
})

test_that("goldfish's own terms pass through, and those that differ are named", {
  f <- suppressMessages(specify_dynam(a ~ inertia + trans(b) + sim(x) + tertius(b, x)))
  expect_equal(.deparsed(f[[3]]), "inertia + trans(b) + sim(x) + tertius(b, x)")
  # Written for goldfish already, so nothing means anything different.
  expect_equal(unique(record_of(f)$status), "fits")
  f <- specify_dynam(a ~ sim(x) + ego(x))
  expect_equal(record_of(f)$status, c("interpreted", "fits"))
})

test_that("an ergm formula is written for goldfish", {
  expect_warning(
    f <- specify_dynam(net ~ edges + mutual + ttriple + ctriple + cycle(4) +
                         gwesp(0.5) + nodematch("grp")),
    "`gwesp(0.5)` has no goldfish term", fixed = TRUE)
  expect_equal(.deparsed(f[[3]]), "recip + trans + cycle + four + same(grp)")
})

test_that("a formula with nothing goldfish can fit aborts", {
  expect_error(specify_dynam(a ~ same(grp, diff = TRUE) + mix(grp)),
               "No term of the formula can be fitted by goldfish")
})

# ---- specify_ergm() without the network -----------------------------------------

test_that("without the network, each term ergm names is written by name", {
  f <- suppressMessages(specify_ergm(
    a ~ ego(x) + alter(x) + same(grp) + same(x, diff = TRUE) + mix(grp) + diff(x) +
      diff(x, transformer_fn = identity) + b + recip + trans + (1 | receiver)))
  expect_s3_class(f, c("ergm_formula", "formula"), exact = TRUE)
  expect_equal(attr(stats::terms(f), "term.labels"),
               c("edges", "nodeocov(\"x\")", "nodeicov(\"x\")", "nodematch(\"grp\")",
                 "nodematch(\"x\", diff = TRUE)", "nodemix(\"grp\")",
                 "absdiff(\"x\")", "diff(\"x\")", "edgecov(\"b\")", "mutual",
                 "ttriple", "receiver"))
})

test_that("without the network, a term that needs its matrix is dropped", {
  expect_warning(f <- suppressMessages(specify_ergm(a ~ ego(x) + sim(x) + indeg(b))),
                 "Dropped 2 terms that ergm cannot fit")
  expect_match(record_of(f)$reason[record_of(f)$term == "sim(x)"], "needs .data",
               fixed = TRUE)
})

test_that("an ergm formula is returned as it is", {
  f <- specify_ergm(net ~ edges + mutual + gwesp(0.5, fixed = TRUE))
  expect_equal(.deparsed(f[[3]]), "edges + mutual + gwesp(0.5, fixed = TRUE)")
  expect_equal(record_of(f)$term, c("edges", "mutual", "gwesp(0.5, fixed = TRUE)"))
  expect_equal(unique(record_of(f)$status), "fits")
})

test_that("each specify function reads the class the others give", {
  f <- suppressWarnings(suppressMessages(
    specify_mrqap(net ~ edges + mutual + nodematch("grp") + ttriple)))
  expect_equal(attr(f, "input"), "ergm")
  g <- suppressMessages(specify_ergm(f))
  expect_equal(attr(g, "input"), "mrqap")
  expect_equal(attr(stats::terms(g), "term.labels"),
               c("edges", "mutual", "nodematch(\"grp\")"))
  h <- suppressMessages(specify_dynam(g))
  expect_equal(attr(h, "input"), "ergm")
  expect_equal(.deparsed(h[[3]]), "recip + same(grp)")
})
