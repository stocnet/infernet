# What each model makes of each term: the record that every `specify_*()`
# function keeps, and `tidy()` of it. These tests hold the status of every
# registered term in every model, so a new term fails here until its status in
# each model is written down.

# `term_example()`, one example of each registered term, is in
# helper-functional.R.

# The status a model gives one term, next to `ego(x)` so that something is
# always left to fit.
status_in <- function(specify, label, g, tidied = FALSE) {
  f <- suppressWarnings(suppressMessages(
    specify(stats::as.formula(paste("a ~", label, "+ ego(x)")), g)))
  record <- if (tidied) suppressWarnings(tidy(f, g)) else attr(f, "record")
  unique(record$status[record$term == label])
}

# mrqap is the record; tidied is `tidy()` with the network, which builds each
# term. ergm names the term in its own formula, and the registry's `ergm` says
# which term that is.
expected_status <- utils::read.table(header = TRUE, stringsAsFactors = FALSE, text = "
  term                  mrqap  tidied   ergm
  ego                   fits   fits     adapted
  alter                 fits   fits     adapted
  same                  fits   fits     adapted
  diff                  fits   fits     adapted
  dist                  warns  warns    adapted
  sim                   fits   fits     adapted
  match                 warns  warns    adapted
  mix                   fits   fits     adapted
  tie                   fits   fits     adapted
  recip                 fits   fits     adapted
  exchange              fits   fits     adapted
  indeg                 fits   fits     adapted
  outdeg                fits   fits     adapted
  tertius               fits   fits     adapted
  tertius_diff          fits   fits     adapted
  global                fits   refuses  drops
  reporter              fits   refuses  drops
  reporter_ego          fits   refuses  drops
  reporter_alter        fits   refuses  drops
  reporter_same         fits   refuses  drops
  inertia               drops  drops    drops
  trans                 drops  drops    adapted
  cycle                 drops  drops    adapted
  common_sender         drops  drops    drops
  common_receiver       drops  drops    drops
  four                  drops  drops    adapted
  triangle              drops  drops    fits
  node_trans            drops  drops    drops
  degree                drops  drops    drops
  mixed_trans           drops  drops    drops
  mixed_cycle           drops  drops    drops
  mixed_common_sender   drops  drops    drops
  mixed_common_receiver drops  drops    drops
  ego_alter_interaction drops  drops    drops
  alterdeg              drops  drops    drops
  alterpop              drops  drops    drops
  egodeg                drops  drops    drops
  egopop                drops  drops    drops
  dyad                  drops  drops    drops
  dyadXdiff             drops  drops    drops
  dyadXego              drops  drops    drops
  size                  drops  drops    drops
  sizeXdiff             drops  drops    drops
  sizeXego              drops  drops    drops
  intercept             drops  drops    drops
")

test_that("every registered term has an expected status in every model", {
  registry <- .formula_terms()
  expect_setequal(expected_status$term, names(registry))
  statuses <- unlist(expected_status[, -1])
  expect_true(all(statuses %in% .statuses()))
})

test_that("each model gives each registered term its expected status", {
  g <- formula_net()
  registry <- .formula_terms()
  for (k in seq_len(nrow(expected_status))) {
    row <- expected_status[k, ]
    label <- term_example(row$term, registry[[row$term]])
    expect_equal(status_in(specify_mrqap, label, g), row$mrqap, info = label)
    expect_equal(status_in(specify_mrqap, label, g, tidied = TRUE), row$tidied,
                 info = label)
    expect_equal(status_in(specify_ergm, label, g), row$ergm, info = label)
  }
})

test_that("the registry's MRQAP status agrees with the record", {
  registry <- .formula_terms()
  for (term in names(registry)) {
    status <- registry[[term]]$mrqap
    expect_true(status %in% c("fits", "warns", "drops", "refuses"), info = term)
    if (status %in% c("warns", "drops")) {
      expect_true(is.character(registry[[term]]$reason) &&
                    nzchar(registry[[term]]$reason), info = term)
    }
    # A term the registry drops is dropped wherever it stands, and a term it
    # fits is not.
    row <- expected_status[expected_status$term == term, ]
    expect_equal(row$mrqap == "drops", status == "drops", info = term)
  }
})

# ---- the record ------------------------------------------------------------------

test_that("every term of the formula has a row, in the order of the formula", {
  f <- suppressWarnings(specify_mrqap(
    a ~ inertia + ego(x) + sim(x) + recip + dist(x) + (1 | sender)))
  record <- attr(f, "record")
  expect_equal(record$term,
               c("inertia", "ego(x)", "sim(x)", "recip", "dist(x)", "(1 | sender)"))
  expect_equal(record$status,
               c("drops", "fits", "interpreted", "warns", "warns", "fits"))
  expect_true(all(record$status %in% .statuses()))
})

test_that("a term that reads the outcome warns or is refused, and another layer fits", {
  f <- specify_mrqap(a ~ recip + recip(a) + recip(b) + tie(a) + indeg(a) + tie(b))
  record <- attr(f, "record")
  expect_equal(record$status, c("warns", "warns", "fits", "refuses", "refuses", "fits"))
  expect_match(record$reason[record$term == "tie(a)"], "from those same ties")
  # With the network, `.` is read as its focal layer, or a named layer.
  f <- specify_mrqap(a ~ tie(a) + tie(b), formula_net())
  expect_equal(attr(f, "record")$status, c("refuses", "fits"))
})

test_that("an ergm formula read for an MRQAP records each ergm term", {
  f <- suppressWarnings(specify_mrqap(
    net ~ edges + mutual + nodematch("grp") + gwesp(0.5, fixed = TRUE), formula_net()))
  record <- attr(f, "record")
  expect_equal(record$term, c("net", "edges", "mutual", "nodematch(\"grp\")",
                              "gwesp(0.5, fixed = TRUE)"))
  expect_equal(record$status, c("adapted", "adapted", "adapted", "adapted", "drops"))
  expect_equal(record$as[record$term == "net"], ".")
})

# ---- tidy() ----------------------------------------------------------------------

test_that("tidy() returns the record as a tibble, with the same columns for each model", {
  f <- suppressWarnings(specify_mrqap(a ~ inertia + ego(x) + sim(x)))
  t <- tidy(f)
  expect_s3_class(t, "tbl_df")
  expect_named(t, c("term", "status", "as", "reason", "columns"))
  expect_equal(t$status, c("drops", "fits", "interpreted"))
  expect_true(is.na(t$as[1]))
  expect_true(is.na(t$reason[2]))
  # Without the network, nothing is built.
  expect_equal(lengths(t$columns), c(0, 0, 0))
  e <- suppressMessages(specify_ergm(a ~ ego(x) + same(grp)))
  expect_named(tidy(e), c("term", "status", "as", "reason"))
  expect_equal(tidy(e)$as, c("nodeocov(\"x\")", "nodematch(\"grp\")"))
})

test_that("tidy() with the network names the columns estimate_mrqap() builds", {
  g <- formula_net()
  f <- suppressWarnings(specify_mrqap(
    a ~ ego(x) + same(grp, diff = TRUE) + indeg(b, node = "ego") + ego(x):same(grp) + b + inertia, g))
  t <- suppressWarnings(tidy(f, g))
  # `stats::terms()` puts an interaction after the terms of lower order.
  expect_equal(t$term[6], "ego(x):same(grp)")
  expect_equal(t$columns,
               list("ego_x", c("same_grp_p", "same_grp_q"), "ego_indeg_b",
                    "b", character(0), "ego_x:same_grp"))
  fit <- suppressWarnings(suppressMessages(
    estimate_mrqap(f, g, times = 5, control = list(seed = 1))))
  expect_equal(names(fit$coefficients)[-1], unlist(t$columns))
})

test_that("tidy() with the network refuses a term whose matrix cannot be built", {
  g <- formula_net()
  f <- specify_mrqap(a ~ ego(x) + global(gdp))
  t <- tidy(f, g)
  expect_equal(t$status, c("fits", "refuses"))
  expect_false(is.na(t$reason[2]))
  expect_length(t$columns[[2]], 0)
})

test_that("tidy() of an ergm read of a rewritten term names each ergm term", {
  g <- formula_net()
  f <- suppressWarnings(specify_mrqap(net ~ edges + nodecov("x"), g))
  t <- tidy(f, g)
  expect_equal(t$as[t$term == "nodecov(\"x\")"], "ego(x) + alter(x)")
  expect_equal(t$columns[[which(t$term == "nodecov(\"x\")")]], c("ego_x", "alter_x"))
})

# ---- formulas from the goldfish documentation -------------------------------------

test_that("formulas from goldfish's documentation are read without aborting", {
  formulas <- list(
    calls ~ 1 + inertia + recip + trans,
    calls ~ inertia + alter(gradeType) + recip + trans,
    calls ~ 1 + indeg(friendship) + tie(friendship),
    calls ~ 1 + node_trans(calls, type = "ego") + alter(gradeType),
    calls ~ inertia + recip + same(gradeType) + diff(age, transformer_fn = abs) +
      tertius_diff(friendship, age),
    calls ~ 1 + indeg + outdeg + tie(friendship),
    calls ~ inertia + recip(calls, weighted = TRUE) + tie(friendship),
    calls ~ inertia + offset(recip, coef = 0) + tie(friendship),
    calls ~ inertia + ego(actors$floor))
  expected <- list(
    c(inertia = "drops", recip = "warns", trans = "drops"),
    c(inertia = "drops", `alter(gradeType)` = "fits", recip = "warns", trans = "drops"),
    c(`indeg(friendship)` = "fits", `tie(friendship)` = "fits"),
    c(`node_trans(calls, type = "ego")` = "drops", `alter(gradeType)` = "fits"),
    c(inertia = "drops", recip = "warns", `same(gradeType)` = "fits",
      `diff(age, transformer_fn = abs)` = "fits",
      `tertius_diff(friendship, age)` = "interpreted"),
    c(indeg = "drops", outdeg = "drops", `tie(friendship)` = "fits"),
    c(inertia = "drops", `recip(calls, weighted = TRUE)` = "warns",
      `tie(friendship)` = "fits"),
    c(inertia = "drops", `tie(friendship)` = "fits",
      `offset(recip, coef = 0)` = "drops"),
    c(inertia = "drops", `ego(actors$floor)` = "adapted"))
  for (k in seq_along(formulas)) {
    t <- suppressWarnings(suppressMessages(tidy(specify_mrqap(formulas[[k]]))))
    expect_equal(stats::setNames(t$status, t$term), expected[[k]],
                 info = deparse(formulas[[k]]))
  }
  # goldfish's `~ indeg` reads the outcome's own degree, and is dropped, where
  # naming the outcome is refused.
  t <- suppressWarnings(tidy(specify_mrqap(calls ~ indeg(calls) + tie(friendship))))
  expect_equal(t$status[1], "refuses")
  expect_error(suppressWarnings(specify_mrqap(calls ~ 1 + indeg + outdeg)),
               "No term.*indeg.*outdeg")
  t <- suppressWarnings(tidy(specify_mrqap(calls ~ ego(actors$floor))))
  expect_equal(t$as, "ego(floor)")
})

test_that("goldfish's formulas are written for ergm as they should be", {
  e <- suppressWarnings(suppressMessages(tidy(specify_ergm(
    calls ~ inertia + indeg + offset(recip, coef = 0) + ego(actors$floor)))))
  expect_equal(stats::setNames(e$reason, e$term),
               c(inertia = "has no ergm term",
                 indeg = "depends on the outcome's own ties",
                 `ego(actors$floor)` = NA,
                 `offset(recip, coef = 0)` = "is an offset for goldfish, which is not yet written for ergm"))
  expect_equal(e$as[e$term == "ego(actors$floor)"], "nodeocov(\"floor\")")
})

test_that("the offsets of an ergm formula are kept for ergm", {
  f <- suppressMessages(specify_ergm(net ~ edges + offset(mutual) + nodematch("grp")))
  expect_match(paste(deparse(f[[3]]), collapse = ""), "offset(mutual)", fixed = TRUE)
})

test_that("tidy() gives each reason as text, without cli markup", {
  e <- suppressWarnings(suppressMessages(tidy(specify_ergm(
    a ~ ego(x) + sim(x) + indeg(b)))))
  expect_false(any(grepl("{", e$reason, fixed = TRUE)))
  expect_match(e$reason[e$term == "sim(x)"], "needs .data", fixed = TRUE)
})
