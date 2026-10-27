# What each model makes of each term: the record that every `specify_*()`
# function keeps, and `tidy()` of it. These tests hold the status of every
# registered term in every model, so a new term fails here until its status in
# each model is written down.

# One example of each registered term, by its kind.
term_example <- function(name, entry) {
  if (name %in% c("match", "mix")) return(paste0(name, "(grp)"))
  switch(entry$kind,
    attribute = , interaction = paste0(name, "(x)"),
    layer = , mixed = paste0(name, "(b)"),
    tertius = paste0(name, "(b, x)"),
    global = paste0(name, "(gdp)"),
    name)
}

# The status a model gives one term, next to `ego(x)` so that something is
# always left to fit.
status_in <- function(specify, label, g, tidied = FALSE) {
  f <- suppressWarnings(suppressMessages(
    specify(stats::as.formula(paste("a ~", label, "+ ego(x)")), g)))
  record <- if (tidied) suppressWarnings(tidy(f, g)) else attr(f, "record")
  unique(record$status[record$term == label])
}

# mrqap is the record; tidied is `tidy()` with the network, which builds each
# term. The other models name the term in their own formula, and the registry's
# `dynam`, `saom`, and `ergm` say which term that is.
expected_status <- utils::read.table(header = TRUE, stringsAsFactors = FALSE, text = "
  term                  mrqap  tidied   dynam       ergm
  ego                   fits   fits     fits        adapted
  alter                 fits   fits     fits        adapted
  same                  fits   fits     fits        adapted
  diff                  fits   fits     fits        adapted
  dist                  warns  warns    adapted     adapted
  sim                   fits   fits     interpreted adapted
  match                 warns  warns    drops       adapted
  mix                   fits   fits     drops       adapted
  tie                   fits   fits     fits        adapted
  recip                 fits   fits     fits        adapted
  exchange              fits   fits     adapted     adapted
  indeg                 fits   fits     fits        adapted
  outdeg                fits   fits     adapted     adapted
  tertius               fits   fits     interpreted adapted
  tertius_diff          fits   fits     interpreted adapted
  global                fits   refuses  fits        drops
  reporter              fits   refuses  drops       drops
  reporter_ego          fits   refuses  drops       drops
  reporter_alter        fits   refuses  drops       drops
  reporter_same         fits   refuses  drops       drops
  inertia               drops  drops    fits        drops
  trans                 drops  drops    fits        adapted
  cycle                 drops  drops    fits        adapted
  common_sender         drops  drops    fits        drops
  common_receiver       drops  drops    fits        drops
  four                  drops  drops    fits        adapted
  triangle              drops  drops    fits        fits
  node_trans            drops  drops    fits        drops
  degree                drops  drops    fits        drops
  mixed_trans           drops  drops    fits        drops
  mixed_cycle           drops  drops    fits        drops
  mixed_common_sender   drops  drops    fits        drops
  mixed_common_receiver drops  drops    fits        drops
  ego_alter_interaction drops  drops    fits        drops
  alterdeg              drops  drops    fits        drops
  alterpop              drops  drops    fits        drops
  egodeg                drops  drops    fits        drops
  egopop                drops  drops    fits        drops
  dyad                  drops  drops    fits        drops
  dyadXdiff             drops  drops    fits        drops
  dyadXego              drops  drops    fits        drops
  size                  drops  drops    fits        drops
  sizeXdiff             drops  drops    fits        drops
  sizeXego              drops  drops    fits        drops
  intercept             drops  drops    fits        drops
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
    expect_equal(status_in(specify_dynam, label, g), row$dynam, info = label)
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
  d <- suppressWarnings(specify_dynam(a ~ ego(x) + same(grp, diff = TRUE)))
  expect_named(tidy(d), c("term", "status", "as", "reason"))
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
      tertius_diff(friendship, age))
  expected <- list(
    c(inertia = "drops", recip = "warns", trans = "drops"),
    c(inertia = "drops", `alter(gradeType)` = "fits", recip = "warns", trans = "drops"),
    c(`indeg(friendship)` = "fits", `tie(friendship)` = "fits"),
    c(`node_trans(calls, type = "ego")` = "drops", `alter(gradeType)` = "fits"),
    c(inertia = "drops", recip = "warns", `same(gradeType)` = "fits",
      `diff(age, transformer_fn = abs)` = "fits",
      `tertius_diff(friendship, age)` = "interpreted"))
  for (k in seq_along(formulas)) {
    t <- suppressWarnings(suppressMessages(tidy(specify_mrqap(formulas[[k]]))))
    expect_equal(stats::setNames(t$status, t$term), expected[[k]],
                 info = deparse(formulas[[k]]))
  }
})
