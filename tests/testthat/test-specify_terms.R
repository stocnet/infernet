# The table of every term that a model can take for a network, as
# `RSiena::getEffects()` gives for a Siena data object. These tests hold which
# terms each shape of network gets in each model, and how chosen rows become a
# formula.

quietly <- function(expr) suppressWarnings(suppressMessages(expr))

test_that("a directed network gets every term of its layers and attributes", {
  lf <- quietly(specify_mrqap(.data = manynet::ison_lawfirm))
  expect_s3_class(lf, c("mrqap_terms", "specify_terms", "tbl_df", "tbl", "data.frame"),
                  exact = TRUE)
  expect_named(lf, c("term", "effect", "layer", "attribute"))
  # A categorical attribute gets same() and mix(), and a numeric one diff().
  expect_true(all(c("ego(office)", "alter(office)", "same(office)",
                    "same(office, diff = TRUE)", "mix(office)") %in% lf$term))
  expect_true(all(c("ego(age)", "alter(age)", "diff(age)",
                    "diff(age, transformer_fn = identity)", "sim(age)") %in% lf$term))
  expect_false(any(c("diff(office)", "same(age)", "mix(age)") %in% lf$term))
  expect_true(all(c("tie(advice)", "exchange(advice)", "indeg(advice, node = 'ego')",
                    "outdeg(advice, node = 'alter')", "tertius(advice, age)")
                  %in% lf$term))
  expect_equal(lf$effect[lf$term == "indeg(advice, node = 'ego')"], "indeg")
  expect_equal(lf$layer[lf$term == "tertius(advice, age)"], "advice")
  expect_equal(lf$attribute[lf$term == "tertius(advice, age)"], "age")
  # A multiplex network without a focal layer reads its first layer as the
  # outcome, so a term that reads that layer has no row: `tie()` is refused,
  # and `exchange()` and `recip` would read the outcome's own ties.
  expect_equal(.deparsed(attr(lf, "outcome")), "friends")
  expect_false(any(c("tie(friends)", "exchange(friends)", "recip") %in% lf$term))
  # Terms an MRQAP drops, or that need another shape, have no row.
  expect_false(any(c("inertia", "trans", "cycle") %in% lf$term))
  expect_false(any(grepl("^reporter|^global", lf$term)))
  # A name, a label, or a marker of missing data gets no term.
  expect_false(any(c("name", "label", "na") %in% lf$attribute))
})

test_that("the outcome on the left-hand side sets the terms", {
  lf <- quietly(specify_mrqap(advice ~ ., manynet::ison_lawfirm))
  expect_equal(.deparsed(attr(lf, "outcome")), "advice")
  expect_false("tie(advice)" %in% lf$term)
  expect_true("tie(friends)" %in% lf$term)
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old))
  expect_any_message(suppressWarnings(specify_mrqap(.data = manynet::ison_lawfirm)),
                     "first layer. Name another outcome")
})

test_that("an undirected network has no terms of the second side", {
  und <- quietly(specify_mrqap(a ~ ., functional_nets()$undirected))
  expect_true(all(c("ego(x)", "diff(x)", "same(grp)", "tie(b)", "indeg(b)",
                    "indeg(b, node = 'ego')") %in% und$term))
  expect_false(any(grepl("^alter\\(|^exchange\\(|^outdeg\\(", und$term)))
  expect_false("recip" %in% und$term)
  # A network with one layer and no attributes has nothing to write out.
  expect_equal(nrow(quietly(specify_mrqap(.data = manynet::ison_adolescents))), 0)
})

test_that("a two-mode network reads ego() on one mode and alter() on the other", {
  tm <- quietly(specify_mrqap(a ~ ., functional_nets()$twomode))
  expect_true(all(c("ego(x)", "alter(x)", "diff(x)", "same(grp)", "mix(grp)",
                    "tie(b)", "indeg(b)", "outdeg(b)") %in% tm$term))
  expect_false(any(grepl("^exchange\\(|node = ", tm$term)))
  # The Southern Women's titles are on the first mode only, so there is no
  # alter() or same() of them, and the events' names are no attribute.
  sw <- quietly(specify_mrqap(.data = manynet::ison_southern_women))
  expect_true("ego(Title)" %in% sw$term)
  expect_false(any(c("alter(Title)", "same(Title)", "ego(active)") %in% sw$term))
})

test_that("a cognitive social structure gets the reporter terms", {
  ht <- quietly(specify_mrqap(advice ~ ., manynet::ison_hightech))
  expect_true(all(c("reporter(dept)", "reporter(age)", "reporter_ego", "reporter_alter",
                    "reporter_same(dept)", "reporter_same(dept, node = 'alter')")
                  %in% ht$term))
  expect_false("reporter_same(age)" %in% ht$term)
  # ergm fits no CSS.
  expect_error(quietly(specify_ergm(advice ~ ., manynet::ison_hightech)),
               "cognitive social structure")
  # A network that is not a CSS gets no reporter term.
  expect_false(any(startsWith(quietly(specify_mrqap(a ~ ., formula_net()))$term,
                              "reporter")))
})

test_that("a panel gets the terms that read the previous wave", {
  fr <- quietly(specify_mrqap(.data = manynet::ison_fraternity))
  expect_setequal(fr$term, c("recip", "inertia", "trans"))
})

test_that("a list of networks gets global()", {
  ls <- quietly(specify_mrqap(a ~ ., functional_nets()$list))
  expect_true("global(gdp)" %in% ls$term)
  expect_false("global(gdp)" %in% quietly(specify_mrqap(a ~ ., formula_net()))$term)
  expect_error(quietly(specify_ergm(a ~ ., functional_nets()$list)), "one network")
})

test_that("ergm gets the table of the terms it takes", {
  lf <- manynet::ison_lawfirm
  er <- quietly(specify_ergm(friends ~ ., lf))
  expect_s3_class(er, c("ergm_terms", "specify_terms"))
  expect_equal(attr(er, "model"), "ergm")
  expect_true("ego(office)" %in% er$term)
  expect_true("cycle" %in% er$term)
  expect_false("node_trans" %in% er$term)
})

test_that("a formula of the table's terms takes every term", {
  lf <- manynet::ison_lawfirm
  tab <- quietly(specify_mrqap(advice ~ ., lf))
  chosen <- tab[tab$term %in% c("ego(age)", "same(office)", "tie(friends)",
                                "exchange(advice)"), ]
  expect_s3_class(chosen, "specify_terms")
  f <- quietly(formula(chosen))
  expect_s3_class(f, "mrqap_formula")
  expect_equal(.deparsed(f[[2]]), "advice")
  record <- attr(f, "record")
  expect_setequal(record$term, chosen$term)
  expect_false(any(record$status %in% c("drops", "refuses")))
  # The outcome can be named as a name or a string.
  g <- quietly(formula(chosen, friends))
  expect_equal(.deparsed(g[[2]]), "friends")
  expect_equal(attr(g, "record")$status[attr(g, "record")$term == "tie(friends)"], "refuses")
  expect_equal(.deparsed(quietly(formula(chosen, "friends"))[[2]]), "friends")
  # A formula of the table fits as a written one does.
  fit <- quietly(estimate_mrqap(formula(tab[tab$term %in% c("ego(age)", "tie(friends)"), ]),
                                lf, times = 5, control = list(seed = 1)))
  expect_named(fit$coefficients, c("(Intercept)", "ego_age", "tie_friends"))
  # ergm's table gives an ergm formula.
  er <- quietly(specify_ergm(advice ~ ., lf))
  expect_s3_class(quietly(formula(er[er$term == "ego(office)", ])), "ergm_formula")
  expect_error(formula(tab[0, ]), "no rows")
  # Of the terms that measure the same thing in different ways, the formula
  # keeps the first, and says which it leaves out.
  same <- tab[tab$attribute %in% c("office", "age"), ]
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old))
  expect_any_message(h <- suppressWarnings(formula(same)), "Leaving out")
  terms <- attr(h, "record")$term
  expect_true(all(c("same(office)", "diff(age)", "ego(office)", "alter(age)") %in% terms))
  expect_false(any(c("same(office, diff = TRUE)", "mix(office)", "sim(age)",
                     "diff(age, transformer_fn = identity)") %in% terms))
  # A row chosen alone is kept.
  expect_equal(attr(quietly(formula(tab[tab$term == "mix(office)", ])), "record")$term,
               "mix(office)")
})

test_that("the table prints its terms, and tidy() gives a plain tibble", {
  tab <- quietly(specify_mrqap(advice ~ ., manynet::ison_lawfirm))
  expect_output(print(tab), "[0-9]+ terms for an MRQAP of advice")
  expect_output(print(tab), "indeg(friends, node = 'ego')", fixed = TRUE)
  expect_output(print(tab[c("term", "effect")]), "ego\\(status\\)")
  tidied <- tidy(tab)
  expect_s3_class(tidied, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
  expect_equal(nrow(tidied), nrow(tab))
  expect_null(attr(tidied, "data"))
})

test_that("a missing formula without a network keeps its error", {
  expect_error(specify_mrqap(), "formula")
  expect_error(specify_mrqap(advice ~ .), "needs `.data`")
  expect_error(specify_ergm(), "formula")
})

test_that("tidy() of a formula builds its columns on a list of networks", {
  ls <- functional_nets()$list
  out <- quietly(tidy(specify_mrqap(a ~ ego(x) + global(gdp)), ls))
  expect_equal(out$status, c("fits", "fits"))
  expect_equal(out$columns[[2]], "global_gdp")
})
