# `specify_saom()` writes a formula for RSiena: each term as its RSiena effect,
# named by its short name with the covariate it reads, such as `egoX(sex)`.
# These tests check the mappings and the terms that are dropped without the
# network, the reading of a formula written for RSiena and of a `sienaEffects`
# object, and, with RSiena, the effects object that `RSiena::siena()` fits.

rhs_of <- function(f) attr(stats::terms(f), "term.labels")

# `manynet::as_siena()` of the classmates warns where a changing covariate has
# a wave of missing values only, which is not what these tests check.
saom_of <- function(formula, .data = manynet::ison_classmates, input = NULL) {
  suppressWarnings(suppressMessages(specify_saom(formula, .data, input = input)))
}

included <- function(eff, dep = "friends") {
  class(eff) <- "data.frame"
  rows <- eff[eff$include & eff$type == "eval" & eff$name == dep, ]
  ifelse(nzchar(rows$interaction1),
         paste0(rows$shortName, "(", rows$interaction1, ")"), rows$shortName)
}

# ---- without the network ---------------------------------------------------

# `trans` and `cycle` are goldfish's names, so the formula is read as goldfish
# reads it, and goldfish's `outdeg` on its own reads the receiver: `outPop`.
test_that("each term becomes its RSiena effect, with density added", {
  f <- suppressMessages(specify_saom(
    friends ~ recip + trans + cycle + ego(sex) + alter(age) + same(sex) +
      diff(age) + diff(age, transformer_fn = identity) + sim(age) +
      tie(primary) + indeg + outdeg))
  expect_s3_class(f, "saom_formula")
  expect_equal(rhs_of(f),
               c("density", "recip", "transTrip", "cycle3", "egoX(sex)",
                 "altX(age)", "sameX(sex)", "absDiffX(age)", "diffX(age)",
                 "simX(age)", "X(primary)", "inPop", "outPop"))
  f <- suppressMessages(specify_saom(friends ~ outdeg + indeg(node = "ego")))
  expect_equal(rhs_of(f), c("density", "outAct", "inAct"))
})

test_that("terms without an RSiena effect are dropped, with a reason", {
  expect_warning(
    f <- suppressMessages(specify_saom(
      friends ~ ego(sex) + mix(sex) + same(sex, diff = TRUE) + ego(sex, levels = "male") +
        global(x) + ego(sex):alter(sex) + (1 | sender))),
    "Dropped")
  expect_equal(rhs_of(f), c("density", "egoX(sex)"))
  record <- summary(f)
  dropped <- record$term[record$status == "drops"]
  expect_setequal(dropped, c("mix(sex)", "same(sex, diff = TRUE)",
                             "ego(sex, levels = \"male\")", "global(x)",
                             "ego(sex):alter(sex)", "(1 | sender)"))
  expect_match(record$reason[record$term == "ego(sex):alter(sex)"], "includeInteraction")
})

test_that("a tie of the outcome is dropped, since a SAOM models it", {
  f <- suppressWarnings(suppressMessages(specify_saom(friends ~ tie(friends) + ego(sex))))
  expect_equal(rhs_of(f), c("density", "egoX(sex)"))
})

test_that("a formula written for RSiena is read as one, and kept", {
  f <- suppressMessages(specify_saom(friends ~ recip + transTrip + inAct + egoX(sex)))
  expect_identical(attr(f, "input"), "saom")
  expect_equal(rhs_of(f), c("density", "recip", "transTrip", "inAct", "egoX(sex)"))
})

test_that("an MRQAP reads a formula written for RSiena, and drops what it cannot fit", {
  f <- suppressWarnings(suppressMessages(specify_mrqap(
    friends ~ density + recip + transTrip + inAct + egoX(sex) + X(primary))))
  expect_identical(attr(f, "input"), "saom")
  expect_equal(rhs_of(f), c("recip", "ego(sex)", "primary"))
  record <- summary(f)
  expect_identical(record$status[record$term == "inAct"], "drops")
})

test_that("ergm reads a formula written for RSiena, and drops RSiena's own effects", {
  f <- suppressWarnings(suppressMessages(specify_ergm(
    friends ~ recip + inAct + egoX(sex))))
  expect_equal(rhs_of(f), c("edges", "mutual", "nodeocov(\"sex\")"))
})

test_that("a list of formulas gives one SAOM formula for each", {
  fs <- suppressMessages(specify_saom(list(friends ~ recip, primary ~ X(friends))))
  expect_length(fs, 2)
  expect_s3_class(fs[[2]], "saom_formula")
})

test_that("the table of every term gives the RSiena effect of each", {
  terms <- specify_saom(friends ~ ., manynet::ison_classmates)
  expect_s3_class(terms, "saom_terms")
  expect_identical(terms$as[terms$term == "ego(sex)"], "egoX(sex)")
  expect_identical(terms$as[terms$term == "tie(primary)"], "X(primary)")
  skip_if_not_installed("RSiena")
  f <- suppressWarnings(suppressMessages(
    formula(terms[terms$term %in% c("ego(sex)", "recip"), ])))
  expect_s3_class(f, "sienaEffects")
})

# ---- with the network ------------------------------------------------------

test_that("with the network, the result is the sienaEffects object of the formula", {
  skip_if_not_installed("RSiena")
  eff <- saom_of(friends ~ trans + ego(sex) + same(sex) + primary)
  expect_s3_class(eff, "sienaEffects")
  expect_true(inherits(attr(eff, "data"), c("siena", "sienadata")))
  expect_s3_class(attr(eff, "formula"), "saom_formula")
  # recip is RSiena's default, and is left out where the formula does not name it.
  expect_setequal(included(eff),
                  c("density", "transTrip", "egoX(sex)", "sameX(sex)", "X(primary)"))
})

test_that("only the left-hand side is a dependent variable", {
  skip_if_not_installed("RSiena")
  eff <- saom_of(friends ~ recip + ego(alcohol))
  data <- attr(eff, "data")
  expect_identical(names(data$depvars), "friends")
  expect_true("alcohol" %in% names(data$vCovars))
  # "primary" has ties at the first wave only, so `manynet::as_siena()`
  # makes it a constant dyadic covariate, not a varying one
  expect_true("primary" %in% names(data$dycCovars))
  expect_false("primary" %in% names(data$depvars))
})

test_that("a list of formulas makes several dependent networks", {
  skip_if_not_installed("RSiena")
  eff <- saom_of(list(friends ~ recip + ego(sex), primary ~ recip))
  expect_setequal(names(attr(eff, "data")$depvars), c("friends", "primary"))
  expect_true("recip" %in% included(eff, "primary"))
})

test_that("an effect or covariate that RSiena does not have aborts", {
  skip_if_not_installed("RSiena")
  expect_error(saom_of(friends ~ egoX(sexx)), "no covariate")
  expect_error(saom_of(friends ~ inPopp(sex)), "not a formula term")
  expect_error(saom_of(friends ~ egoSqX(sex) + inPopX(sex) + transTrip + inPopp, input = "saom"),
               "no effect")
  expect_error(saom_of(list(friends ~ X(primary), primary ~ recip)), "dependent network")
  expect_error(saom_of(advice ~ recip), "not a layer")
})

test_that("every model reads the effects object back", {
  skip_if_not_installed("RSiena")
  eff <- saom_of(friends ~ recip + ego(sex) + primary)
  f <- suppressWarnings(suppressMessages(specify_mrqap(eff)))
  expect_setequal(rhs_of(f), c("recip", "ego(sex)", "primary"))
  g <- suppressMessages(specify_saom(eff))
  expect_setequal(rhs_of(g), c("density", "recip", "X(primary)", "egoX(sex)"))
})
