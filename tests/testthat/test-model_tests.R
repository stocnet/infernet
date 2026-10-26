# Making sure the tests family of functions works as intended.
# Ported from {migraph}, which these functions supersede.

marvel_friends <- manynet::to_uniplex(manynet::fict_marvel, "relationship") |>
  manynet::to_giant() |> manynet::to_unsigned() |>
  manynet::to_subgraph(PowerOrigin == "Human")

cugtest <- test_random(marvel_friends,
                       netrics::net_by_heterophily,
                       attribute = "Attractive",
                       times = 200)
cugtest2 <- test_random(marvel_friends,
                        netrics::net_by_betweenness,
                        times = 200)

test_that("test_random works", {
  expect_equal(as.numeric(cugtest$testval), -0.85714, tolerance = 0.001)
  expect_length(cugtest$testdist, 200) # NB: Stochastic
  expect_false(cugtest$directed)
  expect_false(cugtest$loops)
  expect_equal(cugtest$cmode, "edges")
  expect_type(cugtest$plteobs, "double")
  expect_type(cugtest$pgteobs, "double")
  expect_equal(cugtest$times, 200)
  expect_s3_class(cugtest, "network_test")
  expect_equal(as.numeric(cugtest2$testval), 0.2375, tolerance = 0.001)
  expect_length(cugtest2$testdist, 200) # NB: Stochastic
  expect_equal(round(cugtest2$plteobs), 1)
  expect_equal(round(cugtest2$pgteobs), 0)
  expect_s3_class(cugtest2, "network_test")
})

qaptest <- test_permutation(marvel_friends,
                            netrics::net_by_heterophily,
                            attribute = "Attractive",
                            times = 200)

test_that("test_permutation works", {
  expect_equal(as.numeric(qaptest$testval), -0.85714, tolerance = 0.001)
  expect_type(qaptest$plteobs, "double") # NB: Stochastic
  expect_type(qaptest$pgteobs, "double") # NB: Stochastic
  expect_length(qaptest$testdist, 200) # NB: Stochastic
  expect_equal(qaptest$times, 200)
  expect_s3_class(qaptest, "network_test")
})

test_that("test_configuration works", {
  testthat::skip_on_os("linux")
  configtest <- test_configuration(marvel_friends,
                                   netrics::net_by_heterophily,
                                   attribute = "Attractive",
                                   times = 200)
  expect_s3_class(configtest, "network_test")
  expect_equal(as.numeric(configtest$testval), -0.85714, tolerance = 0.001)
  expect_type(configtest$plteobs, "double") # NB: Stochastic
  expect_type(configtest$pgteobs, "double") # NB: Stochastic
  expect_length(configtest$testdist, 200) # NB: Stochastic
})

test_that("print.network_test prints and returns its input invisibly", {
  expect_output(print(cugtest), "CUG Test Results")
  expect_output(shown <- withVisible(print(cugtest)))
  expect_false(shown$visible)
  expect_identical(shown$value, cugtest)
})

test_that("a stocnet is drawn from as a tbl_graph where the statistic agrees", {
  het <- function(g) netrics::net_by_heterophily(g, "Gender")
  expect_s3_class(.draw_from(marvel_friends, het), "tbl_graph")
  # A statistic that reads the class itself keeps the stocnet.
  is_stocnet <- function(g) as.numeric(inherits(g, "stocnet"))
  expect_s3_class(.draw_from(marvel_friends, is_stocnet), "stocnet")
  # Another class is drawn from as it is.
  ig <- manynet::as_igraph(marvel_friends)
  expect_identical(.draw_from(ig, het), ig)
})

test_that("the draws from a stocnet keep its attributes and its statistic", {
  set.seed(1)
  qap <- test_permutation(marvel_friends, netrics::net_by_heterophily,
                          attribute = "Gender", times = 20)
  expect_false(anyNA(qap$testdist))
  # Each draw is a different network, not one network drawn again.
  expect_gt(length(unique(qap$testdist)), 1)
  set.seed(1)
  cug <- test_random(marvel_friends, netrics::net_by_heterophily,
                     attribute = "Gender", times = 20)
  expect_false(anyNA(cug$testdist))
})

test_that("the tests take ncores and seed as controls, and restore the plan", {
  future::plan(future::sequential)
  one <- test_permutation(marvel_friends, netrics::net_by_heterophily,
                          attribute = "Attractive", times = 20,
                          control = list(ncores = 1, seed = 1))
  two <- test_permutation(marvel_friends, netrics::net_by_heterophily,
                          attribute = "Attractive", times = 20,
                          control = list(ncores = 2, seed = 1))
  expect_equal(one$testdist, two$testdist)
  expect_s3_class(future::plan(), "sequential")
  expect_error(test_random(marvel_friends, netrics::net_by_heterophily,
                           attribute = "Attractive", times = 5,
                           control = list(ncores = 0)),
               "ncores")
})

test_that("the tests read the controls of a model whole", {
  ctrl <- control_mrqap(family = "poisson", seed = 3)
  a <- test_random(marvel_friends, netrics::net_by_heterophily,
                   attribute = "Attractive", times = 10, control = ctrl)
  b <- test_random(marvel_friends, netrics::net_by_heterophily,
                   attribute = "Attractive", times = 10,
                   control = list(seed = 3))
  expect_equal(a$testdist, b$testdist)
})

test_that("strategy and ncores given by name still work, with a warning", {
  expect_warning(
    old <- test_permutation(marvel_friends, netrics::net_by_heterophily,
                            attribute = "Attractive", times = 10,
                            strategy = "sequential",
                            control = list(seed = 2)),
    "control")
  new <- test_permutation(marvel_friends, netrics::net_by_heterophily,
                          attribute = "Attractive", times = 10,
                          control = list(seed = 2))
  expect_equal(old$testdist, new$testdist)
  expect_error(suppressWarnings(
    test_random(marvel_friends, netrics::net_by_heterophily,
                attribute = "Attractive", times = 5, ncores = 1,
                control = list(ncores = 2))),
    "both")
})
