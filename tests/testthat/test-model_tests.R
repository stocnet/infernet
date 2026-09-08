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
  expect_false(cugtest$diag)
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
  expect_invisible(print(cugtest))
  expect_identical(withVisible(print(cugtest))$value, cugtest)
})
