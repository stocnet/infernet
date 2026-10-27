test_that("test_parameter() and test_time() have the signature of RSiena and goldfish", {
  # A method must keep the generic's arguments, so the generics must agree
  # with the ones that other packages register methods on.
  expect_equal(names(formals(test_parameter)), c("x", "..."))
  expect_equal(names(formals(test_time)), c("x", "..."))
})

test_that("test_parameter() and test_time() refuse a class with no method", {
  expect_error(test_parameter(1:3), "no method")
  expect_error(test_time(1:3), "no method")
})
