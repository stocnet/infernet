# The diagnostics of a sequence that `{infernet}` holds for every stocnet model (D22).

test_that("the diagnostics of a sequence have the signature of goldfish", {
  expect_equal(names(formals(diagnose_changepoints)), c("x", "..."))
  expect_equal(names(formals(diagnose_onset)), c("x", "..."))
})

test_that("the diagnostics of a sequence refuse a class with no method", {
  expect_error(diagnose_changepoints(1:3), "no method")
  expect_error(diagnose_onset(1:3), "no method")
})
