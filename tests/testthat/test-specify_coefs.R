# declare_coefs(): a set of coefficients from a specification or from a fit.

coefs_net <- qap_net_binary(n = 12)
coefs_spec <- specify_mrqap(. ~ ego(Age) + same(Grp))
coefs_fit <- estimate_mrqap(coefs_spec, coefs_net, times = 5)

test_that("the names are the columns that the specification builds on a network", {
  coefs <- declare_coefs(coefs_spec, "(Intercept)" = -1, ego_Age = 0.1,
                         family = "binomial", .data = coefs_net)
  expect_named(coefs, names(coef(coefs_fit)))
  expect_named(coefs, colnames(.mrqap_design(coefs_spec, coefs_net)$X))
  # A column without a value is not stated.
  expect_equal(unname(coef(coefs)), c(-1, 0.1, NA))
  expect_s3_class(coefs, "infernetMrqapCoefs", exact = TRUE)
  expect_identical(attr(coefs, "family"), "binomial")
  expect_s3_class(attr(coefs, "spec"), "mrqap_formula")
  expect_null(attr(coefs, "sigma"))
})

test_that("without a family, it is read from the outcome, as estimate_mrqap() does", {
  binary <- declare_coefs(coefs_spec, ego_Age = 0.1, .data = coefs_net)
  expect_identical(attr(binary, "family"), "binomial")
  expect_identical(attr(binary, "family"), coefs_fit$family)
  count_net <- qap_net_count(n = 12)
  age_spec <- specify_mrqap(. ~ ego(Age))
  count <- declare_coefs(age_spec, ego_Age = 0.1, .data = count_net)
  expect_identical(attr(count, "family"), "poisson")
  expect_identical(attr(count, "family"),
                   suppressMessages(estimate_mrqap(age_spec, count_net, times = 2))$family)
  gauss_net <- qap_net_gaussian(n = 12)
  valued <- declare_coefs(age_spec, ego_Age = 0.1, sigma = 1, .data = gauss_net)
  expect_identical(attr(valued, "family"), "gaussian")
  expect_equal(attr(valued, "sigma"), 1)
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_any_message(declare_coefs(coefs_spec, ego_Age = 0.1, .data = coefs_net),
                     "Treating the outcome as \"binomial\"")
})

test_that("one named vector gives the values, as coef() of a fit does", {
  coefs <- declare_coefs(coefs_spec, coef(coefs_fit), family = "binomial")
  expect_equal(coef(coefs), coef(coefs_fit))
})

test_that("coef() gives a plain named vector", {
  values <- coef(declare_coefs(coefs_fit))
  expect_type(values, "double")
  expect_identical(names(attributes(values)), "names")
  expect_equal(values, coefs_fit$coefficients)
  fit_values <- coef(coefs_fit)
  expect_identical(names(attributes(fit_values)), "names")
  expect_identical(fit_values, values)
})

test_that("the coefficients of a fit carry its specification, family, and sigma", {
  coefs <- declare_coefs(coefs_fit)
  expect_identical(attr(coefs, "family"), "binomial")
  expect_equal(.plain_formula(attr(coefs, "spec")), coefs_fit$formula)
  expect_null(attr(coefs, "sigma"))
  gaussian <- suppressMessages(
    estimate_mrqap(. ~ ego(Age), qap_net_gaussian(n = 12), times = 5))
  expect_equal(attr(declare_coefs(gaussian), "sigma"),
               stats::sigma(gaussian$simple_fit))
})

test_that("the print gives the family, the formula, and the values", {
  coefs <- declare_coefs(coefs_spec, "(Intercept)" = -1, ego_Age = 0.1,
                         family = "binomial", .data = coefs_net)
  out <- capture.output(print(coefs))
  expect_match(out[1], "Coefficients of a binomial MRQAP")
  expect_true(any(grepl("2 of 3 coefficients stated", out)))
  expect_true(any(grepl("ego(Age)", out, fixed = TRUE)))
})

test_that("declare_coefs() names what it needs", {
  expect_error(declare_coefs(coefs_spec, "(Intercept)" = -1), "needs .*\\.data")
  expect_error(declare_coefs(coefs_spec, "(Intercept)" = -1, family = "auto"),
               "must be one of")
  expect_error(declare_coefs(coefs_spec, "(Intercept)" = -1, family = "logit"),
               "must be one of")
  expect_error(declare_coefs(coefs_spec, family = "binomial"),
               "a value for each coefficient")
  expect_error(declare_coefs(coefs_spec, -1, 0.1, family = "binomial"),
               "named for its column")
  expect_error(declare_coefs(coefs_spec, ego_Age = 1, ego_Age = 2,
                             family = "binomial"), "named once")
  expect_error(declare_coefs(coefs_spec, ego_age = 0.1, family = "binomial",
                             .data = coefs_net), "no column named")
  expect_error(declare_coefs(coefs_spec, ego_Age = 0.1, family = "gaussian",
                             sigma = -1), "positive")
  expect_snet_warning(
    declare_coefs(coefs_spec, ego_Age = 0.1, family = "binomial", sigma = 1),
    "not used")
  expect_error(declare_coefs(1:3), "no method")
})
