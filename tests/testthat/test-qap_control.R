# `control` is merged over the defaults by name, so a name that is not a control
# would be added silently and the option it was meant to set would keep its
# default, with nothing to say so.

FORM <- weight ~ ego(Age) + alter(Age)

test_that("an unknown control name is rejected, with the nearest match", {
  g <- qap_net_gaussian(n = 15)
  expect_error(
    estimate_mrqap(FORM, g, times = 5, control = list(strateggy = "sequential")),
    "strategy")
  expect_error(
    estimate_mrqap(FORM, g, times = 5, control = list(nonsense = 1)),
    "nonsense")
})

test_that("an unnamed control entry is rejected", {
  g <- qap_net_gaussian(n = 15)
  expect_error(
    estimate_mrqap(FORM, g, times = 5, control = list("sequential")),
    "named")
})

test_that("an empty control list gives the defaults", {
  expect_equal(.resolve_control(list()), .resolve_control())
  expect_equal(.resolve_control()$permute, "predictor")
  expect_equal(.resolve_control()$strategy, "sequential")
  expect_equal(.resolve_control()$family, "auto")
})

test_that("a named control overrides only that default", {
  ctrl <- .resolve_control(list(family = "poisson"))
  expect_equal(ctrl$family, "poisson")
  expect_equal(ctrl$strategy, "sequential")
  expect_null(ctrl$directed)
})

test_that("permute takes only the two spellings it documents", {
  expect_equal(.resolve_control(list(permute = "outcome"))$permute, "outcome")
  expect_error(.resolve_control(list(permute = "qapy")))
})

test_that("permute = 'outcome' is recorded on the fit and gives a full matrix", {
  g <- qap_net_gaussian(n = 20)
  fit <- estimate_mrqap(FORM, g, times = 10,
                        control = list(seed = 1, permute = "outcome"))
  expect_equal(fit$permute, "outcome")
  # Permuting y alone tests every coefficient, the intercept included, whereas
  # double semi-partialling residualises one predictor at a time.
  expect_false(anyNA(fit$lower))
  spp <- estimate_mrqap(FORM, g, times = 10, control = list(seed = 1))
  expect_equal(spp$permute, "predictor")
  expect_true(all(is.na(spp$lower[, "(Intercept)"])))
})

test_that("a single predictor falls back to permuting the outcome", {
  g <- qap_net_gaussian(n = 20)
  fit <- estimate_mrqap(weight ~ ego(Age), g, times = 10,
                        control = list(seed = 1, permute = "predictor"))
  # Double semi-partialling residualises a predictor against the others, and
  # with one predictor there are none.
  expect_equal(fit$permute, "outcome")
})

test_that("directed and diag are read from the network unless set", {
  g <- qap_net_gaussian(n = 15)
  expect_true(estimate_mrqap(FORM, g, times = 5,
                             control = list(seed = 1))$directed)
  expect_false(estimate_mrqap(FORM, g, times = 5,
                              control = list(seed = 1,
                                             directed = FALSE))$directed)
  loops <- estimate_mrqap(FORM, g, times = 5,
                          control = list(seed = 1, diag = TRUE))
  expect_true(loops$diag)
  expect_equal(nrow(loops$pred), 15 * 15)
})

# ---- control_mrqap() ---------------------------------------------------------

test_that("control_mrqap() gives every control, with its default", {
  ctrl <- control_mrqap()
  expect_s3_class(ctrl, "mrqap_control")
  expect_equal(ctrl$permute, "predictor")
  expect_equal(ctrl$family, "auto")
  expect_null(ctrl$directed)
  expect_true("directed" %in% names(ctrl))
  expect_false(ctrl$less_mem)
})

test_that("a list and control_mrqap() are read the same way", {
  expect_identical(.resolve_control(list(family = "poisson", seed = 1)),
                   control_mrqap(family = "poisson", seed = 1))
  ctrl <- control_mrqap(permute = "outcome")
  expect_identical(.resolve_control(ctrl), ctrl)
  g <- qap_net_gaussian(n = 15)
  a <- estimate_mrqap(FORM, g, times = 5, control = list(seed = 1, permute = "outcome"))
  b <- estimate_mrqap(FORM, g, times = 5, control = control_mrqap(seed = 1, permute = "outcome"))
  expect_equal(a$lower, b$lower)
})

test_that("a control prints with the ones that were set marked", {
  out <- capture.output(print(control_mrqap(family = "poisson")))
  expect_match(out, "family +poisson  \\(set\\)", all = FALSE)
  expect_match(out, "directed +NULL, read from the data$", all = FALSE)
  expect_false(any(grepl("permute.*\\(set\\)", out)))
})

test_that("a control with a value it cannot take aborts", {
  expect_error(control_mrqap(family = "logit"), "family")
  expect_error(control_mrqap(strategy = "parallel"), "strategy")
  expect_error(control_mrqap(directed = "yes"), "directed")
  expect_error(control_mrqap(less_mem = NA), "less_mem")
  expect_error(control_mrqap(seed = "a"), "seed")
  expect_error(control_mrqap(ncores = 0), "ncores")
  expect_error(control_mrqap(missing = "impute"), "not yet available")
  expect_error(.resolve_control("sequential"), "must be a list")
})

test_that("a name that infernet used before is converted, with a warning", {
  expect_warning(ctrl <- control_mrqap(nullhyp = "qapy"), "permute")
  expect_equal(ctrl$permute, "outcome")
  expect_warning(ctrl <- .resolve_control(list(method = "qapspp")), "permute")
  expect_equal(ctrl$permute, "predictor")
  expect_warning(ctrl <- control_mrqap(mode = "undirected"), "directed")
  expect_false(ctrl$directed)
  expect_warning(ctrl <- control_mrqap(random_intercept_perceiver = TRUE),
                 "random_intercept_reporter")
  expect_true(ctrl$random_intercept_reporter)
  # Both names for one control cannot be told apart.
  expect_error(suppressWarnings(control_mrqap(permute = "outcome", nullhyp = "qapy")),
               "both")
})

test_that("goldfish's estimation options are dropped with one warning", {
  expect_warning(ctrl <- control_mrqap(max_iterations = 50, engine = "default",
                                       seed = 1),
                 "Dropped 2 controls")
  expect_equal(ctrl$seed, 1)
  expect_false("max_iterations" %in% names(ctrl))
  opt <- structure(list(max_iterations = 20, convergence_criterion = 0.001,
                        engine = "default_c"),
                   class = "estimation_opt.goldfish")
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  msgs <- testthat::capture_messages(
    expect_warning(ctrl <- .resolve_control(opt), "Dropped 3 controls"))
  expect_match(paste(msgs, collapse = ""), "goldfish")
  expect_identical(ctrl, control_mrqap())
})

test_that("an unknown name suggests the nearest, old names included", {
  expect_error(control_mrqap(familly = "poisson"), "Did you mean")
  expect_error(control_mrqap(nullhyps = "qapy"), "nullhyp")
})
