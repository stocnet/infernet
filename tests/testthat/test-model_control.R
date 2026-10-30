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
  expect_null(.resolve_control()$strategy)
  expect_equal(.resolve_control()$family, "auto")
})

test_that("a named control overrides only that default", {
  ctrl <- .resolve_control(list(family = "poisson"))
  expect_equal(ctrl$family, "poisson")
  expect_null(ctrl$strategy)
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

test_that("directed and loops are read from the network unless set", {
  g <- qap_net_gaussian(n = 15)
  expect_true(estimate_mrqap(FORM, g, times = 5,
                             control = list(seed = 1))$directed)
  expect_false(estimate_mrqap(FORM, g, times = 5,
                              control = list(seed = 1,
                                             directed = FALSE))$directed)
  loops <- estimate_mrqap(FORM, g, times = 5,
                          control = list(seed = 1, loops = TRUE))
  expect_true(loops$loops)
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

test_that("a control prints the ones that were set, and counts the defaults", {
  out <- capture.output(print(control_mrqap(family = "poisson")))
  expect_match(out, "1 set, 14 at their defaults", all = FALSE)
  expect_match(out, '^  family +"poisson"$', all = FALSE)
  # `?control_mrqap` lists the defaults, so the print does not.
  expect_false(any(grepl("permute", out)))
})

test_that("diag is now called loops, with a warning", {
  expect_warning(ctrl <- control_mrqap(diag = TRUE), "loops")
  expect_true(ctrl$loops)
  expect_null(ctrl$diag)
  g <- qap_net_gaussian(n = 15)
  expect_warning(fit <- estimate_mrqap(FORM, g, times = 5,
                                       control = list(seed = 1, diag = FALSE)),
                 "loops")
  expect_false(fit$loops)
})

# Two workers or more start a plan, which test-parallel.R checks once. Here only
# the sequential cases, which start no workers.
test_that("ncores = 1 runs sequentially, whatever strategy says", {
  on.exit(future::plan(future::sequential), add = TRUE)
  old <- setup_future_plan("multisession", ncores = 1)
  expect_s3_class(future::plan(), "sequential")
  restore_future_plan(old)
  old <- setup_future_plan("sequential", ncores = NULL)
  expect_s3_class(future::plan(), "sequential")
  restore_future_plan(old)
})

test_that("the default leaves the plan set with future::plan() as it is", {
  on.exit(future::plan(future::sequential), add = TRUE)
  # A sequential plan with an option of its own, so that a plan the default
  # replaced would not be identical to it.
  future::plan(future::sequential, split = TRUE)
  before <- future::plan()
  old <- setup_future_plan()
  expect_identical(future::plan(), before)
  restore_future_plan(old)
  expect_identical(future::plan(), before)
  expect_null(control_mrqap()$strategy)
  expect_error(control_mrqap(strategy = "parallel"), "NULL")
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

test_that("goldfish 2.0's option objects are read whole", {
  # The fields of goldfish 2.0.0's `set_algorithm_newton()` and
  # `set_preprocessing()`, built by hand so that goldfish need not be installed.
  algo <- structure(list(initial_parameters = NULL, fixed_parameters = NULL,
                         offset_coef = NULL, max_iterations = 20,
                         score_tol = 1e-6, step_tol = 1e-8,
                         initial_damping = NULL, damping_increase_factor = 2,
                         damping_decrease_factor = 3,
                         return_interval_loglik = TRUE,
                         return_probabilities = FALSE,
                         return_event_scores = TRUE,
                         diagnostics = c("loglik", "scores"),
                         optimizer = "newton_raphson", backend = "cpp"),
                    class = c("goldfishAlgoNewton", "goldfishAlgo", "list"))
  prep <- structure(list(start_time = NULL, end_time = NULL,
                         opportunities_list = NULL, impute = NULL, db = NULL,
                         db_table = "stats"),
                    class = c("goldfishPrepCtrl", "list"))
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  msgs <- testthat::capture_messages(
    expect_warning(ctrl <- .resolve_control(algo), "Dropped 15 controls"))
  expect_match(paste(msgs, collapse = ""), "estimation options")
  expect_identical(ctrl, control_mrqap())
  msgs <- testthat::capture_messages(
    expect_warning(ctrl <- .resolve_control(prep), "Dropped 6 controls"))
  expect_match(paste(msgs, collapse = ""), "preprocessing options")
  expect_identical(ctrl, control_mrqap())
  g <- qap_net_gaussian(n = 15)
  expect_warning(fit <- suppressMessages(
    estimate_mrqap(FORM, g, times = 5, control = algo)), "Dropped")
  expect_s3_class(fit, "netreg")
})

test_that("an unknown name suggests the nearest, old names included", {
  expect_error(control_mrqap(familly = "poisson"), "Did you mean")
  expect_error(control_mrqap(nullhyps = "qapy"), "nullhyp")
})

# ---- reporting what the model resolved -----------------------------------------

# Anything the model resolves for itself is reported, so that a user can
# describe the model they actually fitted.
#
# These run with `snet_verbosity = "verbose"`, which is not the default. That
# matters: an informational message is silent in every other test, so a broken
# one stays invisible. A `{cli}` brace expression beginning with a dot is read
# as a style rather than as code, and two of these messages shipped that way
# before this file existed.

verbosely <- function(expr) {
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  force(expr)
}

FORM_REPORT <- weight ~ ego(Age) + alter(Age) + sim(Age)

test_that("every reporting message renders", {
  g <- qap_net_gaussian(n = 15)
  # A message that cli cannot parse aborts, so reaching the end is the test.
  expect_no_error(testthat::capture_messages(verbosely(
    estimate_mrqap(FORM_REPORT, g, times = 5, control = list(seed = 1)))))
  expect_no_error(testthat::capture_messages(verbosely(
    estimate_mrqap(weight ~ ego(Age), g, times = 5, control = list(seed = 1)))))
})

test_that("a family resolved from the outcome is reported", {
  expect_any_message(
    verbosely(estimate_mrqap(. ~ ego(Age) + alter(Age), qap_net_binary(n = 15),
                             times = 5, control = list(seed = 1))),
    "binomial")
})

test_that("directedness read from the network is reported", {
  expect_any_message(
    verbosely(estimate_mrqap(weight ~ ego(Age) + alter(Age),
                             qap_net_undirected(n = 14),
                             times = 5, control = list(seed = 1))),
    "undirected")
})

test_that("a stated family and directedness are not reported", {
  g <- qap_net_gaussian(n = 15)
  expect_no_message(
    verbosely(estimate_mrqap(FORM_REPORT, g, times = 5,
                             control = list(seed = 1, family = "gaussian",
                                            directed = TRUE))))
})

test_that("the fallback to permuting the outcome is reported", {
  expect_any_message(
    verbosely(estimate_mrqap(weight ~ ego(Age), qap_net_gaussian(n = 15),
                             times = 5,
                             control = list(seed = 1, permute = "predictor"))),
    "residualise")
})


test_that("the model advice on homophily terms is reported", {
  expect_any_message(
    verbosely(estimate_mrqap(weight ~ same(Grp), qap_net_gaussian(n = 15),
                             times = 5, control = list(seed = 1))),
    "ego\\(Grp\\)")
})

test_that("the advice on homophily leaves out what the fixed effects absorb", {
  msgs <- testthat::capture_messages(
    verbosely(estimate_mrqap(weight ~ same(Grp) | sender + receiver,
                             qap_net_gaussian(n = 15), times = 5,
                             control = list(seed = 1))))
  expect_no_match(paste(msgs, collapse = ""), "more fundamental effects")
  msgs <- testthat::capture_messages(
    verbosely(estimate_mrqap(weight ~ same(Grp) | sender,
                             qap_net_gaussian(n = 15), times = 5,
                             control = list(seed = 1))))
  msgs <- paste(msgs, collapse = "")
  expect_match(msgs, "alter\\(Grp\\)")
  expect_no_match(msgs, "ego\\(Grp\\)")
})

test_that("dropped dyads and fixed nodes are reported", {
  g <- qap_net_gaussian(n = 15)
  m <- manynet::as_matrix(g)
  m[2, 3] <- NA
  holed <- manynet::mutate(manynet::as_tidygraph(m),
                           Age = manynet::node_attribute(g, "Age"))
  msgs <- testthat::capture_messages(
    verbosely(estimate_mrqap(weight ~ ego(Age), holed, times = 5,
                             control = list(seed = 1))))
  msgs <- paste(msgs, collapse = "")
  expect_match(msgs, "Using 209 of 210 dyads")
  expect_match(msgs, "1 on weight")
  expect_match(msgs, "2 nodes with scattered unobserved or fixed cells stay in place")
})

test_that("control_mrqap() refuses groups that are not a vector", {
  expect_error(control_mrqap(groups = list(1, 2)), "vector with a group")
})

# ---- persistent controls -------------------------------------------------------

# Each test sets `options(infernet.control)` and restores it on exit, since the
# test files run in the same process one after another.
persisting <- function(expr, value = NULL) {
  old <- options(infernet.control = value)
  on.exit(options(old), add = TRUE)
  force(expr)
}

test_that("persist = TRUE writes the given controls, checked, to the options", {
  persisting({
    ctrl <- control_mrqap(ncores = 1, missing = "fail", persist = TRUE)
    stored <- getOption("infernet.control")
    expect_identical(stored[c("missing", "ncores")],
                     list(missing = "fail", ncores = 1))
    expect_equal(attr(ctrl, "source")[["ncores"]], "given")
    # A control is checked before it persists, so nothing is written.
    options(infernet.control = NULL)
    expect_error(control_mrqap(ncores = 0, persist = TRUE), "ncores")
    expect_null(getOption("infernet.control"))
    expect_error(control_mrqap(persist = NA), "persist")
  })
})

test_that("a later call uses a persistent control, and a given one wins", {
  persisting({
    ctrl <- control_mrqap()
    expect_equal(ctrl$missing, "fail")
    expect_equal(ctrl$seed, 3)
    expect_equal(attr(ctrl, "source")[["seed"]], "option")
    expect_equal(control_mrqap(seed = 4)$seed, 4)
    expect_equal(.resolve_control(list(missing = "available"))$missing,
                 "available")
    # A control given as NULL is given too.
    expect_null(.resolve_control(list(seed = NULL))$seed)
  }, value = list(missing = "fail", seed = 3))
})

test_that("persist = TRUE adds to the persistent controls, and NULL removes one", {
  persisting({
    control_mrqap(ncores = 1, persist = TRUE)
    control_mrqap(seed = 2, persist = TRUE)
    expect_identical(getOption("infernet.control"), list(ncores = 1, seed = 2))
    control_mrqap(ncores = NULL, persist = TRUE)
    expect_identical(getOption("infernet.control"), list(seed = 2))
  })
})

test_that("persist = FALSE, given, clears the persistent controls", {
  persisting({
    ctrl <- control_mrqap(persist = FALSE)
    expect_null(getOption("infernet.control"))
    expect_null(ctrl$seed)
    # The default leaves them as they are.
    options(infernet.control = list(seed = 2))
    control_mrqap(family = "poisson")
    expect_identical(getOption("infernet.control"), list(seed = 2))
  }, value = list(seed = 2, ncores = 1))
})

test_that("an option set by hand is checked when it is read", {
  persisting(expect_error(control_mrqap(), "ncores"),
             value = list(ncores = 0))
  persisting(expect_error(control_mrqap(), "not a control"),
             value = list(foo = 1))
  persisting(expect_error(control_mrqap(), "named list"),
             value = list(1))
})

test_that("a strategy persists, and strategy = NULL in a call keeps the plan", {
  persisting({
    expect_equal(control_mrqap()$strategy, "sequential")
    ctrl <- .resolve_control(list(strategy = NULL))
    expect_true("strategy" %in% names(ctrl))
    expect_null(ctrl$strategy)
  }, value = list(strategy = "sequential"))
  expect_true("strategy" %in% names(control_mrqap()))
})

test_that("a print marks the controls that come from the options", {
  persisting({
    out <- capture.output(print(control_mrqap(family = "poisson")))
    expect_match(out, "seed +5  \\(option\\)", all = FALSE)
    expect_match(out, 'family +"poisson"$', all = FALSE)
    expect_match(out, "persist = FALSE", all = FALSE)
  }, value = list(seed = 5))
  out <- capture.output(print(control_mrqap()))
  expect_false(any(grepl("\\(option\\)", out)))
})

test_that("a fit uses the persistent controls, reports them, and records them", {
  g <- qap_net_gaussian(n = 15)
  persisting({
    msgs <- testthat::capture_messages(
      verbosely(fit <- estimate_mrqap(FORM, g, times = 5)))
    msgs <- paste(msgs, collapse = "")
    expect_match(msgs, "2 persistent controls")
    expect_match(msgs, "permute")
    expect_equal(fit$permute, "outcome")
    expect_s3_class(fit$control, "mrqap_control")
    expect_equal(fit$control$seed, 7)
    expect_match(capture.output(print(fit)), "seed = 7", all = FALSE)
    # The record fits the same model again without the options.
    options(infernet.control = NULL)
    again <- estimate_mrqap(FORM, g, times = 5, control = fit$control)
    expect_equal(again$lower, fit$lower)
    expect_equal(again$permute, "outcome")
  }, value = list(seed = 7, permute = "outcome"))
})

test_that("the tests and the diffusions use and record a persistent seed", {
  g <- qap_net_gaussian(n = 10)
  persisting({
    a <- test_permutation(g, netrics::net_by_density, times = 5)
    b <- test_permutation(g, netrics::net_by_density, times = 5)
    expect_identical(a$testdist, b$testdist)
    expect_equal(a$control$seed, 11)
    expect_match(capture.output(print(a)), "seed = 11", all = FALSE)
    d <- play_diffusions(manynet::ison_adolescents, times = 3)
    expect_equal(attr(d, "control")$seed, 11)
  }, value = list(seed = 11))
})
