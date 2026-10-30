# The controls of an MRQAP ------------------------------------------------------
#
# `control_mrqap()` is the one place a control is named, defaulted, and
# checked. `estimate_mrqap()` passes its `control` list through it, so a list
# and the result of `control_mrqap()` are read the same way. A name that is no
# control is read against three tables: the names infernet itself used before
# (`.control_renames()`), the options of other models that an MRQAP has no use
# for (`.control_foreign()`), and otherwise it aborts with the nearest name.
# Renaming a control is then a row in `.control_renames()`.
#
# `persist = TRUE` writes the given controls to `options(infernet.control)`,
# and each later `control_mrqap()` fills the controls that it is not given from
# there. The attribute `source` says where each control came from, so that a
# print and `.report_persisted()` can say so, and a result records the
# controls whole, so that it can be fitted again without the option.

#' Controls for an MRQAP
#'
#' @description
#'   `control_mrqap()` sets the controls of [estimate_mrqap()], checks each
#'   one, and returns them as an object of class `mrqap_control`.
#'   `estimate_mrqap()` passes its `control` argument through this function,
#'   so `control = list(family = "poisson")` and
#'   `control = control_mrqap(family = "poisson")` fit the same model.
#'   Calling `control_mrqap()` first shows the full set of controls, and finds
#'   a mistake before a long run starts.
#'   The tests, such as [test_permutation()], read `ncores`, `strategy`, and
#'   `seed` from the same object, so one set of controls serves a model and
#'   a test alike.
#'
#'   A control that is not named keeps its default.
#'   A name that infernet used before is converted to its current name, with a
#'   warning.
#'   An option of another model that has no meaning in an MRQAP, such as
#'   goldfish's `max_iterations`, is dropped, with one warning that names each.
#'   The result of goldfish's `set_algorithm_newton()` or `set_preprocessing()`
#'   (or `set_estimation_opt()` before goldfish 2.0.0) can be given whole.
#'   Any other name aborts, and the nearest control is suggested.
#' @param permute What the null distribution permutes.
#'   `"predictor"` (the default) residualises each main predictor against the
#'   others and permutes that residual, which is Dekker et al.'s (2007) double
#'   semi-partialling.
#'   `"outcome"` permutes the outcome and leaves the predictors alone.
#'   With one predictor there is nothing to residualise against, so
#'   `"predictor"` reduces to `"outcome"`, and the model says so.
#' @param family The family of the model.
#'   `"auto"` (the default) reads it from the outcome: `"binomial"` for a binary
#'   outcome, otherwise `"gaussian"`, and says which.
#'   The others are `"gaussian"`, `"binomial"`, `"poisson"`, `"negbin"`
#'   (with `{MASS}`), and `"zip"`, a zero-inflated Poisson (with `{pscl}`).
#' @param directed Logical, whether a tie from i to j differs from one from
#'   j to i. `NULL` (the default) reads it from the network, and says so.
#' @param loops Logical, whether loops, the ties from a node to itself, enter
#'   the model. `NULL` (the default) includes them only where the network has
#'   loops.
#' @param seed A number to set the random seed with before the permutations,
#'   so that a fit can be reproduced. `NULL` (the default) leaves the seed as it
#'   is. The same seed gives the same result on any `strategy`.
#' @param groups A vector with a group for each node, so that each node is
#'   permuted only among the nodes of its group.
#'   On a two-mode network, its length says which mode it groups.
#'   `NULL` (the default) permutes all nodes together.
#' @param ncores A number of cores to run the permutations on.
#'   More than one runs them on that many `"multisession"` workers, and one runs
#'   them sequentially, whatever `strategy` says.
#'   `NULL` (the default) leaves the choice to `strategy`.
#'   A parallel run helps only where each permutation is costly: a family
#'   other than `"gaussian"`, random intercepts, or a cognitive social
#'   structure, on a network of more than about 50 nodes, with 1,000 or more
#'   `times`. Elsewhere the start of the workers costs more than it saves:
#'   a gaussian model solves its design once, so each permutation is cheap.
#'   Give `ncores` rather than `strategy = "multisession"`, which starts a
#'   worker on every core.
#' @param strategy The `{future}` plan that the permutations run on where
#'   `ncores` is `NULL`: `"sequential"`, `"multisession"`, `"multicore"`, or
#'   `"cluster"`. The plan that was set before is restored after the fit.
#'   `NULL` (the default) uses the plan that was set with `future::plan()`,
#'   which is sequential unless it was changed. Set a plan once in a session,
#'   with `future::plan(future::multisession, workers = 2)`, and its workers
#'   start once, not once in each call.
#' @param use_robust_errors Logical, whether to use HC3 robust standard errors
#'   for the observed model's t values. `FALSE` by default.
#' @param random_intercept_nets,random_intercept_sender,random_intercept_receiver,random_intercept_reporter
#'   Logical, whether to fit an `{lme4}` random intercept for each network of a
#'   list, each sender, each receiver, or each reporter of a cognitive social
#'   structure. `FALSE` by default. In the formula, `(1 | network)`,
#'   `(1 | sender)`, `(1 | receiver)`, and `(1 | reporter)` set the same
#'   controls.
#' @param missing What to do with an unobserved dyad, that is, an `NA` in the
#'   outcome or in a predictor.
#'   `"available"` (the default) drops the dyad, and permutes nodes only among
#'   nodes with the same unobserved cells, so that every permutation is fitted on
#'   the same dyads as the observed model.
#'   `"fail"` stops instead, and names how many dyads are unobserved on each
#'   variable. See the section on missing data in [estimate_mrqap()].
#' @param less_mem Logical, whether to leave the observed model object out of
#'   the result, to save memory on a large network. `FALSE` by default.
#' @param persist Logical, whether the controls given in this call become the
#'   defaults of the session. See the section on persistent controls.
#'   `FALSE` by default, which changes nothing. Given as `persist = FALSE`,
#'   it clears the persistent controls.
#' @param ... Controls by another name: a name that infernet used before, or an
#'   option of another model. See the description.
#' @param x An object of class `mrqap_control`.
#' @section Persistent controls:
#'   `control_mrqap(ncores = 2, missing = "fail", persist = TRUE)` writes the
#'   controls that it is given to `options(infernet.control)`, after it checks
#'   them. Each later call of [estimate_mrqap()], [test_random()],
#'   [test_configuration()], [test_permutation()], [test_gof()],
#'   [test_distributions()], [test_parameter()], [test_time()], and
#'   [play_diffusions()] then uses them as defaults, and says so with
#'   `options(snet_verbosity = "verbose")`.
#'
#'   - A control given in a call takes priority over a persistent one.
#'     Give `strategy = NULL` in a call to use the plan of `future::plan()`
#'     where a `strategy` or `ncores` persists.
#'   - A second call with `persist = TRUE` adds to the persistent controls, and
#'     replaces a control that it gives again. A control given as `NULL`, such
#'     as `control_mrqap(ncores = NULL, persist = TRUE)`, is removed.
#'   - `control_mrqap(persist = FALSE)` clears all of them.
#'   - `control_mrqap()` shows the controls in force, and marks each one that
#'     comes from the persistent controls with `(option)`.
#'   - `control_mrqap()` reads the persistent controls when it is called, so
#'     its result keeps the same controls if the options change later.
#'
#'   A `seed` can persist. Each call then sets the same seed, and so draws the
#'   same random numbers, which reproduces each result. The print of a result
#'   gives the seed that it used.
#'
#'   Every result records the controls that it used, as `control` (an
#'   attribute of a `diffs_model`). Give that record as `control` to fit the
#'   same model again without the options of the session.
#' @return An object of class `mrqap_control`: a list with one element for
#'   each control. Its attribute `source` says whether each control was
#'   `"given"`, comes from an `"option"`, or keeps its `"default"`.
#'   `print()` returns `x`, invisibly.
#' @references
#'   Dekker, David, David Krackhardt, and Tom A.B. Snijders. 2007.
#'   "Sensitivity of MRQAP tests to collinearity and autocorrelation conditions."
#'   _Psychometrika_ 72(4): 563-581.
#'   \doi{10.1007/s11336-007-9016-1}
#' @family models
#' @examples
#' control_mrqap(family = "poisson", seed = 1)
#' # A name that infernet used before is converted, with a warning
#' control_mrqap(nullhyp = "qapy")
#' # Controls that later calls use as defaults
#' control_mrqap(missing = "fail", seed = 1, persist = TRUE)
#' control_mrqap()
#' control_mrqap(persist = FALSE)
#' @export
control_mrqap <- function(permute = c("predictor", "outcome"),
                          family = c("auto", "gaussian", "binomial", "poisson",
                                     "negbin", "zip"),
                          directed = NULL,
                          loops = NULL,
                          seed = NULL,
                          groups = NULL,
                          ncores = NULL,
                          strategy = NULL,
                          use_robust_errors = FALSE,
                          random_intercept_nets = FALSE,
                          random_intercept_sender = FALSE,
                          random_intercept_receiver = FALSE,
                          random_intercept_reporter = FALSE,
                          missing = "available",
                          less_mem = FALSE,
                          persist = FALSE,
                          ...) {
  .check_flag(persist, "persist")
  # `persist = FALSE`, given, clears the persistent controls before they are
  # read, where the default leaves them alone.
  if (!missing(persist) && !persist) options(infernet.control = NULL)
  ctrl <- list(permute = permute, family = family, directed = directed,
               loops = loops, seed = seed, groups = groups, ncores = ncores,
               strategy = strategy, use_robust_errors = use_robust_errors,
               random_intercept_nets = random_intercept_nets,
               random_intercept_sender = random_intercept_sender,
               random_intercept_receiver = random_intercept_receiver,
               random_intercept_reporter = random_intercept_reporter,
               missing = missing, less_mem = less_mem)
  # A control named in the call, even as NULL, is given, and takes priority
  # over a persistent one.
  given <- intersect(names(match.call())[-1], names(ctrl))
  # A NULL in `list()` keeps its name, as a NULL default must.
  others <- list(...)
  if (length(others) > 0) {
    if (is.null(names(others)) || any(!nzchar(names(others)))) {
      manynet::snet_abort("Every entry of {.arg control} must be named.")
    }
    renamed <- .convert_controls(others, names(ctrl))
    clash <- intersect(names(renamed), given)
    if (length(clash) > 0) {
      manynet::snet_abort(
        "{.val {clash}} is given both by its current name and by an old one.")
    }
    ctrl[names(renamed)] <- renamed
    given <- c(given, names(renamed))
  }
  ctrl <- .check_controls(ctrl)
  stored <- .persisted_controls()
  from_option <- setdiff(names(stored), given)
  for (nm in from_option) ctrl[nm] <- list(stored[[nm]])
  if (persist) {
    for (nm in given) stored[nm] <- list(ctrl[[nm]])
    stored <- stored[!vapply(stored, is.null, logical(1))]
    options(infernet.control = if (length(stored) > 0) stored)
  }
  source <- stats::setNames(rep("default", length(ctrl)), names(ctrl))
  source[given] <- "given"
  source[from_option] <- "option"
  structure(ctrl, class = "mrqap_control", source = source)
}

# Each check of a control, so that the controls of a call and the persistent
# ones are checked alike. A vector default, such as `permute`'s, becomes its
# first value.
#' @keywords internal
#' @noRd
.check_controls <- function(ctrl) {
  ctrl$permute <- .check_choice(ctrl$permute, .permute_schemes(), "permute")
  ctrl$family  <- .check_choice(ctrl$family, .control_families(), "family")
  # `$<-` with NULL would drop the control, so it is set as a list.
  ctrl["strategy"] <- list(.check_strategy(ctrl$strategy))
  .check_missing_control(ctrl$missing)
  for (nm in c("directed", "loops")) .check_flag(ctrl[[nm]], nm, null = TRUE)
  for (nm in c("use_robust_errors", "less_mem",
               grep("^random_intercept_", names(ctrl), value = TRUE))) {
    .check_flag(ctrl[[nm]], nm)
  }
  if (!is.null(ctrl$seed) &&
      !(is.numeric(ctrl$seed) && length(ctrl$seed) == 1 && !is.na(ctrl$seed))) {
    manynet::snet_abort("{.arg seed} must be one number, or {.code NULL}.")
  }
  .check_ncores(ctrl$ncores)
  if (!is.null(ctrl$groups) && !is.atomic(ctrl$groups)) {
    manynet::snet_abort("{.arg groups} must be a vector with a group for each node.")
  }
  ctrl
}

# ---- persistent controls -----------------------------------------------------

# The controls in `options(infernet.control)`, checked, since a user can set
# the option by hand as well as with `persist = TRUE`.
#' @keywords internal
#' @noRd
.persisted_controls <- function() {
  stored <- getOption("infernet.control")
  if (is.null(stored)) return(list())
  how <- "Set it with {.code control_mrqap(..., persist = TRUE)}, or clear it with {.code control_mrqap(persist = FALSE)}."
  if (!is.list(stored) ||
      (length(stored) > 0 && (is.null(names(stored)) || any(!nzchar(names(stored)))))) {
    manynet::snet_abort(
      c("{.code options(infernet.control)} must be a named list of controls.",
        i = how))
  }
  defaults <- .control_defaults()
  unknown <- setdiff(names(stored), names(defaults))
  if (length(unknown) > 0) {
    manynet::snet_abort(
      c("{.code options(infernet.control)} holds {.val {unknown}}, which {?is not a control/are not controls}.",
        i = how))
  }
  for (nm in names(stored)) defaults[nm] <- list(stored[[nm]])
  checked <- tryCatch(.check_controls(defaults), error = function(e) {
    problem <- gsub("\\}", "}}", gsub("\\{", "{{", conditionMessage(e)))
    manynet::snet_abort(
      c("{.code options(infernet.control)} holds a control that cannot be used.",
        i = problem, i = how))
  })
  checked[names(stored)]
}

# The default of each control, from the signature of `control_mrqap()`.
#' @keywords internal
#' @noRd
.control_defaults <- function() {
  args <- formals(control_mrqap)
  args <- args[setdiff(names(args), c("persist", "..."))]
  .check_controls(lapply(args, eval))
}

# One line that names the persistent controls that a call uses.
#' @keywords internal
#' @noRd
.report_persisted <- function(ctrl) {
  source <- attr(ctrl, "source")
  from_option <- names(source)[source == "option"]
  if (length(from_option) == 0) return(invisible(ctrl))
  shown <- vapply(from_option, function(nm) {
    paste(nm, "=", .control_label(nm, ctrl[[nm]]))
  }, character(1))
  manynet::snet_info(
    "Using {length(shown)} persistent control{?s}: {.code {shown}}.")
  invisible(ctrl)
}

# A short value of a control, for a message or a print.
#' @keywords internal
#' @noRd
.control_label <- function(nm, v) {
  if (is.null(v)) return("NULL")
  if (nm == "groups") {
    return(paste(length(v), "nodes in", length(unique(v)), "groups"))
  }
  if (is.function(v)) return("a plan function")
  if (is.character(v)) return(paste0("\"", v, "\"", collapse = ", "))
  paste(format(v), collapse = ", ")
}

# A list given as `control` is read by `control_mrqap()`, with the same checks
# and conversions, and the persistent controls fill what it does not name. An
# unnamed entry cannot be matched to a control, so it aborts rather than being
# matched by position. The result of `control_mrqap()` already holds the
# persistent controls of the time it was made, so it is used as it is.
#' @keywords internal
#' @noRd
.resolve_control <- function(control = list()) {
  .report_persisted(.read_control(control))
}

#' @keywords internal
#' @noRd
.read_control <- function(control) {
  if (inherits(control, "mrqap_control")) return(control)
  if (inherits(control, c("estimation_opt.goldfish", "goldfishAlgo"))) {
    manynet::snet_info("Reading {.arg control} as goldfish's estimation options.")
  }
  if (inherits(control, "goldfishPrepCtrl")) {
    manynet::snet_info("Reading {.arg control} as goldfish's preprocessing options.")
  }
  control <- unclass(control)
  if (!is.list(control)) {
    manynet::snet_abort(
      "{.arg control} must be a list, or the result of {.fn control_mrqap}.")
  }
  if (length(control) > 0 &&
      (is.null(names(control)) || any(!nzchar(names(control))))) {
    manynet::snet_abort("Every entry of {.arg control} must be named.")
  }
  do.call(control_mrqap, control)
}

#' @keywords internal
#' @noRd
.control_families <- function() {
  c("auto", "gaussian", "binomial", "poisson", "negbin", "zip")
}

# ---- other names -------------------------------------------------------------

# The names infernet used before, and what each is called now. `values`
# converts an old value to the new one, where the values changed too.
#' @keywords internal
#' @noRd
.control_renames <- function() {
  permute <- c(qap = "predictor", qapspp = "predictor", qapy = "outcome")
  list(
    nullhyp = list(to = "permute", values = permute, since = "0.2.0"),
    method  = list(to = "permute", values = permute, since = "0.2.0"),
    mode    = list(to = "directed", since = "0.2.0",
                   values = list(digraph = TRUE, directed = TRUE,
                                 graph = FALSE, undirected = FALSE)),
    random_intercept_perceiver = list(to = "random_intercept_reporter",
                                      since = "0.3.0"),
    diag    = list(to = "loops", since = "0.5.0")
  )
}

# Options of other models, and of earlier versions, that an MRQAP has no use
# for. Each is dropped with the reason. goldfish's come from
# `set_algorithm_newton()` (`set_estimation_opt()` before goldfish 2.0.0), which
# steers an iterative estimation that an MRQAP does not do, and from
# `set_preprocessing()`, which prepares goldfish's events.
#' @keywords internal
#' @noRd
.control_foreign <- function() {
  iterative <- "steers goldfish's iterative estimation, which an MRQAP does not do"
  c(max_iterations          = iterative,
    convergence_criterion   = iterative,
    initial_damping         = iterative,
    damping_increase_factor = iterative,
    damping_decrease_factor = iterative,
    initial_parameters      = iterative,
    engine                  = "chooses goldfish's estimation engine",
    return_interval_loglik  = "is goldfish's, and an MRQAP has no log-likelihood for each event",
    return_probabilities    = "is goldfish's, and an MRQAP has no choice probabilities",
    fixed_parameters        = "fixes goldfish's parameters, and offsets are not yet supported here",
    score_tol               = iterative,
    step_tol                = iterative,
    optimizer               = iterative,
    backend                 = "chooses goldfish's estimation backend",
    offset_coef             = "sets goldfish's offsets, and offsets are not yet supported here",
    diagnostics             = "chooses goldfish's diagnostics for each event, which an MRQAP does not have",
    return_event_scores     = "is goldfish's, and an MRQAP has no score for each event",
    start_time              = "times goldfish's events, which an MRQAP does not have",
    end_time                = "times goldfish's events, which an MRQAP does not have",
    opportunities_list      = "sets goldfish's choice sets, which an MRQAP does not have",
    impute                  = "is goldfish's imputation; see `missing` for an MRQAP's",
    db                      = "stores goldfish's preprocessed statistics",
    db_table                = "stores goldfish's preprocessed statistics",
    use_gpu                 = "chose a GPU path that is no longer in infernet (see stocnet/infernet#11)")
}

# Renames each old name, warns about it, and drops each option of another
# model with one warning. Any other name aborts with the nearest control.
#' @keywords internal
#' @noRd
.convert_controls <- function(others, known) {
  renames <- .control_renames()
  foreign <- .control_foreign()
  unknown <- setdiff(names(others), c(names(renames), names(foreign)))
  if (length(unknown) > 0) {
    candidates <- c(known, names(renames))
    near <- vapply(unknown, function(u) {
      d <- utils::adist(u, candidates, ignore.case = TRUE)[1, ]
      if (min(d) <= max(2, nchar(u) %/% 3)) candidates[which.min(d)] else NA_character_
    }, character(1))
    msg <- c("{.arg control} does not take {.val {unknown}}.")
    if (any(!is.na(near))) {
      msg <- c(msg, i = "Did you mean {.val {unname(near[!is.na(near)])}}?")
    }
    msg <- c(msg, i = "Available controls: {.val {known}}.",
             i = "See {.fn control_mrqap} for what each does.")
    manynet::snet_abort(msg)
  }
  dropped <- intersect(names(others), names(foreign))
  if (length(dropped) > 0) {
    reasons <- paste0("{.code ", dropped, "} ", foreign[dropped])
    manynet::snet_warn(
      c("Dropped {length(dropped)} control{?s} that an MRQAP does not use.",
        stats::setNames(reasons, rep("*", length(reasons)))))
  }
  out <- list()
  for (old in intersect(names(others), names(renames))) {
    r <- renames[[old]]
    value <- others[[old]]
    if (!is.null(r$values) && is.character(value) && length(value) == 1 &&
        value %in% names(r$values)) {
      new_value <- r$values[[value]]
      manynet::snet_warn(
        c("{.code {old} = \"{value}\"} is now {.code {r$to} = {deparse(new_value)}}.",
          i = "The control was renamed in infernet {r$since}."))
      value <- new_value
    } else {
      manynet::snet_warn(
        c("{.code {old}} is now called {.code {r$to}}.",
          i = "The control was renamed in infernet {r$since}."))
    }
    if (r$to %in% names(out)) {
      manynet::snet_abort("{.val {r$to}} is given by more than one old name.")
    }
    out[[r$to]] <- value
  }
  out
}

# ---- checks ------------------------------------------------------------------

# One of a set of values. A vector default, as in the signature, is its first
# value.
#' @keywords internal
#' @noRd
.check_choice <- function(value, choices, what) {
  if (identical(value, choices)) return(choices[1])
  if (!(is.character(value) && length(value) == 1 && value %in% choices)) {
    manynet::snet_abort("{.arg {what}} must be one of {.val {choices}}.")
  }
  value
}

#' @keywords internal
#' @noRd
.check_flag <- function(value, what, null = FALSE) {
  if (null && is.null(value)) return(invisible(value))
  if (!(is.logical(value) && length(value) == 1 && !is.na(value))) {
    if (null) {
      manynet::snet_abort("{.arg {what}} must be {.val {TRUE}}, {.val {FALSE}}, or {.code NULL}.")
    }
    manynet::snet_abort("{.arg {what}} must be {.val {TRUE}} or {.val {FALSE}}.")
  }
  invisible(value)
}

#' @keywords internal
#' @noRd
.check_ncores <- function(value) {
  if (!is.null(value) &&
      !(is.numeric(value) && length(value) == 1 &&
        !is.na(value) && value >= 1 && value %% 1 == 0)) {
    manynet::snet_abort("{.arg ncores} must be a whole number of at least 1, or {.code NULL}.")
  }
  invisible(value)
}

# A `{future}` plan by name. A plan function, such as `future::multisession`,
# is also what `future::plan()` takes.
#' @keywords internal
#' @noRd
.check_strategy <- function(value) {
  plans <- c("sequential", "multisession", "multicore", "cluster")
  if (is.null(value) || is.function(value)) return(value)
  if (!(is.character(value) && length(value) == 1 && value %in% plans)) {
    manynet::snet_abort("{.arg strategy} must be one of {.val {plans}}, or {.code NULL}.")
  }
  value
}
