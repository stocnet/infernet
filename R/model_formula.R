# Formula -> specification -> matrices -----------------------------------------
#
# A formula is read in two steps. `.specify()` parses it into a specification:
# the outcome, and for each term the function it calls and the arguments it
# names. `.evaluate_spec()` then builds, against one network, one matrix for
# each column of the model.
#
# Each term is one entry in `.formula_terms()`: its arguments, what it reads,
# what data it accepts, how to build it, and its nearest ergm term. A new term
# is a new entry rather than a new branch, and the list of term names lives in
# one place. The design follows `register_effect()` on goldfish's
# `refactor/rename-functions` branch.
#
# Before this, the formula was split as a string on its parentheses and commas.
# That could not read a named argument such as `node = "ego"`, multiplied only
# a term of exactly two parts, and could not give several columns from one
# term. The specification stays internal until the stocnet maintainers agree a
# shared one; see "Formula conventions" in CONTRIBUTING.md.

# ---- the term registry -------------------------------------------------------

# `formals` is the signature a term is matched against, as for a function.
# `kind` says what its first argument names: a node attribute, a layer, or a
# global variable. `twomode` is FALSE where a two-mode network has no meaning
# for the term, and `hint` then says what to use instead. `outcome` says what
# happens where a layer term names the outcome: "refuse", or "warn" where the
# term is defined on the outcome but is then not permuted with it. `bare` is
# TRUE where the term can be named on its own, as goldfish's endogenous effects
# are: `~ recip` is then `recip()` of the outcome. `panel` is TRUE where the
# term reads the outcome's previous wave on a longitudinal network, as goldfish
# and RSiena read the state of the network before each change. It then fits
# there without a warning, and `build` reads `ctx$previous`.
#
# The rest say what each model makes of the term, so that one formula can be
# read by several (see "Formula conventions" in CONTRIBUTING.md). `mrqap` is
# what `estimate_mrqap()` does with it: "fits", "warns" where it fits but the
# user should know something, "drops" where the rest of the model can still be
# fitted without it, or "refuses". A term that warns or is dropped has a
# `reason`. These are the statuses of `.statuses()`, which `specify_*()`
# records for each term. The other models are named for the model rather than
# the package: `dynam` (a DyNAM or REM in goldfish), `saom` (RSiena), and
# `ergm` name the equivalent term in each, or the nearest, and are NA where
# there is none. `to_ergm` converts the term for `specify_ergm()`,
# in model_formula_ergm.R: to the named ergm term, or to `edgecov()` of the
# matrix `build` makes. A term with an `ergm` name and no `to_ergm` is not yet
# converted.
#' @keywords internal
#' @noRd
.formula_terms <- function() {
  c(list(
    ego = list(
      formals = alist(x = , levels = NULL), kind = "attribute", build = .build_ego,
      mrqap = "fits", dynam = "ego", saom = "egoX",
      ergm = "nodeocov, or nodeofactor for a categorical attribute",
      to_ergm = .ergm_ego),
    alter = list(
      formals = alist(x = , levels = NULL), kind = "attribute", build = .build_alter,
      mrqap = "fits", dynam = "alter", saom = "altX",
      ergm = "nodeicov, or nodeifactor for a categorical attribute",
      to_ergm = .ergm_alter),
    same = list(
      formals = alist(x = , diff = FALSE, levels = NULL), kind = "attribute",
      build = .build_same, mrqap = "fits",
      dynam = "same, without diff = TRUE", saom = "sameX, without diff = TRUE",
      ergm = "nodematch, with the same diff",
      to_ergm = .ergm_same),
    diff = list(
      formals = alist(x = , transformer_fn = abs), kind = "attribute",
      build = .build_diff, mrqap = "fits", dynam = "diff",
      saom = "absDiffX, or diffX with transformer_fn = identity",
      ergm = "absdiff, or diff with transformer_fn = identity",
      to_ergm = .ergm_diff),
    dist = list(
      formals = alist(x = ), kind = "attribute", build = .build_dist,
      mrqap = "warns", reason = "is a deprecated alias of diff()",
      dynam = "diff", saom = "absDiffX", ergm = "absdiff",
      to_ergm = .ergm_dist),
    sim = list(
      formals = alist(x = ), kind = "attribute", build = .build_sim,
      mrqap = "fits", dynam = "sim, as a negative difference",
      saom = "simX, centred", ergm = "edgecov",
      to_ergm = .ergm_built),
    match = list(
      formals = alist(x = ), kind = "attribute", build = .build_match,
      twomode = FALSE, hint = "Use {.fn mix} for a two-mode network.",
      mrqap = "warns", reason = "is a deprecated alias of same(x, diff = TRUE)",
      dynam = NA_character_, saom = NA_character_,
      ergm = "nodematch with diff = TRUE",
      to_ergm = .ergm_match),
    mix = list(
      formals = alist(x = , levels = NULL), kind = "attribute", build = .build_mix,
      mrqap = "fits", dynam = NA_character_, saom = NA_character_,
      ergm = "nodemix",
      to_ergm = .ergm_mix),
    tie = list(
      formals = alist(layer = , weighted = FALSE), kind = "layer",
      build = .build_tie, outcome = "refuse",
      mrqap = "fits", dynam = "tie", saom = "X", ergm = "edgecov",
      to_ergm = .ergm_built),
    recip = list(
      formals = alist(layer = ), kind = "layer", build = .build_recip,
      twomode = FALSE, bare = TRUE, panel = TRUE,
      hint = "A two-mode tie has no reverse tie of the same shape.",
      outcome = "warn", mrqap = "warns", reason = .outcome_reason(),
      dynam = "recip", saom = "recip",
      ergm = "mutual, or edgecov of the transpose",
      to_ergm = .ergm_recip),
    exchange = list(
      formals = alist(layer = ), kind = "layer", build = .build_recip,
      twomode = FALSE,
      hint = "A two-mode tie has no reverse tie of the same shape.",
      outcome = "warn", mrqap = "fits", dynam = "recip with a network",
      saom = NA_character_, ergm = "edgecov of the transpose",
      to_ergm = .ergm_recip),
    indeg = list(
      formals = alist(layer = , node = "alter", weighted = FALSE, type = NULL),
      kind = "layer", build = .build_indeg, outcome = "refuse",
      mrqap = "fits", dynam = "indeg", saom = NA_character_,
      ergm = "edgecov, or nodeicov of the degree",
      to_ergm = .ergm_built),
    outdeg = list(
      formals = alist(layer = , node = "ego", weighted = FALSE, type = NULL),
      kind = "layer", build = .build_outdeg, outcome = "refuse",
      mrqap = "fits", dynam = "outdeg", saom = NA_character_,
      ergm = "edgecov, or nodeocov of the degree",
      to_ergm = .ergm_built),
    tertius = list(
      formals = alist(layer = , x = , summarizer_fn = "mean"),
      kind = "tertius", build = .build_tertius, outcome = "warn",
      mrqap = "fits", dynam = "tertius, with ego", saom = NA_character_,
      ergm = "edgecov",
      to_ergm = .ergm_built),
    tertius_diff = list(
      formals = alist(layer = , x = , summarizer_fn = "mean"),
      kind = "tertius", build = .build_tertius_diff, outcome = "warn",
      mrqap = "fits", dynam = "tertius_diff, with ego", saom = NA_character_,
      ergm = "edgecov",
      to_ergm = .ergm_built),
    global = list(
      formals = alist(x = ), kind = "global", build = .build_global,
      mrqap = "fits", dynam = "global", saom = NA_character_,
      ergm = NA_character_),
    # A cognitive social structure's terms, named for its reporter. No other
    # model has a reporter, so `css` marks them for the other models' readers.
    reporter = list(
      formals = alist(x = , levels = NULL), kind = "attribute",
      build = .build_reporter, css = TRUE,
      mrqap = "fits", dynam = NA_character_, saom = NA_character_,
      ergm = NA_character_),
    reporter_ego = list(
      formals = alist(), kind = "reporter", build = .build_reporter_ego,
      bare = TRUE, css = TRUE,
      mrqap = "fits", dynam = NA_character_, saom = NA_character_,
      ergm = NA_character_),
    reporter_alter = list(
      formals = alist(), kind = "reporter", build = .build_reporter_alter,
      bare = TRUE, css = TRUE,
      mrqap = "fits", dynam = NA_character_, saom = NA_character_,
      ergm = NA_character_),
    reporter_same = list(
      formals = alist(x = , node = "ego"), kind = "attribute",
      build = .build_reporter_same, css = TRUE, twomode = FALSE,
      hint = "Use {.fn reporter} with {.fn ego} or {.fn alter} on the attribute instead.",
      mrqap = "fits", dynam = NA_character_, saom = NA_character_,
      ergm = NA_character_)
  ),
  .dynam_terms())
}

# goldfish's effects that `estimate_mrqap()` cannot build, so that a formula
# written for goldfish still fits here without them (stocnet/goldfish#122 names
# them). Most read the dependent events by default: a statistic of the
# outcome's own ties, which permuting the outcome would not permute with it.
# The others need a pair of layers, a group, or a time order that a single
# MRQAP does not have. `specify_mrqap()` drops them, and every argument
# is accepted, since none is read.
#' @keywords internal
#' @noRd
.dynam_terms <- function() {
  endogenous <- function(saom, ergm, to_ergm = NULL, kind = "endogenous") {
    list(formals = alist(layer = , ... = ), kind = kind, bare = TRUE,
         mrqap = "drops", reason = "depends on the outcome's own ties",
         saom = saom, ergm = ergm, to_ergm = to_ergm)
  }
  mixed <- function(ergm = NA_character_) {
    list(formals = alist(layer = , ... = ), kind = "mixed",
         mrqap = "drops", reason = "closes a path through two layers, which is not yet built here",
         saom = NA_character_, ergm = ergm)
  }
  groups <- function() {
    list(formals = alist(... = ), kind = "groups", bare = TRUE,
         mrqap = "drops", reason = "is defined on the groups of a DyNAM-i",
         saom = NA_character_, ergm = NA_character_)
  }
  # Fitted on the previous wave of a longitudinal network, and dropped from a
  # cross-sectional one, where they could only read the outcome's own ties.
  lagged <- function(entry, build) {
    entry$panel <- TRUE
    entry$build <- build
    entry$twomode <- FALSE
    entry$hint <- "A two-mode network has no two-paths or repeated ties of this kind."
    entry$reason <- "needs a longitudinal network, where it reads the outcome's previous wave"
    entry
  }
  terms <- list(
    inertia         = lagged(endogenous(NA_character_, NA_character_),
                             .build_inertia),
    trans           = lagged(endogenous("transTrip", "ttriple, or dgwesp(type = \"OTP\")",
                                        .ergm_endogenous("ttriple", "triangle")),
                             .build_trans),
    cycle           = endogenous("cycle3", "ctriple, or dgwesp(type = \"ITP\")",
                                 .ergm_endogenous("ctriple")),
    common_sender   = endogenous("gwespBF, nearest", "dgwesp(type = \"ISP\")"),
    common_receiver = endogenous("gwespFB, nearest", "dgwesp(type = \"OSP\")"),
    four            = endogenous("cycle4", "cycle(4)",
                                 .ergm_endogenous("cycle(4)", "cycle(4)")),
    triangle        = endogenous(NA_character_, "triangle",
                                 .ergm_endogenous("triangle", "triangle")),
    node_trans      = endogenous(NA_character_, NA_character_),
    degree          = endogenous(NA_character_, "degree"),
    mixed_trans           = mixed(),
    mixed_cycle           = mixed(),
    mixed_common_sender   = mixed(),
    mixed_common_receiver = mixed(),
    ego_alter_interaction = list(
      formals = alist(x = , ... = ), kind = "interaction",
      mrqap = "drops", reason = "is written {.code ego(x):alter(y)} here",
      saom = "egoXaltX", ergm = "nodeocov(x):nodeicov(y)"),
    alterdeg  = groups(), alterpop = groups(), egodeg = groups(),
    egopop    = groups(), dyad     = groups(), dyadXdiff = groups(),
    dyadXego  = groups(), size     = groups(), sizeXdiff = groups(),
    sizeXego  = groups(), intercept = groups()
  )
  for (nm in names(terms)) terms[[nm]]$dynam <- nm
  terms
}

# ---- parsing -----------------------------------------------------------------

#' @keywords internal
#' @noRd
.specify <- function(formula) {
  if (!inherits(formula, "formula") || length(formula) != 3) {
    manynet::snet_abort(
      c("The formula needs an outcome on its left-hand side.",
        i = "For example: {.code weight ~ ego(age)}."))
  }
  split <- .split_fixed(formula)
  formula <- split$formula
  tt <- stats::terms(formula)
  labels <- attr(tt, "term.labels")
  # A bar means what it means in {lme4}: `(1 | sender)` is a random intercept
  # for each sender (stocnet/infernet#8). It names one of the indices the engine
  # builds, and sets the same switch as its control.
  bars <- grepl("|", labels, fixed = TRUE)
  random <- vapply(labels[bars], .specify_random, character(1), USE.NAMES = FALSE)
  both <- intersect(random, split$fixed)
  if (length(both) > 0) {
    manynet::snet_abort(
      c("{.val {both}} has both a fixed effect and a random intercept.",
        i = "Keep {.code | {both}} for a fixed effect, or {.code (1 | {both})} for a random intercept."))
  }
  labels <- labels[!bars]
  if (!is.null(attr(tt, "offset"))) {
    manynet::snet_abort("{.fn offset} is not yet supported in the formula.")
  }
  if (length(labels) == 0) {
    manynet::snet_abort("The formula needs at least one predictor on its right-hand side.")
  }
  terms <- lapply(labels, function(lab) {
    operands <- .flatten_interaction(str2lang(lab))
    list(label = lab, operands = lapply(operands, .specify_operand))
  })
  .check_same_once(terms)
  env <- environment(formula)
  if (is.null(env)) env <- globalenv()
  structure(list(outcome = paste(deparse(formula[[2]]), collapse = ""),
                 terms   = terms,
                 random  = unique(random),
                 fixed   = split$fixed,
                 env     = env),
            class = "infernet_spec")
}

# The columns of `same(x, diff = TRUE)` add up to `same(x)`, so a model with
# both cannot be fitted: `lm()` gives one coefficient as NA, and double
# semi-partialling has nothing left to residualise. `match()` is the old name
# of the first.
#' @keywords internal
#' @noRd
.check_same_once <- function(terms) {
  ops <- unlist(lapply(Filter(function(t) length(t$operands) == 1, terms),
                       `[[`, "operands"), recursive = FALSE)
  same <- Filter(function(op) op$fn %in% c("same", "match"), ops)
  vars <- vapply(same, `[[`, character(1), "name")
  twice <- unique(vars[duplicated(vars)])
  if (length(twice) > 0) {
    manynet::snet_abort(
      c("{.fn same} names {.val {twice}} more than once.",
        i = "The columns of {.code same(x, diff = TRUE)} add up to {.code same(x)}, so the two cannot be fitted together.",
        i = "Keep one of them."))
  }
  invisible(TRUE)
}

# A bar outside parentheses means what it means in {fixest}: what follows it
# is absorbed as a fixed effect, so `y ~ x | sender + receiver` has a fixed
# effect for each sender and each receiver (stocnet/infernet#8). A bar inside
# parentheses is an {lme4} random intercept, and `(1 | sender)` is never read
# here, since R parses the parentheses as one term. Returns the formula without
# the bar, and the groups after it.
#' @keywords internal
#' @noRd
.split_fixed <- function(formula) {
  rhs <- formula[[3]]
  if (!(is.call(rhs) && identical(rhs[[1]], as.name("|")))) {
    return(list(formula = formula, fixed = character(0)))
  }
  main <- rhs[[2]]
  if (is.call(main) && identical(main[[1]], as.name("|"))) {
    manynet::snet_abort(
      c("The formula has more than one bar outside parentheses.",
        i = "Write the fixed effects after one bar: {.code y ~ x | sender + receiver}."))
  }
  groups <- .flatten_plus(rhs[[3]])
  names_ok <- vapply(groups, is.name, logical(1))
  groups <- vapply(groups, .deparsed, character(1))
  allowed <- names(.random_groups())
  wrong <- groups[!names_ok | !groups %in% allowed]
  if (length(wrong) > 0) {
    manynet::snet_abort(
      c("{.code | {wrong}} names {.val {wrong}}, which has no fixed effect.",
        i = "A fixed effect can be for {.val {allowed}}."))
  }
  out <- formula
  out[[3]] <- main
  list(formula = out, fixed = unique(groups))
}

# `a + b + c` arrives as `(a + b) + c`, so its parts are the leaves of a tree of
# `+` calls.
#' @keywords internal
#' @noRd
.flatten_plus <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], as.name("+")) && length(expr) == 3) {
    return(c(.flatten_plus(expr[[2]]), .flatten_plus(expr[[3]])))
  }
  list(expr)
}

# A random-effect term: an intercept, `1`, for one of the indices the engine
# builds. Its name in `control` is `random_intercept_` and the index, where the
# network index is called `nets`.
#' @keywords internal
#' @noRd
.specify_random <- function(label) {
  expr <- str2lang(label)
  groups <- names(.random_groups())
  if (!identical(expr[[2]], 1) && !identical(expr[[2]], 1L)) {
    manynet::snet_abort(
      c("{.code ({label})} is a random slope, and only random intercepts are supported.",
        i = "Write {.code (1 | {deparse(expr[[3]])})}."))
  }
  group <- if (is.name(expr[[3]])) as.character(expr[[3]]) else deparse(expr[[3]])
  if (!group %in% groups) {
    manynet::snet_abort(
      c("{.code ({label})} names {.val {group}}, which has no random intercept.",
        i = "A random intercept can be for {.val {groups}}."))
  }
  group
}

#' @keywords internal
#' @noRd
.random_groups <- function() {
  c(sender   = "random_intercept_sender",
    receiver = "random_intercept_receiver",
    reporter = "random_intercept_reporter",
    network  = "random_intercept_nets")
}

# `a:b:c` arrives as `(a:b):c`, so the operands of an interaction of any order
# are the leaves of a tree of `:` calls.
#' @keywords internal
#' @noRd
.flatten_interaction <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], as.name(":"))) {
    return(c(.flatten_interaction(expr[[2]]), .flatten_interaction(expr[[3]])))
  }
  list(expr)
}

# One operand of a term: a bare name, which is a dyadic covariate, or a call to
# a registered term, matched against that term's signature so that arguments
# can be given by position or by name.
#' @keywords internal
#' @noRd
.specify_operand <- function(expr) {
  if (is.name(expr)) {
    return(list(fn = NA_character_, name = as.character(expr),
                args = list(), supplied = character(0)))
  }
  if (!is.call(expr) || !is.name(expr[[1]])) {
    manynet::snet_abort("Cannot read {.code {deparse(expr)}} as a formula term.")
  }
  fn <- as.character(expr[[1]])
  registry <- .formula_terms()
  entry <- registry[[fn]]
  if (is.null(entry)) {
    # A name that no model knows is probably a typing error, so it aborts and
    # suggests the nearest name, where the terms of other models are dropped.
    fits <- names(registry)[vapply(registry, function(e) e$mrqap != "drops", logical(1))]
    d <- utils::adist(fn, names(registry), ignore.case = TRUE)[1, ]
    msg <- c("{.fn {fn}} is not a formula term.")
    if (min(d) <= max(2, nchar(fn) %/% 3)) {
      near <- names(registry)[which.min(d)]
      msg <- c(msg, i = "Did you mean {.fn {near}}?")
    }
    manynet::snet_abort(
      c(msg, i = "Available terms: {.fn {fits}}, or a layer named on its own."))
  }
  signature <- as.function(c(entry$formals, list(NULL)))
  matched <- tryCatch(
    match.call(signature, expr),
    error = function(e) {
      manynet::snet_abort(
        c("Cannot read {.code {deparse(expr)}}.",
          i = "{.fn {fn}} takes {.arg {names(entry$formals)}}."))
    })
  supplied <- names(as.list(matched))[-1]
  args <- lapply(names(entry$formals), function(nm) {
    if (nm %in% supplied) return(matched[[nm]])
    # A required argument has the empty symbol as its default, which cannot be
    # held in a variable without R reading it as a missing argument.
    if (identical(entry$formals[[nm]], quote(expr = ))) NULL else entry$formals[[nm]]
  })
  names(args) <- names(entry$formals)
  # A term with no arguments, such as `reporter_ego()`, names nothing.
  name <- if (length(args) > 0) .arg_name(args[[1]]) else NA_character_
  list(fn = fn, name = name, args = args, supplied = supplied)
}

# The name an argument gives: a bare name or a string, as `ego(age)` and
# `ego("age")` both name the attribute "age".
#' @keywords internal
#' @noRd
.arg_name <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.name(x)) return(as.character(x))
  if (is.character(x)) return(x[1])
  paste(deparse(x), collapse = "")
}

# A function-valued argument, such as `transformer_fn = identity`, evaluated
# where the formula was written.
#' @keywords internal
#' @noRd
.arg_fun <- function(x, env, what) {
  f <- tryCatch(if (is.character(x)) get(x, envir = env, mode = "function")
                else eval(x, env),
                error = function(e) NULL)
  if (!is.function(f)) {
    manynet::snet_abort("{.arg {what}} must be a function, such as {.code abs}.")
  }
  f
}

#' @keywords internal
#' @noRd
.arg_flag <- function(x, env, what) {
  value <- tryCatch(eval(x, env), error = function(e) NA)
  if (!is.logical(value) || length(value) != 1 || is.na(value)) {
    manynet::snet_abort("{.arg {what}} must be {.val {TRUE}} or {.val {FALSE}}.")
  }
  value
}

# ---- evaluation --------------------------------------------------------------

#' @keywords internal
#' @noRd
.evaluate_spec <- function(spec, .data, advise = TRUE, pooled = FALSE,
                           previous = NULL) {
  ctx <- .spec_context(spec, .data, pooled, previous)
  out <- ctx$out
  DV <- ctx$DV
  css <- ctx$css
  spec <- .resolve_bare_terms(spec, ctx)
  .check_formula_vars(spec, ctx)
  if (advise) specificationAdvice(spec, ctx$net)

  lift <- if (css) function(x) .lift_to_css(x, DV) else identity
  built <- lapply(spec$terms, function(term) {
    ops <- lapply(term$operands, .build_operand, ctx = ctx)
    cols <- Reduce(.interact, lapply(ops, function(o) lapply(o, lift)))
    parts <- if (length(ops) == 1) attr(ops[[1]], "parts")
      else .interaction_parts(names(cols), lapply(ops, names))
    list(cols = cols, parts = parts)
  })
  IVs <- unlist(lapply(built, `[[`, "cols"), recursive = FALSE)
  terms <- do.call(rbind, lapply(built, `[[`, "parts"))

  rhs <- Reduce(function(a, b) call("+", a, b), lapply(names(IVs), as.name))
  out_formula <- eval(call("~", as.name(out$label), rhs))
  # A fixed tie travels with the outcome to the engine, which leaves it out.
  if (!css) attr(DV, "fixed") <- .fixed_ties(out$net)
  mydata <- c(list(DV), IVs)
  names(mydata)[1] <- out$label
  list(mydata = mydata, formula = out_formula, css = css, terms = terms)
}

# The outcome's ties that `{manynet}` marks as fixed by design, in its `fixed`
# tie column: ties that the process did not choose, such as an assignment, so
# they are not observations of it. NULL where none is fixed.
#' @keywords internal
#' @noRd
.fixed_ties <- function(net) {
  if (!("fixed" %in% manynet::net_tie_attributes(net))) return(NULL)
  fixed <- manynet::as_matrix(manynet::filter_ties(net, fixed %in% TRUE))
  fixed <- !is.na(fixed) & fixed != 0
  if (!any(fixed)) return(NULL)
  fixed
}

# ---- the term table ----------------------------------------------------------

# A column's name is a label that cannot be parsed back into its parts, since an
# attribute or a category can hold underscores. So each operand also records its
# parts, one row per column: the term it comes from (`effect`), and its layer,
# attribute, category, and reference category. The fit keeps these as
# `fit$terms`, and `tidy()` joins them to the coefficients. `term` is the
# column's name, as in `{broom}`; goldfish's `coef_layout()` calls it `name`.
#' @keywords internal
#' @noRd
.term_table <- function(term, effect = NA_character_, layer = NA_character_,
                        attribute = NA_character_, category = NA_character_,
                        reference = NA_character_, operands = NULL) {
  n <- length(term)
  out <- data.frame(term = term,
                    effect = rep_len(as.character(effect), n),
                    layer = rep_len(as.character(layer), n),
                    attribute = rep_len(as.character(attribute), n),
                    category = rep_len(as.character(category), n),
                    reference = rep_len(as.character(reference), n),
                    stringsAsFactors = FALSE)
  out$operands <- if (is.null(operands)) rep(list(character(0)), n) else operands
  out
}

# The parts of one operand's columns, from the operand and its term's kind. A
# builder that knows more, such as the category of each column, leaves it in
# the "parts" attribute of what it returns, as arguments of `.term_table()`.
#' @keywords internal
#' @noRd
.operand_parts <- function(out, op, kind) {
  parts <- list(term = names(out),
                effect = if (is.na(op$fn)) "tie" else op$fn,
                layer = if (kind %in% c("layer", "covariate")) op$name else NA_character_,
                attribute = if (kind %in% c("attribute", "global")) op$name else NA_character_)
  given <- attr(out, "parts")
  parts[names(given)] <- given
  do.call(.term_table, parts)
}

# An interaction names its operands: the columns it multiplies, in the order
# that `.interact()` makes them.
#' @keywords internal
#' @noRd
.interaction_parts <- function(cols, operand_names) {
  operands <- Reduce(function(a, b) {
    unlist(lapply(a, function(i) lapply(b, function(j) c(i, j))), recursive = FALSE)
  }, lapply(operand_names, as.list))
  .term_table(cols, effect = "interaction", operands = operands)
}

# The term table of a fit: one row for each coefficient, in their order.
#' @keywords internal
#' @noRd
.coef_terms <- function(coefs, terms) {
  terms <- rbind(.term_table("(Intercept)", effect = "intercept"), terms)
  out <- terms[match(coefs, terms$term), , drop = FALSE]
  out$term <- coefs
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

# What every builder reads: the network, its outcome, and its shape. `out` is
# the resolved outcome, with its label and its network.
#' @keywords internal
#' @noRd
.spec_context <- function(spec, .data, pooled = FALSE, previous = NULL) {
  net <- manynet::as_tidygraph(.data)
  layers <- .layer_names(net)
  out <- .resolve_outcome(spec$outcome, net, layers)
  twomode <- manynet::is_twomode(net)
  # The outcome in the previous wave of a panel, which `inertia`, `recip`, and
  # `trans` read. It is NULL for a cross-sectional network.
  if (!is.null(previous)) {
    previous <- manynet::as_tidygraph(previous)
    previous <- manynet::as_matrix(
      if (!is.na(out$layer)) manynet::to_uniplex(previous, layer = out$layer) else previous)
  }
  list(
    previous      = previous,
    data          = .data,
    net           = net,
    layers        = layers,
    tie_names     = c(manynet::net_tie_attributes(net), layers),
    outcome_layer = .outcome_ties(out$layer, spec$outcome, net),
    out           = out,
    DV            = out$DV,
    css           = length(dim(out$DV)) == 3,
    twomode       = twomode,
    node_type     = if (twomode) manynet::node_attribute(net, "type") else NULL,
    directed      = isTRUE(all(manynet::is_directed(out$net))),
    env           = spec$env,
    pooled        = pooled
  )
}

# One warning names every term a `specify_*()` function dropped, and why.
#' @keywords internal
#' @noRd
.warn_dropped <- function(dropped, model = "an MRQAP") {
  if (is.null(dropped) || nrow(dropped) == 0) return(invisible())
  manynet::snet_warn(
    c("Dropped {nrow(dropped)} term{?s} that {model} cannot fit:",
      .dropped_bullets(dropped, "*")))
}

# One bullet for each dropped term: its label, then why. The label is the
# user's own text, so its braces are escaped before it reaches `{cli}`.
#' @keywords internal
#' @noRd
.dropped_bullets <- function(dropped, bullet) {
  label <- gsub("}", "}}", gsub("{", "{{", dropped$label, fixed = TRUE), fixed = TRUE)
  stats::setNames(paste0("{.code ", label, "} ", dropped$reason),
                  rep(bullet, nrow(dropped)))
}

# The outcome is the layer the formula names, or for `.` the layer the network
# declares as its focal one, and otherwise the whole network. A layer of a
# cognitive social structure stays one, so its matrix is an array of sender,
# receiver, and reporter, and every predictor is lifted to match it.
#' @keywords internal
#' @noRd
.resolve_outcome <- function(lhs, net, layers) {
  layer <- NA_character_
  label <- if (lhs == ".") "ties" else lhs
  if (lhs %in% layers) {
    layer <- lhs
  } else if (lhs == ".") {
    focal <- tryCatch(manynet::as_infolist(net)$focal, error = function(e) NULL)
    if (length(focal) == 1 && !is.na(focal) && nzchar(focal)) {
      if (focal %in% layers) {
        layer <- focal
        manynet::snet_info("Taking the outcome from the focal layer {.val {focal}}.")
      } else if (focal %in% manynet::net_node_attributes(net)) {
        manynet::snet_abort(
          c("The network's focal variable {.val {focal}} is a node attribute.",
            i = "A network regression needs ties as its outcome, so name a layer on the left-hand side."))
      }
      label <- focal
    }
  }
  outcome <- if (!is.na(layer)) manynet::to_uniplex(net, layer = layer) else net
  DV <- manynet::as_matrix(outcome)
  if (is.list(DV)) {
    manynet::snet_abort(
      c("The network has several layers, so the formula must name one as the outcome.",
        i = "Available layers: {.val {layers}}.",
        i = "For example: {.code {layers[1]} ~ ego(...)}.",
        i = "Or declare it once as the network's {.code info$focal}, and write {.code . ~ ego(...)}."))
  }
  list(DV = DV, layer = layer, label = label, net = outcome)
}

# The name under which a term would read the outcome's own ties: its layer, a
# tie attribute the left-hand side names, or the weight of a network whose ties
# are the outcome as a whole.
#' @keywords internal
#' @noRd
.outcome_ties <- function(layer, lhs, net) {
  if (!is.na(layer)) return(layer)
  tie_attrs <- manynet::net_tie_attributes(net)
  if (lhs %in% tie_attrs) return(lhs)
  if (lhs == "." && "weight" %in% tie_attrs) return("weight")
  NA_character_
}

# A bare name is a layer or a tie attribute, as before. Where none has that name
# and a term can be named on its own, it is that term with its defaults, so
# `~ recip` reads as it does in goldfish. A name that is both aborts.
#' @keywords internal
#' @noRd
.resolve_bare_terms <- function(spec, ctx) {
  registry <- .formula_terms()
  bare <- names(registry)[vapply(registry, function(e) isTRUE(e$bare), logical(1))]
  spec$terms <- lapply(spec$terms, function(term) {
    term$operands <- lapply(term$operands, function(op) {
      if (!is.na(op$fn) || !(op$name %in% bare)) return(op)
      if (op$name %in% ctx$tie_names) {
        manynet::snet_abort(
          c("{.val {op$name}} is both a layer of the network and a formula term.",
            i = "Write {.code tie({op$name})} for the layer, or {.code {op$name}()} for the term."))
      }
      .specify_operand(call(op$name))
    })
    term
  })
  spec
}

# Builds one operand: a registered term, or a layer named on its own.
#' @keywords internal
#' @noRd
.build_operand <- function(op, ctx) {
  if (is.na(op$fn)) {
    out <- .build_covariate(op, ctx)
    attr(out, "parts") <- .operand_parts(out, op, "covariate")
    return(out)
  }
  entry <- .formula_terms()[[op$fn]]
  if (is.null(entry$build)) {
    manynet::snet_abort(
      c("{.fn {op$fn}} cannot be fitted here.",
        i = "Read the formula with {.fn specify_mrqap} first, which drops it."))
  }
  if (ctx$twomode && identical(entry$twomode, FALSE)) {
    manynet::snet_abort(
      c("{.fn {op$fn}} is not defined for a two-mode network.",
        i = entry$hint))
  }
  out <- entry$build(op, ctx)
  attr(out, "parts") <- .operand_parts(out, op, entry$kind)
  out
}

# The product of two operands, each a named list of matrices. An operand that
# gives several columns, such as `same(x, diff = TRUE)`, gives one product for each.
#' @keywords internal
#' @noRd
.interact <- function(a, b) {
  out <- list()
  for (i in names(a)) for (j in names(b)) {
    out[[paste(i, j, sep = ":")]] <- a[[i]] * b[[j]]
  }
  out
}

# ---- builders: node attributes -------------------------------------------------

# The sender and the receiver of a tie come from one nodeset in a one-mode
# network and from two in a two-mode one, so a dyadic term reads the attribute
# once per mode rather than once per network.
#' @keywords internal
#' @noRd
.node_values <- function(ctx, x, side = c("ego", "alter", "reporter")) {
  side <- match.arg(side)
  v <- manynet::node_attribute(ctx$net, x)
  if (!ctx$twomode) return(v)
  # A two-mode CSS has a report from every node of both modes, the row nodes'
  # first, so a reporter's value is read in that order.
  switch(side, ego = v[!ctx$node_type], alter = v[ctx$node_type],
         reporter = c(v[!ctx$node_type], v[ctx$node_type]))
}

#' @keywords internal
#' @noRd
.side_matrices <- function(ctx, x) {
  list(rows = matrix(.node_values(ctx, x, "ego"), nrow(ctx$DV), ncol(ctx$DV)),
       cols = matrix(.node_values(ctx, x, "alter"), nrow(ctx$DV), ncol(ctx$DV),
                     byrow = TRUE))
}

#' @keywords internal
#' @noRd
.named <- function(m, label) stats::setNames(list(m), label)

# A column name: the term, then its layer, attribute, and category, joined by
# underscores. A syntactic name needs no backticks in the internal formula or in
# `coef(fit)[...]`. A category such as "Social Science" can hold characters a
# name cannot, so each run of them becomes one underscore. The name is a label,
# and is not parsed back into its parts.
#' @keywords internal
#' @noRd
.col_name <- function(...) {
  parts <- as.character(c(...))
  paste(gsub("[^[:alnum:]._]+", "_", parts), collapse = "_")
}

#' @keywords internal
#' @noRd
.refuse_categorical <- function(ctx, op) {
  if (is.character(manynet::node_attribute(ctx$net, op$name))) {
    manynet::snet_abort(
      c("{.fn {op$fn}} is undefined for a categorical attribute.",
        i = "Try {.fn same} instead."))
  }
}

.build_ego <- function(op, ctx) {
  .build_side(op, ctx, "ego", .side_matrices(ctx, op$name)$rows)
}

.build_alter <- function(op, ctx) {
  .build_side(op, ctx, "alter", .side_matrices(ctx, op$name)$cols)
}

# A categorical attribute is dummy-coded here, one numeric column per category
# less the first as the reference, as `migraph` did. Left as a character matrix,
# the fitter would dummy-code it instead, but then double semi-partialling has
# no numeric column to residualise and every level's p-value is NA. A column's
# name holds only its own category, so the reference is named in a message and
# in the term table. `levels =` chooses the categories that get a column.
#' @keywords internal
#' @noRd
.build_side <- function(op, ctx, side, m) {
  label <- .col_name(side, op$name)
  v <- .node_values(ctx, op$name, side)
  if (!is.character(v) && !is.factor(v)) {
    if (!is.null(op$args$levels)) {
      manynet::snet_abort(
        c("{.arg levels} needs a categorical attribute, and {.val {op$name}} is numeric.",
          i = "Convert it with {.fn as.character} to read its values as categories."))
    }
    return(.named(m, label))
  }
  cats <- .categories(v)
  if (length(cats) < 2) {
    manynet::snet_abort(
      "{.fn {side}} needs at least two categories of {.val {op$name}}.")
  }
  chosen <- .with_reference(.select_levels(op, cats, ctx$env), cats)
  .warn_many_levels(op, length(cats), length(chosen$columns))
  manynet::snet_info(
    "{.fn {side}} leaves out {.val {chosen$reference}} of {.val {op$name}} as the reference.")
  out <- lapply(chosen$columns, function(k) (m == k) * 1)
  out <- stats::setNames(out, vapply(chosen$columns, function(k) .col_name(label, k), ""))
  structure(out, parts = list(category = chosen$columns,
                              reference = paste(chosen$reference, collapse = ", ")))
}

# The categories of an attribute, in the order of their columns: a factor's
# levels, or else the sorted values, as ergm orders them. Positions in
# `levels =` count in this order.
#' @keywords internal
#' @noRd
.categories <- function(v) {
  if (is.factor(v)) return(levels(droplevels(v)))
  as.character(sort(unique(stats::na.omit(v))))
}

# `levels =`, as in ergm: the categories, or for `mix()` the pairs, that get a
# column, in the order given. A name is a category as the data hold it, or a
# pair as the term table writes it. A number is a position in `choices`, and
# negative positions leave those out, so `levels = -1` is every one but the
# first. NULL keeps the term's default.
#' @keywords internal
#' @noRd
.select_levels <- function(op, choices, env, what = c("category", "categories")) {
  if (is.null(op$args$levels)) return(NULL)
  value <- tryCatch(eval(op$args$levels, env), error = function(e) e)
  if (is.null(value)) return(NULL)
  if (is.factor(value)) value <- as.character(value)
  n <- length(choices)
  if (is.character(value) && length(value) > 0 && !anyNA(value)) {
    if (what[1] == "pair") value <- gsub("\\s*(->|--)\\s*", " \\1 ", trimws(value))
    unknown <- setdiff(value, choices)
    if (length(unknown) > 0) {
      manynet::snet_abort(
        c("{.code {op$fn}({op$name})} has no {what[1]} {.val {unknown}}.",
          i = "The {what[2]} are {.val {choices}}."))
    }
    chosen <- unique(value)
  } else if (is.numeric(value) && length(value) > 0 && !anyNA(value) &&
             all(value == round(value))) {
    if (any(value == 0) || any(abs(value) > n) || (any(value > 0) && any(value < 0))) {
      manynet::snet_abort(
        c("{.arg levels} must be positions from 1 to {n}, or from -1 to -{n} to leave {what[2]} out.",
          i = "The {what[2]} of {.code {op$fn}({op$name})} are {.val {choices}}."))
    }
    chosen <- choices[unique(value)]
  } else {
    manynet::snet_abort(
      c("{.arg levels} must be names of {what[2]}, or their positions.",
        i = "The {what[2]} of {.code {op$fn}({op$name})} are {.val {choices}}.",
        i = "For example: {.code levels = {deparse(choices[1])}} or {.code levels = -1}."))
  }
  if (length(chosen) == 0) {
    manynet::snet_abort("{.arg levels} leaves out every {what[1]} of {.code {op$fn}({op$name})}.")
  }
  chosen
}

# The columns and the reference of a term whose columns cover every dyad. By
# default the first category is the reference. With `levels =`, the categories
# not chosen are the reference together; where every one is chosen, the first
# chosen is.
#' @keywords internal
#' @noRd
.with_reference <- function(chosen, choices) {
  if (is.null(chosen)) return(list(columns = choices[-1], reference = choices[1]))
  if (all(choices %in% chosen)) return(list(columns = chosen[-1], reference = chosen[1]))
  list(columns = chosen, reference = setdiff(choices, chosen))
}

# Each column is a separate set of permutations under `permute = "predictor"`,
# so many categories are slow and hard to read. goldfish warns at about ten.
#' @keywords internal
#' @noRd
.warn_many_levels <- function(op, categories, columns) {
  if (!is.null(op$args$levels) || categories <= 10) return(invisible())
  manynet::snet_warn(
    c("{.code {op$fn}({op$name})} reads {categories} categories, and gives {columns} columns.",
      i = "Choose the categories that get a column with {.arg levels}, e.g. {.code {op$fn}({op$name}, levels = c(...))}."))
}

.build_same <- function(op, ctx) {
  if (.same_by_category(op, ctx$env)) {
    if (ctx$twomode) {
      manynet::snet_abort(
        c("{.code same({op$name}, diff = TRUE)} is not defined for a two-mode network.",
          i = "Use {.fn mix} for a two-mode network."))
    }
    return(.same_each(op, ctx, "same"))
  }
  .data <- ctx$data
  DV <- ctx$DV
  attrib <- manynet::node_attribute(ctx$net, op$name)
  if (ctx$css && ctx$twomode) {
    manynet::snet_abort(
      c("{.fn same} is not yet available for a two-mode cognitive social structure.",
        i = "Use {.fn ego} and {.fn alter} on the attribute instead."))
  }
  if (manynet::is_twomode(.data)) {
    # A two-mode network has no tie between two nodes of one mode, so sameness
    # is the share of a node's ties that go to nodes like the other one. With
    # `levels =`, only those of the chosen categories count.
    # Each node's share leaves the node out. The matrix is built once and the
    # node's row or column dropped from it: deleting the node from the network
    # and rebuilding the matrix, once for each node, took 98% of a fit on the
    # 448 by 12489 network of #4.
    whole <- manynet::as_matrix(.data)
    if (all(is.na(attrib[!manynet::node_is_mode(.data)]))) {
      attrib <- attrib[manynet::node_is_mode(.data)]
      chosen <- .select_levels(op, .categories(attrib), ctx$env)
      if (is.null(chosen)) chosen <- .categories(attrib)
      out <- vapply(seq_along(attrib), function(x) {
        held_out <- whole[, -x, drop = FALSE]
        rowSums(held_out * matrix((attrib[-x] == attrib[x] & attrib[x] %in% chosen) * 1,
                                  nrow(DV), ncol(DV) - 1, byrow = TRUE)) /
          rowSums(held_out)
      }, FUN.VALUE = numeric(nrow(DV)))
    } else {
      attrib <- attrib[!manynet::node_is_mode(.data)]
      chosen <- .select_levels(op, .categories(attrib), ctx$env)
      if (is.null(chosen)) chosen <- .categories(attrib)
      out <- t(vapply(seq_along(attrib), function(x) {
        held_out <- whole[-x, , drop = FALSE]
        colSums(held_out * matrix((attrib[-x] == attrib[x] & attrib[x] %in% chosen) * 1,
                                  nrow(DV) - 1, ncol(DV))) /
          colSums(held_out)
      }, FUN.VALUE = numeric(ncol(DV))))
    }
    out[is.nan(out)] <- 0
  } else {
    sides <- .side_matrices(ctx, op$name)
    out <- (sides$rows == sides$cols) * 1
    chosen <- .select_levels(op, .categories(.node_values(ctx, op$name, "ego")), ctx$env)
    if (!is.null(chosen)) out[!sides$rows %in% chosen] <- 0
  }
  .named(out, .col_name("same", op$name))
}

# goldfish's `diff()`: the transformed difference between the sender's and the
# receiver's values, absolute by default and signed with `identity`.
.build_diff <- function(op, ctx) {
  .refuse_categorical(ctx, op)
  f <- .arg_fun(op$args$transformer_fn, ctx$env, "transformer_fn")
  sides <- .side_matrices(ctx, op$name)
  .named(f(sides$rows - sides$cols), .col_name("diff", op$name))
}

# The old name, kept so that existing scripts run, with its old column name.
.build_dist <- function(op, ctx) {
  if (!isTRUE(.infernet_state$dist_warned)) {
    .infernet_state$dist_warned <- TRUE
    manynet::snet_warn(
      c("{.fn dist} is deprecated, and is now called {.fn diff}.",
        i = "{.code diff({op$name})} gives the same absolute difference."))
  }
  .refuse_categorical(ctx, op)
  sides <- .side_matrices(ctx, op$name)
  .named(abs(sides$rows - sides$cols), .col_name("dist", op$name))
}

.build_sim <- function(op, ctx) {
  .refuse_categorical(ctx, op)
  sides <- .side_matrices(ctx, op$name)
  d <- abs(sides$rows - sides$cols)
  denom <- max(d, na.rm = TRUE)
  if (!is.finite(denom) || denom == 0) denom <- 1
  .named(abs(1 - d / denom), .col_name("sim", op$name))
}

# `diff = TRUE`, as in ergm's `nodematch()`: one column per category rather
# than one for any shared category.
#' @keywords internal
#' @noRd
.same_by_category <- function(op, env) {
  if (identical(op$fn, "match")) return(TRUE)
  !is.null(op$args$diff) && .arg_flag(op$args$diff, env, "diff")
}

# Differential homophily: one column per category, 1 where the sender and the
# receiver both hold it. Numeric columns rather than a factor, so that double
# semi-partialling can residualise each one. No category is left out: a dyad
# of two different categories is in none of the columns, and is the reference.
# `levels =` chooses the categories that get a column; a dyad that shares
# another category is then in the reference too.
.same_each <- function(op, ctx, term) {
  sides <- .side_matrices(ctx, op$name)
  all_cats <- .categories(.node_values(ctx, op$name, "ego"))
  cats <- .select_levels(op, all_cats, ctx$env)
  if (is.null(cats)) cats <- all_cats
  .warn_many_levels(op, length(all_cats), length(cats))
  out <- lapply(cats, function(k) (sides$rows == k & sides$cols == k) * 1)
  out <- stats::setNames(out, vapply(cats, function(k) .col_name(term, op$name, k), ""))
  structure(out, parts = list(category = cats))
}

# The old name, kept so that existing scripts run, with its old column names.
.build_match <- function(op, ctx) {
  if (!isTRUE(.infernet_state$match_warned)) {
    .infernet_state$match_warned <- TRUE
    manynet::snet_warn(
      c("{.fn match} is deprecated, and is now {.code same(x, diff = TRUE)}.",
        i = "{.code same({op$name}, diff = TRUE)} gives the same columns."))
  }
  .same_each(op, ctx, "match")
}

# Mixing: one column per pair of a sender's and a receiver's category, less the
# first pair as the reference. An undirected tie has no sender, so its pairs are
# unordered; a two-mode network pairs the categories of its two modes. A
# column names the sender's category, then the receiver's. The pairs are in
# ergm's order for `nodemix()`, the sender's category changing fastest, so that
# a position in `levels =` is a position in ergm's `levels2 =`.
.build_mix <- function(op, ctx) {
  sides <- .side_matrices(ctx, op$name)
  ego_cats <- .categories(.node_values(ctx, op$name, "ego"))
  alter_cats <- .categories(.node_values(ctx, op$name, "alter"))
  ordered <- ctx$directed || ctx$twomode
  pairs <- expand.grid(ego = ego_cats, alter = alter_cats, stringsAsFactors = FALSE)
  if (!ordered) {
    pairs <- pairs[match(pairs$ego, ego_cats) <= match(pairs$alter, alter_cats), ,
                   drop = FALSE]
  }
  if (nrow(pairs) < 2) {
    manynet::snet_abort("{.fn mix} needs at least two categories of {.val {op$name}}.")
  }
  # In the term table, a pair is written as a tie is in DOT: `->` from the
  # sender's category to the receiver's, or `--` where the pair is unordered.
  pair <- paste(pairs$ego, pairs$alter, sep = if (ordered) " -> " else " -- ")
  chosen <- .with_reference(.select_levels(op, pair, ctx$env, c("pair", "pairs")), pair)
  .warn_many_levels(op, length(union(ego_cats, alter_cats)), length(chosen$columns))
  manynet::snet_info("{.fn mix} leaves out {.val {chosen$reference}} as the reference.")
  keep <- match(chosen$columns, pair)
  out <- lapply(keep, function(p) {
    a <- pairs$ego[p]
    b <- pairs$alter[p]
    cell <- sides$rows == a & sides$cols == b
    if (!ordered) cell <- cell | (sides$rows == b & sides$cols == a)
    cell * 1
  })
  labels <- vapply(keep, function(p) {
    .col_name("mix", op$name, pairs$ego[p], pairs$alter[p])
  }, "")
  # The positions of the columns among all pairs are ergm's `levels2 =`.
  structure(stats::setNames(out, labels),
            parts = list(category = chosen$columns,
                         reference = paste(chosen$reference, collapse = ", ")),
            positions = keep)
}

# ---- builders: layers ----------------------------------------------------------

# A layer, or a tie attribute, as a matrix. A layer of a cognitive social
# structure gives each reporter's own report of it, as an array.
#' @keywords internal
#' @noRd
.layer_matrix <- function(ctx, layer) {
  manynet::as_matrix(manynet::to_uniplex(ctx$net, layer = layer))
}

# Applies `f` to a matrix, or to each report of a CSS array.
#' @keywords internal
#' @noRd
.per_report <- function(m, f) {
  if (length(dim(m)) != 3) return(f(m))
  out <- array(NA_real_, dim = dim(m), dimnames = dimnames(m))
  for (k in seq_len(dim(m)[3])) out[, , k] <- f(m[, , k])
  out
}

# A layer term built from the outcome predicts the outcome's ties from those
# same ties, which permuting the outcome does not undo.
#' @keywords internal
#' @noRd
.outcome_layer_check <- function(op, ctx, layer) {
  if (is.na(ctx$outcome_layer) || !identical(layer, ctx$outcome_layer)) return(invisible())
  entry <- .formula_terms()[[op$fn]]
  if (identical(entry$outcome, "refuse")) {
    manynet::snet_abort(
      c("{.fn {op$fn}} cannot read the outcome layer {.val {layer}}.",
        i = "It would predict the outcome's ties from those same ties. Name another layer."))
  }
  .warn_outcome_ties(op$fn, .outcome_example(op$fn))
}

#' @keywords internal
#' @noRd
.outcome_reason <- function() {
  "reads the outcome's own ties, which permuting the outcome does not permute with it"
}

# What a model does with a term: the registry's `mrqap` values, and what a
# `specify_*()` function records where it rewrites a term ("adapted") or keeps a
# term that the model defines differently ("interpreted").
#' @keywords internal
#' @noRd
.statuses <- function() {
  c("fits", "adapted", "interpreted", "warns", "drops", "refuses")
}

#' @keywords internal
#' @noRd
.outcome_example <- function(fn) {
  if (identical(.formula_terms()[[fn]]$kind, "tertius")) paste0(fn, "(friends, age)")
  else paste0(fn, "(friends)")
}

#' @keywords internal
#' @noRd
.warn_outcome_ties <- function(fn, example = paste0(fn, "(friends, age)")) {
  manynet::snet_warn(
    c("{.fn {fn}} reads the outcome's own ties.",
      i = "Permuting the outcome does not permute this predictor with it, so its test is not exact.",
      i = "Name another layer as its first argument, for example {.code {example}}."))
}

.build_covariate <- function(op, ctx) {
  if (!(op$name %in% ctx$tie_names)) {
    manynet::snet_abort("Predictor {.val {op$name}} not found in the network.")
  }
  .named(.layer_matrix(ctx, op$name), op$name)
}

# goldfish's `tie()`: 1 where the layer has a positive tie, or its value with
# `weighted = TRUE`. A layer named on its own gives the values as stored.
.build_tie <- function(op, ctx) {
  .outcome_layer_check(op, ctx, op$name)
  weighted <- .arg_flag(op$args$weighted, ctx$env, "weighted")
  m <- .layer_matrix(ctx, op$name)
  if (!weighted) m <- (m > 0) * 1
  .named(m, .col_name("tie", op$name))
}

# The reverse tie: the transpose of a layer. `exchange(layer)` names the layer,
# as goldfish proposes for its exogenous twin (stocnet/goldfish#122), and
# `recip()` without one reads the outcome, as goldfish's endogenous `recip` does.
.build_recip <- function(op, ctx) {
  if (is.na(op$name)) {
    if (identical(op$fn, "exchange")) {
      manynet::snet_abort("{.fn exchange} needs a layer, for example {.code exchange(friends)}.")
    }
    label <- op$fn
    if (!is.null(ctx$previous)) {
      # On a panel, the reverse tie in the previous wave, as goldfish reads it.
      m <- ctx$previous
    } else {
      .warn_outcome_ties(op$fn, "exchange(friends)")
      m <- ctx$DV
    }
  } else {
    .outcome_layer_check(op, ctx, op$name)
    m <- .layer_matrix(ctx, op$name)
    label <- .col_name(op$fn, op$name)
  }
  out <- if (length(dim(m)) == 3) aperm(m, c(2, 1, 3)) else t(m)
  dimnames(out) <- dimnames(m)
  .named(out, label)
}

# The outcome's own tie in the previous wave of a panel, 0/1 as goldfish's
# `inertia` counts it. An unobserved tie stays NA.
#' @keywords internal
#' @noRd
.build_inertia <- function(op, ctx) {
  m <- .previous_wave(op, ctx)
  .named((m > 0) * 1, "inertia")
}

# The two-paths i -> k -> j in the previous wave of a panel, as goldfish's
# `trans` and RSiena's `transTrip` count them. A two-path through an unobserved
# tie is not counted, so the count is of observed two-paths.
#' @keywords internal
#' @noRd
.build_trans <- function(op, ctx) {
  m <- .previous_wave(op, ctx)
  a <- (m > 0) * 1
  a[is.na(a)] <- 0
  diag(a) <- 0
  out <- a %*% a
  diag(out) <- 0
  dimnames(out) <- dimnames(m)
  .named(out, "trans")
}

#' @keywords internal
#' @noRd
.previous_wave <- function(op, ctx) {
  if (is.null(ctx$previous)) {
    manynet::snet_abort(
      c("{.fn {op$fn}} needs a longitudinal network, where it reads the outcome's previous wave.",
        i = "Read the formula with {.fn specify_mrqap} first, which drops it from a cross-sectional network."))
  }
  if (!is.na(op$name) && !identical(op$name, ctx$outcome_layer)) {
    manynet::snet_abort(
      c("{.fn {op$fn}} reads the outcome's previous wave, not another layer.",
        i = "See stocnet/infernet#18 for a lagged layer."))
  }
  ctx$previous
}

.build_indeg <- function(op, ctx) .build_degree(op, ctx, "in")
.build_outdeg <- function(op, ctx) .build_degree(op, ctx, "out")

# A node's degree in another layer, placed on the sender's or the receiver's
# side of each dyad. It counts ties, as goldfish does, unless `weighted = TRUE`.
#' @keywords internal
#' @noRd
.build_degree <- function(op, ctx, direction) {
  .outcome_layer_check(op, ctx, op$name)
  node <- .degree_node(op)
  weighted <- .arg_flag(op$args$weighted, ctx$env, "weighted")
  # In a two-mode network only the column nodes receive, and only the row nodes
  # send, so the other side has no such degree to read.
  if (ctx$twomode && node == if (direction == "in") "ego" else "alter") {
    manynet::snet_abort(
      c("{.fn {op$fn}} with {.code node = \"{node}\"} is not defined for a two-mode network.",
        i = "Its {node}s never {if (direction == 'in') 'receive' else 'send'} a tie."))
  }
  m <- .layer_matrix(ctx, op$name)
  if (length(dim(m)) != 3 && anyNA(m)) {
    gaps <- sum(apply(m, if (direction == "in") 2 else 1, anyNA))
    manynet::snet_info(
      "{.fn {op$fn}} leaves out unobserved ties, which affects {gaps} node{?s} of {.val {op$name}}.")
  }
  degree_of <- function(s) {
    if (!ctx$twomode) diag(s) <- 0
    w <- if (weighted) s else (s > 0) * 1
    deg <- if (direction == "in") colSums(w, na.rm = TRUE) else rowSums(w, na.rm = TRUE)
    matrix(deg, nrow(s), ncol(s), byrow = node == "alter")
  }
  fn_label <- .col_name(if (node == "ego") "ego", op$fn, op$name)
  .named(.per_report(m, degree_of), fn_label)
}

# Whose degree a cell (i, j) holds: the sender's, `"ego"`, or the receiver's,
# `"alter"`. `outdeg()` reads the sender by default and `indeg()` the receiver.
# `type =`, the name before infernet 0.6.0 and still goldfish's, is kept as an
# alias, and warns once.
#' @keywords internal
#' @noRd
.degree_node <- function(op) {
  if ("type" %in% op$supplied) {
    if ("node" %in% op$supplied) {
      manynet::snet_abort(
        c("{.fn {op$fn}} takes {.arg node} or {.arg type}, not both.",
          i = "{.arg type} is the old name of {.arg node}."))
    }
    if (!isTRUE(.infernet_state$type_warned)) {
      .infernet_state$type_warned <- TRUE
      side <- .arg_name(op$args$type)
      manynet::snet_warn(
        c("{.arg type} of {.fn indeg} and {.fn outdeg} is deprecated, and is now called {.arg node}.",
          i = "{.code {op$fn}({op$name}, node = \"{side}\")} gives the same column."))
    }
    op$args$node <- op$args$type
  }
  node <- .arg_name(op$args$node)
  if (!node %in% c("alter", "ego")) {
    manynet::snet_abort("{.arg node} must be {.val alter} or {.val ego}, not {.val {node}}.")
  }
  node
}

# ---- builders: others' attributes ------------------------------------------------

# The layer and attribute of `tertius()` and `tertius_diff()`. The older
# spelling, `tertius(x)` or `tertius(x, "mean")`, names the attribute first and
# reads the outcome's own ties.
#' @keywords internal
#' @noRd
.tertius_args <- function(op, ctx) {
  a <- op$args
  layer <- .arg_name(a$layer)
  x <- .arg_name(a$x)
  summarizer <- a$summarizer_fn
  node_attrs <- manynet::net_node_attributes(ctx$net)
  if (!is.na(layer) && !(layer %in% ctx$tie_names) && layer %in% node_attrs) {
    if ("x" %in% op$supplied) summarizer <- a$x
    x <- layer
    layer <- NA_character_
  }
  fn <- .arg_name(summarizer)
  if (!fn %in% c("mean", "sum")) {
    manynet::snet_abort(
      "{.fn {op$fn}} takes {.val mean} or {.val sum}, not {.val {fn}}.")
  }
  if (is.character(manynet::node_attribute(ctx$net, x))) {
    manynet::snet_abort("{.fn {op$fn}} needs a numeric attribute, and {.val {x}} is categorical.")
  }
  if (is.na(layer)) {
    .warn_outcome_ties(op$fn)
    m <- ctx$DV
  } else {
    .outcome_layer_check(op, ctx, layer)
    m <- .layer_matrix(ctx, layer)
  }
  list(layer = layer, x = x, fn = fn, m = m,
       v = .node_values(ctx, x, "ego"))
}

# For each dyad (i, j), the attribute of j's other in-neighbours: the nodes
# k other than i with a tie to j. "mean" is their mean (Haunss and Hollway
# 2023); a j with no other in-neighbour takes the mean of the defined cells, as
# goldfish does. "sum" is the sum weighted by tie value, as before.
#' @keywords internal
#' @noRd
.tertius_matrix <- function(m, v, fn, fill = TRUE) {
  onemode <- nrow(m) == ncol(m)
  if (fn == "sum") {
    w <- m
    w[is.na(w)] <- 0
    val <- w * v
    val[is.na(val)] <- 0
    return(matrix(colSums(val), nrow(m), ncol(m), byrow = TRUE) - val)
  }
  b <- (m > 0) * 1
  b[is.na(b)] <- 0
  if (onemode) diag(b) <- 0
  known <- !is.na(v)
  bv <- b * ifelse(known, v, 0)
  bk <- b * known
  total <- matrix(colSums(bv), nrow(m), ncol(m), byrow = TRUE) - bv
  count <- matrix(colSums(bk), nrow(m), ncol(m), byrow = TRUE) - bk
  out <- total / count
  out[count == 0] <- NA_real_
  if (fill) out <- .fill_undefined(out, onemode)
  out
}

#' @keywords internal
#' @noRd
.fill_undefined <- function(out, onemode) {
  if (onemode) diag(out) <- NA
  out[is.na(out)] <- mean(out, na.rm = TRUE)
  if (onemode) diag(out) <- 0
  out
}

.build_tertius <- function(op, ctx) {
  ta <- .tertius_args(op, ctx)
  out <- .per_report(ta$m, function(s) .tertius_matrix(s, ta$v, ta$fn))
  .tertius_named(out, op, ta)
}

#' @keywords internal
#' @noRd
.tertius_named <- function(out, op, ta) {
  structure(.named(out, .col_name(op$fn, if (!is.na(ta$layer)) ta$layer, ta$x)),
            parts = list(layer = ta$layer, attribute = ta$x))
}

# goldfish's `tertius_diff()` with its default `transformer_fn = abs`: how far
# the sender's value lies from that of the receiver's other in-neighbours.
.build_tertius_diff <- function(op, ctx) {
  ta <- .tertius_args(op, ctx)
  out <- .per_report(ta$m, function(s) {
    agg <- .tertius_matrix(s, ta$v, ta$fn, fill = FALSE)
    .fill_undefined(abs(matrix(ta$v, nrow(s), ncol(s)) - agg), nrow(s) == ncol(s))
  })
  .tertius_named(out, op, ta)
}

# ---- builders: network-level variables -------------------------------------------

# A variable of the whole network, from its `globals`. It is the same for every
# dyad of one network, so it can only be told apart from the intercept in a
# list of networks.
.build_global <- function(op, ctx) {
  if (!ctx$pooled) {
    manynet::snet_abort(
      c("{.fn global} is the same for every dyad of one network, so it cannot be told apart from the intercept.",
        i = "Use it on a list of networks, where its value can differ between them."))
  }
  globals <- tryCatch(manynet::as_globallist(ctx$net), error = function(e) NULL)
  row <- if (is.null(globals)) NULL else globals[globals$var == op$name, , drop = FALSE]
  if (is.null(row) || nrow(row) == 0) {
    manynet::snet_abort("Global variable {.val {op$name}} not found in the network.")
  }
  if (nrow(row) > 1) {
    manynet::snet_abort(
      c("Global variable {.val {op$name}} has {nrow(row)} values over time.",
        i = "A network regression reads one value for each network."))
  }
  .named(matrix(row$value[[1]], nrow(ctx$DV), ncol(ctx$DV)),
         .col_name("global", op$name))
}

# ---- builders: reporters ---------------------------------------------------------

# A cognitive social structure holds each reporter's report of the network, so
# each dyad is observed once for each reporter. These terms describe the
# reporter, and Krackhardt's analyses turn on them: a report about one's own
# ties differs from a report about the ties of others. Each builds an array of
# sender, receiver, and reporter, which `.lift_to_css()` keeps as it is.
#' @keywords internal
#' @noRd
.need_css <- function(op, ctx) {
  if (ctx$css) return(invisible())
  manynet::snet_abort(
    c("{.fn {op$fn}} needs a cognitive social structure, and this outcome has no reporters.",
      i = "{.pkg manynet} records who reported each tie in a {.code by} column of the ties."))
}

# An undirected tie has no sender or receiver, so a reporter cannot be one of
# them. A two-mode tie runs from a row node to a column node.
#' @keywords internal
#' @noRd
.need_sides <- function(op, ctx) {
  if (ctx$directed || ctx$twomode) return(invisible())
  manynet::snet_abort(
    c("{.fn {op$fn}} needs a directed network.",
      i = "An undirected tie has no sender or receiver for the reporter to be."))
}

# A value for each reporter, repeated over every dyad of that reporter's report.
#' @keywords internal
#' @noRd
.reporter_array <- function(ctx, v) {
  d <- dim(ctx$DV)
  if (length(v) != d[3]) {
    manynet::snet_abort(
      c("The outcome has {d[3]} reports, but {length(v)} nodes.",
        i = "A reporter is a node, so each node needs one report."))
  }
  array(rep(v, each = d[1] * d[2]), dim = d, dimnames = dimnames(ctx$DV))
}

# The reporter's own value, as `ego()` gives the sender's, dummy-coded the same
# way where it is categorical. It is the same over a reporter's whole report,
# so a reporter fixed effect absorbs it.
.build_reporter <- function(op, ctx) {
  .need_css(op, ctx)
  m <- .reporter_array(ctx, .node_values(ctx, op$name, "reporter"))
  .build_side(op, ctx, "reporter", m)
}

.build_reporter_ego <- function(op, ctx) .build_reporter_is(op, ctx, "ego")
.build_reporter_alter <- function(op, ctx) .build_reporter_is(op, ctx, "alter")

# 1 where the reporter is the sender, `"ego"`, or the receiver, `"alter"`, of
# the dyad. In a two-mode CSS the column nodes' reports follow the row nodes'.
#' @keywords internal
#' @noRd
.build_reporter_is <- function(op, ctx, node) {
  .need_css(op, ctx)
  .need_sides(op, ctx)
  .named(.reporter_is(ctx, node), .col_name(op$fn))
}

#' @keywords internal
#' @noRd
.reporter_is <- function(ctx, node) {
  d <- dim(ctx$DV)
  k <- slice.index(array(0L, d), 3)
  self <- if (node == "ego") slice.index(k, 1)
    else slice.index(k, 2) + if (ctx$twomode) d[1] else 0L
  array((k == self) * 1, dim = d, dimnames = dimnames(ctx$DV))
}

# 1 where the reporter shares a value of `x` with the sender, `node = "ego"`, or
# with the receiver, `node = "alter"`, as `same()` is for the sender and the
# receiver. The node is part of the term's name, as in `reporter_ego`.
.build_reporter_same <- function(op, ctx) {
  .need_css(op, ctx)
  .need_sides(op, ctx)
  node <- .degree_node(op)
  sides <- .side_matrices(ctx, op$name)
  m <- .lift_to_css(if (node == "ego") sides$rows else sides$cols, ctx$DV)
  r <- .reporter_array(ctx, .node_values(ctx, op$name, "reporter"))
  .named(array((m == r) * 1, dim = dim(ctx$DV), dimnames = dimnames(ctx$DV)),
         .col_name(op$fn, node, op$name))
}

# ---- checks and advice -------------------------------------------------------------

# Every name the formula uses must exist before anything is built, so that a
# typing error names what is available rather than failing halfway.
#' @keywords internal
#' @noRd
.check_formula_vars <- function(spec, ctx) {
  node_attrs <- manynet::net_node_attributes(ctx$net)
  # The engine builds the sender, receiver, network, and reporter indices
  # itself, so they are not attributes of the network.
  tie_names <- c(ctx$tie_names, .structural_vars())
  missing_node <- character(0)
  missing_tie  <- character(0)
  for (term in spec$terms) {
    for (op in term$operands) {
      kind <- if (is.na(op$fn)) "covariate" else .formula_terms()[[op$fn]]$kind
      if (kind == "attribute") {
        if (is.na(op$name) || !(op$name %in% node_attrs))
          missing_node <- c(missing_node, op$name)
      } else if (kind %in% c("layer", "covariate")) {
        if (is.na(op$name)) {
          if (isTRUE(.formula_terms()[[op$fn]]$bare)) next
          manynet::snet_abort("{.fn {op$fn}} needs a layer, for example {.code {op$fn}(friends)}.")
        }
        if (!(op$name %in% tie_names)) missing_tie <- c(missing_tie, op$name)
      } else if (kind == "tertius") {
        first <- op$name
        x <- .arg_name(op$args$x)
        if (!is.na(first) && !(first %in% tie_names) && first %in% node_attrs) next
        if (!(first %in% tie_names)) missing_tie <- c(missing_tie, first)
        if (is.na(x) || !(x %in% node_attrs)) missing_node <- c(missing_node, x)
      }
    }
  }
  if (length(missing_node) > 0) {
    manynet::snet_abort(
      c("Node attribute{?s} {.val {unique(missing_node)}} not found.",
        i = "Available node attributes: {.val {node_attrs}}."))
  }
  if (length(missing_tie) > 0) {
    available   <- setdiff(manynet::net_tie_attributes(ctx$net), c("by", "layer"))
    structurals <- .structural_vars()
    msg <- c("Tie attribute or predictor{?s} {.val {unique(missing_tie)}} not found.")
    if (length(ctx$layers) > 0) msg <- c(msg, i = "Available layers: {.val {ctx$layers}}.")
    if (length(available) > 0) msg <- c(msg, i = "Available tie attributes: {.val {available}}.")
    msg <- c(msg, i = "Sender, receiver, and network indices are also available as {.val {structurals}}.")
    manynet::snet_abort(msg)
  }
  invisible(TRUE)
}

# Homophily is only interpretable net of the effects it contains: a sender and
# a receiver who share a high value are alike, and each is also high.
#' @keywords internal
#' @noRd
specificationAdvice <- function(spec, net) {
  ops <- unlist(lapply(spec$terms, `[[`, "operands"), recursive = FALSE)
  fns <- vapply(ops, function(op) op$fn, character(1))
  vars <- vapply(ops, function(op) op$name, character(1))
  homophily <- unique(vars[fns %in% c("sim", "same")])
  if (length(homophily) == 0) return(invisible())
  basics <- if (manynet::is_twomode(net)) c("ego", "tertius") else c("ego", "alter")
  suggests <- unlist(lapply(homophily, function(x) {
    excl <- setdiff(basics, fns[vars == x])
    if (length(excl) > 0) paste0(excl, "(", x, ")") else NULL
  }))
  if (!manynet::is_directed(net)) {
    suggests <- suggests[!grepl("^ego\\(", suggests)]
  }
  if (length(suggests) > 0) {
    suggests <- paste0(suggests, collapse = ", ")
    manynet::snet_info(
      "When testing for homophily, include all the more fundamental effects.",
      "Try adding {suggests} to the model specification.")
  }
  invisible()
}

# Session state: a deprecation is said once, not once per network of a list.
.infernet_state <- new.env(parent = emptyenv())
