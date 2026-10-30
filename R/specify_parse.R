# Formula -> specification -> matrices -----------------------------------------
#
# A formula is read in two steps. `.specify()` parses it into a specification:
# the outcome, and for each term the function it calls and the arguments it
# names. `.evaluate_spec()` then builds, against one network, one matrix for
# each column of the model, with the builders in specify_builders.R.
#
# Before this, the formula was split as a string on its parentheses and commas.
# That could not read a named argument such as `node = "ego"`, multiplied only
# a term of exactly two parts, and could not give several columns from one
# term. The specification stays internal until the stocnet maintainers agree a
# shared one; see "Formula conventions" in CONTRIBUTING.md.

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
  # A sender or a receiver fixed effect absorbs ego() or alter(), which would
  # then abort, so it is no advice to add them. An undirected network drops
  # those fixed effects, and so keeps the advice.
  if (manynet::is_directed(net) || manynet::is_twomode(net)) {
    if ("sender" %in% spec$fixed) suggests <- suggests[!grepl("^ego\\(", suggests)]
    if ("receiver" %in% spec$fixed) suggests <- suggests[!grepl("^alter\\(", suggests)]
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
