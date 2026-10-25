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
# That could not read a named argument such as `type = "ego"`, multiplied only
# a term of exactly two parts, and could not give several columns from one
# term. The specification stays internal until the stocnet maintainers agree a
# shared one; see "Formula conventions" in CONTRIBUTING.md.

# ---- the term registry -------------------------------------------------------

# `formals` is the signature a term is matched against, as for a function.
# `kind` says what its first argument names: a node attribute, a layer, or a
# global variable. `twomode` is FALSE where a two-mode network has no meaning
# for the term, and `hint` then says what to use instead. `outcome` says what
# happens where a layer term names the outcome: "refuse", or "warn" where the
# term is defined on the outcome but is then not permuted with it. `ergm` is the
# nearest ergm term, or `edgecov` of the matrix the term builds.
#' @keywords internal
#' @noRd
.formula_terms <- function() {
  list(
    ego = list(
      formals = alist(x = ), kind = "attribute", build = .build_ego,
      ergm = "nodeocov, or nodeofactor for a categorical attribute"),
    alter = list(
      formals = alist(x = ), kind = "attribute", build = .build_alter,
      ergm = "nodeicov, or nodeifactor for a categorical attribute"),
    same = list(
      formals = alist(x = ), kind = "attribute", build = .build_same,
      ergm = "nodematch"),
    diff = list(
      formals = alist(x = , transformer_fn = abs), kind = "attribute",
      build = .build_diff, ergm = "absdiff, or diff with transformer_fn = identity"),
    dist = list(
      formals = alist(x = ), kind = "attribute", build = .build_dist,
      ergm = "absdiff"),
    sim = list(
      formals = alist(x = ), kind = "attribute", build = .build_sim,
      ergm = "edgecov"),
    match = list(
      formals = alist(x = ), kind = "attribute", build = .build_match,
      twomode = FALSE, hint = "Use {.fn mix} for a two-mode network.",
      ergm = "nodematch with diff = TRUE"),
    mix = list(
      formals = alist(x = ), kind = "attribute", build = .build_mix,
      ergm = "nodemix"),
    tie = list(
      formals = alist(layer = , weighted = FALSE), kind = "layer",
      build = .build_tie, outcome = "refuse", ergm = "edgecov"),
    recip = list(
      formals = alist(layer = ), kind = "layer", build = .build_recip,
      twomode = FALSE, bare = TRUE,
      hint = "A two-mode tie has no reverse tie of the same shape.",
      outcome = "warn", ergm = "mutual, or edgecov of the transpose"),
    exchange = list(
      formals = alist(layer = ), kind = "layer", build = .build_recip,
      twomode = FALSE,
      hint = "A two-mode tie has no reverse tie of the same shape.",
      outcome = "warn", ergm = "edgecov of the transpose"),
    indeg = list(
      formals = alist(layer = , type = "alter", weighted = FALSE),
      kind = "layer", build = .build_indeg, outcome = "refuse",
      ergm = "edgecov, or nodeicov of the degree"),
    outdeg = list(
      formals = alist(layer = , type = "alter", weighted = FALSE),
      kind = "layer", build = .build_outdeg, outcome = "refuse",
      ergm = "edgecov, or nodeocov of the degree"),
    tertius = list(
      formals = alist(layer = , x = , summarizer_fn = "mean"),
      kind = "tertius", build = .build_tertius, outcome = "warn",
      ergm = "edgecov"),
    tertius_diff = list(
      formals = alist(layer = , x = , summarizer_fn = "mean"),
      kind = "tertius", build = .build_tertius_diff, outcome = "warn",
      ergm = "edgecov"),
    global = list(
      formals = alist(x = ), kind = "global", build = .build_global,
      ergm = NA_character_)
  )
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
  tt <- stats::terms(formula)
  labels <- attr(tt, "term.labels")
  # A bar has no settled meaning here yet (stocnet/infernet#8), and the old
  # rewrite of it never reached a model: the grouping index it named was looked
  # for among the layers and not found.
  bars <- grepl("|", labels, fixed = TRUE)
  if (any(bars)) {
    manynet::snet_abort(
      c("A bar in the formula is not yet supported: {.code {labels[bars]}}.",
        i = "Random intercepts are set in {.arg control}, for example {.code control = list(random_intercept_sender = TRUE)}."))
  }
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
  env <- environment(formula)
  if (is.null(env)) env <- globalenv()
  structure(list(outcome = paste(deparse(formula[[2]]), collapse = ""),
                 terms   = terms,
                 env     = env),
            class = "infernet_spec")
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
    manynet::snet_abort(
      c("{.fn {fn}} is not a formula term.",
        i = "Available terms: {.fn {names(registry)}}, or a layer named on its own."))
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
  list(fn = fn, name = .arg_name(args[[1]]), args = args, supplied = supplied)
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
.evaluate_spec <- function(spec, .data, advise = TRUE, pooled = FALSE) {
  net <- manynet::as_tidygraph(.data)
  layers <- .layer_names(net)
  out <- .resolve_outcome(spec$outcome, net, layers)
  DV <- out$DV
  css <- length(dim(DV)) == 3
  twomode <- manynet::is_twomode(net)
  ctx <- list(
    data          = .data,
    net           = net,
    layers        = layers,
    tie_names     = c(manynet::net_tie_attributes(net), layers),
    outcome_layer = .outcome_ties(out$layer, spec$outcome, net),
    DV            = DV,
    css           = css,
    twomode       = twomode,
    node_type     = if (twomode) manynet::node_attribute(net, "type") else NULL,
    directed      = isTRUE(all(manynet::is_directed(out$net))),
    env           = spec$env,
    pooled        = pooled
  )
  .check_formula_vars(spec, ctx)
  if (advise) specificationAdvice(spec, net)

  lift <- if (css) function(x) .lift_to_css(x, DV) else identity
  IVs <- lapply(spec$terms, function(term) {
    parts <- lapply(term$operands, function(op) lapply(.build_operand(op, ctx), lift))
    Reduce(.interact, parts)
  })
  IVs <- unlist(IVs, recursive = FALSE)

  rhs <- Reduce(function(a, b) call("+", a, b), lapply(names(IVs), as.name))
  out_formula <- eval(call("~", as.name(out$label), rhs))
  mydata <- c(list(DV), IVs)
  names(mydata)[1] <- out$label
  list(mydata = mydata, formula = out_formula, css = css)
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

# Builds one operand: a registered term, or a layer named on its own.
#' @keywords internal
#' @noRd
.build_operand <- function(op, ctx) {
  if (is.na(op$fn)) return(.build_covariate(op, ctx))
  entry <- .formula_terms()[[op$fn]]
  if (ctx$twomode && identical(entry$twomode, FALSE)) {
    manynet::snet_abort(
      c("{.fn {op$fn}} is not defined for a two-mode network.",
        i = entry$hint))
  }
  entry$build(op, ctx)
}

# The product of two operands, each a named list of matrices. An operand that
# gives several columns, such as `match()`, gives one product for each.
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
.node_values <- function(ctx, x, side = c("ego", "alter")) {
  side <- match.arg(side)
  v <- manynet::node_attribute(ctx$net, x)
  if (ctx$twomode) v <- if (side == "ego") v[!ctx$node_type] else v[ctx$node_type]
  v
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
# no numeric column to residualise and every level's p-value is NA. The
# reference goes into each column's name, so that the table says what a level
# is compared with.
#' @keywords internal
#' @noRd
.build_side <- function(op, ctx, side, m) {
  label <- paste(side, op$name)
  v <- .node_values(ctx, op$name, side)
  if (!is.character(v) && !is.factor(v)) return(.named(m, label))
  cats <- if (is.factor(v)) levels(droplevels(v)) else
    sort(unique(stats::na.omit(v)))
  if (length(cats) < 2) {
    manynet::snet_abort(
      "{.fn {side}} needs at least two categories of {.val {op$name}}.")
  }
  manynet::snet_info(
    "{.fn {side}} leaves out {.val {cats[1]}} of {.val {op$name}} as the reference.")
  out <- lapply(cats[-1], function(k) (m == k) * 1)
  stats::setNames(out, paste0(label, " ", cats[-1], " [", cats[1], "]"))
}

.build_same <- function(op, ctx) {
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
    # is the share of a node's ties that go to nodes like the other one.
    if (all(is.na(attrib[!manynet::node_is_mode(.data)]))) {
      attrib <- attrib[manynet::node_is_mode(.data)]
      out <- vapply(seq_along(attrib), function(x) {
        held_out <- manynet::as_matrix(
          manynet::delete_nodes(.data, manynet::net_dims(.data)[1] + x))
        rowSums(held_out * matrix((attrib[-x] == attrib[x]) * 1,
                                  nrow(DV), ncol(DV) - 1, byrow = TRUE)) /
          rowSums(held_out)
      }, FUN.VALUE = numeric(nrow(DV)))
    } else {
      attrib <- attrib[!manynet::node_is_mode(.data)]
      out <- t(vapply(seq_along(attrib), function(x) {
        held_out <- manynet::as_matrix(manynet::delete_nodes(.data, x))
        colSums(held_out * matrix((attrib[-x] == attrib[x]) * 1,
                                  nrow(DV) - 1, ncol(DV))) /
          colSums(held_out)
      }, FUN.VALUE = numeric(ncol(DV))))
    }
    out[is.nan(out)] <- 0
  } else {
    sides <- .side_matrices(ctx, op$name)
    out <- (sides$rows == sides$cols) * 1
  }
  .named(out, paste("same", op$name))
}

# goldfish's `diff()`: the transformed difference between the sender's and the
# receiver's values, absolute by default and signed with `identity`.
.build_diff <- function(op, ctx) {
  .refuse_categorical(ctx, op)
  f <- .arg_fun(op$args$transformer_fn, ctx$env, "transformer_fn")
  sides <- .side_matrices(ctx, op$name)
  .named(f(sides$rows - sides$cols), paste("diff", op$name))
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
  .named(abs(sides$rows - sides$cols), paste("dist", op$name))
}

.build_sim <- function(op, ctx) {
  .refuse_categorical(ctx, op)
  sides <- .side_matrices(ctx, op$name)
  d <- abs(sides$rows - sides$cols)
  denom <- max(d, na.rm = TRUE)
  if (!is.finite(denom) || denom == 0) denom <- 1
  .named(abs(1 - d / denom), paste("sim", op$name))
}

# Differential homophily: one column per category, 1 where the sender and the
# receiver both hold it. Numeric columns rather than a factor, so that double
# semi-partialling can residualise each one.
.build_match <- function(op, ctx) {
  sides <- .side_matrices(ctx, op$name)
  cats <- sort(unique(stats::na.omit(.node_values(ctx, op$name, "ego"))))
  out <- lapply(cats, function(k) (sides$rows == k & sides$cols == k) * 1)
  stats::setNames(out, paste("match", op$name, cats))
}

# Mixing: one column per pair of a sender's and a receiver's category, less the
# first pair as the reference. An undirected tie has no sender, so its pairs are
# unordered; a two-mode network pairs the categories of its two modes.
.build_mix <- function(op, ctx) {
  sides <- .side_matrices(ctx, op$name)
  ego_cats <- sort(unique(stats::na.omit(.node_values(ctx, op$name, "ego"))))
  alter_cats <- sort(unique(stats::na.omit(.node_values(ctx, op$name, "alter"))))
  ordered <- ctx$directed || ctx$twomode
  pairs <- expand.grid(alter = alter_cats, ego = ego_cats,
                       stringsAsFactors = FALSE)[, c("ego", "alter")]
  if (!ordered) pairs <- pairs[pairs$ego <= pairs$alter, , drop = FALSE]
  if (nrow(pairs) < 2) {
    manynet::snet_abort("{.fn mix} needs at least two categories of {.val {op$name}}.")
  }
  sep <- if (ordered) "->" else "--"
  labels <- paste0("mix ", op$name, " ", pairs$ego, sep, pairs$alter)
  manynet::snet_info("{.fn mix} leaves out {.val {labels[1]}} as the reference.")
  out <- lapply(seq_len(nrow(pairs))[-1], function(p) {
    a <- pairs$ego[p]
    b <- pairs$alter[p]
    cell <- sides$rows == a & sides$cols == b
    if (!ordered) cell <- cell | (sides$rows == b & sides$cols == a)
    cell * 1
  })
  stats::setNames(out, labels[-1])
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
  .warn_outcome_ties(op$fn)
}

#' @keywords internal
#' @noRd
.warn_outcome_ties <- function(fn) {
  manynet::snet_warn(
    c("{.fn {fn}} reads the outcome's own ties.",
      i = "Permuting the outcome does not permute this predictor with it, so its test is not exact.",
      i = "Name another layer as its first argument, for example {.code {fn}(friends, age)}."))
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
  .named(m, paste("tie", op$name))
}

# The reverse tie: the transpose of a layer. `exchange(layer)` names the layer,
# as goldfish proposes for its exogenous twin (stocnet/goldfish#122), and
# `recip()` without one reads the outcome, as goldfish's endogenous `recip` does.
.build_recip <- function(op, ctx) {
  if (is.na(op$name)) {
    if (identical(op$fn, "exchange")) {
      manynet::snet_abort("{.fn exchange} needs a layer, for example {.code exchange(friends)}.")
    }
    .warn_outcome_ties(op$fn, "exchange(friends)")
    m <- ctx$DV
    label <- op$fn
  } else {
    .outcome_layer_check(op, ctx, op$name)
    m <- .layer_matrix(ctx, op$name)
    label <- .col_name(op$fn, op$name)
  }
  out <- if (length(dim(m)) == 3) aperm(m, c(2, 1, 3)) else t(m)
  dimnames(out) <- dimnames(m)
  .named(out, label)
}

.build_indeg <- function(op, ctx) .build_degree(op, ctx, "in")
.build_outdeg <- function(op, ctx) .build_degree(op, ctx, "out")

# A node's degree in another layer, placed on the sender's or the receiver's
# side of each dyad. It counts ties, as goldfish does, unless `weighted = TRUE`.
#' @keywords internal
#' @noRd
.build_degree <- function(op, ctx, direction) {
  .outcome_layer_check(op, ctx, op$name)
  type <- .arg_name(op$args$type)
  if (!type %in% c("alter", "ego")) {
    manynet::snet_abort("{.arg type} must be {.val alter} or {.val ego}, not {.val {type}}.")
  }
  weighted <- .arg_flag(op$args$weighted, ctx$env, "weighted")
  # In a two-mode network only the column nodes receive, and only the row nodes
  # send, so the other side has no such degree to read.
  if (ctx$twomode && type == if (direction == "in") "ego" else "alter") {
    manynet::snet_abort(
      c("{.fn {op$fn}} with {.code type = \"{type}\"} is not defined for a two-mode network.",
        i = "Its {type}s never {if (direction == 'in') 'receive' else 'send'} a tie."))
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
    matrix(deg, nrow(s), ncol(s), byrow = type == "alter")
  }
  fn_label <- paste(op$fn, op$name)
  if (type == "ego") fn_label <- paste("ego", fn_label)
  .named(.per_report(m, degree_of), fn_label)
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
  label <- paste(c(op$fn, if (!is.na(ta$layer)) ta$layer, ta$x), collapse = " ")
  .named(out, label)
}

# goldfish's `tertius_diff()` with its default `transformer_fn = abs`: how far
# the sender's value lies from that of the receiver's other in-neighbours.
.build_tertius_diff <- function(op, ctx) {
  ta <- .tertius_args(op, ctx)
  out <- .per_report(ta$m, function(s) {
    agg <- .tertius_matrix(s, ta$v, ta$fn, fill = FALSE)
    .fill_undefined(abs(matrix(ta$v, nrow(s), ncol(s)) - agg), nrow(s) == ncol(s))
  })
  label <- paste(c(op$fn, if (!is.na(ta$layer)) ta$layer, ta$x), collapse = " ")
  .named(out, label)
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
         paste("global", op$name))
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
