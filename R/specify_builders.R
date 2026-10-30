# The builders of the terms ----------------------------------------------------
#
# Each term of the registry, `.formula_terms()` in specify_registry.R, names
# one of these as its `build`. A builder takes the parsed operand and the
# context of one network, and returns a named list of matrices, one for each
# column of the term. The equations on the help page `terms_mrqap` are what
# these compute.

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
    # The ties are the outcome's. `as_matrix()` of a multiplex two-mode
    # network is one square matrix of every layer and both modes.
    whole <- DV
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

# Why a term that reads only the outcome's own ties, such as goldfish's
# `inertia` or `~ indeg`, is dropped.
#' @keywords internal
#' @noRd
.endogenous_reason <- function() "depends on the outcome's own ties"

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
# `weighted = FALSE` gives 1 where the reverse tie is positive, and
# `weighted = TRUE` its value, as goldfish's `recip()` does. Without it, the
# values are as stored, as before.
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
  if (!is.null(op$args$weighted) && !.arg_flag(op$args$weighted, ctx$env, "weighted")) {
    out <- (out > 0) * 1
  }
  .named(out, label)
}

# The outcome's own tie in the previous wave of a panel, 0/1 as goldfish's
# `inertia` counts it, or its value with `weighted = TRUE`, as goldfish's
# `inertia(weighted = TRUE)` reads it. A complete ranking has a tie in every
# dyad, so only the value varies there. An unobserved tie stays NA.
#' @keywords internal
#' @noRd
.build_inertia <- function(op, ctx) {
  m <- .previous_wave(op, ctx)
  weighted <- .arg_flag(op$args$weighted, ctx$env, "weighted")
  .named(if (weighted) m else (m > 0) * 1, "inertia")
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
  # The waves of a panel are one network, with one value of each global
  # variable, so it is the intercept again.
  if (!is.null(ctx$previous)) {
    manynet::snet_abort(
      c("{.fn global} is the same in every wave of a panel, so it cannot be told apart from the intercept.",
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
