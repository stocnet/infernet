# A network -> every term a model can take for it ----------------------------------------
#
# `RSiena::getEffects()` lists every effect that a Siena data object allows, so
# that a user chooses effects from a table rather than from a help page. The
# `specify_*()` functions do the same for a network without a formula: each
# term of the registry `.formula_terms()` that suits the network is written out
# for its layers and node attributes, and a term is kept where the record of a
# formula of it alone says that the model fits it. The table and a formula of
# its terms therefore always agree.

#' @rdname specify
#' @param lhs The outcome of the formula, as a name or a string. `NULL`, the
#'   default, is the outcome the table was made for.
#' @examples
#' # Every term for a network, and a formula of some of them
#' terms <- specify_mrqap(advice ~ ., ison_hightech)
#' terms
#' formula(terms[terms$term %in% c("ego(age)", "same(dept)", "tie(friends)"), ])
#' formula(terms[terms$effect == "reporter", ], friends)
#' @export
formula.specify_terms <- function(x, lhs = NULL, ...) {
  lhs <- substitute(lhs)
  if (is.null(lhs)) lhs <- attr(x, "outcome")
  if (is.character(lhs)) lhs <- as.name(lhs)
  if (nrow(x) == 0) {
    manynet::snet_abort(
      c("The table has no rows, so there is no term for the formula.",
        i = "Choose at least one row, for example {.code x[x$effect == \"ego\", ]}."))
  }
  terms <- .one_of_each(x)
  f <- eval(call("~", lhs, str2lang(paste(terms, collapse = " + "))))
  environment(f) <- globalenv()
  specify <- switch(attr(x, "model"), mrqap = specify_mrqap, ergm = specify_ergm)
  specify(f, attr(x, "data"), input = "mrqap")
}

#' @rdname specify
#' @export
tidy.specify_terms <- function(x, ...) {
  out <- as.data.frame(x)
  attr(out, "outcome") <- NULL
  attr(out, "model") <- NULL
  attr(out, "data") <- NULL
  tibble::as_tibble(out)
}

#' @export
print.specify_terms <- function(x, ...) {
  outcome <- .deparsed(attr(x, "outcome"))
  cat(nrow(x), " terms for ", .model_label(attr(x, "model")),
      if (outcome != ".") paste0(" of ", outcome), "\n", sep = "")
  print(tidy(x), n = Inf, ...)
  cat("Choose rows, and `formula()` of them gives their formula.\n")
  invisible(x)
}

# Whether a `specify_*()` call asks for the table: no formula, but a network,
# or a formula whose right-hand side is only `.`, as in `lm(y ~ .)`. Without
# either, a missing formula is the error it was before.
#' @keywords internal
#' @noRd
.wants_term_table <- function(formula, .data) {
  if (missing(formula)) {
    if (is.null(.data)) formula
    return(TRUE)
  }
  every <- inherits(formula, "formula") && length(formula) == 3 &&
    identical(formula[[3]], as.name("."))
  if (every && is.null(.data)) {
    manynet::snet_abort(
      c("A formula of {.code .} asks for every term, which needs {.arg .data}.",
        i = "Give the network, for example {.code specify_mrqap(advice ~ ., ison_hightech)}."))
  }
  every
}

#' @keywords internal
#' @noRd
.table_lhs <- function(formula) {
  if (missing(formula)) return(as.name("."))
  formula[[2]]
}

# ---- the table ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.every_term_table <- function(.data, lhs, model) {
  if (model == "ergm") .ergm_one_network(.data)
  net <- manynet::as_tidygraph(if (.is_list_of_graphs(.data)) .data[[1]] else .data)
  lhs <- .table_outcome(net, lhs)
  if (model == "ergm") {
    .ergm_one_report(.spec_context(list(outcome = .deparsed(lhs), env = globalenv()),
                                   .data))
  }
  rows <- .term_candidates(.data, net)
  status <- vapply(rows$term, .term_status, "", lhs = lhs, .data = .data, model = model)
  # A term that the model warns about reads the outcome's own ties, as `recip`
  # does in an MRQAP of one network, so the model cannot test it either.
  rows <- rows[!status %in% c("drops", "refuses", "warns"), , drop = FALSE]
  rownames(rows) <- NULL
  out <- tibble::as_tibble(rows)
  structure(out, class = c(paste0(model, "_terms"), "specify_terms", class(out)),
            outcome = lhs, model = model, data = .data)
}

# The outcome that the terms are for. `.` is the network's focal layer, or
# its ties where it has one layer. A multiplex network without one focal layer
# has no outcome for `.`, so its first layer is read, and the message says how
# to choose another.
#' @keywords internal
#' @noRd
.table_outcome <- function(net, lhs) {
  if (!identical(lhs, as.name("."))) return(lhs)
  layers <- .layer_names(net)
  if (length(layers) == 0) return(lhs)
  focal <- tryCatch(manynet::as_infolist(net)$focal, error = function(e) NULL)
  if (length(focal) == 1 && !is.na(focal) && focal %in% layers) return(lhs)
  manynet::snet_info(
    paste("Giving the terms for the outcome {.val {layers[1]}}, the first layer.",
          "Name another outcome on the left-hand side,",
          "for example {.code {layers[length(layers)]} ~ .}."))
  as.name(layers[1])
}

# What a model makes of one term, from the record of a formula of that term
# alone. An MRQAP also builds the term, as `tidy()` does, so that a term the
# network cannot build is refused.
#' @keywords internal
#' @noRd
.term_status <- function(term, lhs, .data, model) {
  f <- eval(call("~", lhs, str2lang(term)))
  environment(f) <- globalenv()
  specify <- switch(model, mrqap = .specify_mrqap, ergm = .specify_ergm)
  tryCatch(suppressWarnings(suppressMessages({
    g <- specify(f, .data, input = "mrqap", abort = FALSE)
    record <- if (model == "mrqap" && !identical(g[[3]], 1)) tidy.mrqap_formula(g, .data)
      else .tidy_specified(g)
    record$status[record$term != .deparsed(lhs)][1]
  })), error = function(e) "refuses")
}

# The terms of the rows, with one term of each set of alternatives: `same()`,
# `same(diff = TRUE)`, and `mix()` of one attribute measure the same thing in
# different ways, as `diff()` and `sim()` do, so a formula of all of them is
# collinear. The first row of each set is kept, and a message names the others.
#' @keywords internal
#' @noRd
.one_of_each <- function(x) {
  set <- ifelse(x$effect %in% c("same", "mix"), paste("category", x$attribute),
                ifelse(x$effect %in% c("diff", "sim"), paste("number", x$attribute),
                       paste("term", seq_len(nrow(x)))))
  left <- duplicated(set)
  if (any(left)) {
    kept <- x$term[!left][match(set[left], set[!left])]
    manynet::snet_info(paste(
      "Leaving out {.code {x$term[left]}}, which measure{?s/} the same as",
      "{.code {unique(kept)}}. Choose the rows of the terms to keep."))
  }
  x$term[!left]
}

# ---- the terms that suit a network ------------------------------------------------------

# Every term of the registry that suits the shape of the network, written out
# for its layers and node attributes. The model then drops those it cannot
# fit, such as goldfish's `cycle` in an MRQAP. A string argument is in single
# quotes, so that the printed term has no escaped quotes.
#' @keywords internal
#' @noRd
.term_candidates <- function(.data, net) {
  registry <- .formula_terms()
  twomode <- manynet::is_twomode(net)
  directed <- isTRUE(all(manynet::is_directed(net))) && !twomode
  css <- "by" %in% manynet::net_tie_attributes(net)
  layers <- .layer_names(net)
  attrs <- .term_attributes(net)
  numeric <- attrs$name[attrs$numeric]
  rows <- list()
  add <- function(term, effect, layer = NA_character_, attribute = NA_character_) {
    rows[[length(rows) + 1]] <<- data.frame(term = term, effect = effect,
                                           layer = layer, attribute = attribute)
  }
  call_of <- function(fn, ...) paste0(fn, "(", paste(c(...), collapse = ", "), ")")

  # An undirected tie has the same two sides, so `alter()` is `ego()` again.
  for (k in seq_len(nrow(attrs))) {
    a <- attrs[k, ]
    if (a$ego) add(call_of("ego", a$name), "ego", attribute = a$name)
    if (a$alter && (directed || twomode)) add(call_of("alter", a$name), "alter", attribute = a$name)
    if (!(a$ego && a$alter)) next
    if (a$numeric) {
      add(call_of("diff", a$name), "diff", attribute = a$name)
      add(call_of("diff", a$name, "transformer_fn = identity"), "diff", attribute = a$name)
      add(call_of("sim", a$name), "sim", attribute = a$name)
    } else {
      add(call_of("same", a$name), "same", attribute = a$name)
      add(call_of("same", a$name, "diff = TRUE"), "same", attribute = a$name)
      add(call_of("mix", a$name), "mix", attribute = a$name)
    }
  }

  for (layer in layers) {
    add(call_of("tie", layer), "tie", layer = layer)
    if (directed) add(call_of("exchange", layer), "exchange", layer = layer)
    add(call_of("indeg", layer), "indeg", layer = layer)
    if (!twomode) add(call_of("indeg", layer, "node = 'ego'"), "indeg", layer = layer)
    if (directed || twomode) add(call_of("outdeg", layer), "outdeg", layer = layer)
    if (directed) add(call_of("outdeg", layer, "node = 'alter'"), "outdeg", layer = layer)
    for (x in numeric) {
      add(call_of("tertius", layer, x), "tertius", layer = layer, attribute = x)
      add(call_of("tertius_diff", layer, x), "tertius_diff", layer = layer, attribute = x)
    }
  }

  # The terms that stand alone and read the outcome's own ties. An MRQAP
  # drops most of them, and fits `inertia` and `trans` only on a panel.
  # goldfish's bare `indeg` and `outdeg` are the outcome's own degree, which
  # an MRQAP and ergm drop.
  for (fn in names(registry)) {
    e <- registry[[fn]]
    if (!isTRUE(e$bare) || isTRUE(e$css) || !e$kind %in% c("layer", "endogenous")) next
    if (fn %in% c("indeg", "outdeg")) next
    if (twomode && isFALSE(e$twomode)) next
    if (!directed && fn == "recip") next
    add(fn, fn)
  }

  if (css) {
    for (x in attrs$name) add(call_of("reporter", x), "reporter", attribute = x)
    if (directed || twomode) {
      add("reporter_ego", "reporter_ego")
      add("reporter_alter", "reporter_alter")
    }
    if (!twomode) {
      for (x in attrs$name[!attrs$numeric]) {
        add(call_of("reporter_same", x), "reporter_same", attribute = x)
        add(call_of("reporter_same", x, "node = 'alter'"), "reporter_same", attribute = x)
      }
    }
  }

  if (.is_list_of_graphs(.data)) {
    globals <- tryCatch(manynet::as_globallist(net), error = function(e) NULL)
    for (v in unique(globals$var)) add(call_of("global", v), "global", attribute = v)
  }

  if (length(rows) == 0) {
    return(data.frame(term = character(0), effect = character(0),
                      layer = character(0), attribute = character(0)))
  }
  do.call(rbind, rows)
}

# The node attributes a term can read, and on which side. A name, a label, the
# mode of a two-mode network, or a marker of missing data describes no node,
# and a categorical attribute with a different value for every node is an
# identifier. A side needs at least two values of the attribute: in a two-mode
# network, the first mode is the sender's side and the second the receiver's.
#' @keywords internal
#' @noRd
.term_attributes <- function(net) {
  names <- setdiff(manynet::net_node_attributes(net), c("name", "label", "type", "na"))
  twomode <- manynet::is_twomode(net)
  type <- if (twomode) manynet::node_attribute(net, "type") else NULL
  usable <- function(v) {
    v <- v[!is.na(v)]
    if (length(unique(v)) < 2) return(FALSE)
    !((is.character(v) || is.factor(v)) && length(unique(v)) == length(v))
  }
  out <- lapply(names, function(x) {
    v <- manynet::node_attribute(net, x)
    if (is.list(v)) return(NULL)
    ego <- usable(if (twomode) v[!type] else v)
    alter <- usable(if (twomode) v[type] else v)
    if (!ego && !alter) return(NULL)
    data.frame(name = x, numeric = is.numeric(v) || is.logical(v), ego = ego, alter = alter)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(name = character(0), numeric = logical(0),
                      ego = logical(0), alter = logical(0))
  }
  out
}
