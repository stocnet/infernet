# Infrastructure for test-functional.R, the functional audit.
#
# The audit reads the term registry, `.formula_terms()`, and builds every term
# that an MRQAP fits on one small network of each shape. A term added to the
# registry is then audited on every shape without a new test. Each term must
# either build columns of the outcome's shape, or be refused with a message
# that names it, where the registry says the shape cannot have it.
#
# A combination that does neither is skipped with a structured "AUDIT [...]"
# message rather than failed, so that `devtools::test()` lists the remaining
# work without stopping a release. CI sets `INFERNET_STRICT_AUDIT=true`, so the
# same cases fail there. The audit asks only whether a term builds and what
# shape it gives. Whether its values are right is in test-model_formula_terms.R,
# and those tests always fail when they are wrong.

# ---- the audit ------------------------------------------------------------------

strict_audit <- function() {
  isTRUE(as.logical(Sys.getenv("INFERNET_STRICT_AUDIT", "false")))
}

audit <- function(term, shape, message) {
  label <- paste0("AUDIT [", term, "() x ", shape, "]: ", message)
  if (strict_audit()) testthat::fail(label) else testthat::skip(label)
}

# ---- the roster -------------------------------------------------------------------

# One example of each registered term, by its kind: `x` is a numeric attribute,
# `grp` a categorical one, `b` a layer other than the outcome, and `gdp` a
# global variable.
term_example <- function(name, entry) {
  if (name %in% c("match", "mix")) return(paste0(name, "(grp)"))
  switch(entry$kind,
    attribute = , interaction = paste0(name, "(x)"),
    layer = , mixed = paste0(name, "(b)"),
    tertius = paste0(name, "(b, x)"),
    global = paste0(name, "(gdp)"),
    name)
}

# The terms an MRQAP fits: those it fits or warns about, and those it fits on a
# panel only. The terms it drops are dropped on every shape, so there is
# nothing to build.
functional_roster <- function() {
  reg <- .formula_terms()
  keep <- vapply(reg, function(e) e$mrqap %in% c("fits", "warns") ||
                   isTRUE(e$panel), logical(1))
  reg[keep]
}

# What the registry says a shape makes of a term: it builds, or it is refused.
functional_expectation <- function(entry, shape) {
  if (isTRUE(entry$css) && shape != "css") return("refuses")
  if (isFALSE(entry$twomode) && shape == "twomode") return("refuses")
  if (identical(entry$kind, "global") && shape != "list") return("refuses")
  if (isTRUE(entry$panel) && identical(entry$mrqap, "drops") &&
      shape != "panel") return("refuses")
  "builds"
}

# ---- the shapes -------------------------------------------------------------------

# One small network of each shape. Each has an outcome layer `a`, a second
# layer `b`, a numeric attribute `x`, a categorical one `grp`, and a global
# variable `gdp`, so that every example in the roster can be read on it.
functional_nets <- function() {
  ties <- data.frame(
    from  = c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 3L,   2L, 3L, 4L, 5L, 6L, 1L),
    to    = c(2L, 3L, 1L, 1L, 1L, 2L, 3L, 2L,   1L, 4L, 5L, 6L, 1L, 4L),
    layer = c(rep("a", 8), rep("b", 6)))
  nodes <- data.frame(label = LETTERS[1:6], x = c(3, 1, 4, 1, 5, 9),
                      grp = c("p", "q", "p", "q", "p", "q"))
  gdp <- data.frame(var = "gdp", value = 5)
  set.seed(1)
  # Six reporters' reports of layer `a`, each missing about a fifth of its
  # ties, and layer `b` as one matrix that every report shares.
  a <- ties[ties$layer == "a", ]
  reports <- do.call(rbind, lapply(1:6, function(k) {
    r <- a[stats::runif(nrow(a)) > 0.2, ]
    r$by <- k
    r
  }))
  shared <- ties[ties$layer == "b", ]
  shared$by <- NA_integer_
  reports <- rbind(reports, shared)
  reports$by <- as.integer(reports$by)
  # Three waves, each missing about a quarter of the ties.
  waves <- do.call(rbind, lapply(1:3, function(w) {
    t <- ties[stats::runif(nrow(ties)) > 0.25, ]
    t$time <- w
    t
  }))
  # Four people at three events, in two layers.
  tm <- data.frame(from = c(1L, 1L, 2L, 3L, 4L, 2L, 3L, 4L, 1L),
                   to = c(5L, 6L, 6L, 7L, 5L, 5L, 7L, 7L, 7L),
                   layer = c(rep("a", 5), rep("b", 4)))
  list(
    directed = formula_net(),
    undirected = manynet::make_stocnet(nodes = nodes, ties = ties,
                                       info = list(directed = FALSE),
                                       globals = gdp),
    twomode = manynet::make_stocnet(
      nodes = data.frame(label = c(paste0("p", 1:4), paste0("e", 1:3)),
                         mode = rep(c("people", "events"), c(4, 3)),
                         x = c(3, 1, 4, 1, 5, 9, 2),
                         grp = c("p", "q", "p", "q", "p", "q", "p")),
      ties = tm, globals = gdp),
    css = manynet::make_stocnet(nodes = nodes, ties = reports,
                                info = list(directed = TRUE), globals = gdp),
    panel = manynet::make_stocnet(nodes = nodes, ties = waves,
                                  info = list(directed = TRUE), globals = gdp),
    list = list(formula_net(gdp = 5), formula_net(gdp = 7)))
}

# The matrices that the engine would fit, as one list for each network or wave.
functional_build <- function(formula, .data, shape) {
  quietly <- function(expr) suppressWarnings(suppressMessages(expr))
  if (shape %in% c("panel", "list")) {
    built <- quietly(switch(shape,
      panel = .prepare_panel(formula, .data),
      list = .prepare_list_of_graphs(formula, .data)))
    k <- length(built$matlist[[1]])
    lapply(seq_len(k), function(i) lapply(built$matlist, `[[`, i))
  } else {
    list(quietly(convertToMatrixList(formula, .data, advise = FALSE))$mydata)
  }
}

# A problem with what a term built, or NULL where it has the outcome's shape:
# at least one column, each with the outcome's dimensions, and not all missing.
functional_problem <- function(built) {
  for (mats in built) {
    shape <- dim(mats[[1]])
    if (length(mats) < 2) return("built no column")
    for (nm in names(mats)[-1]) {
      if (!identical(dim(mats[[nm]]), shape)) {
        return(paste0("column ", nm, " is ", paste(dim(mats[[nm]]), collapse = " x "),
                      ", not ", paste(shape, collapse = " x ")))
      }
      if (all(is.na(mats[[nm]]))) return(paste("column", nm, "is all missing"))
    }
  }
  NULL
}
