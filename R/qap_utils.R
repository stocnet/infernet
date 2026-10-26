# QAP utilities ---------------------------------------------------------------
#
# Support functions for the QAPglm / estimate_mrqap engine.  Ported from the
# MrQAP package (Robert W. Krause).  All functions are internal.

# ---- formula parsing --------------------------------------------------------

#' Parse a QAP formula into its components
#' @keywords internal
#' @noRd
parse_qap_formula <- function(formula) {
  dependent <- all.vars(formula)[1]

  # A bar in the formula means one thing: an {lme4} random-effect term. The
  # front end always writes it inside parentheses, and {fixest} -- which read a
  # bare bar as a fixed effect -- is on feature/fixest-fixed-effects.
  formula_str <- paste(deparse(formula, width.cutoff = 500), collapse = " ")
  has_random <- grepl("\\|", formula_str)

  main <- if (has_random) {
    .drop_bars(formula)
  } else {
    all.vars(formula[-1])
  }

  list(dependent     = dependent,
       main          = main,
       has_random    = has_random,
       all_data_vars = main)
}


# The variables of a formula's right-hand side, leaving out its random-effect
# terms. `stats::terms()` gives each `(1 | g)` as one label, so dropping the
# labels that hold a bar leaves the fixed part.
#' @keywords internal
#' @noRd
.drop_bars <- function(formula) {
  labels <- attr(stats::terms(formula), "term.labels")
  labels <- labels[!grepl("|", labels, fixed = TRUE)]
  unique(unlist(lapply(labels, function(l) all.vars(str2lang(l)))))
}


#' Append random-intercept terms to a formula
#' @keywords internal
#' @noRd
build_internal_formula <- function(formula,
                                   rin = FALSE, ris = FALSE,
                                   rir = FALSE, rib = FALSE) {
  mod_str <- paste(deparse(formula, width.cutoff = 500), collapse = " ")
  rand_int <- ""
  if (rin) rand_int <- paste(rand_int, "+ (1|nv)")
  if (ris) rand_int <- paste(rand_int, "+ (1|sv)")
  if (rir) rand_int <- paste(rand_int, "+ (1|rv)")
  if (rib) rand_int <- paste(rand_int, "+ (1|bv)")
  if (nchar(trimws(rand_int)) > 0) {
    stats::as.formula(paste(mod_str, rand_int))
  } else {
    formula
  }
}


# ---- validation -------------------------------------------------------------

#' @keywords internal
#' @noRd
validate_qap_input <- function(matlist, parsed, css = FALSE) {
  dep <- parsed$dependent
  if (!(dep %in% names(matlist))) {
    manynet::snet_abort("Dependent variable {.val {dep}} not found in the data.")
  }
  structural_vars <- .structural_vars()
  for (v in parsed$all_data_vars) {
    if (v %in% structural_vars) next
    if (!(v %in% names(matlist))) {
      manynet::snet_abort("Predictor {.val {v}} not found in the data.")
    }
  }

  y <- matlist[[dep]]
  large <- is.list(y)

  if (!css) {
    if (!large) {
      if (!is.matrix(y))
        manynet::snet_abort("The dependent variable {.val {dep}} must be a matrix.")
    } else {
      for (i in seq_along(y)) {
        if (!is.matrix(y[[i]]))
          manynet::snet_abort(
            "Network {i} of the dependent variable {.val {dep}} must be a matrix.")
      }
    }
  } else {
    if (!large) {
      if (length(dim(y)) != 3)
        manynet::snet_abort(
          "The dependent variable {.val {dep}} must be a 3-dimensional array.")
    } else {
      for (i in seq_along(y)) {
        if (length(dim(y[[i]])) != 3)
          manynet::snet_abort(
            "Network {i} of the dependent variable {.val {dep}} must be a 3-dimensional array.")
      }
    }
  }
  invisible(TRUE)
}


# ---- future plan + permutation runner ---------------------------------------

#' @keywords internal
#' @noRd
setup_future_plan <- function(strategy = "sequential", ncores = NULL) {
  old_plan <- future::plan()
  old_maxSize <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = +Inf)
  if (!is.null(ncores) && ncores > 1) {
    future::plan(future::multisession, workers = ncores)
  } else {
    future::plan(strategy)
  }
  attr(old_plan, "old_maxSize") <- old_maxSize
  invisible(old_plan)
}


#' @keywords internal
#' @noRd
run_permutations <- function(times, FUN, ...) {
  future.apply::future_lapply(
    seq_len(times),
    FUN,
    ...,
    future.seed = TRUE
  )
}


# ---- matrix permutation -----------------------------------------------------

# One permutation of the node order, respecting a blocking factor where its
# length matches the mode being permuted. A `groups` vector of the wrong length
# cannot describe this mode, so that mode permutes freely rather than silently
# recycling the factor, which is what `split()` did before.
#
# A block of one node must keep that node in place. `sample(5)` draws from 1:5,
# so each block is shuffled by index rather than passed to `sample()`, which
# draws the same random numbers for every block of two or more.
#' @keywords internal
#' @noRd
.perm_order <- function(n, groups = NULL) {
  if (is.null(groups) || length(groups) != n) return(sample(seq_len(n)))
  groups <- as.character(groups)
  unsplit(lapply(split(seq_len(n), groups),
                 function(v) v[sample.int(length(v))]), groups)
}

# The blocking factor for one mode. The engine passes either the user's
# `groups`, one factor that `.perm_order()` applies to any mode of its length,
# or the blocks that `.missing_blocks()` builds, which name each mode.
#' @keywords internal
#' @noRd
.mode_groups <- function(groups, which) {
  if (is.list(groups)) groups[[which]] else groups
}


# ---- missing cells ----------------------------------------------------------

# Which nodes a permutation may exchange, so that the missing cells of the
# permuted matrix map onto themselves. Every draw then drops exactly the dyads
# that the observed model drops. Recomputing the dyads after a free permutation
# moved the missing cells of the permuted matrix but not those of the others,
# so each draw was fitted on a different, and usually larger, set of dyads.
#
# A node's signature says which of its lines are wholly unobserved: its row as
# a sender, its column as a receiver, and its slice as a reporter. Nodes with
# the same signature can be exchanged. A missing cell that no wholly unobserved
# line explains is scattered, and the nodes at its ends stay in place. Each
# node then maps to a node with the same unobserved lines, and a scattered cell
# does not move, so the missing cells map onto themselves.
#
# Returns NULL where no usable cell is missing, so that a complete network
# permutes exactly as before.
#' @keywords internal
#' @noRd
.missing_blocks <- function(m, groups = NULL, diag = FALSE) {
  d  <- dim(m)
  nr <- d[1]; nc <- d[2]
  css <- length(d) == 3
  use <- matrix(TRUE, nr, nc)
  if (!diag && nr == nc) diag(use) <- FALSE
  if (css) use <- array(use, dim = d)
  na <- is.na(m) & use
  if (!any(na)) return(NULL)

  whole <- function(margin) {
    n_use <- apply(use, margin, sum)
    n_use > 0 & apply(na, margin, sum) == n_use
  }
  row_na <- whole(1)
  col_na <- whole(2)
  explained <- outer(row_na, col_na, "|")
  if (css) {
    slice_na  <- whole(3)
    explained <- outer(explained, slice_na, "|")
  }
  scattered <- na & !explained
  fix_row <- apply(scattered, 1, any)
  fix_col <- apply(scattered, 2, any)

  if (!css) {
    if (nr == nc) {
      rows <- .block_labels(paste(row_na, col_na), fix_row | fix_col, groups)
      return(list(rows = rows, cols = rows))
    }
    return(list(rows = .block_labels(paste(row_na), fix_row, groups),
                cols = .block_labels(paste(col_na), fix_col, groups)))
  }

  # The slices of a CSS are its nodes, in the order `.perm_css()` reads them.
  np <- d[3]
  fix_slice <- apply(scattered, 3, any)
  if (nr == nc && nc == np) {
    rows <- .block_labels(paste(row_na, col_na, slice_na),
                          fix_row | fix_col | fix_slice, groups)
    return(list(rows = rows, cols = rows))
  }
  if (np != nr + nc) return(NULL)
  ci <- nr + seq_len(nc)
  list(rows = .block_labels(paste(row_na, slice_na[seq_len(nr)]),
                            fix_row | fix_slice[seq_len(nr)], groups),
       cols = .block_labels(paste(col_na, slice_na[ci]),
                            fix_col | fix_slice[ci], groups))
}

# Combines a mode's missing-data signature with the user's blocking factor,
# where that factor describes this mode, and gives each fixed node a block of
# its own.
#' @keywords internal
#' @noRd
.block_labels <- function(signature, fixed, groups = NULL) {
  lab <- signature
  if (!is.null(groups) && length(groups) == length(lab))
    lab <- paste(as.character(groups), lab, sep = "|")
  lab[fixed] <- paste0("fixed", which(fixed))
  lab
}

# The blocks for the matrix a permutation set draws, for one network or for
# each of a pooled list, and what they cost. A fixed node and a small block
# reduce the number of distinct permutations, so say so where the null
# distribution would repeat itself, and stop where no draw differs from the
# observed data.
#' @keywords internal
#' @noRd
.permutation_blocks <- function(target, groups, diag, times, label) {
  if (is.list(target)) {
    blocks <- lapply(seq_along(target), function(net) {
      g <- if (is.list(groups)) groups[[net]] else groups
      .missing_blocks(target[[net]], g, diag)
    })
    if (all(vapply(blocks, is.null, logical(1)))) return(NULL)
    # A network with no missing cells keeps the user's groups. `[<-` with a
    # list, since assigning NULL with `[[<-` would delete the network's entry.
    for (net in seq_along(blocks)) {
      if (is.null(blocks[[net]]))
        blocks[net] <- list(if (is.list(groups)) groups[[net]] else groups)
    }
    parts <- Filter(is.list, blocks)
  } else {
    blocks <- .missing_blocks(target, groups, diag)
    if (is.null(blocks)) return(NULL)
    parts <- list(blocks)
  }

  .log_perms <- function(b) {
    modes <- if (identical(b$rows, b$cols)) list(b$rows) else list(b$rows, b$cols)
    sum(vapply(modes, function(lab) sum(lfactorial(table(lab))), numeric(1)))
  }
  log_perms <- sum(vapply(parts, .log_perms, numeric(1)))
  fixed <- sum(vapply(parts, function(b) {
    lab <- if (identical(b$rows, b$cols)) b$rows else c(b$rows, b$cols)
    sum(startsWith(lab, "fixed"))
  }, numeric(1)))

  if (log_perms == 0) {
    manynet::snet_abort(
      c("No permutation of {.val {label}} keeps its missing cells in place.",
        i = "Every node is fixed, or alone in its block.",
        i = "Remove the scattered missing cells, for example with {.fn manynet::to_no_missing}."))
  }
  if (log_perms < log(times)) {
    distinct <- round(exp(log_perms))
    manynet::snet_warn(
      c("Only {distinct} distinct permutation{?s} of {.val {label}} keep{?s/} its missing cells in place.",
        i = "The {times} draws repeat some of them."))
  }
  attr(blocks, "fixed") <- fixed
  blocks
}

# The cells a model could use, and how many of them are unobserved on each
# variable. A loop that the model leaves out, or the upper half of an undirected
# network, is not a cell the model could use, so it is not counted as missing.
#' @keywords internal
#' @noRd
.missing_summary <- function(matlist, dep, data_vars, diag, directed, large) {
  vars <- unique(c(dep, data_vars))
  n_nets <- if (large) length(matlist[[dep]]) else 1L
  by_variable <- stats::setNames(integer(length(vars)), vars)
  possible <- 0L
  for (net in seq_len(n_nets)) {
    get <- function(v) if (large) matlist[[v]][[net]] else matlist[[v]]
    use <- .structural_mask(dim(get(dep)), diag, directed)
    possible <- possible + sum(use)
    for (v in vars) {
      by_variable[[v]] <- by_variable[[v]] + sum(is.na(get(v)) & use)
    }
  }
  list(possible = possible, by_variable = by_variable)
}

# The cells of a network, or of each report of a CSS, that a model could use:
# every cell but the loops, unless the model keeps them, and only the lower
# half of an undirected network. `make_qap_data()` and `make_css_data()` read
# the same cells.
#' @keywords internal
#' @noRd
.structural_mask <- function(d, diag, directed) {
  nr <- d[1]; nc <- d[2]
  use <- matrix(TRUE, nr, nc)
  if (nr == nc) {
    if (!diag) diag(use) <- FALSE
    if (!directed) use[upper.tri(use)] <- FALSE
  }
  if (length(d) == 3) use <- array(use, dim = d)
  use
}

# One permutation of a CSS array. A permutation relabels the nodes, and in a
# CSS a reporter is a node too, so the same relabelling applies to the slices:
# the report of the node now called j is the report node j made, re-read under
# the new labels. Permuting the senders and receivers but not the reporters
# would break the tie between a reporter and their own position in the network.
#
# A one-mode CSS has one nodeset on all three axes, so one order serves all of
# them. A two-mode CSS from `{manynet}` has the first mode on its rows, the
# second on its columns, and every node of both modes on its slices, so the
# slice order is the row order followed by the column order, offset by the
# number of rows.
#' @keywords internal
#' @noRd
.perm_css <- function(m, groups = NULL) {
  d <- dim(m)
  nr <- d[1]; nc <- d[2]; np <- d[3]
  if (nr == nc && nc == np) {
    o <- .perm_order(nr, .mode_groups(groups, "rows"))
    out <- m[o, o, o, drop = FALSE]
  } else if (np == nr + nc) {
    or <- .perm_order(nr, .mode_groups(groups, "rows"))
    oc <- .perm_order(nc, .mode_groups(groups, "cols"))
    out <- m[or, oc, c(or, nr + oc), drop = FALSE]
  } else {
    manynet::snet_abort(
      c("Cannot permute a CSS array of dimensions {d}.",
        i = "Its reporters must be its nodes: as many as its rows, or as its rows and columns together."))
  }
  dimnames(out) <- dimnames(m)
  out
}

#' @keywords internal
#' @noRd
RMPerm <- function(m, groups = NULL, CSS = FALSE) {

  if (is.list(m)) {
    return(lapply(m, RMPerm, groups = groups, CSS = CSS))
  }

  if (length(dim(m)) == 2) {
    nr <- dim(m)[1]
    nc <- dim(m)[2]
    if (nr == nc) {
      o <- .perm_order(nr, .mode_groups(groups, "rows"))
      p <- matrix(data = m[o, o], nrow = nr, ncol = nc)
    } else {
      # A two-mode incidence matrix has two nodesets, so the rows and the
      # columns permute independently. Permuting both by one order, as the
      # square case does, indexes past the shorter side and errors.
      or <- .perm_order(nr, .mode_groups(groups, "rows"))
      oc <- .perm_order(nc, .mode_groups(groups, "cols"))
      p <- matrix(data = m[or, oc], nrow = nr, ncol = nc)
    }
    dimnames(p) <- dimnames(m)
  } else if (CSS) {
    p <- .perm_css(m, groups)
  } else {
    groups <- if (is.null(groups)) rep(1, dim(m)[2]) else as.character(groups)
    p <- array(dim = c(dim(m)[1], dim(m)[2], dim(m)[3]))
    for (i in 1:dim(m)[1]) {
      o <- unsplit(lapply(split(1:dim(m)[2], groups), FUN = sample), groups)
      p[i, , ] <- array(m[i, o, o])
    }
  }
  return(p)
}


# ---- data frame builder -----------------------------------------------------

#' @keywords internal
#' @noRd
make_qap_data <- function(y, x, g = NULL, diag = FALSE, directed = TRUE,
                          net = 1, perm = FALSE, xi = NULL) {
  nx <- length(x)

  if (perm && is.null(xi)) {
    y <- RMPerm(y, g)
  } else if (perm && !is.null(xi)) {
    x[[xi]] <- RMPerm(x[[xi]], g)
  }

  # The dependent matrix is square for a one-mode network and rectangular for a
  # two-mode one. Reading `nc` from the matrix rather than assuming `nr` is what
  # keeps a two-mode network from being read as a square one, which used to
  # wrap past the last column and invent dyads.
  nr <- dim(y)[1]
  nc <- dim(y)[2]
  square <- identical(nr, nc)

  valid <- matrix(TRUE, nr, nc)
  if (!diag && square) diag(valid) <- FALSE
  # An undirected one-mode network holds each dyad twice, once on each side of
  # the diagonal. Keeping both halves doubles the sample and shrinks every
  # standard error, so take the lower triangle only. A two-mode incidence
  # matrix has no such symmetry, and keeps every cell.
  if (!directed && square) valid[upper.tri(valid)] <- FALSE

  for (var in seq_len(nx)) {
    valid[is.na(x[[var]])] <- FALSE
  }
  valid[is.na(y)] <- FALSE
  y[!valid] <- NA

  vv <- as.vector(valid)

  for (var in seq_len(nx)) {
    x[[var]][!valid] <- NA
  }

  if (sum(vv) == 0) {
    manynet::snet_abort(
      c("No valid dyads remain in network {net} after dropping NA and diagonal cells.",
        i = "Check that the predictors and the outcome are observed for the same node pairs."))
  }

  pred <- data.frame(
    location = as.vector(matrix(seq_len(nr * nc), nr, nc))[vv],
    yv       = as.vector(y)[vv]
  )
  pred$nv <- as.factor(net)

  sv <- matrix(seq_len(nr), nr, nc)
  sv[!valid] <- NA
  pred$sv <- as.vector(sv)[vv]

  rv <- matrix(seq_len(nc), nr, nc, byrow = TRUE)
  rv[!valid] <- NA
  pred$rv <- as.vector(rv)[vv]

  for (var in seq_len(nx)) {
    pred[[names(x)[var]]] <- as.vector(x[[var]])[vv]
  }
  return(pred)
}


# ---- robust SE --------------------------------------------------------------

#' @keywords internal
#' @noRd
HC3 <- function(X, e) {
  XO <- cbind(matrix(1, dim(X)[1], 1), X)
  XTXINV <- solve(t(XO) %*% XO)
  om <- c()

  hf <- function(z, XTXINV = XTXINV) {
    h <- z %*% XTXINV %*% z
    return(h)
  }
  h  <- apply(XO, 1, hf, XTXINV = XTXINV)
  om <- e^2 / (1 - h)^2
  # No gc() here: this runs once per permutation, and forcing a collection
  # thousands of times costs far more than the memory it returns.
  sqrt(diag(t(t(XTXINV %*% t(XO)) * om) %*% XO %*% XTXINV))
}


# ---- baseline + perm fitting ------------------------------------------------

# An interaction's name carries a colon ("ego_Age:alter_Age"), and a layer's
# name can carry a space, so the model formula quotes them
# and several fitters hand the backticks back in the coefficient names. Double
# semi-partialling then looks a column up by the unquoted name and fails with a
# subscript error. The inner function has many early returns, one per family,
# so the names are cleaned here, where every path passes through exactly once.
#' @keywords internal
#' @noRd
fit_qap_model <- function(...) {
  fit <- .fit_qap_model(...)
  for (el in c("coefficients", "t", "zi_coefficients")) {
    if (!is.null(fit[[el]]) && !is.null(names(fit[[el]]))) {
      names(fit[[el]]) <- gsub("`", "", names(fit[[el]]), fixed = TRUE)
    }
  }
  fit
}

#' @keywords internal
#' @noRd
.fit_qap_model <- function(mod, pred, family,
                          use_robust_errors = FALSE,
                          main_vars = NULL,
                          has_random = FALSE) {
  fit <- list()
  dep_var <- all.vars(mod)[1]
  nx  <- length(main_vars)

  if (family == "zip") {
    if (has_random) {
      # The mixed variant needs {glmmTMB}, which is on feature/glmmtmb-mixed.
      manynet::snet_abort(
        c("Random intercepts are not available for the {.val zip} family.",
          i = "Drop the random intercepts, or use {.val poisson}."))
    } else {
      thisRequires("pscl", "for zero-inflated Poisson models")
      base_model <- pscl::zeroinfl(mod, data = pred, dist = "poisson")
      fit$coefficients <- base_model$coefficients$count
      resid <- stats::residuals(base_model, type = "response")
      if (use_robust_errors) {
        xv <- as.matrix(pred[, main_vars, drop = FALSE])
        fit$t <- fit$coefficients / HC3(xv, resid)
      } else {
        fit$t <- summary(base_model)$coefficients$count[, 3]
      }
      fit$zi_coefficients <- base_model$coefficients$zero
    }
    fit$base_model <- base_model
    return(fit)
  }

  if (!has_random) {
    if (family == "gaussian") {
      base_model        <- stats::lm(mod, data = pred)
      fit$r.squared     <- summary(base_model)$r.squared
      fit$adj.r.squared <- summary(base_model)$adj.r.squared
    } else if (family == "negbin") {
      thisRequires("MASS", "for negative binomial models")
      base_model <- MASS::glm.nb(mod, data = pred)
      fit$theta  <- base_model$theta
    } else {
      base_model <- stats::glm(mod, data = pred, family = family)
    }
    fit$coefficients <- base_model$coefficients
    resid <- stats::residuals(base_model)

    if (use_robust_errors) {
      xv <- as.matrix(pred[, main_vars, drop = FALSE])
      fit$t <- fit$coefficients / HC3(xv, resid)
    } else {
      fit$t <- summary(base_model)$coefficients[, 3]
    }
  } else {
    if (family == "gaussian") {
      thisRequires("lme4", "for random effects")
      base_model <- lme4::lmer(mod, data = pred)
    } else if (family == "negbin") {
      # The mixed variant needs {glmmTMB}, which is on feature/glmmtmb-mixed.
      manynet::snet_abort(
        c("Random intercepts are not available for the {.val negbin} family.",
          i = "Drop the random intercepts, or use {.val poisson}."))
    } else {
      thisRequires("lme4", "for random effects")
      base_model <- lme4::glmer(mod, data = pred, family = family,
                                control = lme4::glmerControl(
                                  calc.derivs = FALSE,
                                  optimizer   = "bobyqa"),
                                nAGQ = 0)
      fit$log_lik <- stats::logLik(base_model)
    }

    fit$coefficients <- summary(base_model)$coefficients[, 1]
    resid <- stats::residuals(base_model)

    if (use_robust_errors) {
      xv <- as.matrix(pred[, main_vars, drop = FALSE])
      fit$t <- fit$coefficients / HC3(xv, resid)
    } else {
      fit$t <- summary(base_model)$coefficients[, 3]
    }

    fit$random.intercepts <- list()
    for (rV in names(stats::coefficients(base_model))) {
      fit$random.intercepts[[rV]] <- stats::coefficients(base_model)[[rV]][, 1]
    }
  }

  fit$base_model <- base_model
  return(fit)
}


# ---- permutation comparison -------------------------------------------------

#' @keywords internal
#' @noRd
compare_perm_to_baseline <- function(perm_coefs, perm_t, base_fit,
                                     xi = NULL) {
  pres <- rbind(perm_coefs, perm_t)
  bres <- rbind(base_fit$coefficients, base_fit$t)

  out <- list()
  if (is.null(xi)) {
    out$lower  <- pres <= bres
    out$larger <- pres >= bres
    out$abs    <- abs(pres) >= abs(bres)
    out$t      <- perm_t
  } else {
    out$lower  <- (pres <= bres)[, xi]
    out$larger <- (pres >= bres)[, xi]
    out$abs    <- (abs(pres) >= abs(bres))[, xi]
    out$t      <- perm_t[xi]
  }
  return(out)
}


#' @keywords internal
#' @noRd
aggregate_perm_results <- function(results, times) {
  results <- Filter(Negate(is.null), results)
  n_valid <- length(results)
  if (n_valid == 0)
    manynet::snet_abort(
      c("All {times} permutations failed to converge.",
        i = "Try a simpler model, another {.arg family}, or fewer predictors."))
  if (n_valid < times) {
    manynet::snet_warn(
      "{times - n_valid} of {times} permutation{?s} failed and {?was/were} excluded.")
  }
  resL <- unlist(results, recursive = FALSE)
  list(
    lower  = Reduce("+", resL[names(resL) == "lower"],  0) / n_valid,
    larger = Reduce("+", resL[names(resL) == "larger"], 0) / n_valid,
    abs    = Reduce("+", resL[names(resL) == "abs"],    0) / n_valid,
    # The permuted test statistics themselves, one row per permutation that
    # succeeded. The three shares above are summaries of this; keeping it lets a
    # plot draw the null distribution the shares were read from.
    testdist = do.call(rbind, lapply(results, function(r) r$t))
  )
}


# ---- residualisation for permute = "predictor" ---------------------------------------------

#' @keywords internal
#' @noRd
residualise_predictor <- function(xi, pred, main_vars,
                                  has_random = FALSE,
                                  rand_formula = "") {
  bq <- function(v) paste0("`", v, "`")
  others <- setdiff(main_vars, xi)
  modx_str <- paste(bq(xi), "~ 1")
  if (length(others) > 0) {
    modx_str <- paste(modx_str,
                      paste(vapply(others, bq, character(1)),
                            collapse = " + "),
                      sep = " + ")
  }
  if (has_random && nchar(trimws(rand_formula)) > 0) {
    modx_str <- paste(modx_str, rand_formula)
  }
  modx <- stats::as.formula(modx_str)

  if (!has_random) {
    xm <- stats::lm(modx, data = pred)
  } else {
    thisRequires("lme4", "for random effects")
    # Residualising is a step towards the null distribution, not a result the
    # user reads, so a degenerate mixed fit here must not abort the whole run.
    # Crossed sender and receiver intercepts on a predictor are often singular,
    # and `lmer()` then stops with "Downdated VtV is not positive definite".
    xm <- tryCatch(suppressWarnings(lme4::lmer(modx, data = pred)),
                   error = function(e) NULL)
    if (is.null(xm)) {
      manynet::snet_warn(
        c("Could not residualise {.val {xi}} with random intercepts.",
          i = "Residualising it without them instead."))
      xm <- stats::lm(stats::as.formula(
        paste(bq(xi), "~ 1",
              if (length(others) > 0)
                paste("+", paste(vapply(others, bq, character(1)),
                                 collapse = " + ")) else "")),
        data = pred)
    }
  }
  stats::residuals(xm)
}


# A residualised predictor is permuted, so every cell outside the model must
# be NA. A cell that kept the raw predictor would carry a value that is not a
# residual into the model whenever a permutation moved it there, and its NA
# is what `.missing_blocks()` reads to keep the model's dyads in place. An
# undirected network is fitted on its lower triangle, so the residuals are
# mirrored to the upper one, which a permutation also reads from.
#' @keywords internal
#' @noRd
residuals_to_matrix <- function(xR, original_matrix, pred, large = FALSE,
                                directed = TRUE) {
  write_back <- function(original, location, values) {
    out <- original
    out[] <- NA
    out[location] <- values
    .mirror_lower(out, directed)
  }
  if (!large) return(write_back(original_matrix, pred$location, xR))
  out <- original_matrix
  for (net_id in unique(as.character(pred$nv))) {
    idx <- as.character(pred$nv) == net_id
    net_num <- as.integer(net_id)
    out[[net_num]] <- write_back(original_matrix[[net_num]],
                                 pred$location[idx], xR[idx])
  }
  out
}


#' @keywords internal
#' @noRd
residuals_to_array <- function(xR, original_array, valid, pred,
                               large = FALSE, valid_list = NULL,
                               directed = TRUE) {
  write_back <- function(original, valid, values) {
    out <- array(NA, dim = dim(original), dimnames = dimnames(original))
    out[valid] <- values
    for (r in seq_len(dim(out)[3])) {
      out[, , r] <- .mirror_lower(out[, , r], directed)
    }
    out
  }
  if (!large) return(write_back(original_array, valid, xR))
  out <- original_array
  for (gr in seq_along(original_array)) {
    out[[gr]] <- write_back(original_array[[gr]], valid_list[[gr]],
                            xR[pred$nv == gr])
  }
  out
}

#' @keywords internal
#' @noRd
.mirror_lower <- function(m, directed) {
  if (directed || nrow(m) != ncol(m)) return(m)
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  m
}
