# Draws -------------------------------------------------------------------------
#
# Internal. What a permutation set needs so that a draw neither rebuilds the
# data nor refits it from the start. A permutation of the nodes moves the values
# of one matrix, the outcome or a predictor, and nothing else: the dyads the
# model reads stay the same, and so do the other columns. So the plan holds
# the data of the unpermuted networks, the cell that each row reads, and the
# stored values of the matrix that is permuted. A draw is then a permutation of
# those cell positions, read from the stored values, with no permuted matrix and
# no new data frame.
#
# A gaussian model without random or {fixest} effects goes further. Its design
# matrix is built once. Permuting the outcome leaves it unchanged, so it is
# decomposed once and each draw is one solve; permuting a predictor replaces
# one column of it in each draw. Both give the same numbers as `lm()` and
# `summary.lm()`, to the last bit, so a seed gives the same p-values as before.
#
# Where neither holds, `QAPPermEst()` draws and vectorises as it always did.

# The plan for one permutation set, or NULL where a draw must permute the whole
# matrix: a dyadic network that is not a matrix, or a panel of CSS arrays.
#' @keywords internal
#' @noRd
.draw_plan <- function(shape, matlist, target, dep, data_vars, groups, loops,
                       directed, large, exclude, panel, n_obs, mod, family,
                       has_random, fixed_effects, use_robust_errors, main) {
  mats <- if (large) matlist[[target]] else list(matlist[[target]])
  dims <- lapply(mats, dim)
  if (any(lengths(dims) != shape$ndim)) return(NULL)
  if (panel && shape$css) return(NULL)

  vec  <- .vectorise_matlist(shape, matlist, dep, data_vars, groups, loops,
                             directed, large, exclude)
  pred <- vec$pred
  names(pred)[names(pred) == "yv"] <- dep
  if (!is.null(n_obs) && nrow(pred) != n_obs) return(NULL)

  # Which cell each row reads: the same vectoriser, given the position of each
  # cell in place of its value. The missing cells stay missing, so the rows
  # are the rows of `pred`, in its order.
  positions <- lapply(mats, function(m) {
    p <- array(seq_along(m), dim = dim(m))
    p[is.na(m)] <- NA
    p
  })
  at_list <- matlist
  at_list[[target]] <- if (large) positions else positions[[1]]
  at <- .vectorise_matlist(shape, at_list, dep, data_vars, groups, loops,
                           directed, large, exclude)$pred
  cells <- at[[if (target == dep) "yv" else target]]
  net <- if (large) as.integer(as.character(at$nv)) else rep(1L, length(cells))
  at <- lapply(seq_along(mats), function(k) .cell_axes(cells[net == k], dims[[k]]))

  others <- setdiff(intersect(data_vars, names(pred)), target)
  plan <- list(
    target = target,
    dep    = dep,
    large  = large,
    panel  = panel,
    dims   = dims,
    at     = at,
    values = lapply(mats, as.vector),
    pred   = pred,
    # What `.sufficient_data()` asks of the columns a draw does not change.
    others_vary = (target == dep ||
                     length(stats::na.omit(unique(pred[[dep]]))) > 1) &&
      all(vapply(pred[others], function(col) {
        !is.numeric(col) || length(unique(col)) > 1
      }, logical(1)))
  )
  if (!has_random && is.null(fixed_effects)) {
    if (family == "gaussian") {
      plan$solve <- .lm_solver(mod, pred, target, dep, main, use_robust_errors)
    } else if (family %in% c("binomial", "poisson") && !use_robust_errors) {
      plan$solve <- .glm_solver(mod, pred, target, dep, family)
    }
  }
  plan
}

# The row, column, and slice of each cell, so that a draw can find where each
# cell's value now comes from without the matrix.
#' @keywords internal
#' @noRd
.cell_axes <- function(cells, d) {
  c0 <- cells - 1
  nr <- as.double(d[1])
  nc <- as.double(d[2])
  out <- list(i = c0 %% nr + 1, j = (c0 %/% nr) %% nc + 1, nr = nr)
  if (length(d) == 3) {
    out$k   <- c0 %/% (nr * nc) + 1
    out$nrc <- nr * nc
  }
  out
}

# Where each cell's value comes from under a permutation's orders: the cell at
# row i and column j of the permuted matrix holds the original's value at row
# `rows[i]` and column `cols[j]`, as `m[rows, cols]` does.
#' @keywords internal
#' @noRd
.cell_source <- function(o, at) {
  src <- o$rows[at$i] + (o$cols[at$j] - 1) * at$nr
  if (!is.null(at$k)) src <- src + (o$slices[at$k] - 1) * at$nrc
  src
}

# One draw's values of the permuted matrix at the model's cells. It draws the
# orders as `QAPPermEst()` draws them for a whole matrix: once for every wave of
# a panel, and once for each network of a pooled list, in turn. So a seed
# gives the same permutations on either path.
#' @keywords internal
#' @noRd
.draw_values <- function(plan, shape, perm_groups) {
  n <- length(plan$dims)
  orders <- if (plan$panel) {
    rep(list(.perm_orders(plan$dims[[1]], perm_groups)), n)
  } else if (plan$large) {
    per_net <- is.list(perm_groups) && is.null(names(perm_groups))
    lapply(seq_len(n), function(k) {
      shape$orders(plan$dims[[k]], if (per_net) perm_groups[[k]] else perm_groups)
    })
  } else {
    list(shape$orders(plan$dims[[1]], perm_groups))
  }
  vals <- lapply(seq_len(n), function(k) {
    plan$values[[k]][.cell_source(orders[[k]], plan$at[[k]])]
  })
  if (n == 1) vals[[1]] else unlist(vals, use.names = FALSE)
}

# Whether a draw can be fitted, as `.sufficient_data()` asks of the whole data,
# from the one column that the draw changed.
#' @keywords internal
#' @noRd
.draw_varies <- function(plan, v) {
  if (!plan$others_vary) return(FALSE)
  if (plan$target == plan$dep) return(length(stats::na.omit(unique(v))) > 1)
  !is.numeric(v) || length(unique(v)) > 1
}


# ---- the linear solve ---------------------------------------------------------

# A function from one draw's values to the coefficients and t-statistics of
# `lm()`, or NULL where the shortcut does not hold for the set. The design
# matrix is built once, here, as `lm()` builds it. Where the outcome is
# permuted, it is also decomposed once, here; where a predictor is permuted,
# the draw replaces the predictor's one column, and the design is decomposed in
# each draw. The function itself returns NULL for a draw whose design is rank
# deficient, which `lm()` fits by dropping a coefficient, so that the draw is
# refitted from the data.
#
# A predictor that enters the model other than as its own column, in an
# interaction or as a factor, cannot be replaced as one column, so that model
# refits from the data as before. So does a model whose design is rank
# deficient, and one that drops rows.
#' @keywords internal
#' @noRd
.lm_solver <- function(mod, pred, target, dep, main, use_robust_errors) {
  design <- .stored_design(mod, pred, target, dep)
  if (is.null(design)) return(NULL)
  X  <- design$X
  y  <- as.double(design$y)
  xv <- if (use_robust_errors) as.matrix(pred[, main, drop = FALSE])

  if (target == dep) {
    qx <- qr(X)
    if (qx$rank < ncol(X)) return(NULL)
    hc <- if (use_robust_errors) .hc3_design(xv)
    return(function(v) {
      v <- as.double(v)
      .lm_t(qr.coef(qx, v), qr.resid(qx, v), qx, colnames(X), hc)
    })
  }

  col <- design$col
  function(v) {
    X[, col] <- as.double(v)
    z <- stats::lm.fit(X, y)
    if (z$rank < ncol(X)) return(NULL)
    hc <- if (use_robust_errors) {
      xv[, target] <- v
      .hc3_design(xv)
    }
    .lm_t(z$coefficients, z$residuals, z$qr, colnames(X), hc)
  }
}

# The same for `glm()`, for the families whose dispersion is one. Each draw
# runs `glm.fit()` on the stored design, which is what `glm()` runs, and reads
# the t-statistics as `summary.glm()` does.
#' @keywords internal
#' @noRd
.glm_solver <- function(mod, pred, target, dep, family) {
  design <- .stored_design(mod, pred, target, dep)
  if (is.null(design)) return(NULL)
  X <- design$X
  y <- design$y
  col <- design$col
  fam <- get(family, mode = "function", envir = asNamespace("stats"))()
  intercept <- design$intercept
  function(v) {
    if (is.null(col)) y <- v else X[, col] <- as.double(v)
    z <- stats::glm.fit(X, y, family = fam, control = stats::glm.control(),
                        intercept = intercept)
    if (z$rank < ncol(X)) return(NULL)
    p1 <- seq_len(z$rank)
    coef_p <- z$coefficients[z$qr$pivot[p1]]
    covmat <- 1 * chol2inv(z$qr$qr[p1, p1, drop = FALSE])
    list(coefficients = z$coefficients,
         t = coef_p / sqrt(diag(covmat)))
  }
}

# The design matrix and response as `lm()` and `glm()` build them, and, where a
# predictor is permuted, the column it fills. NULL where the model drops rows,
# or where the predictor fills no single column of its own.
#' @keywords internal
#' @noRd
.stored_design <- function(mod, pred, target, dep) {
  mf <- stats::model.frame(mod, pred, drop.unused.levels = TRUE)
  if (nrow(mf) != nrow(pred)) return(NULL)
  tt <- attr(mf, "terms")
  X <- stats::model.matrix(tt, mf)
  col <- NULL
  if (target != dep) {
    col <- .own_column(tt, X, target)
    if (is.null(col) || !is.numeric(pred[[target]])) return(NULL)
  }
  list(X = X, y = stats::model.response(mf, "any"), col = col,
       intercept = attr(tt, "intercept") > 0L)
}

# The coefficients and t-statistics as `summary.lm()` computes them, step for
# step, so that they agree with it to the last bit. With robust errors, the
# t-statistics are those of `.fit_qap_model()`.
#' @keywords internal
#' @noRd
.lm_t <- function(coefs, resid, qx, nm, hc = NULL) {
  names(coefs) <- nm
  p <- qx$rank
  rdf <- length(resid) - p
  out <- list(coefficients = coefs, nobs = length(resid), df.residual = rdf)
  if (!is.null(hc)) {
    out$t <- coefs / .hc3_se(hc, resid)
    return(out)
  }
  resvar <- sum(resid^2) / rdf
  p1 <- seq_len(p)
  R <- chol2inv(qx$qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(R) * resvar)
  out$t <- coefs[qx$pivot[p1]] / se
  out
}

# The one column of the design matrix that a variable fills, or NULL where it
# fills another or more than one.
#' @keywords internal
#' @noRd
.own_column <- function(tt, X, v) {
  fac <- attr(tt, "factors")
  row <- match(v, gsub("`", "", rownames(fac), fixed = TRUE))
  if (is.na(row)) return(NULL)
  uses <- which(fac[row, ] != 0)
  if (length(uses) != 1 ||
      gsub("`", "", colnames(fac)[uses], fixed = TRUE) != v) return(NULL)
  col <- which(attr(X, "assign") == uses)
  if (length(col) != 1) return(NULL)
  col
}
