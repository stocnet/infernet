#' Regression for network data
#'
#' @description
#' Fits a regression model to one (or a list of) manynet-compatible network(s)
#' using the multiple regression quadratic assignment procedure (MRQAP).
#'
#' The formula front-end is the familiar
#' `y ~ ego(attr) + alter(attr) + same(attr) + dist(attr) + sim(attr) +
#' tertius(attr, fn)` syntax, plus plain references to other networks in the
#' object. Internally the response and predictors are packed into matrices
#' and handed to a QAP engine (ported from MrQAP) that supports:
#'
#' - gaussian, binomial, poisson, negative binomial, and zero-inflated
#'   Poisson families;
#' - two permutation schemes: `"predictor"` (Dekker's double semi-partialling)
#'   and `"outcome"`;
#' - random intercepts (lme4);
#' - robust (HC3) standard errors;
#' - lists of networks, in which graphs that are missing any predictor are
#'   dropped with a warning and the remaining networks are pooled.
#'
#' @param formula A formula describing the model.  The left-hand side is the
#'   dependent network (e.g. `weight` for a weighted graph, or `.` for an
#'   unweighted graph).  The right-hand side accepts:
#'   - `ego(attr)` — matrix of the sender's value of a node attribute,
#'   - `alter(attr)` — matrix of the receiver's value,
#'   - `same(attr)` — 1 where sender and receiver share an attribute value,
#'   - `dist(attr)` — absolute difference in a numeric attribute,
#'   - `sim(attr)` — proportional similarity in a numeric attribute,
#'   - `tertius(attr, fn)` — aggregate of an attribute across a node's
#'     other ties (`fn` is `"mean"` or `"sum"`),
#'   - plain tie-attribute names, which pull in a second network as a
#'     dyadic covariate.
#' @param .data A manynet-consistent network (see
#'   [manynet::as_tidygraph()]), or a list of such networks.  When a list is
#'   supplied the model is fit jointly; graphs missing any predictor are
#'   dropped with a warning.
#' @param times Integer.  Number of permutations for the null distribution.
#'   1000 is the default; publication-ready work usually needs 1000-10000.
#' @param control Named list of additional controls; unspecified entries fall
#'   back to the defaults below.
#'   - `permute`: what the null distribution permutes. `"predictor"` (the
#'     default) residualises each main predictor against the others and
#'     permutes that residual, following Dekker et al. (2007). `"outcome"`
#'     permutes the dependent matrix and leaves the predictors alone. With one
#'     predictor there is nothing to residualise against, so `"predictor"`
#'     reduces to `"outcome"` and says so.
#'   - `strategy`: future plan, e.g. `"sequential"` (default), `"multisession"`.
#'   - `family`: `"auto"` (default; gaussian for weighted networks, binomial
#'     for binary), `"gaussian"`, `"binomial"`, `"poisson"`, `"negbin"`,
#'     or `"zip"`.
#'   - `directed`: logical, whether a tie from i to j differs from one from j
#'     to i. Read from `.data` unless given, and reported when read.
#'   - `diag`: logical, include loops (default auto-detected).
#'   - `seed`, `groups`, `ncores`: passed through to the engine.
#'   - `use_robust_errors`: HC3 standard errors.
#'   - `random_intercept_nets` / `_sender` / `_receiver`: lme4-style REs.
#'   - `less_mem`: drop the baseline model object from the return.
#' @return An object of class `net_regression` inheriting from either
#'   `QAPRegression` (gaussian) or `QAPGLM` (other families).  When the
#'   outcome is binary -- either `family = "binomial"` or `"gaussian"` with
#'   a 0/1 dependent -- a probabilistic confusion matrix is attached as
#'   `$confusion_matrix`; for the gaussian-binary case the linear
#'   probability fitted values are clamped to 0/1 before sampling.
#' @name regression
#' @family models
#' @references
#'   Krackhardt, David. 1988.
#'   "Predicting with Networks: Nonparametric Multiple Regression Analysis
#'   of Dyadic Data."
#'   _Social Networks_ 10(4):359-81.
#'   \doi{10.1016/0378-8733(88)90004-4}.
#'
#'   Dekker, David, David Krackhardt, and Tom A. B. Snijders. 2007.
#'   "Sensitivity of MRQAP tests to collinearity and autocorrelation
#'   conditions."
#'   _Psychometrika_ 72(4): 563-581.
#'   \doi{10.1007/s11336-007-9016-1}.
#' @examples
#' \dontrun{
#' networkers <- manynet::ison_networkers |>
#'   manynet::to_subgraph(Discipline == "Sociology")
#' model1 <- net_regression(
#'   weight ~ ego(Citations) + alter(Citations) + sim(Citations),
#'   networkers, times = 20)
#' print(model1)
#' }
#' @export
net_regression <- function(formula,
                           .data,
                           times   = 1000,
                           control = list()) {

  ctrl <- .resolve_control(control)

  if (.is_list_of_graphs(.data)) {
    prepared <- .prepare_list_of_graphs(formula, .data)
    matlist   <- prepared$matlist
    formula <- prepared$formula
    first_graph <- prepared$first_graph
  } else {
    ml <- convertToMatrixList(formula, .data)
    matlist  <- ml$mydata
    formula <- ml$formula
    first_graph <- manynet::as_tidygraph(.data)
  }

  dep <- .dep_name(formula)

  # Each of these is resolved from the data rather than stated by the user, so
  # each is reported. A defaulted family or a fallback that nobody sees is a
  # model the user cannot describe in a paper.
  if (identical(ctrl$family, "auto")) {
    ctrl$family <- if (.is_binary_outcome(matlist[[dep]])) "binomial" else "gaussian"
    manynet::snet_info(
      "Treating the outcome as {.val {ctrl$family}}, from its values.")
  }
  user_requested_gaussian_binary <-
    identical(ctrl$family, "gaussian") && .is_binary_outcome(matlist[[dep]])

  if (is.null(ctrl$directed)) {
    ctrl$directed <- manynet::is_directed(first_graph)
    # `{cli}` reads a brace expression beginning with a dot as a style, so a
    # call to a dot-prefixed function has to be resolved before interpolation.
    direction <- .directed_label(ctrl$directed)
    manynet::snet_info("Reading the network as {.val {direction}}.")
  }
  if (is.null(ctrl$diag)) {
    ctrl$diag <- isTRUE(manynet::is_complex(first_graph))
  }

  fit <- QAPengine(
    formula   = formula,
    matlist   = matlist,
    css       = FALSE,
    family    = ctrl$family,
    directed  = ctrl$directed,
    diag      = ctrl$diag,
    permute   = ctrl$permute,
    times      = times,
    seed      = ctrl$seed,
    groups    = ctrl$groups,
    strategy  = ctrl$strategy,
    ncores    = ctrl$ncores,
    random_intercept_nets     = ctrl$random_intercept_nets,
    random_intercept_sender   = ctrl$random_intercept_sender,
    random_intercept_receiver = ctrl$random_intercept_receiver,
    use_robust_errors = ctrl$use_robust_errors,
    less_mem = ctrl$less_mem
  )

  if (user_requested_gaussian_binary) {
    fit$confusion_matrix <- .lpm_confusion_matrix(fit, ctrl$seed)
  }

  class(fit) <- c("net_regression", class(fit))
  fit
}


# ---- default control -------------------------------------------------------

# Merges the user's list over the defaults. A name that is not a control is
# rejected rather than added silently: a misspelt name would otherwise leave the
# option it was meant to set at its default, with nothing to say so.
#' @keywords internal
#' @noRd
.resolve_control <- function(control = list()) {
  ctrl <- .default_control()
  if (length(control) == 0) {
    ctrl$permute <- match.arg(ctrl$permute, choices = .permute_schemes())
    return(ctrl)
  }
  if (is.null(names(control)) || any(!nzchar(names(control)))) {
    manynet::snet_abort("Every entry of {.arg control} must be named.")
  }
  unknown <- setdiff(names(control), names(ctrl))
  if (length(unknown) > 0) {
    near <- vapply(unknown, function(u) {
      d <- utils::adist(u, names(ctrl), ignore.case = TRUE)[1, ]
      if (min(d) <= max(2, nchar(u) %/% 3)) names(ctrl)[which.min(d)] else NA_character_
    }, character(1))
    msg <- c("{.arg control} does not take {.val {unknown}}.")
    if (any(!is.na(near))) {
      msg <- c(msg, i = "Did you mean {.val {unname(near[!is.na(near)])}}?")
    }
    msg <- c(msg, i = "Available controls: {.val {names(ctrl)}}.")
    manynet::snet_abort(msg)
  }
  ctrl[names(control)] <- control
  ctrl$permute <- match.arg(ctrl$permute, choices = .permute_schemes())
  ctrl
}

# What the permutation scheme permutes, which is the only thing that separates
# the two. "predictor" is Dekker et al's double semi-partialling: each main
# predictor is residualised against the others and that residual is permuted.
# "outcome" permutes the dependent matrix and leaves the predictors alone.
#' @keywords internal
#' @noRd
.permute_schemes <- function() c("predictor", "outcome")

#' @keywords internal
#' @noRd
.permute_label <- function(permute) {
  switch(permute,
         predictor = "each predictor's residuals (Dekker's double semi-partialling)",
         outcome   = "the outcome only")
}

.default_control <- function() {
  list(
    permute   = .permute_schemes(),
    strategy  = "sequential",
    family    = "auto",
    directed  = NULL,
    diag      = NULL,
    seed      = NULL,
    groups    = NULL,
    ncores    = NULL,
    use_robust_errors = FALSE,
    random_intercept_nets     = FALSE,
    random_intercept_sender   = FALSE,
    random_intercept_receiver = FALSE,
    less_mem = FALSE
  )
}


# ---- input dispatch --------------------------------------------------------

.is_list_of_graphs <- function(x) {
  if (!is.list(x)) return(FALSE)
  if (inherits(x, c("igraph", "tbl_graph", "network", "data.frame")))
    return(FALSE)
  if (length(x) == 0) return(FALSE)
  first <- x[[1]]
  inherits(first, c("igraph", "tbl_graph", "network")) || is.matrix(first)
}


.prepare_list_of_graphs <- function(formula, glist) {
  ml_list <- vector("list", length(glist))
  fail_idx <- integer(0)
  fail_reason <- character(0)

  for (i in seq_along(glist)) {
    res <- tryCatch(
      convertToMatrixList(formula, glist[[i]], advise = FALSE),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(res$error)) {
      fail_idx <- c(fail_idx, i)
      fail_reason <- c(fail_reason, res$error)
      ml_list[[i]] <- NULL
    } else {
      ml_list[[i]] <- res
    }
  }

  keep <- setdiff(seq_along(glist), fail_idx)
  if (length(keep) == 0) {
    manynet::snet_abort(
      c("None of the supplied networks could be converted for this formula.",
        stats::setNames(unique(fail_reason), rep("x", length(unique(fail_reason))))))
  }

  ref_names <- names(ml_list[[keep[1]]]$mydata)
  mismatched <- integer(0)
  for (i in keep) {
    if (!setequal(names(ml_list[[i]]$mydata), ref_names)) {
      mismatched <- c(mismatched, i)
    }
  }
  dropped <- sort(unique(c(fail_idx, mismatched)))
  keep <- setdiff(seq_along(glist), dropped)

  if (length(dropped) > 0) {
    nms <- names(glist)
    dropped_labels <- if (!is.null(nms) && all(nzchar(nms[dropped]))) {
      nms[dropped]
    } else {
      paste0("[", dropped, "]")
    }
    manynet::snet_warn(
      "Dropping {length(dropped)} network{?s} missing one or more predictors: {.val {dropped_labels}}.")
  }
  if (length(keep) == 0) {
    manynet::snet_abort("All the supplied networks were dropped for missing predictors.")
  }

  kept_mls <- ml_list[keep]
  ref_names <- names(kept_mls[[1]]$mydata)

  matlist <- vector("list", length(ref_names))
  names(matlist) <- ref_names
  for (nm in ref_names) {
    matlist[[nm]] <- lapply(kept_mls, function(ml) ml$mydata[[nm]])
  }

  first_graph <- manynet::as_tidygraph(glist[[keep[1]]])
  specificationAdvice(getRHSNames(formula)$IVnames, first_graph)

  list(
    matlist        = matlist,
    formula     = kept_mls[[1]]$formula,
    first_graph = first_graph
  )
}


# ---- helpers ---------------------------------------------------------------

.dep_name <- function(formula) {
  v <- all.vars(formula)
  if (length(v) == 0) return(NA_character_)
  v[1]
}


.is_binary_outcome <- function(y) {
  vals <- if (is.list(y)) unlist(y, use.names = FALSE) else as.vector(y)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(FALSE)
  all(vals %in% c(0, 1))
}


.lpm_confusion_matrix <- function(fit, seed) {
  bm <- fit$simple_fit
  if (is.null(bm)) return(NULL)
  actual <- fit$pred[[fit$dep]]
  predicted <- stats::fitted(bm)
  predicted <- pmin(pmax(predicted, 0), 1)
  probabilistic_confusion_matrix(
    actual = as.integer(actual),
    predicted_prob = predicted,
    n_draws = 1000,
    seed = seed
  )
}


# ---- print method ----------------------------------------------------------

#' @rdname regression
#' @param x A `net_regression` object.
#' @param ... Additional arguments passed to the underlying print helpers.
#' @param print_b Logical; also print coefficient-based (not t-based)
#'   permutation p-values.
#' @param print_random Logical; print random intercepts when present.
#' @export
print.net_regression <- function(x, ...,
                                 print_b = FALSE,
                                 print_random = FALSE) {
  if (inherits(x, "QAPRegression")) {
    .print_qap_regression(x, print_b = print_b, print_random = print_random)
  } else if (inherits(x, "QAPGLM")) {
    .print_qap_glm(x, print_b = print_b, print_random = print_random)
  } else {
    cat("<net_regression>\n")
    utils::str(x, max.level = 1)
  }
  invisible(x)
}


.print_qap_regression <- function(x, print_b = FALSE, print_random = FALSE) {
  if (is.null(x$random.intercepts)) {
    cat("\nOLS Network Model\n\n")
  } else {
    cat("\nLinear Mixed Network Model fit by REML\n\n")
  }

  if (!is.null(x$groups))
    cat("Permutations were performed within groups only.\n")

  if (x$permute == "outcome")
    cat("The outcome matrix Y was permuted", format(x$times), "times.\n")
  if (x$permute == "predictor") {
    cat("Significance was estimated using Dekker's\n")
    cat("  'semi-partialling plus' procedure with",
        format(x$times), "permutations.\n")
  }

  if (x$diag) {
    cat("Diagonal values (loops) were used in the estimation.\n")
  } else {
    cat("Diagonal values (loops) were ignored.\n")
  }
  cat("The outcome was treated as",
      format(paste0(.directed_label(x$directed), ".")), "\n")

  if (!is.null(x$r.squared)) {
    cat("\nR-squared:    ", format(round(x$r.squared, 4)))
    cat("\nAdj R-squared:", format(round(x$adj.r.squared, 4)), "\n")
  }

  cat("\nCoefficients:\n")
  if (print_b) {
    cmat <- cbind(format(as.numeric(x$coefficients)),
                  format(x$lower[1, ]),
                  format(x$larger[1, ]),
                  format(x$abs[1, ]))
    colnames(cmat) <- c("Estimate", "Pr(<=b)", "Pr(>=b)", "Pr(>=|b|)")
    rownames(cmat) <- names(x$coefficients)
    if (x$permute == "predictor") cmat[1, 2:4] <- "*"
    print.table(cmat)
    cat("\n--------------\n")
  }

  cmat <- cbind(format(as.numeric(x$coefficients)),
                format(x$lower[2, ]),
                format(x$larger[2, ]),
                format(x$abs[2, ]))
  colnames(cmat) <- c("Estimate", "Pr(<=t)", "Pr(>=t)", "Pr(>=|t|)")
  rownames(cmat) <- names(x$coefficients)
  if (x$permute == "predictor") cmat[1, 2:4] <- "*"
  print.table(cmat)

  if (x$permute == "predictor")
    cat("\n* The intercept has no significance test when predictors are permuted.\n")

  cat("\n--------------\n")

  if (!is.null(x$random.intercepts) && print_random) {
    cmat <- matrix(round(x$random.intercepts, 3),
                   nrow = length(x$random.intercepts), ncol = 1)
    colnames(cmat) <- "Random Intercepts:"
    print.table(cmat)
    cat("\n--------------\n")
  }

  if (!is.null(x$confusion_matrix)) {
    cat("\n")
    print(x$confusion_matrix)
  }
  cat("\n")
}


.print_qap_glm <- function(x, print_b = FALSE, print_random = FALSE) {
  if (is.null(x$random.intercepts)) {
    cat("\nGeneralized Linear Network Model\n")
  } else {
    cat("\nGeneralized Linear Mixed Network Model fit by REML\n")
  }
  if (!is.null(x$theta))
    cat("\nNegative binomial dispersion (theta):", format(round(x$theta, 4)))
  if (!is.null(x$zi_coefficients)) {
    cat("\nZero-inflation coefficients:")
    cat("\n  ", paste(names(x$zi_coefficients),
                      format(round(x$zi_coefficients, 4)),
                      sep = " = ", collapse = ", "))
  }
  if (!is.null(x$groups))
    cat("\nPermutations were performed within groups only.")

  if (x$permute == "outcome")
    cat("\nThe outcome matrix Y was permuted", format(x$times), "times.")
  if (x$permute == "predictor") {
    cat("\nSignificance was estimated using Dekker's")
    cat("\n  'semi-partialling plus' procedure with",
        format(x$times), "permutations.")
  }

  if (x$diag) {
    cat("\nDiagonal values (loops) were used in the estimation.")
  } else {
    cat("\nDiagonal values (loops) were ignored.")
  }
  cat("\nThe outcome was treated as",
      format(paste0(.directed_label(x$directed), ".")))
  cat("\nModel family:", format(x$family))

  cat("\n\nCoefficients:\n")
  if (print_b) {
    cmat <- matrix(NA, nrow = length(x$coefficients), ncol = 5)
    cmat[, 1] <- format(as.numeric(x$coefficients))
    cmat[, 2] <- format(exp(as.numeric(x$coefficients)))
    cmat[, 3] <- format(x$lower[1, ])
    cmat[, 4] <- format(x$larger[1, ])
    cmat[, 5] <- format(x$abs[1, ])
    if (x$permute == "predictor") cmat[1, 3:5] <- "*"
    colnames(cmat) <- c("Estimate", "Exp(b)", "Pr(<=b)", "Pr(>=b)", "Pr(>=|b|)")
    rownames(cmat) <- names(x$coefficients)
    print.table(cmat)
    cat("--------------\n")
  }

  cmat <- matrix(NA, nrow = length(x$coefficients), ncol = 5)
  cmat[, 1] <- format(as.numeric(x$coefficients))
  cmat[, 2] <- format(exp(as.numeric(x$coefficients)))
  cmat[, 3] <- format(x$lower[2, ])
  cmat[, 4] <- format(x$larger[2, ])
  cmat[, 5] <- format(x$abs[2, ])
  if (x$permute == "predictor") cmat[1, 3:5] <- "*"
  colnames(cmat) <- c("Estimate", "Exp(b)", "Pr(<=t)", "Pr(>=t)", "Pr(>=|t|)")
  rownames(cmat) <- names(x$coefficients)
  print.table(cmat)

  if (x$permute == "predictor")
    cat("\n* The intercept has no significance test when predictors are permuted.\n")

  cat("--------------\n")

  if (!is.null(x$random.intercepts) && print_random) {
    cmat <- matrix(round(x$random.intercepts, 3),
                   nrow = length(x$random.intercepts), ncol = 1)
    colnames(cmat) <- "Random Intercepts:"
    print.table(cmat)
    cat("--------------\n")
  }

  if (!is.null(x$simple_fit)) {
    cat("\nAIC:", format(stats::AIC(x$simple_fit)))
    cat("\nBIC:", format(stats::BIC(x$simple_fit)))
  }

  if (!is.null(x$confusion_matrix)) {
    cat("\n\n")
    print(x$confusion_matrix)
  }
  cat("\n")
}


.print_glm_table <- function(base, lower, larger, abs_mat, permute, print_b) {
  cat("\n\nCoefficients:\n")
  nc <- length(base$coefficients)
  cmat <- matrix(NA, nrow = nc, ncol = 4)
  cmat[, 1] <- format(round(as.numeric(base$coefficients), 4))
  cmat[, 2] <- format(lower[2, ])
  cmat[, 3] <- format(larger[2, ])
  cmat[, 4] <- format(abs_mat[2, ])
  if (permute == "predictor") cmat[1, 2:4] <- "*"
  colnames(cmat) <- c("Estimate", "Pr(<=t)", "Pr(>=t)", "Pr(>=|t|)")
  rownames(cmat) <- names(base$coefficients)
  print.table(cmat)
}


# ============================================================================
# Formula -> matrix-list front end
# ============================================================================

#' @keywords internal
#' @noRd
convertToMatrixList <- function(formula, .data, advise = TRUE) {
  net <- manynet::as_tidygraph(.data)
  DV <- manynet::as_matrix(net)
  # The sender and the receiver of a tie come from one nodeset in a one-mode
  # network and from two in a two-mode one, so a dyadic term must read the
  # attribute once per mode rather than once per network.
  side_matrices <- function(attrib, DV, twomode, type) {
    if (twomode) {
      rows <- matrix(attrib[!type], nrow(DV), ncol(DV))
      cols <- matrix(attrib[type], nrow(DV), ncol(DV), byrow = TRUE)
    } else {
      rows <- matrix(attrib, nrow(DV), ncol(DV))
      cols <- matrix(attrib, nrow(DV), ncol(DV), byrow = TRUE)
    }
    list(rows = rows, cols = cols)
  }
  twomode <- manynet::is_twomode(net)
  node_type <- if (twomode) manynet::node_attribute(net, "type") else NULL

  names_form <- getRHSNames(formula)
  .check_formula_vars(names_form$IVnames, net)
  if (advise) specificationAdvice(names_form$IVnames, net)
  IVs <- lapply(names_form$IVnames, function(IV) {
    out <- lapply(seq_along(IV), function(elem) {
      if (IV[[elem]][1] == "ego") {
        vct <- manynet::node_attribute(net, IV[[elem]][2])
        if (manynet::is_twomode(net)) {
          vct <- vct[!manynet::node_attribute(net, "type")]
        }
        out <- matrix(vct, nrow(DV), ncol(DV))
        out <- list(out)
        names(out) <- paste(IV[[elem]], collapse = " ")
        out
      } else if (IV[[elem]][1] == "alter") {
        vct <- manynet::node_attribute(net, IV[[elem]][2])
        if (manynet::is_twomode(net)) {
          vct <- vct[manynet::node_attribute(net, "type")]
        }
        out <- matrix(vct, nrow(DV), ncol(DV), byrow = TRUE)
        out <- list(out)
        names(out) <- paste(IV[[elem]], collapse = " ")
        out
      } else if (IV[[elem]][1] == "same") {
        attrib <- manynet::node_attribute(net, IV[[elem]][2])
        if (manynet::is_twomode(.data)) {
          if (all(is.na(attrib[!manynet::node_is_mode(.data)]))) {
            attrib <- attrib[manynet::node_is_mode(.data)]
            out <- vapply(1:length(attrib), function(x) {
              held_out <- manynet::as_matrix(
                manynet::delete_nodes(.data, manynet::net_dims(.data)[1] + x))
              rowSums(held_out * matrix((attrib[-x] == attrib[x]) * 1,
                                        nrow(DV), ncol(DV) - 1, byrow = TRUE)) /
                rowSums(held_out)
            }, FUN.VALUE = numeric(nrow(DV)))
            out[is.nan(out)] <- 0
          } else {
            attrib <- attrib[!manynet::node_is_mode(.data)]
            out <- t(vapply(1:length(attrib), function(x) {
              held_out <- manynet::as_matrix(manynet::delete_nodes(.data, x))
              colSums(held_out * matrix((attrib[-x] == attrib[x]) * 1,
                                        nrow(DV) - 1, ncol(DV))) /
                colSums(held_out)
            }, FUN.VALUE = numeric(ncol(DV))))
            out[is.nan(out)] <- 0
          }
        } else {
          rows <- matrix(attrib, nrow(DV), ncol(DV))
          cols <- matrix(attrib, nrow(DV), ncol(DV), byrow = TRUE)
          out <- (rows == cols) * 1
        }
        out <- list(out)
        names(out) <- paste(IV[[elem]], collapse = " ")
        out
      } else if (IV[[elem]][1] == "dist") {
        if (is.character(manynet::node_attribute(net, IV[[elem]][2]))) {
          manynet::snet_abort(
            c("{.fn dist} is undefined for a categorical attribute.",
              i = "Try {.fn same} instead."))
        }
        sides <- side_matrices(manynet::node_attribute(net, IV[[elem]][2]),
                               DV, twomode, node_type)
        rows <- sides$rows
        cols <- sides$cols
        out <- abs(rows - cols)
        out <- list(out)
        names(out) <- paste(IV[[elem]], collapse = " ")
        out
      } else if (IV[[elem]][1] == "sim") {
        if (is.character(manynet::node_attribute(net, IV[[elem]][2]))) {
          manynet::snet_abort(
            c("{.fn sim} is undefined for a categorical attribute.",
              i = "Try {.fn same} instead."))
        }
        sides <- side_matrices(manynet::node_attribute(net, IV[[elem]][2]),
                               DV, twomode, node_type)
        rows <- sides$rows
        cols <- sides$cols
        denom <- max(abs(rows - cols), na.rm = TRUE)
        if (!is.finite(denom) || denom == 0) denom <- 1
        out <- abs(1 - abs(rows - cols) / denom)
        out <- list(out)
        names(out) <- paste(IV[[elem]], collapse = " ")
        out
      } else if (IV[[elem]][1] == "tertius") {
        vct <- manynet::node_attribute(net, IV[[elem]][2])
        if (manynet::is_twomode(net)) {
          vct <- vct[!manynet::node_attribute(net, "type")]
        }
        val <- matrix(vct, nrow(DV), ncol(DV)) * DV
        if (is.na(IV[[elem]][3])) {
          IV[[elem]][3] <- "mean"
        }
        # The deparsed term keeps the quotation marks of `tertius(x, "mean")`,
        # so strip them; otherwise only the unquoted spelling is recognised.
        IV[[elem]][3] <- gsub('^"|"$', "", IV[[elem]][3])
        out <- t(vapply(seq_len(nrow(DV)),
                        function(x) {
                          if (IV[[elem]][3] == "mean") {
                            colMeans(val[-x, ], na.rm = TRUE)
                          } else if (IV[[elem]][3] == "sum") {
                            colSums(val[-x, ], na.rm = TRUE)
                          } else {
                            manynet::snet_abort(
                              "{.fn tertius} takes {.val mean} or {.val sum}, not {.val {IV[[elem]][3]}}.")
                          }
                        },
                        FUN.VALUE = numeric(ncol(DV))))
        out <- list(out)
        names(out) <- paste(IV[[elem]][1:2], collapse = " ")
        out
      } else {
        if (IV[[elem]][1] %in% manynet::net_tie_attributes(net)) {
          out <- manynet::as_matrix(manynet::to_uniplex(net,
                                                       tie = IV[[elem]][1]))
          out <- list(out)
          names(out) <- IV[[elem]][1]
          out
        } else {
          manynet::snet_abort("Predictor {.val {IV[[elem]][1]}} not found in the network.")
        }
      }
    })
    if (length(out) == 2) {
      namo <- paste(vapply(out, names, FUN.VALUE = character(1)),
                    collapse = ":")
      out <- list(out[[1]][[1]] * out[[2]][[1]])
      names(out) <- namo
      out
    } else {
      if (is.list(out[[1]])) {
        out[[1]]
      } else {
        out <- list(out[[1]])
        names(out) <- attr(out[[1]], "names")[1]
        attr(out[[1]], "names") <- NULL
        out
      }
    }
  })
  IVs <- purrr::flatten(IVs)
  out <- c(list(DV), IVs)

  DVname <- formula[[2]]
  out_formula <- names_form$formula
  if (DVname == ".") {
    DVname <- "ties"
    out_formula[[2]] <- as.name("ties")
  }
  names(out)[1] <- as.character(DVname)

  return(list(mydata = out,
              formula = out_formula))
}


#' @keywords internal
#' @noRd
getRHSNames <- function(formula) {
  rhs <- c(attr(stats::terms(formula), "term.labels"))
  rhs <- strsplit(rhs, ":")
  rhs <- lapply(rhs, function(term) {
    strsplit(term, "\\|")
  })
  if (!is.list(rhs)) {
    rhs <- list(rhs)
  }
  if (any(grepl("\\|", formula))) {
    rand <- c(1:length(rhs))[unlist(lapply(rhs, function(term) {
      length(term[[1]]) > 1
    }))]
    rhs2 <- rhs[c(1:length(rhs))[-rand]]
    rhsn <- lapply(rhs2, function(term) {
      strsplit(gsub("\\)", "", term), "\\(|,|, ")
    })
    term_names <- lapply(rhsn, function(term) {
      paste0("`", term[[1]][1], " ", term[[1]][2], "`")
    })
    form <- paste(paste(formula[[2]], "~"),
                  paste(term_names, collapse = " + "))
    for (term in rand) {
      form <- paste(form, "+", "(")
      for (i in 1:length(rhs[[term]][[1]])) {
        string <- gsub("[[:space:]]", "", rhs[[term]][[1]][[i]])
        string <- strsplit(string, "\\+")[[1]]

        for (j in 1:length(string)) {
          n <- strsplit(gsub("\\)", "", string[j]), "\\(|,|, ")
          if (i == 1) {
            if (length(n[[1]]) == 1) {
              form <- paste0(form, n)
            } else {
              n <- paste0("`", paste(n[[1]], collapse = " "), "`")
              form <- paste(form, n)
            }
            if (j < length(string)) {
              form <- paste(form, "+")
            }
          } else {
            n <- paste0("`", paste(n[[1]], collapse = " "), "`")
            form <- paste(form, "|", n)
          }
          if (string[j] != "1" && !(string[j] %in% unlist(rhs2))) {
            rhs2 <- append(rhs2, string[j])
          }
        }
      }
      form <- paste0(form, ")")
    }
    rhs <- rhs2
  }
  rhsn <- lapply(rhs, function(term) {
    strsplit(gsub("\\)", "", term), "\\(|,|, ")
  })

  if (!any(grepl("\\|", formula))) {
    form <- paste(paste(formula[[2]], "~"),
                  paste(lapply(rhsn, function(term) {
                    paste0("`", term[[1]][1], " ", term[[1]][2], "`")
                  }), collapse = " + "))
  }
  return(list(IVnames = rhsn,
              formula = stats::as.formula(form)))
}


#' @keywords internal
#' @noRd
.check_formula_vars <- function(IVnames, net) {
  node_fns <- c("ego", "alter", "same", "dist", "sim", "tertius")
  node_attrs <- manynet::net_node_attributes(net)
  # The engine builds these columns itself in `make_qap_data()`: the sender, the
  # receiver, the network, and the perceiver index. They are what a user names
  # after a `|` to absorb sender or receiver fixed effects, so they are not
  # attributes of the network and must not be looked for among them.
  tie_attrs  <- c(manynet::net_tie_attributes(net), .structural_vars())

  missing_node <- character(0)
  missing_tie  <- character(0)

  for (IV in IVnames) {
    for (term in IV) {
      fn  <- term[1]
      arg <- term[2]
      if (fn %in% node_fns) {
        if (is.na(arg) || !(arg %in% node_attrs))
          missing_node <- c(missing_node, arg)
      } else {
        if (!(fn %in% tie_attrs))
          missing_tie <- c(missing_tie, fn)
      }
    }
  }

  if (length(missing_node) > 0) {
    manynet::snet_abort(
      c("Node attribute{?s} {.val {unique(missing_node)}} not found.",
        i = "Available node attributes: {.val {node_attrs}}."))
  }
  if (length(missing_tie) > 0) {
    available   <- manynet::net_tie_attributes(net)
    structurals <- .structural_vars()
    manynet::snet_abort(
      c("Tie attribute or predictor{?s} {.val {unique(missing_tie)}} not found.",
        i = "Available tie attributes: {.val {available}}.",
        i = "Sender, receiver, and network indices are also available as {.val {structurals}}."))
  }
  invisible(TRUE)
}

# Columns the engine builds for every dyad, rather than reads off the network.
#' @keywords internal
#' @noRd
.structural_vars <- function() c("sv", "rv", "nv", "pv")


#' @keywords internal
#' @noRd
getDependentName <- function(formula) {
  dep <- list(formula[[2]])
  unlist(lapply(dep, deparse))
}


#' @keywords internal
#' @noRd
specificationAdvice <- function(formula, net) {
  formdf <- t(data.frame(formula))
  if (any(formdf[, 1] %in% c("sim", "same"))) {
    vars <- formdf[formdf[, 1] %in% c("sim", "same"), 2]
    suggests <- vapply(vars, function(x) {
      incl <- unname(formdf[formdf[, 2] == x, 1])
      if (manynet::is_twomode(net)) {
        excl <- setdiff(c("ego", "tertius"), incl)
      } else excl <- setdiff(c("ego", "alter"), incl)
      if (length(excl) > 0) {
        paste0(excl, "(", x, ")", collapse = ", ")
      } else {
        NA_character_
      }
    }, FUN.VALUE = character(1))
    suggests <- suggests[!is.na(suggests)]
    if (!manynet::is_directed(net)) {
      suggests <- suggests[!grepl("ego\\(", suggests)]
    }
    if (length(suggests) > 0) {
      if (length(suggests) > 1) {
        suggests <- paste0(suggests, collapse = ", ")
      }
      manynet::snet_info(
        "When testing for homophily, include all the more fundamental effects.",
        "Try adding {suggests} to the model specification.")
    }
  }
}
