# Miscellaneous QAP helpers --------------------------------------------------
#
# Internal helpers for combining model estimates across runs and for producing
# dyadic / triadic matrices from long-format data frames.

#' @keywords internal
#' @noRd
combine_qap_estimates <- function(res, res2 = NULL) {
  if (inherits(res, c("QAPCSS", "QAPRegression", "QAPGLM")) &&
      !is.null(res2)) {
    res <- list(res, res2)
  }
  n_res <- length(res)

  return_res <- res[[1]]
  if (is.null(return_res$comp)) {
    for (i in 1:(n_res - 1)) {
      return_res$lower <- (return_res$lower  *  return_res$times +
                             res[[i + 1]]$lower * res[[i + 1]]$times) /
        (return_res$times + res[[i + 1]]$times)

      return_res$larger <- (return_res$larger  *  return_res$times +
                              res[[i + 1]]$larger * res[[i + 1]]$times) /
        (return_res$times + res[[i + 1]]$times)

      return_res$abs <- (return_res$abs  *  return_res$times +
                           res[[i + 1]]$abs * res[[i + 1]]$times) /
        (return_res$times + res[[i + 1]]$times)

      return_res$times <- return_res$times + res[[i + 1]]$times
    }
  } else {
    for (i in 1:(n_res - 1)) {
      for (com in names(return_res$comp)) {
        return_res[[com]]$lower <- (return_res[[com]]$lower *
                                      return_res$times +
                               res[[i + 1]][[com]]$lower *
                                 res[[i + 1]]$times) /
          (return_res$times + res[[i + 1]]$times)

        return_res[[com]]$larger <- (return_res[[com]]$larger *
                                       return_res$times +
                                res[[i + 1]][[com]]$larger *
                                  res[[i + 1]]$times) /
          (return_res$times + res[[i + 1]]$times)

        return_res[[com]]$abs <- (return_res[[com]]$abs *
                                    return_res$times +
                             res[[i + 1]][[com]]$abs *
                               res[[i + 1]]$times) /
          (return_res$times + res[[i + 1]]$times)
      }
      return_res$times <- return_res$times + res[[i + 1]]$times
    }
  }

  return(return_res)
}


#' @keywords internal
#' @noRd
df_to_mat <- function(df,
                      sender,
                      receiver,
                      perceiver  = NULL,
                      directed   = TRUE,
                      loops      = FALSE,
                      multi_mode = FALSE,
                      split_by   = NULL) {
  var_names <- setdiff(colnames(df), c(sender, receiver, perceiver, split_by))

  if (!is.null(split_by)) {
    result <- lapply(split(df, df[[split_by]]),
                     df_to_mat,
                     sender = sender,
                     receiver = receiver,
                     perceiver = perceiver,
                     directed = directed,
                     loops = loops,
                     multi_mode = multi_mode)
    return(purrr::transpose(result))
  }

  if (multi_mode) {
    nodes_s <- unique(df[[sender]])
    nodes_r <- unique(df[[receiver]])
    nodes_p <- if (!is.null(perceiver)) unique(df[[perceiver]]) else NULL
  } else {
    nodes_s <- nodes_r <- nodes_p <- unique(c(
      df[[sender]], df[[receiver]],
      if (!is.null(perceiver)) df[[perceiver]]
    ))
  }
  n_s <- length(nodes_s)
  n_r <- length(nodes_r)
  n_p <- if (!is.null(perceiver)) length(nodes_p) else NULL

  expected <- if (!directed) {
    if (loops) n_s * (n_s + 1) / 2 else n_s * (n_s - 1) / 2
  } else {
    if (loops) n_s * n_r else n_s * n_r - min(n_s, n_r)
  }
  if (anyNA(df[var_names]) || nrow(df) != expected) {
    manynet::snet_warn(
      c("Incomplete dyadic data, so some cells will be {.val NA}.",
        i = "Check the data, or code the matrices manually."))
  }

  make_structure <- function(var) {
    if (is.null(perceiver)) {
      mat <- matrix(NA_real_, nrow = n_s, ncol = n_r,
                    dimnames = list(nodes_s, nodes_r))
      mat[cbind(df[[sender]], df[[receiver]])] <- df[[var]]
      if (!directed)
        mat[cbind(df[[receiver]], df[[sender]])] <- df[[var]]
      if (!loops) diag(mat) <- NA
      mat
    } else {
      arr <- array(NA_real_, dim = c(n_s, n_r, n_p),
                   dimnames = list(nodes_s, nodes_r, nodes_p))
      arr[cbind(df[[sender]], df[[receiver]], df[[perceiver]])] <- df[[var]]
      if (!directed)
        arr[cbind(df[[receiver]], df[[sender]], df[[perceiver]])] <- df[[var]]
      if (!loops && !multi_mode)
        arr[cbind(nodes_s, nodes_s, rep(nodes_p, each = n_s))] <- NA
      arr
    }
  }

  stats::setNames(lapply(var_names, make_structure), var_names)
}


# The fit records directedness as a logical, because that is what
# `manynet::is_directed()` returns and what the engine branches on. Users read
# the word, so the print methods render it here rather than each spelling it.
#' @keywords internal
#' @noRd
.directed_label <- function(directed) {
  if (isTRUE(directed)) "directed" else "undirected"
}
