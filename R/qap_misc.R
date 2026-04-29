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
      return_res$lower <- (return_res$lower  *  return_res$reps +
                             res[[i + 1]]$lower * res[[i + 1]]$reps) /
        (return_res$reps + res[[i + 1]]$reps)

      return_res$larger <- (return_res$larger  *  return_res$reps +
                              res[[i + 1]]$larger * res[[i + 1]]$reps) /
        (return_res$reps + res[[i + 1]]$reps)

      return_res$abs <- (return_res$abs  *  return_res$reps +
                           res[[i + 1]]$abs * res[[i + 1]]$reps) /
        (return_res$reps + res[[i + 1]]$reps)

      return_res$reps <- return_res$reps + res[[i + 1]]$reps
    }
  } else {
    for (i in 1:(n_res - 1)) {
      for (com in names(return_res$comp)) {
        return_res[[com]]$lower <- (return_res[[com]]$lower *
                                      return_res$reps +
                               res[[i + 1]][[com]]$lower *
                                 res[[i + 1]]$reps) /
          (return_res$reps + res[[i + 1]]$reps)

        return_res[[com]]$larger <- (return_res[[com]]$larger *
                                       return_res$reps +
                                res[[i + 1]][[com]]$larger *
                                  res[[i + 1]]$reps) /
          (return_res$reps + res[[i + 1]]$reps)

        return_res[[com]]$abs <- (return_res[[com]]$abs *
                                    return_res$reps +
                             res[[i + 1]][[com]]$abs *
                               res[[i + 1]]$reps) /
          (return_res$reps + res[[i + 1]]$reps)
      }
      return_res$reps <- return_res$reps + res[[i + 1]]$reps
    }
  }

  return(return_res)
}


#' Convert a long-format edge list to a named list of matrices
#'
#' @description
#' A convenience function for users who have dyadic data in long format
#' (one row per sender-receiver pair) and need to convert it into the
#' named list of matrices expected by [net_regression()].
#'
#' @param df A data frame with one row per dyad.
#' @param sender Name of the column identifying the sender node.
#' @param receiver Name of the column identifying the receiver node.
#' @param perceiver Optional name of a third dimension (perceiver) for
#'   Cognitive Social Structure (CSS) data.  Default `NULL`.
#' @param mode `"directed"` (default) or `"undirected"`.
#' @param loops Logical; include self-loops (diagonal).  Default `FALSE`.
#' @param multi_mode Logical; if `TRUE`, sender and receiver node sets
#'   are treated as distinct (two-mode / bipartite).  Default `FALSE`.
#' @param split_by Optional column name; if supplied, one matrix per
#'   unique value of this column is returned (useful for building a
#'   list-of-networks input).  Default `NULL`.
#' @return A named list.  Each element corresponds to a non-structural
#'   column of `df` and is either a matrix (when `perceiver = NULL`) or a
#'   3-D array.  When `split_by` is set the result is a list of such
#'   named lists.
#' @family models
#' @seealso [net_regression()]
#' @examples
#' df <- data.frame(
#'   from  = c("A", "A", "B", "B", "C", "C"),
#'   to    = c("B", "C", "A", "C", "A", "B"),
#'   weight = c(1, 2, 3, 4, 5, 6)
#' )
#' mats <- net_from_edgelist(df, sender = "from", receiver = "to")
#' mats$weight
#' @export
net_from_edgelist <- function(df,
                              sender,
                              receiver,
                              perceiver  = NULL,
                              mode       = c("directed", "undirected"),
                              loops      = FALSE,
                              multi_mode = FALSE,
                              split_by   = NULL) {
  df_to_mat(df,
            sender    = sender,
            receiver  = receiver,
            perceiver = perceiver,
            mode      = mode,
            loops     = loops,
            multi_mode = multi_mode,
            split_by  = split_by)
}


#' @keywords internal
#' @noRd
df_to_mat <- function(df,
                      sender,
                      receiver,
                      perceiver  = NULL,
                      mode       = c("directed", "undirected"),
                      loops      = FALSE,
                      multi_mode = FALSE,
                      split_by   = NULL) {
  mode <- match.arg(mode)
  var_names <- setdiff(colnames(df), c(sender, receiver, perceiver, split_by))

  if (!is.null(split_by)) {
    result <- lapply(split(df, df[[split_by]]),
                     df_to_mat,
                     sender = sender,
                     receiver = receiver,
                     perceiver = perceiver,
                     mode = mode,
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

  expected <- if (mode == "undirected") {
    if (loops) n_s * (n_s + 1) / 2 else n_s * (n_s - 1) / 2
  } else {
    if (loops) n_s * n_r else n_s * n_r - min(n_s, n_r)
  }
  if (anyNA(df[var_names]) || nrow(df) != expected) {
    warning("Incomplete dyadic data: some cells will be NA.",
            "\nCheck the data and consider coding matrices manually.")
  }

  make_structure <- function(var) {
    if (is.null(perceiver)) {
      mat <- matrix(NA_real_, nrow = n_s, ncol = n_r,
                    dimnames = list(nodes_s, nodes_r))
      mat[cbind(df[[sender]], df[[receiver]])] <- df[[var]]
      if (mode == "undirected")
        mat[cbind(df[[receiver]], df[[sender]])] <- df[[var]]
      if (!loops) diag(mat) <- NA
      mat
    } else {
      arr <- array(NA_real_, dim = c(n_s, n_r, n_p),
                   dimnames = list(nodes_s, nodes_r, nodes_p))
      arr[cbind(df[[sender]], df[[receiver]], df[[perceiver]])] <- df[[var]]
      if (mode == "undirected")
        arr[cbind(df[[receiver]], df[[sender]], df[[perceiver]])] <- df[[var]]
      if (!loops && !multi_mode)
        arr[cbind(nodes_s, nodes_s, rep(nodes_p, each = n_s))] <- NA
      arr
    }
  }

  stats::setNames(lapply(var_names, make_structure), var_names)
}
