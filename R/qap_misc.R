# Miscellaneous QAP helpers --------------------------------------------------
#
# Internal helpers for combining model estimates across runs. Building a CSS
# array from long data is `{manynet}`'s job: `as_matrix()` on a network whose
# ties carry a `by` column.

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


# The fit records directedness as a logical, because that is what
# `manynet::is_directed()` returns and what the engine branches on. Users read
# the word, so the print methods render it here rather than each spelling it.
#' @keywords internal
#' @noRd
.directed_label <- function(directed) {
  if (isTRUE(directed)) "directed" else "undirected"
}
