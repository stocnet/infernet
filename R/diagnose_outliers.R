# Finding the outliers of a fitted model ####

#' Finding the outliers of a fitted model
#'
#' @description
#'   `diagnose_outliers()` finds the observations that a model fits least
#'   well. `{infernet}` holds the generic, so that `{goldfish}` and other
#'   packages register methods for their own fits on the same generic, and a
#'   user calls one function for any model.
#'   It has a method for an `infernetMrqapFit`, from [estimate_mrqap()],
#'   which finds the dyads with the most extreme residuals.
#'
#'   [diagnose_changepoints()], [diagnose_onset()], and
#'   [diagnose_confusion()] are diagnostics of the same family.
#' @details
#'   For an `infernetMrqapFit`, `diagnose_outliers()` reads the Pearson
#'   residual of each dyad of the observed model: the residual of a gaussian
#'   model, and the residual over its standard deviation for another family.
#'   - `"Hampel"` flags a dyad whose residual is further than `threshold`
#'     times the scaled median absolute deviation from the median residual.
#'     A network has no order of its dyads, so the Hampel identifier reads
#'     all the dyads together, not in a moving window.
#'   - `"IQR"` flags a dyad whose residual is further than `threshold / 2`
#'     times the interquartile range from the median residual.
#'   - `"Top"` flags the `threshold` dyads with the largest absolute
#'     residuals.
#'
#'   The residuals come from the observed model, which the permutations do
#'   not change, so the result says nothing about the p-values.
#' @param x A fitted model, such as an `infernetMrqapFit` from
#'   [estimate_mrqap()].
#' @param method How to flag an outlier: `"Hampel"` (the default), `"IQR"`,
#'   or `"Top"`, as in `{goldfish}`.
#' @param threshold The number of scaled median absolute deviations for
#'   `"Hampel"`, the number of half interquartile ranges for `"IQR"`, or the
#'   number of dyads to flag for `"Top"`.
#' @param ... Arguments for a method. Not used by the methods here.
#' @return For an `infernetMrqapFit`, an object of class `infernetOutliers`:
#'   a tibble of one row for each dyad of the model, with its `sender` and
#'   `receiver`, its `network` for a list of networks or a panel, its
#'   `reporter` for a cognitive social structure, its `.residual`, whether it
#'   is an `outlier`, and a `label` of the dyad where it is one.
#'   Its attributes `method` and `threshold` record how it was made.
#'   The print shows the outliers only.
#' @family evaluating functions
#' @examples
#' fit <- estimate_mrqap(weight ~ ego(Citations) + alter(Citations),
#'                       manynet::ison_networkers, times = 5)
#' diagnose_outliers(fit, method = "Top", threshold = 5)
#' @name diagnose_outliers
#' @aliases diagnose
NULL

#' @rdname diagnose_outliers
#' @export
diagnose_outliers <- function(x, ...) UseMethod("diagnose_outliers")

#' @rdname diagnose_outliers
#' @export
diagnose_outliers.default <- function(x, ...) {
  .no_diagnose_method("diagnose_outliers", x)
}

#' @rdname diagnose_outliers
#' @export
diagnose_outliers.infernetMrqapFit <- function(x,
                                               method = c("Hampel", "IQR", "Top"),
                                               threshold = 3, ...) {
  method <- match.arg(method)
  if (!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold) ||
      threshold <= 0) {
    manynet::snet_abort("{.arg threshold} must be one positive number.")
  }
  res <- as.numeric(stats::residuals(x, type = "pearson"))
  pred <- x$pred
  if (is.null(pred) || length(res) != nrow(pred)) {
    manynet::snet_abort(
      c("{.fn diagnose_outliers} cannot match the residuals of this model to its dyads.",
        i = "A model whose fixed effects leave out dyads has fewer residuals than dyads."))
  }
  centre <- stats::median(res)
  flagged <- switch(method,
    Hampel = abs(res - centre) > threshold * stats::mad(res, center = centre),
    IQR    = abs(res - centre) > (threshold / 2) * stats::IQR(res),
    Top    = seq_along(res) %in%
      order(abs(res), decreasing = TRUE)[seq_len(min(threshold, length(res)))])
  out <- .dyad_ids(x)
  out$.residual <- res
  out$outlier <- flagged
  out$label <- ifelse(flagged, .dyad_labels(out), NA_character_)
  structure(out, class = c("infernetOutliers", class(out)),
            method = method, threshold = threshold,
            model = paste0(.fit_label(x), " MRQAP of ", x$dep))
}


# The sender, receiver, network, and reporter of each dyad of a fit, by name
# where the fit keeps the names of the nodes.
#' @keywords internal
#' @noRd
.dyad_ids <- function(x) {
  pred <- x$pred
  name_of <- function(i) {
    i <- as.integer(i)
    nm <- x$node_names
    if (is.null(nm) || any(i > length(nm), na.rm = TRUE)) return(i)
    nm[i]
  }
  out <- tibble::tibble(sender = name_of(pred$sv), receiver = name_of(pred$rv))
  if (length(unique(pred$nv)) > 1) out$network <- pred$nv
  if (isTRUE(x$css) && !is.null(pred$bv)) out$reporter <- name_of(pred$bv)
  out
}

#' @keywords internal
#' @noRd
.dyad_labels <- function(ids) {
  lab <- paste(ids$sender, ids$receiver, sep = "-")
  if ("network" %in% names(ids)) lab <- paste0(lab, " [", ids[["network"]], "]")
  if ("reporter" %in% names(ids)) lab <- paste0(lab, " by ", ids[["reporter"]])
  lab
}

#' @export
#' @noRd
print.infernetOutliers <- function(x, ...) {
  n <- sum(x$outlier)
  .print_title(c(paste("Outliers of a", .lower_first(attr(x, "model"))),
                 paste0(n, " of ", nrow(x), " dyads flagged by ",
                        attr(x, "method"), ", with a threshold of ",
                        format(attr(x, "threshold")))))
  if (n > 0) {
    tab <- tibble::as_tibble(unclass(x)[setdiff(names(x), "label")])
    print(tab[x$outlier, , drop = FALSE])
  }
  .print_footer(character(0),
                .methods_hint("the tibble itself for every dyad",
                              "and plot() from {autograph}"))
  invisible(x)
}
