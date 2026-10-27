# Simulating diffusions ####

#' Simulating many diffusions
#' @name play_diffusions
#' @description
#'   `play_diffusions()` runs `manynet::play_diffusion()` `times` times on the
#'   same network, and binds the reports of every run into one `diffs_model`.
#'   Give the result to `test_gof()`, to compare an observed diffusion with
#'   it, or to `test_distributions()`, to compare it with another model.
#'
#'   Runs in which nothing spreads beyond the seeds return no report from
#'   `manynet::as_diffusion()`, and are dropped with a warning.
#' @param .data A network of a manynet-consistent class, such as a matrix,
#'   an edgelist, an igraph, a network, a tbl_graph, or a stocnet object.
#' @param ... Arguments passed on to `manynet::play_diffusion()`, such as
#'   `transmissibility`, `recovery`, or `steps`.
#' @param times The number of diffusions to simulate.
#'   By default 5, but 1,000-10,000 simulations are recommended for a test.
#' @param control The controls of the simulations, as the result of
#'   [control_mrqap()] or a named list of its arguments, such as
#'   `list(ncores = 4, seed = 1)`.
#'   It reads `ncores`, `strategy`, and `seed`, and leaves the other controls
#'   alone. The same seed gives the same diffusions sequentially and in
#'   parallel.
#' @return A `diffs_model`: a tibble with one row for each step of each run,
#'   the run in `sim`, the step in `time`, and the counts of
#'   `manynet::as_diffusion()`.
#'   `summary()` gives the time of adoption, the number of steps until every
#'   node is infected, for each run.
#' @family models
#' @examples
#' play_diffusions(ison_adolescents, transmissibility = 0.3, times = 5,
#'                 control = list(seed = 1))
#' @export
play_diffusions <- function(.data, ..., times = 5, control = list()) {
  dots <- list(...)
  if ("verbose" %in% names(dots)) {
    manynet::snet_warn(
      c("{.arg verbose} is no longer an argument of {.fn play_diffusions}.",
        i = "Set {.code options(snet_verbosity = \"verbose\")} instead."))
    dots$verbose <- NULL
  }
  test <- .test_control(control, dots)
  ctrl <- test$ctrl
  args <- test$args
  # `manynet::play_diffusion()` calls `igraph::neighborhood()` on `.data`
  # directly, so it stops on a stocnet object.
  if (inherits(.data, "stocnet")) .data <- manynet::as_igraph(.data)

  if (!is.null(ctrl$seed)) set.seed(ctrl$seed)
  old_plan <- setup_future_plan(ctrl$strategy, ctrl$ncores)
  on.exit({
    future::plan(old_plan)
    options(future.globals.maxSize = attr(old_plan, "old_maxSize"))
  }, add = TRUE)
  out <- future.apply::future_lapply(seq_len(times), function(j) {
    diff <- manynet::as_diffusion(do.call(manynet::play_diffusion,
                                          c(list(.data), args)))
    # `manynet::as_diffusion()` returns a 0-row report when nothing spreads
    # beyond the seeds.
    if (nrow(diff) == 0) return(NULL)
    diff <- as.data.frame(diff)
    names(diff)[names(diff) == "t"] <- "time"
    data.frame(sim = j, diff)
  }, future.seed = TRUE)

  empty <- sum(vapply(out, is.null, logical(1)))
  if (empty == times) {
    manynet::snet_abort("None of the {times} diffusion runs returned a report.")
  }
  if (empty > 0) {
    manynet::snet_warn(
      "Dropped {empty} of {times} diffusion runs that returned an empty report.")
  }
  .make_diffs_model(.bind_reports(out), .data)
}

# The runs may report different columns, such as `S_new` on one network and
# not on another, so a column missing from a run is filled with 0.
#' @keywords internal
#' @noRd
.bind_reports <- function(reports) {
  reports <- reports[!vapply(reports, is.null, logical(1))]
  cols <- unique(unlist(lapply(reports, names)))
  reports <- lapply(reports, function(r) {
    for (col in setdiff(cols, names(r))) r[[col]] <- 0
    r[cols]
  })
  out <- do.call(rbind, reports)
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

#' @keywords internal
#' @noRd
.make_diffs_model <- function(report, .data) {
  class(report) <- c("diffs_model", class(report))
  attr(report, "mode") <- manynet::node_is_mode(.data)
  report
}

#' @rdname play_diffusions
#' @param object,x A `diffs_model`.
#' @export
summary.diffs_model <- function(object, ...) {
  fin <- (object$I != object$n) * 1
  toa <- tapply(fin, object$sim, sum) + 1
  tibble::tibble(sim = as.integer(names(toa)), toa = as.numeric(toa))
}

#' @rdname play_diffusions
#' @export
print.diffs_model <- function(x, ...) {
  y <- as.data.frame(x)
  y <- y[, colSums(abs(y), na.rm = TRUE) != 0 | names(y) %in% c("sim", "time"),
         drop = FALSE]
  y$I_new <- NULL
  print(tibble::as_tibble(y), ...)
  invisible(x)
}
