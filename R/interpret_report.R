# Reporting simulated diffusions ####

# The methods of a `diffs_model`, which `play_diffusions()` in
# `R/interpret_diffusions.R` makes.


#' @details
#'   `print()` shows a title that says how many diffusions were simulated,
#'   the report of each step, and a footer with the steps until
#'   every node adopted. `summary()` gives those steps for each simulation.
#' @rdname play_diffusions
#' @param object,x A `diffs_model`.
#' @export
summary.diffs_model <- function(object, ...) {
  fin <- (object$I != object$n) * 1
  toa <- tapply(fin, object$sim, sum) + 1
  tibble::tibble(sim = as.integer(names(toa)), toa = as.numeric(toa))
}

# A subset of the rows of a `diffs_model` is still one, with the attributes
# that its print reads. A subset that drops a column that the print and
# `summary()` read is a plain tibble.
#' @rdname play_diffusions
#' @param i,j,drop As for a tibble.
#' @export
`[.diffs_model` <- function(x, i, j, drop = FALSE) {
  keep <- attributes(x)[c("mode", "control")]
  out <- NextMethod()
  if (!is.data.frame(out)) return(out)
  if (!all(c("sim", "time", "n", "I") %in% names(out))) {
    class(out) <- setdiff(class(out), "diffs_model")
    return(out)
  }
  for (nm in names(keep)) if (!is.null(keep[[nm]])) attr(out, nm) <- keep[[nm]]
  if (!inherits(out, "diffs_model")) class(out) <- c("diffs_model", class(out))
  out
}

#' @export
#' @noRd
print.diffs_model <- function(x, ...) {
  y <- as.data.frame(x)
  y <- y[, colSums(abs(y), na.rm = TRUE) != 0 | names(y) %in% c("sim", "time"),
         drop = FALSE]
  y$I_new <- NULL
  sims <- length(unique(x$sim))
  states <- intersect(c("S", "E", "I", "R"), names(y))
  .print_title(paste0(sims, " simulated ", paste(states, collapse = ""),
                     " diffusion", if (sims != 1) "s"))
  print(tibble::as_tibble(y), ...)
  toa <- summary.diffs_model(x)$toa
  .print_footer(c(paste0("Steps until every node adopted: median ",
                         stats::median(toa), ", from ", min(toa), " to ",
                         max(toa), "."),
                  .controls_line(attr(x, "control"))),
                .methods_hint("summary() for the steps of each simulation",
                              "test_gof() and test_distributions() to compare",
                              "and plot() from {autograph} for the curves"))
  invisible(x)
}
