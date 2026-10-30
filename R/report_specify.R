# Reporting specifications and controls ####

# The print and `tidy()` of what `specify_*()`, `specify_terms()`, and
# `control_mrqap()` return, before a model is fitted.

# ---- formulas -----------------------------------------------------------------

#' @export
print.mrqap_formula <- function(x, ...) .print_specified(x, "mrqap")

#' @export
print.ergm_formula <- function(x, ...) .print_specified(x, "ergm")

#' @rdname specify
#' @order 4
#' @param x A formula returned by a `specify_*()` function.
#' @param ... Not used.
#' @examples
#' # Each term, and what the model makes of it
#' tidy(f)
#' g <- specify_mrqap(advice ~ ego(age) + same(dept) + recip(friends), ison_hightech)
#' tidy(g, ison_hightech)
#' @export
tidy.mrqap_formula <- function(x, .data = NULL, ...) {
  out <- .tidy_specified(x)
  out$columns <- rep(list(character(0)), nrow(out))
  if (is.null(.data)) return(out)
  .record_columns(x, out, .data)
}

#' @rdname specify
#' @order 5
#' @export
tidy.ergm_formula <- function(x, ...) .tidy_specified(x)

#' @keywords internal
#' @noRd
.tidy_specified <- function(x) {
  record <- attr(x, "record")
  if (is.null(record)) record <- .no_record()
  blank <- function(v) ifelse(nzchar(v), v, NA_character_)
  tibble::tibble(term = record$term, status = record$status,
                 as = blank(record$as), reason = blank(record$reason))
}

# A specification as the formula it holds, without its class and its record,
# so that it reads as the formula a user would write.
#' @keywords internal
#' @noRd
.plain_formula <- function(x) {
  attr(x, "record") <- NULL
  attr(x, "input") <- NULL
  class(x) <- "formula"
  x
}

# A term that fits as written needs no note, so the print lists only the terms
# that the model changes, warns about, drops, or refuses. `tidy()` gives them all.
#' @keywords internal
#' @noRd
.print_specified <- function(x, target) {
  record <- attr(x, "record")
  input <- attr(x, "input")
  what <- c(mrqap = "An MRQAP formula", ergm = "An ergm formula")[[target]]
  if (!is.null(input) && !identical(input, target)) {
    what <- paste(what, "read from a formula for", .model_label(input))
  }
  .print_title(what)
  print(.plain_formula(x), showEnv = FALSE)
  if (!is.null(record)) record <- record[record$status != "fits", , drop = FALSE]
  notes <- character(0)
  if (!is.null(record)) {
    for (k in seq_len(nrow(record))) {
      as <- if (nzchar(record$as[k]) && record$as[k] != record$term[k])
        paste0(" -> ", record$as[k]) else ""
      why <- if (nzchar(record$reason[k])) paste0(": ", record$reason[k]) else ""
      notes <- c(notes, paste0(record$status[k], " ", record$term[k], as, why))
    }
  }
  .print_footer(notes, .methods_hint("tidy() for what the model makes of each term"))
  invisible(x)
}

# ---- term tables --------------------------------------------------------------

#' @rdname specify_terms
#' @order 3
#' @export
tidy.specify_terms <- function(x, ...) {
  out <- as.data.frame(x)
  attr(out, "outcome") <- NULL
  attr(out, "model") <- NULL
  attr(out, "data") <- NULL
  tibble::as_tibble(out)
}

#' @export
print.specify_terms <- function(x, ...) {
  outcome <- .deparsed(attr(x, "outcome"))
  cat(nrow(x), " terms for ", .model_label(attr(x, "model")),
      if (outcome != ".") paste0(" of ", outcome), "\n", sep = "")
  print(tidy(x), n = Inf, ...)
  cat("Choose rows, and `formula()` of them gives their formula.\n")
  invisible(x)
}

# ---- controls -----------------------------------------------------------------

# A print lists the controls that were set. `?control_mrqap` gives the defaults.
#' @rdname control_mrqap
#' @export
print.mrqap_control <- function(x, ...) {
  defaults <- .control_defaults()
  ctrl <- unclass(x)
  source <- attr(x, "source")
  # `[[<-` with `NULL` drops an element, so a control at a default of `NULL`
  # may be absent from either list.
  every <- union(names(defaults), setdiff(names(ctrl), "persist"))
  changed <- every[!vapply(every, function(nm) identical(ctrl[[nm]], defaults[[nm]]),
                           logical(1))]
  same <- setdiff(every, changed)
  .print_title(paste0("Controls for an MRQAP: ", length(changed), " set, ",
                      length(same), " at their defaults"))
  if (length(changed) > 0) {
    width <- max(nchar(changed))
    for (nm in changed) {
      mark <- if (identical(source[nm][[1]], "option")) "  (option)" else ""
      cat(sprintf("  %-*s  %s%s\n", width, nm, .control_value(ctrl[[nm]]), mark))
    }
  }
  persisted <- if (any(source == "option"))
    "(option) persists from control_mrqap(..., persist = TRUE); control_mrqap(persist = FALSE) clears it."
  .print_footer(persisted,
                .methods_hint("?control_mrqap for each control and its default"))
  invisible(x)
}
