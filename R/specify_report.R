# Reporting specifications and controls ####

# The print and `summary()` of what `specify_*()`, `specify_terms()`, and
# `control_mrqap()` return, before a model is fitted. `summary()` gives the
# term table. `tidy()` of a specification is deprecated in 0.7.0, since a
# term table is not the table of estimates that `tidy()` gives for a fit.

# ---- formulas -----------------------------------------------------------------

#' @export
print.mrqap_formula <- function(x, ...) .print_specified(x, "mrqap")

#' @export
print.ergm_formula <- function(x, ...) .print_specified(x, "ergm")

#' @rdname specify_mrqap
#' @order 2
#' @examples
#' # Each term, and what the model makes of it
#' summary(f)
#' g <- specify_mrqap(advice ~ ego(age) + same(dept) + recip(friends), ison_hightech)
#' summary(g, ison_hightech)
#' @export
summary.mrqap_formula <- function(object, .data = NULL, ...) {
  out <- .tidy_specified(object)
  out$columns <- rep(list(character(0)), nrow(out))
  if (is.null(.data)) return(out)
  .record_columns(object, out, .data)
}

#' @rdname specify_mrqap
#' @order 3
#' @export
tidy.mrqap_formula <- function(x, .data = NULL, ...) {
  .warn_tidy_spec_deprecated()
  summary.mrqap_formula(x, .data = .data, ...)
}

#' @rdname specify_ergm
#' @order 2
#' @details
#'   `summary()` of the result gives the term table, not the statistics of
#'   the observed network that `{ergm}`'s `summary()` of a formula gives.
#'   For those, use `ergm::summary_formula()` of the result.
#' @examples
#' summary(specify_ergm(advice ~ ego(age) + same(dept) + recip + trans))
#' @export
summary.ergm_formula <- function(object, ...) .tidy_specified(object)

#' @rdname specify_ergm
#' @order 3
#' @export
tidy.ergm_formula <- function(x, ...) {
  .warn_tidy_spec_deprecated()
  summary.ergm_formula(x, ...)
}

# `tidy()` of a specification warns once in a session, as a deprecation does.
#' @keywords internal
#' @noRd
.warn_tidy_spec_deprecated <- function() {
  if (isTRUE(.infernet_state$warned_tidy_spec)) return(invisible())
  .infernet_state$warned_tidy_spec <- TRUE
  manynet::snet_warn(
    c("{.code tidy()} of a specification is deprecated as of infernet 0.7.0.",
      i = "Use {.code summary()} for the table of terms. {.code tidy()} of a fitted model does not change.",
      i = "This warning is shown once in a session."))
  invisible()
}

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
# that the model changes, warns about, drops, or refuses. `summary()` gives them all.
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
  .print_footer(notes, .methods_hint("summary() for what the model makes of each term"))
  invisible(x)
}

# ---- term tables --------------------------------------------------------------

#' @rdname specify_terms
#' @order 3
#' @export
summary.specify_terms <- function(object, ...) .plain_terms(object)

#' @rdname specify_terms
#' @order 4
#' @export
tidy.specify_terms <- function(x, ...) {
  .warn_tidy_spec_deprecated()
  .plain_terms(x)
}

#' @keywords internal
#' @noRd
.plain_terms <- function(x) {
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
  print(.plain_terms(x), n = Inf, ...)
  cat("Choose rows, and `formula()` of them gives their formula.\n")
  invisible(x)
}

# ---- controls -----------------------------------------------------------------

# A print lists the controls that were set. `?control_mrqap` gives the defaults.
#' @export
#' @noRd
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
  from_option <- names(source)[source == "option"]
  persisted <- if (length(from_option) > 0)
    "(option) persists from set_control_computation(); set_control_computation(persist = FALSE) clears it."
  # A model control can come only from the option that is read until 0.8.0.
  if (length(setdiff(from_option, .computation_names())) > 0) {
    persisted <- c(persisted,
                   "A model control persists from the deprecated options(infernet.control); options(infernet.control = NULL) clears it.")
  }
  .print_footer(persisted,
                .methods_hint("?control_mrqap for each control and its default"))
  invisible(x)
}
