# Printing infernet's results ####

# Every result prints in three parts: a title that says what was fitted or
# tested, a body that is `tidy()` of the result, and a footer that says how the
# null distribution was made and which controls differ from the default. Blank lines set the title and the footer apart
# from the body.
# `summary()` adds `glance()` and the parts of a fit, each as its own section.
# The title and the footer are `#` lines, as `{manynet}` prints its own
# results; `{manynet}` does not export its helper, so these are written here.
# The network itself is not described, since `{manynet}` prints it.

#' @keywords internal
#' @noRd
.print_header <- function(lines) {
  lines <- lines[!is.na(lines) & nzchar(lines)]
  for (line in lines) {
    cat(strwrap(line, width = getOption("width"), initial = "# ",
                prefix = "#   "), sep = "\n")
  }
  invisible()
}

# The title of a result, and a blank line before its body.
#' @keywords internal
#' @noRd
.print_title <- function(lines) {
  .print_header(lines)
  cat("\n")
  invisible()
}

# The heading of a section of a summary, after a blank line.
#' @keywords internal
#' @noRd
.print_section <- function(lines) {
  cat("\n")
  .print_header(lines)
}

# The footer comes after a blank line, so that it is not read as a part of the
# table above it. A line that points to more starts with the information sign,
# as a hint in `{pillar}` does, where the session can print it. The hints are
# printed only at `options(snet_verbosity = "debug")`, the most verbose level,
# so that a print stays short.
#' @keywords internal
#' @noRd
.print_footer <- function(lines, hints = character(0)) {
  lines <- lines[!is.na(lines) & nzchar(lines)]
  hints <- hints[!is.na(hints) & nzchar(hints)]
  if (!.verbosity_at("debug")) hints <- character(0)
  if (length(lines) + length(hints) == 0) return(invisible())
  cat("\n")
  .print_header(lines)
  # As `{cli}` decides, which `{pillar}` follows, and testthat turns off.
  unicode <- getOption("cli.unicode", isTRUE(l10n_info()$`UTF-8`))
  info <- if (isTRUE(unicode)) "\u2139 " else "i "
  for (hint in hints) {
    cat(strwrap(hint, width = getOption("width"), initial = paste0("# ", info),
                prefix = "#   "), sep = "\n")
  }
  invisible()
}

# A p-value of 0 says only that no permutation reached the observed value, so
# it is shown as below the smallest p-value the permutations can give, and
# its marks are those of that bound.
#' @keywords internal
#' @noRd
.format_perm_p <- function(p, times) {
  out <- formatC(p, digits = 3, format = "fg")
  out[!is.na(p) & p == 0] <- paste("<", format(1 / times, digits = 3))
  out[is.na(p)] <- NA_character_
  trimws(out)
}

#' @keywords internal
#' @noRd
.signif_marks <- function(p, times) {
  out <- as.character(stats::symnum(pmax(p, 1 / times), corr = FALSE, na = FALSE,
                                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                    symbols = c("***", "**", "*", ".", "")))
  out[is.na(p)] <- ""
  out
}

#' @keywords internal
#' @noRd
.signif_legend <- function(marks) {
  if (!any(nzchar(marks))) return(NULL)
  "Signif. codes: *** 0.001, ** 0.01, * 0.05, . 0.1"
}

# The rows of `tidy()` as a print shows them: each p-value as text, and the
# marks of `p` beside them.
#' @keywords internal
#' @noRd
.print_tidy <- function(tab, times, p = "p.value") {
  marks <- if (p %in% names(tab)) .signif_marks(tab[[p]], times) else character(0)
  for (col in intersect(c("p.value", "p.lower", "p.upper"), names(tab))) {
    tab[[col]] <- .format_perm_p(tab[[col]], times)
  }
  # `noquote()`, since a tibble quotes an empty string.
  if (length(marks)) tab$signif <- noquote(marks)
  print(tab, n = Inf)
  invisible(marks)
}

# `glance()` in a print. A tibble hides the columns that do not fit, and a data
# frame wraps them, so that every value shows.
#' @keywords internal
#' @noRd
.print_glance <- function(tab) {
  print(as.data.frame(tab), digits = 3, row.names = FALSE)
  invisible()
}

# The controls that differ from their default, as `name = value`, leaving out
# those that a print already says in words.
#' @keywords internal
#' @noRd
.changed_controls <- function(ctrl, said = character(0)) {
  if (is.null(ctrl)) return(character(0))
  defaults <- unclass(control_mrqap())
  ctrl <- unclass(ctrl)
  keep <- setdiff(names(ctrl), said)
  keep <- keep[!vapply(keep, function(nm) identical(ctrl[[nm]], defaults[[nm]]),
                       logical(1))]
  keep <- keep[!vapply(ctrl[keep], is.null, logical(1))]
  vapply(keep, function(nm) paste(nm, "=", .control_value(ctrl[[nm]])),
         character(1), USE.NAMES = FALSE)
}

#' @keywords internal
#' @noRd
.control_value <- function(v) {
  if (is.null(v)) return("NULL")
  if (length(v) > 3) return(paste0("<", length(v), " values>"))
  if (is.character(v)) return(paste0('"', v, '"', collapse = ", "))
  paste(format(v), collapse = ", ")
}

#' @keywords internal
#' @noRd
.controls_line <- function(ctrl, said = character(0)) {
  changed <- .changed_controls(ctrl, said)
  if (length(changed) == 0) return(NULL)
  paste("Controls:", paste(changed, collapse = ", "))
}

#' @keywords internal
#' @noRd
.methods_hint <- function(...) {
  paste0("See ", paste(c(...), collapse = ", "), ".")
}

# Whether `snet_verbosity` is at `level` or above, as `{manynet}` reads it.
#' @keywords internal
#' @noRd
.verbosity_at <- function(level) {
  levels <- c("quiet", "normal", "verbose", "debug")
  set <- match(getOption("snet_verbosity", default = "quiet"), levels)
  if (is.na(set)) set <- 2L
  set >= match(level, levels)
}

# Base R has this from 4.4.0, and the package depends on 4.1.0.
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

# The name of the statistic a test was given, as the call wrote it.
#' @keywords internal
#' @noRd
.fun_name <- function(expr) {
  if (is.name(expr) || (is.call(expr) && identical(expr[[1]], as.name("::")))) {
    return(paste0(deparse(expr), "()"))
  }
  "a statistic"
}

#' @keywords internal
#' @noRd
.lower_first <- function(x) paste0(tolower(substring(x, 1, 1)), substring(x, 2))
