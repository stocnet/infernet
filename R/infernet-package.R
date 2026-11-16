# nocov start

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Suppress R CMD check note
# Namespace in Imports field not imported from: PKG
#   All declared Imports should be used.
#' @importFrom netrics net_by_heterophily
ignore_unused_imports <- function() {
  # This function exists only to reference functions and suppress R CMD check notes about unused imports.
  netrics::net_by_heterophily
  NULL
}

# Load `{future}` and `{future.apply}` with infernet, not at their first use.
# `{future}` sets its default options, such as "future.globals.method.default",
# only when it loads. A learnr tutorial removes every option that an exercise
# added, so where `{future}` first loaded inside an exercise, its defaults went
# with it, and the next exercise that used it stopped with "Internal R options
# 'future.globals.method.default' is corrupt: NULL".
#' @importFrom future plan
#' @importFrom future.apply future_lapply
NULL

# nocov end
