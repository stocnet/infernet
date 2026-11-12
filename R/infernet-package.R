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

# nocov end
