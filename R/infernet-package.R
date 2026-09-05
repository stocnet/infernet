# nocov start

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Checks for a package in Suggests, and aborts with the install command where it
# is missing. Deliberately not a prompt: `utils::askYesNo()` reads from stdin,
# and a permutation run started from a script would stall on it.
#' @keywords internal
#' @noRd
thisRequires <- function(pkgname, why) {
  if (!requireNamespace(pkgname, quietly = TRUE)) {
    manynet::snet_abort(
      c(paste0("The {.pkg ", pkgname, "} package is required ", why, "."),
        i = paste0("Install it with {.run install.packages(\"", pkgname, "\")}.")))
  }
  invisible(TRUE)
}

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
