# Miscellaneous QAP helpers --------------------------------------------------
#
# Building a CSS array from long data is `{manynet}`'s job: `as_matrix()` on a
# network whose ties carry a `by` column.

# The fit records directedness as a logical, because that is what
# `manynet::is_directed()` returns and what the engine branches on. Users read
# the word, so the print methods render it here rather than each spelling it.
#' @keywords internal
#' @noRd
.directed_label <- function(directed) {
  if (isTRUE(directed)) "directed" else "undirected"
}
