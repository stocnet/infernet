# nocov start

# The stocnet version check lives in {migraph}, which loads and checks the whole
# stack at once. This package is one of the packages it checks, so it does no
# version check of its own.
#' @importFrom manynet snet_info
.onAttach <- function(...) {

  if (!interactive()) return()

  options(stocnet_theme = getOption("stocnet_theme", "default"))

  local_version <- utils::packageVersion("infernet")
  manynet::snet_info("You are using {.infr infernet} version {.version {local_version}}.")

  greet_startup_cli <- function() {
    tips <- c(
      "i" = "Share bugs, issues, or feature requests at {.url https://github.com/stocnet/infernet/issues}.",
      "i" = "If too many messages appear in the console, run {.run base::options(snet_verbosity = 'quiet')}",
      "i" = "Explore changes since the last version with {.run [news(package = 'infernet')](utils::news(package = 'infernet'))}.",
      # "i" = "Test any network statistic against a null distribution with {.fn test_random}, {.fn test_configuration}, or {.fn test_permutation}.",
      # "i" = "Regress a network on nodal and dyadic covariates with {.fn net_regression}.",
      # "i" = "Write {.code ego()}, {.code alter()}, {.code same()}, {.code dist()}, {.code sim()}, or {.code tertius()} in a formula to build a predictor from a nodal attribute.",
      "i" = "Speed up a long run with {.code control = list(strategy = 'multisession')}.",
      # "i" = "Measures to test are in {.tric netrics}; plots of results are in {.auto autograph}.",
      "i" = "Visit {.url https://stocnet.github.io/infernet/} to learn more.",
      "i" = "Discover new functions at {.url https://stocnet.github.io/infernet/reference/index.html}.",
      "i" = "Discover {.emph stocnet} R packages at {.url https://github.com/stocnet/}."
    )
    manynet::snet_info(sample(tips, 1))
  }

  greet_startup_cli()

}

# nocov end

# Global variables ####
# defining global variables more centrally
utils::globalVariables(c(".data"))
