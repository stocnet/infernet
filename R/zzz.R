#' @importFrom cli cli_div cli_inform cli_end
.onAttach <- function(...) {

  # suppressMessages(suppressPackageStartupMessages(library("manynet", warn.conflicts = FALSE)))
  if (!interactive()) return()
  
  # options(manynet_verbosity = getOption("manynet_verbosity", "verbose"))
  options(stocnet_theme = getOption("stocnet_theme", "default"))
  # options(cli.theme = manynet_console_theme())
  # options(cli.progress_clear = TRUE)
  
  # pkgs <- as.data.frame(utils::available.packages(utils::contrib.url(getOption("repos"))))
  # 
  # cran_version <- pkgs[pkgs$Package == "manynet","Version"]

  local_version <- utils::packageVersion("infernet")
  manynet::snet_info("You are using {.pkg infernet} version {.version {local_version}}.")
  old.list <- as.data.frame(utils::old.packages())
  behind_cran <- "infernet" %in% old.list$Package
  
  greet_startup_cli <- function() {
    tips <- c(
      "i" = "There are lots of ways to contribute to {.pkg infernet} at {.url https://github.com/stocnet/infernet/}.",
      "i" = "Please let us know any bugs, issues, or feature requests at {.url https://github.com/stocnet/infernet/issues}. It's really helpful!",
      # "i" = "To eliminate package startup messages, use: `suppressPackageStartupMessages(library({.pkg autograph}))`.",
      # "i" = "Changing the theme of all your graphs is straightforward with `set_manynet_theme()`",
      # "i" = "If there are too many messages in the console, run `options(manynet_verbosity = 'quiet')`",
      "i" = "Visit the website to learn more: {.url https://stocnet.github.io/infernet/}.",
      "i" = "We recommend the 'Function Overview' page online to discover new analytic opportunities: {.url https://stocnet.github.io/infernet/reference/index.html}.",
      # "i" = "Star me at {.url https://github.com/users/follow?target=jhollway}.",
      # "i" = "You can list all the tutorials available in {.pkg manynet} using {.fn run_tute}, and run them too!",
      "i" = "Discover all the {.emph stocnet} R packages at {.url https://github.com/stocnet/}."
    )
    manynet::snet_info(sample(tips, 1))
  }

  if (interactive()) {
    if (behind_cran) {
      msg <- "A new version of infernet is available with bug fixes and new features."
      packageStartupMessage(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::update.packages("infernet")
      }
    } else {
      greet_startup_cli()
      # packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
    }
  }

}

# Global variables ####
# defining global variables more centrally
utils::globalVariables(c(".data"))




