# Runs the code that each tutorial's chunks would run, in order, so that a
# change to a function the tutorial teaches breaks this test rather than the
# tutorial. The exercises and hints are not purled, only the solutions.

run_tutorial_code <- function(path) {
  tmp <- tempfile(fileext = ".R")
  knitr::purl(input = path, output = tmp, quiet = TRUE)
  exprs <- parse(tmp)
  env <- new.env(parent = globalenv())
  waiting <- character()
  for (i in seq_along(exprs)) {
    if (.plots_a_netreg(exprs[[i]], env)) {
      waiting <- c(waiting, paste(deparse(exprs[[i]]), collapse = " "))
      next
    }
    err <- tryCatch({
      suppressMessages(eval(exprs[[i]], envir = env))
      NULL
    }, error = function(e) conditionMessage(e))
    expect_null(err, info = paste0(basename(path), ", expression ", i, ": ",
                                   paste(deparse(exprs[[i]]), collapse = " ")))
  }
  waiting
}

# {autograph} cannot plot a netreg result yet (stocnet/autograph#68), so such a
# call is set aside until it can.
.plots_a_netreg <- function(expr, env) {
  if (!is.call(expr) || !identical(expr[[1]], as.name("plot"))) return(FALSE)
  obj <- tryCatch(eval(expr[[2]], envir = env), error = function(e) NULL)
  inherits(obj, "netreg") &&
    is.null(utils::getS3method("plot", "netreg", optional = TRUE))
}

test_that("the tutorials run without error", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  skip_if_not_installed("autograph")
  tutes <- list.files(system.file("tutorials", package = "infernet"),
                      pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE)
  expect_gt(length(tutes), 0)
  old <- options(knitr.purl.inline = FALSE)
  on.exit(options(old), add = TRUE)
  waiting <- unlist(lapply(tutes, run_tutorial_code))
  if (length(waiting) > 0)
    skip(paste("autograph cannot plot a netreg yet (autograph#68):",
               paste(waiting, collapse = "; ")))
})
