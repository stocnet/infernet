# The functional audit: every term that an MRQAP fits, on every shape of
# network. helper-functional.R says how it works and what it asks.

functional <- functional_nets()
shape_label <- c(directed = "a directed network", undirected = "an undirected network",
                 twomode = "a two-mode network", css = "a cognitive social structure",
                 panel = "a panel", list = "a list of networks")
roster <- functional_roster()

for (term in names(roster)) {
  for (shape in names(functional)) {
    expected <- functional_expectation(roster[[term]], shape)
    label <- paste0(term, "() ", if (expected == "builds")
      "builds columns of the outcome's shape on " else "is refused, by name, on ",
      shape_label[[shape]])
    test_that(label, {
      f <- stats::as.formula(paste("a ~", term_example(term, roster[[term]])))
      built <- tryCatch(functional_build(f, functional[[shape]], shape),
                        error = function(e) e)
      if (expected == "refuses") {
        if (!inherits(built, "error")) {
          audit(term, shape, "built, where the registry says it is refused")
        }
        # A refusal names the term, so that the user knows what to change.
        expect_match(conditionMessage(built), paste0(term, "()"), fixed = TRUE)
      } else {
        if (inherits(built, "error")) audit(term, shape, conditionMessage(built))
        problem <- functional_problem(built)
        if (!is.null(problem)) audit(term, shape, problem)
        succeed()
      }
    })
  }
}
