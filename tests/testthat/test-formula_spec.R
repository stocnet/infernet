# The formula is parsed into a specification before anything is built. These
# tests hold the grammar: named arguments, interactions of any order, the
# outcome the left-hand side names, and the errors a formula can meet.

test_that("a term's arguments can be given by position or by name", {
  spec <- .specify(y ~ indeg(friends, type = "ego") + indeg(friends, "ego") +
                     diff(age, transformer_fn = identity))
  ops <- lapply(spec$terms, function(t) t$operands[[1]])
  expect_equal(.arg_name(ops[[1]]$args$type), "ego")
  expect_equal(.arg_name(ops[[2]]$args$type), "ego")
  expect_identical(ops[[3]]$args$transformer_fn, quote(identity))
  # A default fills an argument that was not given.
  spec2 <- .specify(y ~ indeg(friends))
  expect_equal(spec2$terms[[1]]$operands[[1]]$args$type, "alter")
})

test_that("a bare name is a dyadic covariate", {
  op <- .specify(y ~ friends)$terms[[1]]$operands[[1]]
  expect_true(is.na(op$fn))
  expect_equal(op$name, "friends")
})

test_that("an unknown term or argument names what is available", {
  expect_error(.specify(y ~ egoo(age)), "not a formula term")
  expect_error(.specify(y ~ egoo(age)), "Did you mean `ego\\(\\)`")
  expect_error(.specify(y ~ indeg(friends, kind = "ego")), "takes")
  # A name that no model knows still aborts, as a bare name or as a term.
  expect_error(convertToMatrixList(a ~ inertai + ego(x), formula_net(),
                                   advise = FALSE), "not found")
  expect_error(convertToMatrixList(a ~ trnas(b) + ego(x), formula_net(),
                                   advise = FALSE), "Did you mean `trans\\(\\)`")
})

test_that("a term of another model is read with any of its arguments", {
  op <- .specify(y ~ trans(friends, window = 10, transformer_fn = sqrt))$terms[[1]]$operands[[1]]
  expect_equal(op$fn, "trans")
  expect_equal(op$name, "friends")
})


test_that("a bar is a random intercept for an index the engine builds", {
  spec <- .specify(y ~ ego(age) + (1 | sender) + (1 | receiver))
  expect_equal(spec$random, c("sender", "receiver"))
  expect_length(spec$terms, 1)
  expect_error(.specify(y ~ ego(age) + (1 | dept)), "has no random intercept")
  expect_error(.specify(y ~ ego(age) + (age | sender)), "random slope")
  expect_error(.specify(y ~ ego(age) + offset(z)), "offset")
})

test_that("a random intercept in the formula fits as its control does", {
  skip_if_not_installed("lme4")
  g <- qap_net_gaussian(n = 12)
  a <- suppressMessages(estimate_mrqap(weight ~ ego(Age) + alter(Age) + (1 | sender),
                                       g, times = 5, control = list(seed = 1)))
  b <- suppressMessages(estimate_mrqap(weight ~ ego(Age) + alter(Age), g, times = 5,
                                       control = list(seed = 1, random_intercept_sender = TRUE)))
  expect_equal(a$coefficients, b$coefficients)
  expect_true(any(a$random))
})

test_that("an interaction of any order is the product of its operands", {
  g <- qap_net_gaussian(n = 12)
  ml <- convertToMatrixList(weight ~ ego(Age) * alter(Age) * ego(Cit), g,
                            advise = FALSE)
  expect_named(ml$mydata, c("weight", "ego_Age", "alter_Age", "ego_Cit",
                            "ego_Age:alter_Age", "ego_Age:ego_Cit",
                            "alter_Age:ego_Cit", "ego_Age:alter_Age:ego_Cit"))
  m <- ml$mydata
  expect_equal(m[["ego_Age:alter_Age:ego_Cit"]],
               m[["ego_Age"]] * m[["alter_Age"]] * m[["ego_Cit"]])
  fit <- estimate_mrqap(weight ~ ego(Age) * alter(Age) * ego(Cit), g,
                        times = 5, control = list(seed = 1))
  expect_length(fit$coefficients, 8)
})

test_that("an interaction with a term of several columns gives one product each", {
  g <- formula_net()
  ml <- convertToMatrixList(a ~ same(grp, diff = TRUE):ego(x), g, advise = FALSE)
  expect_named(ml$mydata, c("a", "same_grp_p:ego_x", "same_grp_q:ego_x"))
})

test_that("`. ~` takes the outcome from the network's focal layer", {
  ht <- manynet::ison_hightech
  ht$info$focal <- "advice"
  fit <- suppressMessages(
    estimate_mrqap(. ~ ego(age) + friends, ht, times = 5,
                   control = list(seed = 1)))
  expect_equal(fit$dep, "advice")
  expect_s3_class(fit, "netreg_css")
  # The message says where the outcome came from.
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(convertToMatrixList(. ~ ego(age), ht, advise = FALSE),
                 "focal layer")
})

test_that("a named outcome takes priority over the focal layer", {
  ht <- manynet::ison_hightech
  ht$info$focal <- "advice"
  ml <- convertToMatrixList(reports ~ ego(age), ht, advise = FALSE)
  expect_equal(names(ml$mydata)[1], "reports")
  expect_false(ml$css)
})

test_that("a focal node attribute is refused, since the outcome must be ties", {
  s <- manynet::make_stocnet(
    nodes = data.frame(label = letters[1:4], x = c(1, 2, 3, 4)),
    ties  = data.frame(from = c(1L, 2L, 3L), to = c(2L, 3L, 4L)),
    info  = list(directed = TRUE, focal = "x"))
  expect_error(convertToMatrixList(. ~ ego(x), s, advise = FALSE),
               "node attribute")
})

test_that("a multiplex network without a focal layer says how to declare one", {
  expect_error(
    convertToMatrixList(. ~ ego(age), manynet::ison_hightech, advise = FALSE),
    "info\\$focal")
})
