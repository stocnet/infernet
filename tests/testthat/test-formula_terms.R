# Each formula term builds one matrix, or several, for each dyad. These tests
# check each against a value computed by hand or by a direct loop on
# `formula_net()`, whose two layers are small enough to read.

terms_of <- function(formula, .data = formula_net(), ...) {
  convertToMatrixList(formula, .data, advise = FALSE, ...)$mydata
}

# The term table: a column's name cannot be parsed back into its parts, so each
# column also has a row of them.
table_of <- function(formula, .data = formula_net(), ...) {
  suppressMessages(convertToMatrixList(formula, .data, advise = FALSE, ...)$terms)
}

layer_of <- function(layer, .data = formula_net()) {
  m <- manynet::as_matrix(manynet::to_uniplex(manynet::as_tidygraph(.data),
                                              layer = layer))
  unname(m)
}

X <- c(3, 1, 4, 1, 5, 9)
GRP <- c("p", "q", "p", "q", "p", "q")

# ---- node attributes ---------------------------------------------------------

test_that("diff() is the absolute difference, and signed with identity", {
  m <- terms_of(a ~ diff(x))
  expect_equal(unname(m[["diff_x"]]), abs(outer(X, X, "-")))
  m <- terms_of(a ~ diff(x, transformer_fn = identity))
  expect_equal(unname(m[["diff_x"]]), outer(X, X, "-"))
  expect_error(terms_of(a ~ diff(grp)), "same")
})

test_that("dist() is a deprecated alias that keeps its column name", {
  .infernet_state$dist_warned <- NULL
  expect_warning(m <- terms_of(a ~ dist(x)), "deprecated")
  expect_equal(unname(m[["dist_x"]]), abs(outer(X, X, "-")))
  # Said once, not once per network of a list.
  expect_no_warning(terms_of(a ~ dist(x)))
})

test_that("ego() and alter() dummy-code a categorical attribute, naming the reference", {
  m <- suppressMessages(terms_of(a ~ ego(grp) + alter(grp)))
  expect_named(m, c("a", "ego_grp_q", "alter_grp_q"))
  expect_equal(unname(m[["ego_grp_q"]]),
               matrix(GRP == "q", 6, 6) * 1)
  expect_equal(unname(m[["alter_grp_q"]]),
               matrix(GRP == "q", 6, 6, byrow = TRUE) * 1)
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(terms_of(a ~ ego(grp)), "reference")
})

test_that("a categorical ego() gets a p-value under double semi-partialling", {
  fit <- suppressMessages(
    estimate_mrqap(a ~ ego(grp) + alter(grp) + same(grp), formula_net(),
                   times = 5, control = list(seed = 1)))
  expect_false(anyNA(fit$abs[, -1]))
})

test_that("same(diff = TRUE) gives one dummy per category where both hold it", {
  m <- terms_of(a ~ same(grp, diff = TRUE))
  expect_named(m, c("a", "same_grp_p", "same_grp_q"))
  expect_equal(unname(m[["same_grp_p"]]), outer(GRP == "p", GRP == "p") * 1)
  expect_equal(unname(m[["same_grp_q"]]), outer(GRP == "q", GRP == "q") * 1)
})

test_that("same(diff = TRUE) is refused on a two-mode network, pointing to mix()", {
  sw <- manynet::mutate(manynet::ison_southern_women,
                        grp = rep(c("u", "v"), length.out = 32))
  expect_error(terms_of(. ~ same(grp, diff = TRUE), sw), "mix")
})

test_that("match() is a deprecated alias that keeps its column names", {
  .infernet_state$match_warned <- NULL
  expect_warning(m <- terms_of(a ~ match(grp)), "deprecated")
  expect_named(m, c("a", "match_grp_p", "match_grp_q"))
  expect_equal(unname(m[["match_grp_p"]]), outer(GRP == "p", GRP == "p") * 1)
  # The warning is given once per session.
  expect_no_warning(terms_of(a ~ match(grp)))
})

test_that("same() aborts where it names one attribute more than once", {
  expect_error(terms_of(a ~ same(grp) + same(grp, diff = TRUE)), "more than once")
  expect_error(terms_of(a ~ same(grp) + match(grp)), "more than once")
  expect_error(terms_of(a ~ same(grp) + same("grp")), "more than once")
  # An interaction is another column, and does not add up to the main effect.
  expect_no_error(terms_of(a ~ same(grp) + same(grp, diff = TRUE):ego(x)))
  expect_error(terms_of(a ~ same(grp, diff = "yes")), "TRUE")
})

test_that("mix() gives one dummy per ordered pair, less the first", {
  m <- suppressMessages(terms_of(a ~ mix(grp)))
  # In ergm's order for nodemix(): the sender's category changes fastest.
  expect_named(m, c("a", "mix_grp_q_p", "mix_grp_p_q", "mix_grp_q_q"))
  expect_equal(unname(m[["mix_grp_q_p"]]), outer(GRP == "q", GRP == "p") * 1)
  # The pairs and the reference cover every dyad exactly once.
  total <- m[["mix_grp_p_q"]] + m[["mix_grp_q_p"]] + m[["mix_grp_q_q"]] +
    outer(GRP == "p", GRP == "p")
  expect_true(all(total == 1))
})

test_that("mix() names its reference pair", {
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(terms_of(a ~ mix(grp)), "reference")
})

test_that("mix() takes unordered pairs on an undirected network", {
  g <- manynet::mutate(qap_net_undirected(n = 8), grp = rep(c("p", "q"), 4))
  m <- suppressMessages(terms_of(weight ~ mix(grp), g))
  expect_named(m, c("weight", "mix_grp_p_q", "mix_grp_q_q"))
  expect_true(isSymmetric(unname(m[["mix_grp_p_q"]])))
})

test_that("mix() pairs the categories of the two modes of a two-mode network", {
  sw <- manynet::ison_southern_women
  type <- manynet::node_is_mode(sw)
  alt <- (seq_along(type) %% 2) + 1
  sw <- manynet::mutate(sw, grp = ifelse(type, c("e1", "e2")[alt], c("w1", "w2")[alt]))
  m <- suppressMessages(terms_of(. ~ mix(grp), sw))
  expect_named(m, c("ties", "mix_grp_w2_e1", "mix_grp_w1_e2", "mix_grp_w2_e2"))
})

test_that("ego() and alter() take levels = as names or positions", {
  m <- suppressMessages(terms_of(a ~ ego(grp, levels = "q") + alter(grp, -1)))
  expect_named(m, c("a", "ego_grp_q", "alter_grp_q"))
  # Naming every category makes the first named the reference.
  m <- suppressMessages(terms_of(a ~ ego(grp, levels = c("q", "p"))))
  expect_named(m, c("a", "ego_grp_p"))
  expect_equal(unname(m[["ego_grp_p"]]), matrix(GRP == "p", 6, 6) * 1)
  expect_equal(names(suppressMessages(terms_of(a ~ ego(grp, levels = 2)))),
               c("a", "ego_grp_q"))
})

test_that("the categories that levels = leaves out are the reference together", {
  g <- manynet::mutate(formula_net(), dept = c("a", "b", "c", "a", "b", "c"))
  m <- suppressMessages(terms_of(a ~ ego(dept, levels = "c"), g))
  expect_named(m, c("a", "ego_dept_c"))
  tt <- table_of(a ~ ego(dept, levels = "c"), g)
  expect_equal(tt$reference, "a, b")
  old <- options(snet_verbosity = "verbose")
  on.exit(options(old), add = TRUE)
  expect_message(terms_of(a ~ ego(dept, levels = "c"), g), "reference")
})

test_that("levels = aborts on an unknown name, a bad position, or a numeric attribute", {
  expect_error(terms_of(a ~ ego(grp, levels = "z")), "\"p\" and \"q\"")
  expect_error(terms_of(a ~ ego(grp, levels = 3)), "positions from 1 to 2")
  expect_error(terms_of(a ~ ego(grp, levels = c(1, -2))), "positions")
  expect_error(terms_of(a ~ ego(grp, levels = 0)), "positions")
  expect_error(terms_of(a ~ ego(grp, levels = TRUE)), "names of categories")
  expect_error(terms_of(a ~ ego(grp, levels = -(1:2))), "every category")
  expect_error(terms_of(a ~ ego(x, levels = 1)), "categorical")
})

test_that("same() takes levels =, with or without diff = TRUE", {
  m <- terms_of(a ~ same(grp, diff = TRUE, levels = "q"))
  expect_named(m, c("a", "same_grp_q"))
  expect_equal(unname(m[["same_grp_q"]]), outer(GRP == "q", GRP == "q") * 1)
  m <- terms_of(a ~ same(grp, levels = "q"))
  expect_equal(unname(m[["same_grp"]]), outer(GRP == "q", GRP == "q") * 1)
  m <- terms_of(a ~ same(x, diff = TRUE, levels = -1))
  expect_named(m, c("a", "same_x_3", "same_x_4", "same_x_5", "same_x_9"))
})

test_that("mix() takes levels = as pairs or their positions", {
  m <- suppressMessages(terms_of(a ~ mix(grp, levels = c("q->q", "p -> q"))))
  expect_named(m, c("a", "mix_grp_q_q", "mix_grp_p_q"))
  expect_equal(unname(m[["mix_grp_p_q"]]), outer(GRP == "p", GRP == "q") * 1)
  m <- suppressMessages(terms_of(a ~ mix(grp, levels = -(1:2))))
  expect_named(m, c("a", "mix_grp_p_q", "mix_grp_q_q"))
  tt <- table_of(a ~ mix(grp, levels = "q -> q"))
  expect_equal(tt$reference, "p -> p, q -> p, p -> q")
  expect_error(terms_of(a ~ mix(grp, levels = "p -- q")), "has no pair")
  g <- manynet::mutate(qap_net_undirected(n = 8), grp = rep(c("p", "q"), 4))
  m <- suppressMessages(terms_of(weight ~ mix(grp, levels = "p--q"), g))
  expect_named(m, c("weight", "mix_grp_p_q"))
})

test_that("more than ten categories warn unless levels = chooses them", {
  g <- manynet::mutate(qap_net_gaussian(n = 12), many = letters[1:12])
  expect_warning(suppressMessages(terms_of(weight ~ ego(many), g)), "levels")
  expect_warning(terms_of(weight ~ same(many, diff = TRUE), g), "12 categories")
  expect_no_warning(suppressMessages(
    terms_of(weight ~ ego(many, levels = c("a", "b")), g)))
  expect_no_warning(terms_of(weight ~ same(Grp, diff = TRUE), g))
})

# ---- layers --------------------------------------------------------------------

test_that("a layer gives the same matrix bare or through tie()", {
  m <- terms_of(a ~ b + tie(b))
  expect_equal(unname(m[["b"]]), unname(m[["tie_b"]]))
  expect_equal(unname(m[["tie_b"]]), layer_of("b"))
})

test_that("tie() gives 0/1 unless weighted", {
  g <- qap_net_gaussian(n = 10)
  g <- manynet::mutate_ties(g, w2 = abs(manynet::tie_attribute(g, "weight")))
  m <- terms_of(weight ~ tie(w2), g)
  expect_true(all(m[["tie_w2"]] %in% c(0, 1)))
  m <- terms_of(weight ~ tie(w2, weighted = TRUE), g)
  expect_false(all(m[["tie_w2"]] %in% c(0, 1)))
})

test_that("recip() and exchange() are the transpose of the layer", {
  m <- terms_of(a ~ recip(b) + exchange(b))
  expect_equal(unname(m[["recip_b"]]), t(layer_of("b")))
  expect_equal(unname(m[["exchange_b"]]), t(layer_of("b")))
})

test_that("recip() and exchange() give 0/1 with weighted = FALSE, and values with TRUE", {
  g <- qap_net_gaussian(n = 10)
  g <- manynet::mutate_ties(g, w2 = abs(manynet::tie_attribute(g, "weight")))
  w2 <- unname(t(manynet::as_matrix(manynet::to_uniplex(g, "w2"))))
  m <- terms_of(weight ~ recip(w2, weighted = FALSE) + exchange(w2), g)
  expect_equal(unname(m[["recip_w2"]]), (w2 > 0) * 1)
  # Without weighted =, the values are as stored.
  expect_equal(unname(m[["exchange_w2"]]), w2)
  m <- terms_of(weight ~ recip(w2, weighted = TRUE), g)
  expect_equal(unname(m[["recip_w2"]]), w2)
})

test_that("recip on its own reads the outcome's reverse tie, and warns", {
  expect_warning(m <- terms_of(a ~ recip), "outcome's own ties")
  expect_equal(unname(m[["recip"]]), t(layer_of("a")))
  expect_warning(m2 <- terms_of(a ~ recip()), "outcome's own ties")
  expect_equal(m2[["recip"]], m[["recip"]])
  expect_warning(terms_of(a ~ recip(a)), "outcome's own ties")
  expect_error(terms_of(a ~ exchange()), "needs a layer")
})

test_that("a bare term name that is also a layer aborts", {
  g <- manynet::mutate_ties(qap_net_gaussian(n = 8), recip = 1)
  expect_error(terms_of(weight ~ recip, g), "both a layer")
})

test_that("indeg() and outdeg() count ties on the side they are asked for", {
  b <- layer_of("b")
  m <- terms_of(a ~ indeg(b) + indeg(b, node = "ego") +
                  outdeg(b, node = "alter") + outdeg(b, node = "ego"))
  n <- nrow(b)
  expect_equal(unname(m[["indeg_b"]]), matrix(colSums(b), n, n, byrow = TRUE))
  expect_equal(unname(m[["ego_indeg_b"]]), matrix(colSums(b), n, n))
  expect_equal(unname(m[["outdeg_b"]]), matrix(rowSums(b), n, n, byrow = TRUE))
  expect_equal(unname(m[["ego_outdeg_b"]]), matrix(rowSums(b), n, n))
})

test_that("indeg() reads the receiver by default, and outdeg() the sender", {
  b <- layer_of("b")
  m <- terms_of(a ~ indeg(b) + outdeg(b))
  n <- nrow(b)
  expect_named(m, c("a", "indeg_b", "ego_outdeg_b"), ignore.order = TRUE)
  expect_equal(unname(m[["ego_outdeg_b"]]), matrix(rowSums(b), n, n))
})

test_that("type = is a deprecated alias of node =", {
  b <- layer_of("b")
  .infernet_state$type_warned <- NULL
  expect_warning(m <- terms_of(a ~ outdeg(b, type = "alter")), "deprecated")
  expect_equal(unname(m[["outdeg_b"]]), matrix(rowSums(b), nrow(b), nrow(b), byrow = TRUE))
  # Said once, not once per network of a list.
  expect_no_warning(terms_of(a ~ indeg(b, type = "ego")))
  expect_error(terms_of(a ~ indeg(b, node = "ego", type = "ego")), "not both")
  expect_error(terms_of(a ~ indeg(b, node = "sender")), "alter")
})

test_that("indeg() on a two-mode network reads only the side that receives", {
  # Built directly: manynet gives a two-mode network with a second tie
  # attribute a square matrix, so a two-mode covariate layer cannot be made yet.
  g <- manynet::as_tidygraph(qap_net_twomode_wide(nr = 6, nc = 9))
  inc <- manynet::as_matrix(g)
  ctx <- list(net = g, twomode = TRUE, css = FALSE, outcome_layer = NA_character_,
              env = globalenv(), DV = inc)
  op <- .specify(y ~ indeg(weight))$terms[[1]]$operands[[1]]
  expect_equal(unname(.build_degree(op, ctx, "in")[["indeg_weight"]]),
               unname(matrix(colSums(inc > 0), 6, 9, byrow = TRUE)))
  op <- .specify(y ~ indeg(weight, node = "ego"))$terms[[1]]$operands[[1]]
  expect_error(.build_degree(op, ctx, "in"), "two-mode")
  # The defaults read the side of a two-mode network that has the degree.
  op <- .specify(y ~ outdeg(weight))$terms[[1]]$operands[[1]]
  expect_equal(unname(.build_degree(op, ctx, "out")[["ego_outdeg_weight"]]),
               unname(matrix(rowSums(inc > 0), 6, 9)))
})

test_that("a layer term refuses the ties of a uniplex outcome too", {
  g <- qap_net_gaussian(n = 10)
  expect_error(terms_of(weight ~ indeg(weight), g), "outcome layer")
})

test_that("a layer term refuses the outcome layer", {
  expect_error(terms_of(a ~ indeg(a)), "outcome layer")
  expect_error(terms_of(a ~ tie(a)), "outcome layer")
})

test_that("recip() is refused on a two-mode network", {
  sw <- manynet::mutate_ties(manynet::ison_southern_women, ties = 1)
  expect_error(terms_of(. ~ recip(ties), sw), "two-mode")
})

# ---- others' attributes ----------------------------------------------------------

# A direct loop over the definition: the mean of x over the nodes k other than
# i that have a tie to j, and the mean of the defined cells where there are none.
tertius_by_loop <- function(m, x) {
  n <- nrow(m)
  out <- matrix(NA_real_, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i == j) next
    k <- setdiff(which(m[, j] > 0), c(i, j))
    if (length(k) > 0) out[i, j] <- mean(x[k])
  }
  out[is.na(out) & row(out) != col(out)] <- mean(out, na.rm = TRUE)
  diag(out) <- 0
  out
}

test_that("tertius() is the mean over j's other in-neighbours", {
  m <- terms_of(a ~ tertius(b, x))
  expect_equal(unname(m[["tertius_b_x"]]), tertius_by_loop(layer_of("b"), X))
  # By hand: A is reached by B (x = 1) and F (x = 9), so for the dyad from C
  # to A the mean is 5, and from F to A only B is left.
  expect_equal(unname(m[["tertius_b_x"]])[3, 1], 5)
  expect_equal(unname(m[["tertius_b_x"]])[6, 1], 1)
})

test_that("tertius_diff() is how far ego lies from that mean", {
  m <- terms_of(a ~ tertius_diff(b, x))
  b <- layer_of("b")
  n <- nrow(b)
  agg <- matrix(NA_real_, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    k <- setdiff(which(b[, j] > 0), c(i, j))
    if (i != j && length(k) > 0) agg[i, j] <- abs(X[i] - mean(X[k]))
  }
  agg[is.na(agg) & row(agg) != col(agg)] <- mean(agg, na.rm = TRUE)
  diag(agg) <- 0
  expect_equal(unname(m[["tertius_diff_b_x"]]), agg)
})

test_that("tertius() without a layer reads the outcome, and warns", {
  expect_warning(m <- terms_of(a ~ tertius(x)), "outcome's own ties")
  expect_equal(unname(m[["tertius_x"]]), tertius_by_loop(layer_of("a"), X))
  expect_warning(terms_of(a ~ tertius(a, x)), "outcome's own ties")
})

test_that("tertius() sum is unchanged: the tie-weighted sum over the others", {
  m <- suppressWarnings(terms_of(a ~ tertius(b, x, "sum")))
  b <- layer_of("b")
  expected <- t(vapply(seq_len(6), function(i) colSums((b * X)[-i, , drop = FALSE]),
                       numeric(6)))
  expect_equal(unname(m[["tertius_b_x"]]), expected)
})

# ---- network-level variables -------------------------------------------------------

test_that("global() gives each pooled network its own value", {
  nets <- list(formula_net(gdp = 5), formula_net(gdp = 8), formula_net(gdp = 2))
  fit <- estimate_mrqap(a ~ ego(x) + global(gdp), nets, times = 5,
                        control = list(seed = 1))
  expect_true("global_gdp" %in% names(fit$coefficients))
  expect_equal(sort(unique(fit$pred[["global_gdp"]])), c(2, 5, 8))
})

test_that("global() on one network is refused, since it is the intercept", {
  expect_error(terms_of(a ~ global(gdp)), "intercept")
})

# ---- fitting -----------------------------------------------------------------

test_that("the layer terms fit a CSS under both permutation schemes", {
  for (permute in c("predictor", "outcome")) {
    fit <- suppressMessages(
      estimate_mrqap(advice ~ recip(friends) + indeg(friends) + same(dept),
                     manynet::ison_hightech, times = 5,
                     control = list(seed = 1, permute = permute)))
    expect_s3_class(fit, "netreg_css")
    expect_named(fit$coefficients,
                 c("(Intercept)", "recip_friends", "indeg_friends", "same_dept"))
  }
})

test_that("the new terms fit a dyadic network under both permutation schemes", {
  for (permute in c("predictor", "outcome")) {
    fit <- suppressMessages(
      estimate_mrqap(a ~ recip(b) + indeg(b) + same(grp, diff = TRUE) + tertius(b, x),
                     formula_net(), times = 5,
                     control = list(seed = 1, permute = permute)))
    expect_s3_class(fit, "netreg")
    expect_false(anyNA(fit$coefficients))
  }
})

# ---- terms of other models ---------------------------------------------------

# The warnings a call raises, in order, so that two can be checked at once.
warnings_of <- function(expr) {
  out <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    out <<- c(out, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = out)
}

test_that("a goldfish formula fits without the terms that read the outcome", {
  res <- warnings_of(suppressMessages(
    estimate_mrqap(a ~ inertia + recip + trans(b) + ego(x), formula_net(),
                   times = 5, control = list(seed = 1))))
  expect_named(res$value$coefficients, c("(Intercept)", "recip", "ego_x"))
  # One warning names every dropped term and why; `recip` keeps its own.
  dropped <- grep("Dropped", res$warnings, value = TRUE)
  expect_length(dropped, 1)
  expect_match(dropped, "Dropped 2 terms")
  expect_match(dropped, "`inertia` needs a longitudinal network", fixed = TRUE)
  expect_match(dropped, "`trans(b)` reads another layer", fixed = TRUE)
  expect_length(grep("reads the outcome's own ties", res$warnings), 1)
})

test_that("a list of networks says once which terms it dropped", {
  res <- warnings_of(suppressMessages(
    estimate_mrqap(a ~ four + ego(x), list(formula_net(), formula_net(gdp = 6)),
                   times = 5, control = list(seed = 1))))
  expect_length(grep("Dropped", res$warnings), 1)
  expect_named(res$value$coefficients, c("(Intercept)", "ego_x"))
})

test_that("every registered term says what each model makes of it", {
  registry <- .formula_terms()
  for (term in names(registry)) {
    entry <- registry[[term]]
    expect_true(entry$mrqap %in% c("fits", "warns", "drops", "refuses"), info = term)
    expect_true(all(c("dynam", "saom", "ergm") %in% names(entry)), info = term)
    if (entry$mrqap == "drops") {
      expect_true(is.character(entry$reason) && nzchar(entry$reason), info = term)
    } else {
      expect_true(is.function(entry$build), info = term)
    }
  }
  expect_equal(registry$trans$dynam, "trans")
  expect_equal(registry$trans$saom, "transTrip")
})

# ---- the term table ----------------------------------------------------------

test_that("the term table has one row for each column, in their order", {
  tt <- table_of(a ~ ego(x) + ego(grp) + same(grp, diff = TRUE) + b + tie(b))
  m <- suppressMessages(terms_of(a ~ ego(x) + ego(grp) + same(grp, diff = TRUE) + b + tie(b)))
  expect_equal(tt$term, names(m)[-1])
  expect_named(tt, c("term", "effect", "layer", "attribute", "category",
                     "reference", "operands"))
  expect_equal(tt$effect, c("ego", "ego", "same", "same", "tie", "tie"))
  expect_equal(tt$attribute, c("x", "grp", "grp", "grp", NA, NA))
  expect_equal(tt$layer, c(NA, NA, NA, NA, "b", "b"))
})

test_that("the term table names a categorical term's category and reference", {
  tt <- table_of(a ~ ego(grp) + same(grp, diff = TRUE))
  expect_equal(tt$category, c("q", "p", "q"))
  expect_equal(tt$reference, c("p", NA, NA))
})

test_that("the term table keeps a category that the column name changes", {
  g <- manynet::mutate(formula_net(), grp = rep(c("a_1", "b c"), 3))
  tt <- table_of(a ~ alter(grp), g)
  expect_equal(tt$term, "alter_grp_b_c")
  expect_equal(tt$category, "b c")
  expect_equal(tt$reference, "a_1")
})

test_that("the term table writes a mix() pair as a tie, directed or not", {
  tt <- table_of(a ~ mix(grp))
  expect_equal(tt$category, c("q -> p", "p -> q", "q -> q"))
  expect_equal(unique(tt$reference), "p -> p")
  g <- manynet::mutate(qap_net_undirected(n = 8), grp = rep(c("p", "q"), 4))
  tt <- table_of(weight ~ mix(grp), g)
  expect_equal(tt$category, c("p -- q", "q -- q"))
})

test_that("the term table gives the layer and attribute of tertius()", {
  tt <- suppressWarnings(table_of(a ~ tertius(b, x) + indeg(b, node = "ego")))
  expect_equal(tt$effect, c("tertius", "indeg"))
  expect_equal(tt$layer, c("b", "b"))
  expect_equal(tt$attribute, c("x", NA))
})

test_that("the term table names the operands of each interaction column", {
  tt <- table_of(a ~ ego(x):same(grp, diff = TRUE))
  expect_equal(tt$term, c("ego_x:same_grp_p", "ego_x:same_grp_q"))
  expect_equal(tt$effect, c("interaction", "interaction"))
  expect_equal(tt$operands, list(c("ego_x", "same_grp_p"), c("ego_x", "same_grp_q")))
  expect_true(all(is.na(tt$attribute)))
})

test_that("a fit keeps the term table, one row for each coefficient", {
  fit <- suppressMessages(suppressWarnings(
    estimate_mrqap(a ~ ego(x) + alter(grp) + ego(x):b, formula_net(),
                   times = 5, control = list(seed = 1))))
  expect_s3_class(fit$terms, "tbl_df")
  expect_equal(fit$terms$term, names(fit$coefficients))
  expect_equal(fit$terms$effect, c("intercept", "ego", "alter", "interaction"))
  expect_equal(fit$terms$reference[3], "p")
})

test_that("a list of networks keeps the term table of the first", {
  fit <- suppressMessages(suppressWarnings(
    estimate_mrqap(a ~ ego(x) + same(grp, diff = TRUE), list(formula_net(), formula_net()),
                   times = 5, control = list(seed = 1))))
  expect_equal(fit$terms$term, names(fit$coefficients))
  expect_equal(fit$terms$category, c(NA, NA, "p", "q"))
})

# ---- the registry and the dictionary -----------------------------------------

test_that("every registered term is in the dictionary in CONTRIBUTING", {
  path <- testthat::test_path("..", "..", ".github", "CONTRIBUTING.md")
  skip_if_not(file.exists(path), "CONTRIBUTING.md is not in the built package")
  guide <- readLines(path)
  start <- grep("^#### The dictionary", guide)
  expect_length(start, 1)
  stop <- start + grep("^#", guide[(start + 1):length(guide)])[1]
  rows <- guide[start:stop]
  registry <- .formula_terms()
  for (term in names(registry)) {
    has <- rows[grepl(paste0("`", term, "("), rows, fixed = TRUE)]
    expect_true(length(has) > 0, info = term)
    # The MRQAP column, the second, begins with the registry's status.
    status <- vapply(strsplit(has, "|", fixed = TRUE), function(r) trimws(r[3]), "")
    expect_true(any(startsWith(status, registry[[term]]$mrqap)), info = term)
  }
})
