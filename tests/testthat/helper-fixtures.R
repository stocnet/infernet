# Shared fixtures: the networks the tests fit, each small and seeded.
#
# Every network carries real signal, so that each family converges and a
# comparison with a standard fit is not testing noise against noise. Each one
# takes its size and seed as arguments, and the defaults are what the tests use.
# `times` stays small everywhere: the tests check the estimator, not the
# precision of the null distribution.

# ---- one-mode and two-mode networks --------------------------------------------

qap_net_gaussian <- function(n = 30, seed = 101) {
  set.seed(seed)
  age <- stats::runif(n, 20, 60)
  m <- outer(age, age, function(a, b) 0.05 * a - 0.03 * b) +
    matrix(stats::rnorm(n^2, sd = 0.5), n, n)
  diag(m) <- 0
  manynet::mutate(manynet::as_tidygraph(m),
                  Age = age,
                  Cit = stats::rpois(n, 5),
                  Grp = rep(c("a", "b", "c"), length.out = n))
}

qap_net_binary <- function(n = 30, seed = 102) {
  set.seed(seed)
  age <- stats::runif(n, 20, 60)
  p <- stats::plogis(outer(age, age, function(a, b) 0.06 * (a - 40) - 0.04 * (b - 40)))
  m <- matrix(stats::rbinom(n^2, 1, p), n, n)
  diag(m) <- 0
  manynet::mutate(manynet::as_tidygraph(m),
                  Age = age,
                  Grp = rep(c("a", "b", "c"), length.out = n))
}

qap_net_count <- function(n = 30, seed = 103) {
  set.seed(seed)
  age <- stats::runif(n, 20, 60)
  lambda <- exp(outer(age, age, function(a, b) 0.02 * (a - 40) - 0.01 * (b - 40)))
  m <- matrix(stats::rpois(n^2, lambda), n, n)
  diag(m) <- 0
  manynet::mutate(manynet::as_tidygraph(m), Age = age)
}

qap_net_zip <- function(n = 30, seed = 104) {
  set.seed(seed)
  age <- stats::runif(n, 20, 60)
  lambda <- exp(outer(age, age, function(a, b) 0.02 * (a - 40) - 0.01 * (b - 40)))
  m <- matrix(stats::rpois(n^2, lambda) * stats::rbinom(n^2, 1, 0.7), n, n)
  diag(m) <- 0
  manynet::mutate(manynet::as_tidygraph(m), Age = age)
}

qap_net_undirected <- function(n = 24, seed = 105) {
  set.seed(seed)
  age <- stats::runif(n, 20, 60)
  m <- outer(age, age, function(a, b) 0.02 * (a + b)) +
    matrix(stats::rnorm(n^2, sd = 0.5), n, n)
  m <- (m + t(m)) / 2
  diag(m) <- 0
  manynet::mutate(manynet::as_tidygraph(m, twomode = FALSE), Age = age)
}

# A two-mode network with more columns than rows. This shape is what turned the
# square-matrix assumption into an error rather than a wrong number: the
# predictor held more cells than the validity mask, and a logical index longer
# than its target extends that target with NA. See stocnet/infernet#4.
qap_net_twomode_wide <- function(nr = 12, nc = 40, seed = 107) {
  set.seed(seed)
  m <- matrix(stats::rbinom(nr * nc, size = 4, prob = 0.3), nr, nc)
  g <- manynet::as_igraph(m)
  manynet::mutate_nodes(g,
    GONGO    = c(rep(c("GON", "GO"), length.out = nr), rep(NA_character_, nc)),
    province = c(rep(LETTERS[1:4], length.out = nr), rep(NA_character_, nc)),
    Att      = c(stats::runif(nr), rep(NA_real_, nc)))
}

qap_net_twomode <- function(seed = 106) {
  set.seed(seed)
  sw <- manynet::ison_southern_women
  manynet::mutate(sw, Att = stats::runif(manynet::net_nodes(sw)))
}

# A longitudinal network: the same nodes over several waves, each wave's ties
# depending on the previous wave's ties (inertia), their reverse (reciprocity),
# and on age.
qap_net_panel <- function(n = 15, waves = 3, seed = 108) {
  set.seed(seed)
  age <- stats::runif(n, 20, 60)
  m <- matrix(stats::rbinom(n^2, 1, 0.2), n, n)
  diag(m) <- 0
  el <- vector("list", waves)
  for (t in seq_len(waves)) {
    if (t > 1) {
      p <- stats::plogis(-2 + 3 * m + 2 * t(m) + 0.03 * outer(age, age, "-"))
      m <- matrix(stats::rbinom(n^2, 1, p), n, n)
      diag(m) <- 0
    }
    w <- which(m == 1, arr.ind = TRUE)
    el[[t]] <- data.frame(from = w[, 1], to = w[, 2], time = t)
  }
  manynet::as_tidygraph(list(
    nodes = data.frame(name = paste0("n", seq_len(n)), age = age),
    ties = do.call(rbind, el)))
}

# A small multiplex network whose matrices can be checked by hand: two directed
# layers, `a` and `b`, a numeric attribute `x`, and a categorical one `grp`.
# `gdp` is a global variable, for the list path.
formula_net <- function(gdp = 5) {
  ties <- data.frame(
    from  = c(1L, 2L, 3L, 4L, 5L, 6L, 1L, 3L,   2L, 3L, 4L, 5L, 6L, 1L),
    to    = c(2L, 3L, 1L, 1L, 1L, 2L, 3L, 2L,   1L, 4L, 5L, 6L, 1L, 4L),
    layer = c(rep("a", 8), rep("b", 6)))
  manynet::make_stocnet(
    nodes   = data.frame(label = LETTERS[1:6],
                         x     = c(3, 1, 4, 1, 5, 9),
                         grp   = c("p", "q", "p", "q", "p", "q")),
    ties    = ties,
    info    = list(directed = TRUE),
    globals = data.frame(var = "gdp", value = gdp))
}

# ---- cognitive social structures ---------------------------------------------

# The first `n` managers of `manynet::ison_hightech`, with only their reports.
# It keeps the three layers, two of them cognitive, and all five departments,
# and fits 10 x 10 x 9 cells rather than 21 x 21 x 20, in a third of the time.
# The real network is fitted once, in test-qap_css.R.
css_hightech <- function(n = 10) {
  ht <- manynet::ison_hightech
  ht$nodes <- ht$nodes[seq_len(n), ]
  keep <- ht$ties$from <= n & ht$ties$to <= n &
    (is.na(ht$ties$by) | ht$ties$by <= n)
  ht$ties <- ht$ties[keep, ]
  ht
}

# A directed CSS of `n` nodes, in which the first `reporting` nodes report and
# the others do not. `mark_missing` records the others as not asked.
css_net <- function(n = 10, reporting = 8, directed = TRUE,
                    mark_missing = FALSE, seed = 3) {
  set.seed(seed)
  truth <- matrix(stats::rbinom(n^2, 1, 0.3), n, n)
  diag(truth) <- 0
  el <- do.call(rbind, lapply(seq_len(reporting), function(k) {
    m <- pmin(truth + matrix(stats::rbinom(n^2, 1, 0.1), n, n), 1)
    diag(m) <- 0
    if (!directed) m[lower.tri(m)] <- 0
    w <- which(m == 1, arr.ind = TRUE)
    data.frame(from = w[, 1], to = w[, 2], by = k)
  }))
  el[] <- lapply(el, as.integer)
  nodes <- data.frame(label = LETTERS[seq_len(n)],
                      age = round(stats::runif(n, 20, 60)))
  g <- manynet::make_stocnet(ties = el, nodes = nodes,
                             info = list(directed = directed))
  if (mark_missing) {
    g <- manynet::mutate_nodes(g, na = seq_len(n) > reporting)
  }
  g
}

# A two-mode CSS: people report which events other people attended.
css_net_twomode <- function(people = 4, events = 3, seed = 4) {
  set.seed(seed)
  nodes <- data.frame(label = c(paste0("p", seq_len(people)), paste0("e", seq_len(events))),
                      mode = rep(c("people", "events"), c(people, events)),
                      age = c(stats::runif(people, 20, 60), rep(NA, events)))
  el <- do.call(rbind, lapply(seq_len(people + events), function(k) {
    w <- which(matrix(stats::rbinom(people * events, 1, 0.5), people, events) == 1,
               arr.ind = TRUE)
    data.frame(from = w[, 1], to = people + w[, 2], by = k)
  }))
  el[] <- lapply(el, as.integer)
  manynet::make_stocnet(ties = el, nodes = nodes)
}

css_fixture <- function(n = 8, seed = 7) {
  set.seed(seed)
  # One true network, which every reporter sees with some noise.
  truth <- matrix(stats::rbinom(n^2, 1, 0.35), n, n)
  diag(truth) <- 0
  y <- array(0L, dim = c(n, n, n))
  for (p in seq_len(n)) {
    y[, , p] <- pmin(truth + matrix(stats::rbinom(n^2, 1, 0.12), n, n), 1)
    diag(y[, , p]) <- 0
  }
  # A dyadic covariate, the same for every reporter.
  dyadic <- outer(stats::runif(n), stats::runif(n), "+")
  x <- array(rep(as.vector(dyadic), n), dim = c(n, n, n))
  list(ties = y, cov = x)
}

# ---- for the tests of parameters ---------------------------------------------

# A network whose outcome depends on the sender's Age, with a sender effect in
# the noise, and an attribute B correlated with Age whose coefficient is
# `b_alter_B`.
param_net <- function(n = 10, family = "gaussian", b_alter_B = 0) {
  age <- stats::rnorm(n)
  B <- 0.6 * age + stats::rnorm(n, sd = 0.8)
  eta <- outer(age, B, function(a, b) 0.5 * a + b_alter_B * b) +
    stats::rnorm(n, sd = 0.5)
  m <- switch(family,
              gaussian = eta + matrix(stats::rnorm(n^2), n, n),
              binomial = matrix(stats::rbinom(n^2, 1, stats::plogis(eta - 0.5)), n, n))
  diag(m) <- 0
  manynet::mutate_nodes(manynet::as_tidygraph(m), Age = age, B = B,
                        Grp = rep(c("a", "b", "c"), length.out = n))
}

# A panel of three waves whose density changes, and whose effect of the
# sender's age is `b` in the second wave and in the third.
param_panel <- function(b = c(0.04, 0.04), n = 15) {
  age <- stats::runif(n, 20, 60)
  m <- matrix(stats::rbinom(n^2, 1, 0.2), n, n)
  diag(m) <- 0
  dens <- c(-1.5, -1)
  el <- list(data.frame(which(m == 1, arr.ind = TRUE), time = 1))
  for (t in 2:3) {
    p <- stats::plogis(dens[t - 1] + 1.5 * t(m) + b[t - 1] * (age - 40))
    m <- matrix(stats::rbinom(n^2, 1, p), n, n)
    diag(m) <- 0
    el[[t]] <- data.frame(which(m == 1, arr.ind = TRUE), time = t)
  }
  ties <- do.call(rbind, el)
  names(ties) <- c("from", "to", "time")
  manynet::as_tidygraph(list(nodes = data.frame(name = paste0("n", seq_len(n)),
                                                age = age),
                             ties = ties))
}

# ---- missing and fixed cells ------------------------------------------------

# A 20-node network with four nodes unobserved on the outcome and five other
# nodes unobserved on a predictor, as in the roadmap's demonstration.
holed_matlist <- function(n = 20, seed = 11) {
  set.seed(seed)
  age <- stats::runif(n, 20, 60)
  y <- outer(age, age, function(a, b) 0.05 * a - 0.03 * b) +
    matrix(stats::rnorm(n^2, sd = 0.5), n, n)
  diag(y) <- 0
  y[1:4, ] <- NA
  y[, 1:4] <- NA
  ego <- matrix(age, n, n)
  ego[5:9, ] <- NA
  alter <- matrix(stats::runif(n), n, n, byrow = TRUE)
  list(y = y, ego = ego, alter = alter)
}

# A tie that `{manynet}` marks as fixed was set by the design, not chosen, so it
# is no observation of the process: it leaves the model, is counted apart from
# the unobserved dyads, and stays in place under permutation.
fixed_net <- function(n = 15, seed = 31) {
  g <- qap_net_binary(n = n, seed = seed)
  ties <- manynet::as_edgelist(g)
  set.seed(seed)
  fixed <- seq_len(nrow(ties)) %in% sample(nrow(ties), 6)
  manynet::mutate_ties(g, fixed = fixed)
}
