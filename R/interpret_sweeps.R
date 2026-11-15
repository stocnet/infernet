# Sweeping models over their settings ####

#' Sweeping a diffusion or an opinion dynamic over its settings
#' @name sweep
#' @description
#'   `sweep_diffusion()` plays `manynet::play_diffusion()`, and
#'   `sweep_opinion()` plays `manynet::play_opinion()`, `times` times for each
#'   setting of the arguments in `vary`, to show how the result changes with
#'   them: for example, the share of the network infected for each chance of
#'   transmission, or the number of opinion clusters for each confidence.
#'
#'   A sweep explores a model. To compare a model with an observed diffusion,
#'   or two models with each other, use `play_diffusions()` with `test_gof()`
#'   or `test_distributions()`.
#' @param .data A network of a manynet-consistent class, such as a matrix,
#'   an edgelist, an igraph, a network, a tbl_graph, or a stocnet object.
#' @param opinions For `sweep_opinion()`, the opinions at the start, as in
#'   `manynet::play_opinion()`: one value for each node, or the name of a
#'   node attribute. Give several sets of opinions in `vary` instead, as
#'   `vary = list(opinions = list(...))`.
#' @param ... Other arguments of `manynet::play_diffusion()` or
#'   `manynet::play_opinion()`, which are the same in every setting,
#'   such as `steps` or `rule`.
#' @param vary A named list of the values to try for one or more arguments,
#'   such as `list(transmissibility = c(0.1, 0.3, 0.5))`.
#'   Each value of a vector is one setting.
#'   For a setting that is itself a vector, such as a set of seeds,
#'   give a list: `list(seeds = list(1:10, 1:20))`.
#'   A named list names its settings: `list(seeds = list(hub = 1, edge = 5))`.
#'   With more than one argument, every combination of their values is played.
#' @param times The number of runs for each setting. By default 5.
#'   A model without chance, such as a diffusion with a `transmissibility`
#'   of 1, or the DeGroot or Hegselmann–Krause rule, gives the same result in
#'   every run.
#' @param control The controls of the runs, as a named list such as
#'   `list(ncores = 4, seed = 1)`. It reads `ncores`, `strategy`, and `seed`.
#'   The same seed gives the same runs sequentially and in parallel.
#'   A control that is not named keeps its default, or the value set with
#'   [set_control_computation()].
#' @return A table with a column first for each argument in `vary`, and the
#'   run in `sim`. Each column of a setting holds the value of the setting
#'   where each value is one number or string, the names of a named list,
#'   the number of seeds for a list of seeds, and otherwise the place of the
#'   setting.
#'
#'   `sweep_diffusion()` returns a `diffusionSweep`, with one row for each step
#'   of each run, and the counts of `manynet::as_diffusion()`.
#'   Its `summary()` gives, for each setting and run, the share of the nodes
#'   that were infected (`infected`, which counts a node again where it is
#'   reinfected), and the steps until every node adopted (`toa`), which is
#'   `NA` where not every node adopted.
#'
#'   `sweep_opinion()` returns an `opinionSweep`, with one row for each run:
#'   the number of opinion `clusters` at the end, whether they reach a
#'   `consensus`, the `spread` (standard deviation) and `mean` of the
#'   opinions at the end, the number of `steps` played, and whether the
#'   opinions `settled` before the last step.
#'   Two opinions are in one cluster where they are within 0.01 of each other,
#'   or of an opinion between them.
#'
#'   `print()` gives the mean of each for each setting, and returns `x`,
#'   invisibly. `plot()` from `{autograph}` draws the result against the
#'   first argument in `vary`.
#' @family interpreting functions
#' @examples
#' # The share infected for three chances of transmission
#' sweep_diffusion(ison_adolescents, steps = 10,
#'                 vary = list(transmissibility = c(0.1, 0.3, 0.5)),
#'                 control = list(seed = 1))
#' # The number of opinion clusters for three widths of confidence
#' set.seed(1)
#' sweep_opinion(ison_networkers, stats::runif(32), rule = "hegselmann_krause",
#'               vary = list(confidence = c(0.1, 0.2, 0.3)), times = 1)
NULL

#' @rdname sweep
#' @export
sweep_diffusion <- function(.data, ..., vary, times = 5, control = list()) {
  args <- list(...)
  # `manynet::play_diffusion()` calls `igraph::neighborhood()` on `.data`
  # directly, so it stops on a stocnet object.
  if (inherits(.data, "stocnet")) .data <- manynet::as_igraph(.data)
  out <- .sweep(.data, args, vary, times, control,
                fun = manynet::play_diffusion, name = "manynet::play_diffusion",
                run = function(.data, args) .play_report(.data, args))
  .make_sweep(.bind_reports(out), "diffusionSweep", out, .data)
}

#' @rdname sweep
#' @export
sweep_opinion <- function(.data, opinions, ..., vary, times = 5,
                          control = list()) {
  args <- list(...)
  if (!missing(opinions)) {
    if (is.matrix(opinions)) {
      manynet::snet_abort(
        c("{.fn sweep_opinion} takes one issue, but {.arg opinions} is a matrix.",
          i = "Sweep one column at a time."))
    }
    args$opinions <- opinions
  }
  out <- .sweep(.data, args, vary, times, control,
                fun = manynet::play_opinion, name = "manynet::play_opinion",
                run = .opinion_ends)
  .make_sweep(tibble::as_tibble(do.call(rbind, out)), "opinionSweep", out, .data)
}

# Plays `run` `times` times for each setting of `vary`, each run with its own
# random stream, and returns the runs that report, with a column for each
# argument in `vary` and the run in `sim`. The names of the arguments in
# `vary` are in the attribute "vary" of the first run.
#' @keywords internal
#' @noRd
.sweep <- function(.data, args, vary, times, control, fun, name, run) {
  if (missing(vary) || is.null(vary)) {
    manynet::snet_abort(
      c("A sweep needs the settings to try, in {.arg vary}.",
        i = "For example, {.code vary = list(transmissibility = c(0.1, 0.5))}."))
  }
  test <- .test_control(control, args)
  ctrl <- test$ctrl
  args <- test$args
  settings <- .vary_settings(vary, args, fun, name)
  if (!is.null(ctrl$seed)) set.seed(ctrl$seed)
  old_plan <- setup_future_plan(ctrl$strategy, ctrl$ncores)
  on.exit(restore_future_plan(old_plan), add = TRUE)
  tasks <- expand.grid(sim = seq_len(times),
                       setting = seq_along(settings$args))
  out <- future.apply::future_lapply(seq_len(nrow(tasks)), function(r) {
    s <- tasks$setting[r]
    res <- run(.data, utils::modifyList(args, settings$args[[s]]))
    if (is.null(res)) return(NULL)
    data.frame(settings$labels[s, , drop = FALSE], sim = tasks$sim[r], res,
               check.names = FALSE)
  }, future.seed = TRUE)
  empty <- sum(vapply(out, is.null, logical(1)))
  if (empty == nrow(tasks)) {
    manynet::snet_abort("None of the {nrow(tasks)} runs returned a report.")
  }
  if (empty > 0) {
    manynet::snet_warn("Dropped {empty} of {nrow(tasks)} runs that returned an empty report.")
  }
  out <- out[!vapply(out, is.null, logical(1))]
  attr(out, "vary") <- names(settings$labels)
  attr(out, "names_of_vary") <- settings$names
  attr(out, "control") <- ctrl
  out
}

#' @keywords internal
#' @noRd
# The runs of a sweep as one table of class `class`, with the settings and
# controls that `runs` records.
.make_sweep <- function(out, class, runs, .data) {
  class(out) <- c(class, setdiff(class(out), class))
  attr(out, "vary") <- attr(runs, "vary")
  attr(out, "control") <- attr(runs, "control")
  attr(out, "names_of_vary") <- attr(runs, "names_of_vary")
  attr(out, "mode") <- manynet::node_is_mode(.data)
  out
}

# One run of `manynet::play_opinion()`, as one row of measures of the opinions
# at the end.
#' @keywords internal
#' @noRd
.opinion_ends <- function(.data, args) {
  played <- do.call(manynet::play_opinion, c(list(.data), args))
  path <- as.data.frame(manynet::as_opinion(played))
  ends <- as.numeric(unlist(path[nrow(path), ]))
  steps <- nrow(path) - 1
  most <- if (is.null(args$steps)) manynet::net_nodes(.data) else args$steps
  clusters <- 1 + sum(diff(sort(ends)) > 0.01)
  data.frame(clusters = clusters, consensus = clusters == 1,
             spread = stats::sd(ends), mean = mean(ends),
             steps = steps, settled = steps < most)
}

# The settings of a sweep: a list with the values of each argument in `vary`
# for each setting, and a tibble with a label for each value. `fun` is the
# function that each run plays, and `name` how the messages name it.
#' @keywords internal
#' @noRd
.vary_settings <- function(vary, args, fun, name) {
  if (!is.list(vary) || length(vary) == 0 || is.null(names(vary)) ||
      any(!nzchar(names(vary)))) {
    manynet::snet_abort(
      c("{.arg vary} must be a named list of the values of each argument.",
        i = "For example, {.code vary = list(transmissibility = c(0.1, 0.5))}."))
  }
  known <- setdiff(names(formals(fun)), c(".data", "..."))
  unknown <- setdiff(names(vary), known)
  if (length(unknown) > 0) {
    manynet::snet_abort(
      "{.val {unknown}} {?is not an argument/are not arguments} of {.fn {name}}.")
  }
  clash <- intersect(names(vary), names(args))
  if (length(clash) > 0) {
    manynet::snet_abort(
      "{.arg {clash}} is given both in {.arg vary} and as an argument.")
  }
  values <- lapply(vary, function(v) if (is.list(v)) v else as.list(v))
  if (any(lengths(values) == 0)) {
    manynet::snet_abort("Each argument in {.arg vary} needs at least one value.")
  }
  labels <- mapply(.vary_labels, values, names(values), SIMPLIFY = FALSE)
  # a longer name for a column that counts, rather than names, its settings
  counted <- vapply(names(values), function(a) a == "seeds" &&
                      is.numeric(labels[[a]]) &&
                      !all(vapply(values[[a]], length, integer(1)) == 1),
                    logical(1))
  # every combination, with the first argument changing fastest
  grid <- expand.grid(lapply(values, seq_along), KEEP.OUT.ATTRS = FALSE)
  list(args = lapply(seq_len(nrow(grid)), function(s)
         stats::setNames(lapply(names(values), function(a)
           values[[a]][[grid[s, a]]]), names(values))),
       labels = tibble::as_tibble(lapply(stats::setNames(names(values),
                                                         names(values)),
                                         function(a) labels[[a]][grid[[a]]])),
       names = c(seeds = "number of seeds")[names(values)[counted]])
}

# A label for each value of an argument in `vary`: its name, where the values
# are named; the value, where each is one number or string; the number of
# seeds, for `seeds`; and otherwise its place in the list.
#' @keywords internal
#' @noRd
.vary_labels <- function(values, name) {
  if (!is.null(names(values)) && all(nzchar(names(values)))) {
    return(factor(names(values), levels = unique(names(values))))
  }
  single <- all(vapply(values, function(v) is.atomic(v) && length(v) == 1,
                       logical(1)))
  if (single) {
    out <- unlist(values)
    # strings keep the order in which they are given
    if (is.character(out)) out <- factor(out, levels = unique(out))
    return(out)
  }
  if (name == "seeds" && all(vapply(values, is.numeric, logical(1)))) {
    return(lengths(values))
  }
  seq_along(values)
}

# Reporting sweeps ####

#' @rdname sweep
#' @param object,x A `diffusionSweep` or an `opinionSweep`.
#' @export
summary.diffusionSweep <- function(object, ...) {
  vary <- attr(object, "vary")
  y <- as.data.frame(object)
  if (!all(c("n", "I") %in% names(y))) {
    manynet::snet_abort("A {.cls diffusionSweep} needs the counts {.val n} and {.val I}.")
  }
  key <- interaction(y[c(vary, "sim")], drop = TRUE, lex.order = TRUE)
  # the seeds are the new infections at the first step of each run
  new <- if ("I_new" %in% names(y)) y$I_new else
    stats::ave(y$I, key, FUN = function(v) c(v[1], pmax(diff(v), 0)))
  first <- !duplicated(key)
  at <- as.character(key[first])
  out <- y[first, c(vary, "sim"), drop = FALSE]
  out$infected <- as.numeric(tapply(new, key, sum)[at]) / y$n[first]
  # the steps until every node adopted, or NA where not every node adopted
  steps <- as.numeric(tapply((y$I != y$n) * 1, key, sum)[at]) + 1
  all_in <- as.logical(tapply(y$I == y$n, key, any)[at])
  out$toa <- ifelse(all_in, steps, NA_real_)
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

#' @rdname sweep
#' @export
summary.opinionSweep <- function(object, ...) {
  out <- tibble::as_tibble(as.data.frame(object))
  attr(out, "vary") <- NULL
  out
}

# What `print()` reports of each kind of sweep: what its runs are, a note on
# its table, and the statistics of the table, each a function of the runs of
# one setting.
#' @keywords internal
#' @noRd
.sweep_report <- function(x) {
  if (inherits(x, "diffusionSweep")) {
    list(runs = summary.diffusionSweep(x), what = "diffusions",
         note = paste("For each setting: the mean share infected, the share of runs",
                      "in which every node adopted, and their mean steps until then."),
         stats = list(
           infected = function(r) mean(r$infected),
           complete = function(r) mean(!is.na(r$toa)),
           toa = function(r) if (all(is.na(r$toa))) NA_real_ else
             mean(r$toa, na.rm = TRUE)))
  } else {
    list(runs = summary.opinionSweep(x), what = "opinion dynamics",
         note = paste("For each setting: the mean number of clusters at the end, and the",
                      "share of runs that reach a consensus or settle."),
         stats = list(
           clusters = function(r) mean(r$clusters),
           consensus = function(r) mean(r$consensus),
           spread = function(r) mean(r$spread),
           settled = function(r) mean(r$settled)))
  }
}

# The mean of each statistic for each setting of a sweep.
#' @keywords internal
#' @noRd
.sweep_means <- function(x, report = .sweep_report(x)) {
  vary <- attr(x, "vary")
  runs <- report$runs
  groups <- interaction(runs[vary], drop = TRUE, lex.order = TRUE)
  first <- !duplicated(groups)
  means <- runs[first, vary, drop = FALSE]
  parts <- split(runs, groups)[as.character(groups[first])]
  for (s in names(report$stats)) {
    means[[s]] <- vapply(parts, report$stats[[s]], numeric(1))
  }
  tibble::as_tibble(means)
}

#' @export
#' @noRd
print.diffusionSweep <- function(x, ...) .print_sweep(x, ...)

#' @export
#' @noRd
print.opinionSweep <- function(x, ...) .print_sweep(x, ...)

# Prints a title, the mean of each statistic for each setting, and a footer.
#' @keywords internal
#' @noRd
.print_sweep <- function(x, ...) {
  report <- .sweep_report(x)
  vary <- attr(x, "vary")
  means <- .sweep_means(x, report)
  .print_title(paste0(nrow(report$runs), " simulated ", report$what, " over ",
                      nrow(means), " setting", if (nrow(means) != 1) "s", " of ",
                      paste(vary, collapse = " and ")))
  print(means, ...)
  .print_footer(c(report$note, .controls_line(attr(x, "control"))),
                .methods_hint("summary() for each run",
                              "and plot() from {autograph} for the bars"))
  invisible(x)
}

# A sweep in an R Markdown document or a learnr tutorial shows its table of
# means for each setting. Without these methods, `{rmarkdown}` printed it as
# a data frame, with every step of every run.
#' @exportS3Method knitr::knit_print
knit_print.diffusionSweep <- function(x, ...) {
  knitr::knit_print(as.data.frame(.sweep_means(x)), ...)
}

#' @exportS3Method knitr::knit_print
knit_print.opinionSweep <- function(x, ...) {
  knitr::knit_print(as.data.frame(.sweep_means(x)), ...)
}
