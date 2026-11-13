# Infernet terms -> RSiena effects ----------------------------------------------
#
# `specify_saom()` in specify_formula.R writes each term as the RSiena effect
# that its registry entry names in `saom`, with its `to_saom` function. An
# effect is written as its RSiena short name, with the covariate or network it
# reads as its argument, such as `egoX(sex)` or `X(primary)`: the short name
# and the `interaction1` that `RSiena::includeEffects()` takes. With the
# network, the formula becomes the `sienaEffects` object that
# `RSiena::siena()` fits. RSiena is only needed for that last step.

# One effect: its label in the formula, and how RSiena names it. `note` says
# how the effect differs from the term it was written from.
#' @keywords internal
#' @noRd
.saom_effect <- function(short, x = "", y = "", note = NULL) {
  args <- c(x, y)
  args <- args[!is.na(args) & nzchar(args)]
  label <- if (length(args) > 0) paste0(short, "(", paste(args, collapse = ", "), ")") else short
  list(label = label, shortName = short,
       interaction1 = if (length(args) > 0) args[1] else "",
       interaction2 = if (length(args) > 1) args[2] else "",
       note = note)
}

# One term of the specification: its label, and either the effect it becomes
# or the reason it has no SAOM form.
#' @keywords internal
#' @noRd
.saom_term <- function(term, ctx) {
  for (op in term$operands) {
    if (is.na(op$fn)) next
    reason <- .saom_reason(op)
    if (!is.null(reason)) return(list(label = term$label, reason = reason))
  }
  if (length(term$operands) > 1) {
    return(list(label = term$label,
                reason = paste("is an interaction, which is not yet written for RSiena;",
                               "RSiena::includeInteraction() adds one to the effects")))
  }
  op <- term$operands[[1]]
  out <- if (is.na(op$fn)) .saom_tie(op, ctx) else .formula_terms()[[op$fn]]$to_saom(op, ctx)
  if (is.character(out)) return(list(label = term$label, reason = out))
  list(label = term$label, effect = out)
}

# Why a term has no SAOM form, or NULL where its `to_saom` decides.
#' @keywords internal
#' @noRd
.saom_reason <- function(op) {
  entry <- .formula_terms()[[op$fn]]
  if (identical(entry$kind, "global")) {
    "is a network-level variable, the same for every dyad of the one network that a SAOM models"
  } else if (isTRUE(entry$css)) {
    "describes the reporter of a cognitive social structure, which RSiena does not have"
  } else if (is.null(entry$to_saom)) {
    if (is.na(entry$saom)) "has no RSiena effect"
    else paste0("is not yet converted here; its nearest RSiena effect is ", entry$saom)
  } else NULL
}

# Whether a term reads the dependent network itself: it names no layer, or
# names the outcome.
#' @keywords internal
#' @noRd
.saom_reads_outcome <- function(layer, ctx) {
  is.na(layer) || layer %in% c(ctx$outcome_layer, ctx$lhs)
}

# ---- the converters of the registry ------------------------------------------------

#' @keywords internal
#' @noRd
.saom_ego <- function(op, ctx) .saom_side(op, "egoX")

#' @keywords internal
#' @noRd
.saom_alter <- function(op, ctx) .saom_side(op, "altX")

#' @keywords internal
#' @noRd
.saom_side <- function(op, short) {
  if (!is.null(op$args$levels)) {
    return(paste("chooses categories with levels =, and RSiena reads the values of a covariate;",
                 "make a 0/1 attribute of the categories instead"))
  }
  .saom_effect(short, op$name)
}

#' @keywords internal
#' @noRd
.saom_same <- function(op, ctx) {
  if (.same_by_category(op, ctx$env)) {
    return("gives one effect for each category, which RSiena does not have")
  }
  if (!is.null(op$args$levels)) {
    return("chooses categories with levels =, which RSiena's sameX does not have")
  }
  .saom_effect("sameX", op$name)
}

#' @keywords internal
#' @noRd
.saom_diff <- function(op, ctx) {
  switch(.arg_name(op$args$transformer_fn),
    abs = .saom_effect("absDiffX", op$name),
    identity = .saom_effect("diffX", op$name),
    "transforms the difference with a function that RSiena does not have")
}

#' @keywords internal
#' @noRd
.saom_dist <- function(op, ctx) .saom_effect("absDiffX", op$name)

#' @keywords internal
#' @noRd
.saom_sim <- function(op, ctx) {
  .saom_effect("simX", op$name,
               note = "centred on its mean in RSiena, so its coefficient is on another scale")
}

#' @keywords internal
#' @noRd
.saom_tie <- function(op, ctx) {
  if (.saom_reads_outcome(op$name, ctx)) {
    return("names the outcome, which a SAOM models rather than reads")
  }
  .saom_effect("X", op$name)
}

#' @keywords internal
#' @noRd
.saom_recip <- function(op, ctx) {
  if (.saom_reads_outcome(op$name, ctx)) return(.saom_effect("recip"))
  "reads the reverse tie in another layer, which has no RSiena effect"
}

# goldfish's `indeg` and `outdeg` read the receiver by default, as
# `.read_dynam_degree()` says, so a bare goldfish `outdeg` is RSiena's `outPop`.
#' @keywords internal
#' @noRd
.saom_degree <- function(op, ctx) {
  if (!.saom_reads_outcome(op$name, ctx)) {
    return("reads the degree in another layer, which has no RSiena effect")
  }
  node <- if (any(c("node", "type") %in% op$supplied)) {
    .arg_name(if ("node" %in% op$supplied) op$args$node else op$args$type)
  } else if (isTRUE(ctx$input %in% c("dynam", "rem"))) {
    "alter"
  } else .arg_name(op$args$node)
  short <- switch(paste(op$fn, node),
    "indeg alter" = "inPop", "indeg ego" = "inAct",
    "outdeg ego" = "outAct", "outdeg alter" = "outPop")
  .saom_effect(short)
}

#' @keywords internal
#' @noRd
.saom_inertia <- function(op, ctx) {
  "is part of every SAOM, since each change starts from the ties already there"
}

#' @keywords internal
#' @noRd
.saom_endogenous <- function(short, nearest = FALSE) {
  function(op, ctx) {
    if (!.saom_reads_outcome(op$name, ctx)) {
      return("reads another layer, which is not yet written for RSiena")
    }
    .saom_effect(short, note = if (nearest) "the nearest RSiena effect, which is not the same statistic")
  }
}

#' @keywords internal
#' @noRd
.saom_ego_alter <- function(op, ctx) .saom_effect("egoXaltX", op$name)

# ---- reading RSiena ------------------------------------------------------------------

# Each RSiena effect of a dependent network that has an infernet equivalent:
# a function of the effect's call that returns the infernet terms it becomes,
# or an empty list for `density`, the intercept. An effect without one, such as
# `inAct`, gives the reason another model cannot fit it, and is kept for
# RSiena. The covariate that an effect reads is its argument, as in
# `egoX(sex)`. The list names the effects of a dependent network that
# `RSiena::getEffects()` gives most often; `specify_saom()` with the network
# checks every name against the effects of that network.
#' @keywords internal
#' @noRd
.saom_terms <- function() {
  attr_of <- function(expr) {
    as.name(.arg_name(if (is.call(expr) && length(expr) > 1) expr[[2]]))
  }
  as_term <- function(fn) function(expr) list(call(fn, attr_of(expr)))
  bare <- function(term) function(expr) list(as.name(term))
  endogenous <- c(
    "antiInIso", "antiIso", "avDeg", "balance", "between", "denseTriads",
    "gwdspFB", "gwdspFF", "gwespBB", "gwespBF", "gwespFB", "gwespFF",
    "gwespRR", "in2Plus", "in3Plus", "inAct", "inActSqrt", "inInAss",
    "inIsDegree", "inOutAss", "inPopSqrt", "inStructEq", "isolateNet",
    "isolatePop", "Jin", "Jout", "nbrDist2", "outActSqrt", "outInAss",
    "outInv", "outIso", "outOutAss", "outPop", "outPopSqrt", "outSqInv",
    "outTrunc", "outTrunc2", "reciAct", "reciPop", "sharedPop",
    "transMedTrip", "transRecTrip", "transTies", "transTriads")
  covariate <- c(
    "altSqX", "egoSqX", "egoPlusAltX", "egoXaltX", "egoDiffX", "diffSqX",
    "higher", "simRecipX", "sameXRecip", "inPopX", "outPopX", "inActX",
    "outActX", "transTripX", "sameXTransTrip", "diffXTransTrip",
    "homXTransTrip", "sameXCycle4", "egoXaltXRecip", "XRecip", "WWX",
    "cyWWX", "InWWX", "OutWWX", "WXX", "XWX", "XXW", "altDist2",
    "totDist2", "simDist2", "altInDist2", "totInDist2")
  read <- list(
    density   = function(expr) list(),
    recip     = bare("recip"),
    egoX      = as_term("ego"),
    altX      = as_term("alter"),
    sameX     = as_term("same"),
    absDiffX  = as_term("diff"),
    diffX     = function(expr) {
      list(call("diff", attr_of(expr), transformer_fn = quote(identity)))
    },
    simX      = as_term("sim"),
    X         = function(expr) list(attr_of(expr)),
    transTrip = bare("trans"),
    cycle3    = bare("cycle"),
    cycle4    = bare("four"),
    inPop     = bare("indeg"),
    outAct    = bare("outdeg")
  )
  c(read,
    stats::setNames(rep(list(.endogenous_reason()), length(endogenous)), endogenous),
    stats::setNames(rep(list("is an RSiena effect with no equivalent here"),
                        length(covariate)), covariate))
}

#' @keywords internal
#' @noRd
.read_saom_terms <- function(labels, ctx) .read_native_terms(labels, ctx, .saom_terms())

# An RSiena effect that is kept as it is written, as its effect. An
# interaction of RSiena effects is not yet read.
#' @keywords internal
#' @noRd
.saom_native <- function(label) {
  expr <- str2lang(label)
  if (is.call(expr) && identical(expr[[1]], as.name(":"))) {
    return(paste("is an interaction, which is not yet written for RSiena;",
                 "RSiena::includeInteraction() adds one to the effects"))
  }
  if (is.name(expr)) return(.saom_effect(as.character(expr)))
  args <- vapply(as.list(expr)[-1], .arg_name, "")
  .saom_effect(as.character(expr[[1]]), if (length(args) > 0) args[1] else "",
               if (length(args) > 1) args[2] else "")
}

# A `sienaEffects` object as the formula of the evaluation effects it includes
# for its first dependent network, so that every `specify_*()` reads it.
#' @keywords internal
#' @noRd
.read_siena_effects <- function(x) {
  if (!inherits(x, "sienaEffects")) return(x)
  class(x) <- "data.frame"
  rows <- x[x$include, , drop = FALSE]
  networks <- unique(rows$name[rows$netType != "behavior"])
  if (length(networks) == 0) {
    manynet::snet_abort("The effects include no effect of a dependent network.")
  }
  dep <- networks[1]
  if (length(networks) > 1) {
    manynet::snet_info("Reading the effects of {.val {dep}}, the first of the dependent networks {.val {networks}}.")
  }
  behaviour <- unique(rows$name[rows$netType == "behavior"])
  if (length(behaviour) > 0) {
    manynet::snet_info("Leaving out the effects of the dependent behaviour {.val {behaviour}}, which are not yet read here.")
  }
  rows <- rows[rows$name == dep, , drop = FALSE]
  other <- unique(rows$type[!rows$type %in% c("eval", "rate")])
  if (length(other) > 0) {
    manynet::snet_info("Reading only the evaluation effects, and leaving out the {.val {other}} effects.")
  }
  rows <- rows[rows$type == "eval" & !rows$shortName %in% c("unspInt", "behUnspInt"), , drop = FALSE]
  labels <- vapply(seq_len(nrow(rows)), function(k) {
    .saom_effect(rows$shortName[k], rows$interaction1[k], rows$interaction2[k])$label
  }, "")
  if (length(labels) == 0) labels <- "density"
  f <- eval(call("~", as.name(dep), str2lang(paste(labels, collapse = " + "))))
  environment(f) <- globalenv()
  structure(f, class = c("saom_formula", "formula"), input = "saom")
}

# ---- the effects object ------------------------------------------------------------

# The `sienadata` object for the network, with the left-hand side of each
# formula as a dependent network and every other layer and attribute as a
# covariate, and the `sienaEffects` object that includes the effects each
# formula names, and no other evaluation effect but `density`.
#' @keywords internal
#' @noRd
.saom_effects <- function(formulas, .data) {
  manynet::snet_requires("RSiena")
  net <- manynet::as_stocnet(.data)
  layers <- .layer_names(manynet::as_tidygraph(.data))
  deps <- vapply(formulas, .saom_dependent, "", net = net, layers = layers)
  if (anyDuplicated(deps)) {
    manynet::snet_abort(
      c("{.val {deps[duplicated(deps)]}} is the outcome of more than one formula.",
        i = "Give one formula for each dependent network."))
  }
  if (length(layers) > 0) net <- manynet::mutate_info(net, focal = deps)
  data <- manynet::as_siena(net)
  # A network with one layer has a dependent network that RSiena names itself.
  if (length(layers) == 0) deps <- names(data$depvars)[1]

  eff <- RSiena::getEffects(data)
  own_eval <- function(dep) eff$name == dep & eff$type == "eval"
  defaults <- character(0)
  for (dep in deps) {
    on <- own_eval(dep) & eff$include & eff$shortName != "density"
    defaults <- c(defaults, eff$shortName[on])
    eff$include[on] <- FALSE
  }
  named <- character(0)
  for (k in seq_along(formulas)) {
    labels <- attr(stats::terms(formulas[[k]]), "term.labels")
    for (lab in labels) {
      x <- .saom_native(lab)
      hit <- own_eval(deps[k]) & eff$shortName == x$shortName &
        eff$interaction1 == x$interaction1 & eff$interaction2 == x$interaction2
      # RSiena leaves out `density` where it cannot vary, as where every node
      # has the same outdegree, so it is not added there.
      if (!any(hit) && x$shortName == "density") {
        manynet::snet_info("RSiena has no {.code density} for {.val {deps[k]}}, so it is left out.")
        next
      }
      if (!any(hit)) .saom_unknown(eff, deps[k], x, deps)
      eff$include[hit] <- TRUE
      named <- c(named, x$shortName)
    }
  }
  left <- setdiff(defaults, named)
  if (length(left) > 0) {
    manynet::snet_info("Leaving out {.code {unique(left)}}, which RSiena includes by default, since no formula names it.")
  }
  records <- lapply(formulas, attr, "record")
  structure(eff, data = data,
            formula = if (length(formulas) == 1) formulas[[1]] else formulas,
            record = do.call(rbind, records[!vapply(records, is.null, logical(1))]),
            input = attr(formulas[[1]], "input"))
}

# The dependent network a formula is for: its left-hand side, which must be a
# layer of the network, or `.` for the focal layer or the first layer.
#' @keywords internal
#' @noRd
.saom_dependent <- function(f, net, layers) {
  lhs <- .deparsed(f[[2]])
  if (length(layers) == 0) return(lhs)
  if (lhs %in% layers) return(lhs)
  if (lhs != ".") {
    manynet::snet_abort(
      c("{.val {lhs}} is not a layer of the network, so it cannot be a dependent network.",
        i = "The layers are {.val {layers}}."))
  }
  focal <- intersect(net$info$focal, layers)
  if (length(focal) > 0) return(focal[1])
  manynet::snet_info("Reading {.code .} as {.val {layers[1]}}, the first layer.")
  layers[1]
}

# An effect that RSiena does not have for the dependent network aborts, and
# names the covariates the data has, or the nearest effect.
#' @keywords internal
#' @noRd
.saom_unknown <- function(eff, dep, x, deps = dep) {
  if (x$interaction1 %in% deps) {
    manynet::snet_abort(
      c("{.code {x$label}} reads {.val {x$interaction1}}, which is a dependent network here.",
        i = "The effects of one dependent network on another are not yet written here; set them with {.fn RSiena::includeEffects} on the result."))
  }
  own <- eff[eff$name == dep & eff$type == "eval", c("shortName", "interaction1")]
  if (nzchar(x$interaction1) && !x$interaction1 %in% own$interaction1) {
    vars <- setdiff(unique(own$interaction1), "")
    manynet::snet_abort(
      c("{.code {x$label}} reads {.val {x$interaction1}}, which is no covariate or network in the data for {.val {dep}}.",
        i = "The data has {.val {vars}}."))
  }
  choices <- unique(own$shortName[own$interaction1 == x$interaction1])
  near <- choices[which.min(utils::adist(x$shortName, choices, ignore.case = TRUE)[1, ])]
  manynet::snet_abort(
    c("RSiena has no effect {.code {x$label}} for {.val {dep}}.",
      i = "Did you mean {.code {near}}? {.code RSiena::getEffects()} lists every effect."))
}
