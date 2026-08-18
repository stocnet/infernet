#' Linear and logistic regression for network data
#' 
#' @description
#' This function provides an implementation of
#' the multiple regression quadratic assignment procedure (MRQAP)
#' for both one-mode and two-mode network linear models.
#' It offers several advantages:
#' - it works with combined graph/network objects such as igraph and network objects
#'   by constructing the various dependent and independent matrices for the user.
#' - it uses a more intuitive formula-based system for specifying the model,
#'   with several ways to specify how nodal attributes should be handled.
#' - it can handle categorical variables (factors/characters) and
#'   interactions intuitively, naming the reference variable where appropriate.
#' - it relies on [`{furrr}`](https://furrr.futureverse.org) for parallelising 
#'   and [`{progressr}`](https://progressr.futureverse.org) 
#'   for reporting progress to the user,
#'   which can be useful when many simulations are required.
#' - results are [`{broom}`](https://broom.tidymodels.org)-compatible, 
#'   with `tidy()` and `glance()` reports to facilitate comparison
#'   with results from different models.
#' Note that a _t_- or _z_-value is always used as the test statistic,
#' and properties of the dependent network 
#' -- modes, directedness, loops, etc --
#' will always be respected in permutations and analysis.
#' @name regression
#' @family models
#' @param formula A formula describing the relationship being tested.
#'   Several additional terms are available to assist users investigate
#'   the effects they are interested in. These include:
#'   - `ego()` constructs a matrix where the cells reflect the value of
#'   a named nodal attribute for an edge's sending node
#'   - `alter()` constructs a matrix where the cells reflect the value of
#'   a named nodal attribute for an edge's receiving node
#'   - `same()` constructs a matrix where a 1 reflects 
#'   if two nodes' attribute values are the same
#'   - `dist()` constructs a matrix where the cells reflect the
#'   absolute difference between the attribute's values 
#'   for the sending and receiving nodes
#'   - `sim()` constructs a matrix where the cells reflect the
#'   proportional similarity between the attribute's values 
#'   for the sending and receiving nodes
#'   - `tertius()` constructs a matrix where the cells reflect some
#'   aggregate of an attribute associated with a node's other ties.
#'   Currently "mean" and "sum" are available aggregating functions.
#'   'ego' is excluded from these calculations.
#'   See Haunss and Hollway (2023) for more on this effect.
#'   - dyadic covariates (other networks) can just be named
#' @param .data A manynet-consistent network. 
#'   See e.g. `manynet::as_tidygraph()` for more details.
#' @param method A method for establishing the null hypothesis.
#'   Note that "qap" uses Dekker et al's (2007) double semi-partialling technique,
#'   whereas "qapy" permutes only the $y$ variable.
#'   "qap" is the default.
#' @param times Integer indicating number of simulations used for quantile estimation. 
#'   (Relevant to the null hypothesis test only - 
#'   the analysis itself is unaffected by this parameter.) 
#'   Note that, as for all Monte Carlo procedures, convergence is slower for more
#'   extreme quantiles.
#'   By default, `times=1000`.
#'   1,000 - 10,000 repetitions recommended for publication-ready results.
#' @param strategy If `{furrr}` is installed, 
#'   then multiple cores can be used to accelerate the function.
#'   By default `"sequential"`, 
#'   but if multiple cores available,
#'   then `"multisession"` or `"multicore"` may be useful.
#'   Generally this is useful only when `times` > 1000.
#'   See [`{furrr}`](https://furrr.futureverse.org) for more.
#' @param verbose Whether the function should report on its progress.
#'   By default FALSE.
#'   See [`{progressr}`](https://progressr.futureverse.org) for more.
#' @importFrom dplyr bind_cols left_join
#' @importFrom purrr flatten
#' @importFrom future plan
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom stats as.formula lm glm fitted.values residuals coefficients
#' @importFrom lme4 lmer glmer deviance.merMod
#' @references 
#'   Krackhardt, David. 1988.
#'   “Predicting with Networks: Nonparametric Multiple Regression Analysis of Dyadic Data.” 
#'   _Social Networks_ 10(4):359–81.
#'   \doi{10.1016/0378-8733(88)90004-4}.
#'   
#'   Dekker, David, David Krackhard, and Tom A. B. Snijders. 2007.
#'   “Sensitivity of MRQAP tests to collinearity and autocorrelation conditions.”
#'   _Psychometrika_ 72(4): 563-581.
#'   \doi{10.1007/s11336-007-9016-1}.
#'   
#' @examples
#' networkers <- ison_networkers %>% to_subgraph(Discipline == "Sociology")
#' model1 <- net_regression(weight ~ ego(Citations) + alter(Citations) + sim(Citations), 
#'                       networkers, times = 20)
#' # Should be run many more `times` for publication-ready results
#' tidy(model1)
#' glance(model1)
#' # if(require("autograph")) plot(model1)
#' @export
net_regression <- function(formula, 
                           .data,
                           times = 1000,
                           control = list(method = c("qap","qapy"),
                                          strategy = "sequential",
                                          family = "auto",
                                          estimator = "auto",
                                          seed = NULL,
                                          groups = NULL,
                                          ncores = NULL,
                                          use_robust_errors = FALSE, 
                                          error_file = NULL,
                                          reference = NULL,
                                          comparison = NULL)) { 
  
  # Setup ####
  rand <- ifelse(any(grepl("\\|",formula)),TRUE,FALSE)
  matrixList <- convertToMatrixList(formula, .data)
  formula <- matrixList$formula
  
  
  
  method <- match.arg(control$method, choices = c("qap","qapy"))
  
  g <- matrixList$mydata
  nx <- length(g) - 1
  n <- dim(g[[1]])
  
  directed <- ifelse(manynet::is_directed(g[[1]]), "digraph", "graph")
  diag <- manynet::is_complex(g[[1]])
  valued <- manynet::is_weighted(g[[1]]) && family == "auto"
  
  
  base_data <- vectorise_list(g, simplex = !diag, 
                              directed = (directed == "digraph"))
  
  # Base ####
  fit <- list()
  
  if (valued) {
    if (!rand) {
      fit$base <- stats::lm(formula, data = base_data)
      fit$coefficients <- stats::coefficients(fit$base)
    } else {
      fit$base <- lme4::lmer(formula, data = base_data)
      fit$coefficients <- fit.base@beta
    }
  
    fit$fitted.values <- stats::fitted.values(fit$base)
    fit$residuals <- stats::residuals(fit$base)
    fit$rank <- length(fit$coefficients)
    fit$n <- length(fit$fitted.values)
    fit$df.residual <- fit$n - fit$rank
    fit$tstat <- fit$coefficients / sqrt(diag(vcov(fit$base)))
  } else {
    if (family == "auto") {
      family <- "binomial"
    }
    if (!rand) {
      fit$base <- stats::glm(formula, data = base_data, family = family)
      fit$coefficients <- stats::coefficients(fit$base)
      fit$deviance <- fit.base$deviance
      fit$null.deviance <- fit.base$null.deviance
      fit$df.null <- fit.base$df.null
      fit$aic <- fit.base$aic
      fit$bic <- fit$deviance + fit$rank * log(fit$n)
    } else {
      fit$base <- lme4::glmer(formula, data = base_data, family = family)
      fit$coefficients <- fit.base@beta
      fit.base.sum <- summary(fit$base)
      fit$deviance <- lme4:::deviance.merMod(fit$base)
      fit$aic <- fit.base.sum$AICtab["AIC"]
      fit$bic <- fit.base.sum$AICtab["BIC"]
      # null model stuff not directly defined for mixed models...
    }
    fit$fitted.values <- fitted.values(fit$base)
    fit$residuals <- residuals(fit$base)
    fit$tstat <- fit$coefficients / sqrt(diag(vcov(fit$base)))
    fit$fitted.values <- stats::fitted.values(fit$base)
    fit$n <- length(fit$fitted.values)
    fit$rank <- length(fit$coefficients)
    fit$df.residual <- fit$n - fit$rank

    fit$ctable <- table(as.numeric(fit$fitted.values >= 0.5), 
                        base_data[[getDependentName(formula)]], 
                        dnn = c("Predicted", "Actual"))
    if (NROW(fit$ctable) == 1) {
      if (rownames(fit$ctable) == "0")  {
        fit$ctable <- rbind(fit$ctable, c(0, 0))
      } else {
        fit$ctable <- rbind(c(0, 0), fit$ctable)
      }
      rownames(fit$ctable) <- c("0", "1")
    }
  }
  
  # Null ####
  # qapy for univariate ####
  if (method == "qapy" || nx == 1) { # why was nx == 2?
    oplan <- future::plan(strategy)
    on.exit(future::plan(oplan), add = TRUE)
    if (valued) {
      repdist <- furrr::future_map_dfr(1:times, function(j) {
        nlmfit(c(list(manynet::generate_permutation(g[[1]], with_attr = FALSE)),
                 g[2:(nx + 1)]),
               directed = directed, diag = diag,
               rety = FALSE)
      }, .progress = verbose, .options = furrr::furrr_options(seed = TRUE))
    } else {
      repdist <- furrr::future_map_dfr(1:times, function(j){
        repfit <- nlgfit(c(list(manynet::generate_permutation(g[[1]], 
                                                              with_attr = FALSE)),
                           g[2:(nx + 1)]),
                         directed = directed, diag = diag)
        repfit$coef/sqrt(diag(chol2inv(repfit$qr$qr)))
      }, .progress = verbose, .options = furrr::furrr_options(seed = TRUE))
    }
    # qapspp for multivariate ####
  } else if (method == "qap") {
    xsel <- matrix(TRUE, n[1], n[2])
    if (!diag) 
      diag(xsel) <- FALSE
    if (directed == "graph") 
      xsel[upper.tri(xsel)] <- FALSE
    repdist <- matrix(0, times, nx)
    
    for (i in 1:nx) {
      xfit <- nlmfit(g[1 + c(i, (1:nx)[-i])], 
                     directed = directed, 
                     diag = diag, rety = TRUE)
      xres <- g[[1 + i]]
      xres[xsel] <- qr.resid(xfit[[1]], xfit[[2]])
      if (directed == "graph") {
        xres[upper.tri(xres)] <- t(xres)[upper.tri(xres)]
      }
      
      oplan <- future::plan(strategy)
      on.exit(future::plan(oplan), add = TRUE)
      if (valued) {
        repdist[,i] <- furrr::future_map_dbl(1:times, function(j){
          nlmfit(c(g[-(1 + i)],
                   list(manynet::generate_permutation(xres, with_attr = FALSE))),
                 directed = directed, diag = diag,
                 rety = FALSE)[nx]
        }, .progress = verbose, .options = furrr::furrr_options(seed = TRUE))
      } else {
        repdist[,i] <- furrr::future_map_dbl(1:times, function(j){
          repfit <- nlgfit(c(g[-(1 + i)],
                             list(manynet::generate_permutation(xres, 
                                                                with_attr = FALSE))),
                           directed = directed, diag = diag)
          repfit$coef[nx]/sqrt(diag(chol2inv(repfit$qr$qr)))[nx]
        }, .progress = verbose, .options = furrr::furrr_options(seed = TRUE))
      }
    }
  }
  
  fit$dist <- repdist
  fit$pleeq <- apply(sweep(fit$dist, 2, fit$tstat, "<="), 
                     2, mean)
  fit$pgreq <- apply(sweep(fit$dist, 2, fit$tstat, ">="), 
                     2, mean)
  fit$pgreqabs <- apply(sweep(abs(fit$dist), 2, abs(fit$tstat), ">="), 2, mean)
  if (method == "qapy" | nx == 2) {
    fit$nullhyp <- "QAPy"
  } else {
    fit$nullhyp <- "QAP-DSP"
  }
  fit$names <- names(matrixList)[-1]
  fit$intercept <- TRUE
  if (valued) {
    class(fit) <- "netlm"
  } else {
    class(fit) <- "netlogit"
  }
  return(fit)  
}

################### auxilliary ----

gettval <- function(x, y, tol) {
  xqr <- qr(x, tol = tol)
  coef <- qr.coef(xqr, y)
  resid <- qr.resid(xqr, y)
  rank <- xqr$rank
  n <- length(y)
  rdf <- n - rank
  resvar <- sum(resid^2)/rdf
  cvm <- chol2inv(xqr$qr)
  se <- sqrt(diag(cvm) * resvar)
  coef/se
}

#nlmfit <- function(glist, directed, diag, rety) {
#  z <- as.matrix(vectorise_list(glist, simplex = !diag, 
#                                directed = (directed == "digraph")))
#  if (!rety) {
#    gettval(z[,2:ncol(z)], z[,1], tol = 1e-07)
#  }
#  else {
#    list(qr(z[,2:ncol(z)], tol = 1e-07), z[,1])
#  }
#}
#
##' @importFrom stats binomial
#nlgfit <- function(glist, directed, diag) {
#  z <- as.matrix(vectorise_list(glist, simplex = !diag, 
#                                directed = (directed == "digraph")))
#  stats::glm.fit(z[,2:ncol(z)], z[,1], 
#                 family = stats::binomial(), intercept = FALSE)
#}

vectorise_list <- function(glist, simplex, directed){
  if (missing(simplex)) {
    simplex <- !manynet::is_complex(glist[[1]])
  }
  if (missing(directed)) {
    directed <- manynet::is_directed(glist[[1]])
  }
  if (simplex) {
    diag(glist[[1]]) <- NA
  }
  if (!directed) {
    glist[[1]][upper.tri(glist[[1]])] <- NA
  }
  
  furrr::future_map(glist,function(x) {
    c(x)}) %>%
    dplyr::bind_cols() %>%
    stats::na.omit() %>%
    suppressMessages()
  
  #suppressMessages(stats::na.omit(dplyr::bind_cols(furrr::future_map(
  #  glist,function(x) c(x)))))
}

convertToMatrixList <- function(formula, .data){
  data <- manynet::as_tidygraph(.data)
  if (manynet::is_weighted(data) & getDependentName(formula) == "weight") {
    DV <- manynet::as_matrix(data) 
  } else {
    DV <- manynet::as_matrix(data)
  }
  names_form <- getRHSNames(formula)
  specificationAdvice(names_form$IVnames, data)
  IVs <- lapply(names_form$IVnames, function(IV) {
    out <- lapply(seq_along(IV), function(elem) {
      # ego ####
      if (IV[[elem]][1] == "ego") {
        vct <- manynet::node_attribute(data, IV[[elem]][2])
        if (manynet::is_twomode(data)) {
          vct <- vct[!manynet::node_attribute(data, "type")]
        }
#        if (is.character(vct) | is.factor(vct)) {
#          fct <- factor(vct)
#          if (length(levels(fct)) == 2) {
#            out <- matrix(as.numeric(fct) - 1,
#                          nrow(DV), ncol(DV))
#            names(out) <- paste(paste(IV[[elem]], collapse = " "),
#                                levels(fct)[2],
#                                paste0("[",levels(fct)[1],"]"))
#            out <- out
#          } else {
#            out <- lapply(2:length(levels(fct)),
#                          function(x) {
#                            matrix((as.numeric(fct) == x)*1,
#                                   nrow(DV), ncol(DV))})
#            names(out) <- paste(paste(IV[[elem]], collapse = " "), 
#                                levels(fct)[2:length(levels(fct))],
#                                paste0("[",levels(fct)[1],"]"))
#            out <- out
#          }
#        } else {
          out <- matrix(vct, nrow(DV), ncol(DV))
          out <- list(out)
          names(out) <- paste(IV[[elem]], collapse = " ")
          out <- out
 #       }
        # alter ####
      } else if (IV[[elem]][1] == "alter") {
        vct <- manynet::node_attribute(data, IV[[elem]][2])
        if (manynet::is_twomode(data)) {
          vct <- vct[manynet::node_attribute(data, "type")]
        }
  #      if (is.character(vct) | is.factor(vct)) {
  #        fct <- factor(vct)
  #        if (length(levels(fct)) == 2) {
  #          out <- matrix(as.numeric(fct) - 1,
  #                        nrow(DV), ncol(DV))
  #          names(out) <- paste(paste(IV[[elem]], collapse = " "),
  #                              levels(fct)[2],
  #                              paste0("[",levels(fct)[1],"]"))
  #          out <- out
  #        } else {
  #          out <- lapply(2:length(levels(fct)),
  #                        function(x) {matrix((as.numeric(fct) == x)*1,
  #                                            nrow(DV), ncol(DV))})
  #          names(out) <- paste(paste(IV[[elem]], collapse = " "), 
  #                              levels(fct)[2:length(levels(fct))],
  #                              paste0("[",levels(fct)[1],"]"))
  #          out <- out
  #        }
  #      } else {
          out <- matrix(vct, nrow(DV), ncol(DV), byrow = TRUE)
          out <- list(out)
          names(out) <- paste(IV[[elem]], collapse = " ")
          out <- out
   #     }
        # same ####
      } else if (IV[[elem]][1] == "same") {
        attrib <- manynet::node_attribute(data, IV[[elem]][2])
        if (manynet::is_twomode(.data)) {
          if (all(is.na(attrib[!manynet::node_is_mode(.data)]))) { # if 2nd mode
            attrib <- attrib[manynet::node_is_mode(.data)]
            out <- vapply(1:length(attrib), function(x){
              net <- manynet::as_matrix(
                manynet::delete_nodes(.data,manynet::net_dims(.data)[1] + x))
              rowSums(net * matrix((attrib[-x] == attrib[x])*1, 
                                   nrow(DV), ncol(DV) - 1, byrow = TRUE)) /
                rowSums(net)
            }, FUN.VALUE = numeric(nrow(DV)))
            out[is.nan(out)] <- 0
          } else { # or then attrib must be on first mode
            attrib <- attrib[!manynet::node_is_mode(.data)]
            out <- t(vapply(1:length(attrib), function(x) {
              net <- manynet::as_matrix(manynet::delete_nodes(.data, x))
              colSums(net * matrix((attrib[-x] == attrib[x])*1, 
                                   nrow(DV) - 1, ncol(DV))) /
                colSums(net)
            }, FUN.VALUE = numeric(ncol(DV))))
            out[is.nan(out)] <- 0
          }
        } else {
          rows <- matrix(attrib, nrow(DV), ncol(DV))
          cols <- matrix(attrib, nrow(DV), ncol(DV), byrow = TRUE)
          out <- (rows == cols)*1  
        }
        out <- list(out)
        names(out) <- paste(IV[[elem]], collapse = " ")
        out <- out
        # dist ####
      } else if (IV[[elem]][1] == "dist") {
        if (is.character(manynet::node_attribute(data, IV[[elem]][2]))) {
          stop("Distance undefined for factors.")
        }
        rows <- matrix(manynet::node_attribute(data, IV[[elem]][2]),
                       nrow(DV), ncol(DV))
        cols <- matrix(manynet::node_attribute(data, IV[[elem]][2]),
                       nrow(DV), ncol(DV), byrow = TRUE)
        out <- abs(rows - cols)
        out <- list(out)
        names(out) <- paste(IV[[elem]], collapse = " ")
        out <- out
        # sim ####
      } else if (IV[[elem]][1] == "sim") {
        if (is.character(manynet::node_attribute(data, IV[[elem]][2]))) {
          stop("Similarity undefined for factors. Try `same()` instead.")
        }
        rows <- matrix(manynet::node_attribute(data, IV[[elem]][2]),
                       nrow(DV), ncol(DV))
        cols <- matrix(manynet::node_attribute(data, IV[[elem]][2]),
                       nrow(DV), ncol(DV), byrow = TRUE)
        out <- abs(1 - abs(rows - cols)/max(abs(rows - cols)))
        out <- list(out)
        names(out) <- paste(IV[[elem]], collapse = " ")
        out <- out
        # tertius ####
      } else if (IV[[elem]][1] == "tertius") {
        vct <- manynet::node_attribute(data, IV[[elem]][2])
        if (manynet::is_twomode(data)) {
          vct <- vct[!manynet::node_attribute(data, "type")]
        }
        val <- matrix(vct, nrow(DV), ncol(DV)) * DV
        if (is.na(IV[[elem]][3])) {
          IV[[elem]][3] <- "mean"
        }
        out <- t(vapply(seq_len(nrow(DV)), 
                        function(x) {
                          if (IV[[elem]][3] == "mean") {
                            colMeans(val[-x,], na.rm = TRUE)
                          } else if (IV[[elem]][3] == "sum") {
                            colSums(val[-x,], na.rm = TRUE)
                          } else {
                            stop("tertius summary function not recognised")
                          }
                        }, 
                        FUN.VALUE = numeric(ncol(DV))))
        out <- list(out)
        names(out) <- paste(IV[[elem]], collapse = " ")
        out <- out
      } else {
        if (IV[[elem]][1] %in% manynet::network_tie_attributes(data)) {
          out <- manynet::as_matrix(manynet::to_uniplex(data, 
                                                        tie = IV[[elem]][1]))
          out <- list(out)
          names(out) <- IV[[elem]][1]
          out <- out
        }
      }
    })
    if (length(out) == 2) {
      namo <- paste(vapply(out, names, FUN.VALUE = character(1)), 
                    collapse = ":")
      out <- list(out[[1]][[1]] * out[[2]][[1]])
      names(out) <- namo
      out
    } else {
      if (is.list(out[[1]])) {
        out[[1]]
      } else {
        out <- list(out[[1]])
        names(out) <- attr(out[[1]], "names")[1]
        attr(out[[1]], "names") <- NULL
        out
      } 
    }})
  IVs <- purrr::flatten(IVs)
  out <- c(list(DV), IVs)
  # Getting the names right
  DVname <- formula[[2]]
  if (DVname == ".") {
    DVname <- "ties"
  }
  names(out)[1] <- as.character(DVname)
  
# OUT FIX
  
  return(list(mydata = out,
              formula = names_form$formula))
}





getRHSNames <- function(formula) {
  rhs <- c(attr(stats::terms(formula), "term.labels"))
  rhs <- strsplit(rhs, ":")
  rhs <- lapply(rhs, function(term) {
    strsplit(term,"\\|")})
  # embed single parameter models in list
  if (!is.list(rhs)) {
    rhs <- list(rhs)
  }
  if (any(grepl("\\|",formula))) { #are there random effects?
    rand <- c(1:length(rhs))[unlist(lapply(rhs, function(term) {
      length(term[[1]]) > 1}))] #where are they
    # create object without random
    rhs2 <- rhs[c(1:length(rhs))[-rand]]
    rhsn <- lapply(rhs2, function(term) {
      strsplit(gsub("\\)", "", term), "\\(|,|, ")})
    term_names <- lapply(rhsn, function(term) {
      paste0("`", term[[1]][1], " ", term[[1]][2],  "`")})
    form <- paste(paste(formula[[2]],"~"),
                  paste(term_names,
                        collapse = " + "))
    # loop through random effects
    for (term in rand) {
      form <- paste(form, "+", "(")
      for (i in 1:length(rhs[[term]][[1]])) {
        string <- gsub("[[:space:]]", "", rhs[[term]][[1]][[i]])
        string <- strsplit(string, "\\+")[[1]]
        
        for (j in 1:length(string)) {
          n <- strsplit(gsub("\\)", "", string[j]), "\\(|,|, ")
          if (i == 1) { #identifies random intercept/slopes from level-> [i = 2] 
            if (length(n[[1]]) == 1) {
              #random intercept
              form <- paste0(form,n)
            } else {
              #random slopes
              n <- paste0("`", paste(n[[1]], collapse = " "),  "`")
              form <- paste(form,n)
            }
            if (j < length(string)) {
              # if more than one random intercept and or slope per level add +
              form <- paste(form,"+")
            }
          } else {
            # add level
            n <- paste0("`", paste(n[[1]], collapse = " "),  "`")
            form <- paste(form,"|",n)
          }
          # add variable name so that corresponding matrix is created
          if (string[j] != "1" && !(string[j] %in% unlist(rhs2))) {
            rhs2 <- append(rhs2,string[j])
          }
        }
      }
      form <- paste0(form, ")")
    }
    rhs <- rhs2
  } 
  # make all the names now
  rhsn <- lapply(rhs, function(term) {
    strsplit(gsub("\\)", "", term), "\\(|,|, ")})
  
  # if there are no random effects, formula needs to be made here
  if (!any(grepl("\\|",formula))) {
    form <- paste(paste(formula[[2]],"~"),
                  paste( lapply(rhsn, function(term) {
                    paste0("`", term[[1]][1], " ", term[[1]][2],  "`")}),
                        collapse = " + "))
  }
  return(list(IVnames = rhsn,
              formula = as.formula(form)))
}

getDependentName <- function(formula) {
  dep <- list(formula[[2]])
  unlist(lapply(dep, deparse))
}

specificationAdvice <- function(formula, data) {
  formdf <- t(data.frame(formula))
  if (any(formdf[,1] %in% c("sim","same"))) {
    vars <- formdf[formdf[,1] %in% c("sim","same"), 2]
    suggests <- vapply(vars, function(x){
      incl <- unname(formdf[formdf[,2] == x, 1])
      if (manynet::is_twomode(data)) {
        excl <- setdiff(c("ego","tertius"), incl)
      } else excl <- setdiff(c("ego","alter"), incl)
      if (length(excl) > 0) {
        paste0(excl, "(", x, ")", collapse = ", ") 
      } else {
        NA_character_
      }
      # incl
    }, FUN.VALUE = character(1))
    suggests <- suggests[!is.na(suggests)]
    if (!manynet::is_directed(data)) {
      suggests <- suggests[!grepl("ego\\(", suggests)]
    }
    if (length(suggests) > 0) {
      if (length(suggests) > 1) {
        suggests <- paste0(suggests, collapse = ", ")
      }
      cat(paste("When testing for homophily,",
                "it is recommended to include all more fundamental effects.\n",
                "Try adding", suggests, "to the model specification.\n\n"))
    }
  }
}
 
logit_moments <- function(theta, data) {
  Y <- data$y
  X <- data$x
  prob <- 1 / (1 + exp(-1 * (X %*% theta)))
  residuals <- as.vector(Y - prob)
  g <- residuals * X
  return(g)
}
 
logit_resid <- function(gmmo) {
  Y <- gmmo$dat$y
  X <- gmmo$dat$x
  prob <- 1 / (1 + exp(-1 * (X %*% gmmo$coefficients)))
  residuals <- as.vector(Y - prob)
  return(residuals)
}
