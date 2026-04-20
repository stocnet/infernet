# GMM moment conditions and residuals ----------------------------------------
#
# Auxiliary estimators for the `estimator = "gmm"` path in fit_qap_model().
# Internal; ported from MrQAP.

#' @keywords internal
#' @noRd
poisson_moments <- function(theta, data) {
  Y <- as.numeric(data$y)
  X <- data.matrix(data$x)
  lambda_hat <- exp(X %*% theta)
  residuals <- as.vector(Y - lambda_hat)
  g <- residuals * X
  return(g)
}

#' @keywords internal
#' @noRd
logit_moments <- function(theta, data) {
  Y <- data$y
  X <- data$x
  prob <- 1 / (1 + exp(-1 * (X %*% theta)))
  residuals <- as.vector(Y - prob)
  g <- residuals * X
  return(g)
}

#' @keywords internal
#' @noRd
logit_resid <- function(gmmo) {
  Y <- gmmo$dat$y
  X <- gmmo$dat$x
  prob <- 1 / (1 + exp(-1 * (X %*% gmmo$coefficients)))
  residuals <- as.vector(Y - prob)
  return(residuals)
}

#' @keywords internal
#' @noRd
poisson_resid <- function(gmmo) {
  Y <- gmmo$dat$y
  X <- gmmo$dat$x
  lambda_hat <- exp(X %*% gmmo$coefficients)
  residuals <- as.vector(Y - lambda_hat)
  return(residuals)
}

#' @keywords internal
#' @noRd
negbin_moments <- function(theta, data) {
  Y <- as.numeric(data$y)
  X <- data.matrix(data$x)
  p <- ncol(X)
  beta   <- theta[1:p]
  alpha  <- exp(theta[p + 1])

  mu   <- as.vector(exp(X %*% beta))
  resid <- Y - mu
  V    <- mu + alpha * mu^2

  g1 <- (resid / V) * X
  g2 <- (resid^2 / V) - 1

  cbind(g1, g2)
}

#' @keywords internal
#' @noRd
negbin_resid <- function(gmmo) {
  Y <- as.numeric(gmmo$dat$y)
  X <- data.matrix(gmmo$dat$x)
  p <- ncol(X)
  beta <- gmmo$coefficients[1:p]
  mu   <- as.vector(exp(X %*% beta))
  as.vector(Y - mu)
}

#' @keywords internal
#' @noRd
zip_moments <- function(theta, data) {
  Y <- as.numeric(data$y)
  X <- data.matrix(data$x)
  p <- ncol(X)
  beta <- theta[1:p]
  pi_z <- 1 / (1 + exp(-theta[p + 1]))

  lambda <- as.vector(exp(X %*% beta))
  mu    <- (1 - pi_z) * lambda
  resid <- Y - mu

  g1 <- resid * X

  p0 <- pi_z + (1 - pi_z) * exp(-lambda)
  is_zero <- as.numeric(Y == 0)
  g2 <- is_zero - p0

  cbind(g1, g2)
}

#' @keywords internal
#' @noRd
zip_resid <- function(gmmo) {
  Y <- as.numeric(gmmo$dat$y)
  X <- data.matrix(gmmo$dat$x)
  p <- ncol(X)
  beta <- gmmo$coefficients[1:p]
  pi_z <- 1 / (1 + exp(-gmmo$coefficients[p + 1]))
  lambda <- as.vector(exp(X %*% beta))
  mu <- (1 - pi_z) * lambda
  as.vector(Y - mu)
}
