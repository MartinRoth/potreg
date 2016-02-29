fD <- function(u, xi, lambda, T) {
  x <- (1 / (T * lambda))^(-xi)
  x <- u * (1 - x) / xi
  return(x)
}

sD <- function(u, gamma, xi, lambda, T) {
  a <- T * lambda
  b <- (1 - (1/a)^(-xi))/xi + log(a) * exp(xi * log(a))
  x <- -gamma * u * b / xi
  return(x)
}

#' Computes standard error of GPD quantile (modelA)
#' @param period Return period
#' @param estimates Model parameter estimates
#' @param u threshold
#' @param lambda Expected number of excesses per block
#' @param cov Covariance matrix
#' @export
GpdQuantileStdErr <- function(period, estimates, u, lambda, cov) {
  gamma <- estimates$gamma
  xi    <- estimates$xi
  a <- fD(u, xi, lambda, period)
  b <- sD(u, gamma, xi, lambda, period)

  var <- a^2 * cov[1,1] + 2 * a * b * cov[1,2] + b^2 * cov[2,2]

  return(sqrt(var))
}
