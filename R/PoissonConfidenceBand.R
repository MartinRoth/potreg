#' Conditional Poisson process
#' @param t,T0 current and total time
#' @param N Total number of excesses
#' @param k quantile
#' @param p probability
#' @description This is based on \href{http://www.sciencedirect.com/science/article/pii/S0022169499001675}{Lang, Ouarda, and Bobee (1999): Towards operational guidelines for over-threshold modeling} under the hood it's the binomial distribution
#' Confidence limits of the Poisson process conditional on the total number of excesses
#' @export
#' @name CondPoisProcess

#' @export
#' @rdname CondPoisProcess
pCondPoisProcess <- function(k, t, T0, N) {
  return(dbinom(k, N, t/T0))
}

#' @export
#' @rdname CondPoisProcess
qCondPoisProcess <- function(p, t, T0, N) {
  return(qbinom(p, N, t/T0))
}
