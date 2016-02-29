#' Fits an GPD to data
#'
#' @param data The data which should be modeled
#' @param xpar The covariates used in
#' @param fpar the model
#' @param numberOfParameters Should be omitted
#' @param start Vector of length numberOfParamaters
#' @param interval Interval if only one parameter
#' @param ... Further arguments passed to optim (or optimize)
#' @return The fitted parameters
#' @export
fitGpdFlex <-function (data, xpar, fpar, numberOfParameters, ...
                       ,start    = NULL
                       ,interval = NULL) {
  ## numberOfParameters should be replaced by method "Brent" in newer versions
  ## (problem is optim) remove then also check on interval and start
  if (numberOfParameters == 1) stopifnot(!is.null(interval))
  if (numberOfParameters >  1) stopifnot(!is.null(start))

  nllGpd <- function(par) {
    pmat <- fpar(par, xpar)
    scale <- pmat$scale
    shape <- pmat$shape
    if (any(scale <= 0)) return(1e+20)
    exponential <- (abs(shape) < 1e-06)
    y <- data/scale
    z <- 1 + shape * y
    if (any(z <= 0, na.rm = TRUE)) return(1e+20)
    nll <- (shape + 1)/shape * log(z)
    nll[exponential] <- y[exponential]
    sum(nll + log(scale), na.rm = TRUE)
  }
  call <- match.call()
  if(numberOfParameters == 1) {
    opt <- optimize(nllGpd, interval,...)
    gpd <- fpar(opt$minimum, xpar)
    out <- list(estimate = opt$minimum, scale = gpd$scale
                ,shape = gpd$shape, deviance = 2 * opt$objective)
  }
  else if (numberOfParameters > 1) {
    opt <- optim(start, nllGpd, ...)
    gpd <- fpar(opt$par, xpar)
    out <- list(estimate = opt$par, std.err = rep(NA, length(opt$par))
                ,cov = NULL, deviance = 2 * opt$value
                ,convergence = opt$convergence
                ,counts = opt$counts
                ,message = opt$message
                ,threshold = gpd$threshold
                ,scale = gpd$scale
                ,shape = gpd$shape)
    cmat <- try(solve(opt$hessian), TRUE)
    if (!inherits(cmat, "try-error")) {
      out$std.err <- sqrt(diag(cmat))
      out$cov <- cmat
    }
  }
  structure(c(out, call = call), class = "gpd")
}

#' Calculates the sensitivity matrix
#' @inheritParams GetGodambeInformation
#' @param progress
#' @note Close to covariance matrix (from optim) - maybe to be exhanged
#' @export
sensitivityMatrix <- function(data, xpar, estimate, score, progress = TRUE) {
  result <- foreach(i = 1 : xpar$T) %dopar% {
    local <- foreach(j = 1 : xpar$S) %do% {
      scoreValue <- score(data, xpar, estimate, i, j)
      return(t(scoreValue) %*% scoreValue)
    }
    local <- Reduce("+", local)
    if(progress) progress(xpar$T, i)
    return(local)
  }
  result <- Reduce("+", result)
  return(result)
}

#' Calculates the variability matrix
#' @inheritParams sensitivityMatrix
#' @export
variabilityMatrix <- function(data, xpar, estimate, score, progress = TRUE) {
  result <- foreach(i = 1 : dim(data)[1]) %dopar% {
    scoreValue <- score(data, xpar, estimate, i)
    if(progress) progress(xpar$T, i)
    return(t(scoreValue) %*% scoreValue)
  }
  tmp <- Reduce("+", result)
  return(tmp)
}

#' Simple process progess
progress <- function (n, i) {
  step <- max(c(10, floor(n / 33)))
  if (n >= 10) {
    if (i%%step == 0) {
      percentage <- round(i/n * 100, 1)
      print(paste(percentage, "% done", sep = ""))
    }
  }
}

#' Computes the Godambe information matrix
#' @param data Data matrix
#' @param xpar List Covariate information
#' @param estimate Vector of the parameter estimates
#' @param score Closure representing the score function
#' @return List with \code{H} the sensitivity matrix, \code{J} the variability
#' matrix, and \code{G} the Godambe information
#' @export
GetGodambeInformation <- function(data, xpar, estimate, score) {
  J <- variabilityMatrix(data, xpar, estimate, score, FALSE)
  H <- sensitivityMatrix(data, xpar, estimate, score, FALSE)
  G <- H %*% solve(J) %*% H
  list(J = J, H = H, G = G)
}

#' Calculates the modified penalty using the Godambe information matrix
#' @param GodambeInformation e.g. the output from GetGodambeInformation
#' @export
GetGodambePenalty <- function(GodambeInformation) {
  return(sum(diag(GodambeInformation$H %*% solve(GodambeInformation$G))))
}
