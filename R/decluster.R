#' Declustering using a simple separation time
#' @param time numeric vector, e.g. julian days
#' @param x numeric vector of corresponding values
#' @param sep numeric separation time
#' @return index integer vector of peaks
#' @export
declusterSimpleSeparation <- function(time, x, sep) {
  if (class(time)!="numeric") stop("time should be a numeric vector")
  if (class(x)!="numeric") stop("x should be a numeric vector")
  if (class(sep)!="numeric" | length(sep) != 1) stop("sep should be a numeric")
  if (length(time) != length(x)) stop("time and x must have same length")
  n     <- length(x)
  index <- integer(n)
  j     <- 1
  i     <- 1
  while (i <= n) {
    sepRange <- which(time %in% seq.int(time[i]-sep,time[i]+sep, by=1 ))
    if (x[i] >= max(x[sepRange])) {
      index[j] <- i
      i <- max(sepRange) + 1
      j <- j+1
    } else {
      i <- i+1
    }
  }
  return(index[index!=0])
}
