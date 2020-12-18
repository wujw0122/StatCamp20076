#' @title Auto- and Cross- Covariance and -Correlation Function Estimation
#' @description The function acf computes (and by default plots) estimates of the autocovariance or autocorrelation function. Function pacf is the function used for the partial autocorrelations. Function ccf computes the cross-correlation or cross-covariance of two univariate series.
#' @param x a univariate or multivariate numeric time series object or a numeric vector or matrix
#' @param lag.max maximum lag at which to calculate the acf. Default is 10*log10(N/m) where N is the number of observations and m the number of series. Will be automatically limited to one less than the number of observations in the series.
#' @return the value of ACF
#' @examples
#' \dontrun{
#' result <- GetAcf(tmp, lag.max = 10)
#' result
#' }
#' @export
GetAcf <- function(x, lag.max = 10){
  rm(list = ls())
  tmp <- sample(c(1:100), 50, replace = T)
  sampleT <- length(x)
  lag.max <- min(lag.max, sampleT - 1L)
  if(lag.max < 0){
    stop("lag.mx must be non-negative...")
  }
  lagList <- 0:lag.max
  muX <- mean(x)
  sigmaX <- sd(x)
  smpAcfCalc <- function(lagNum){
    smp1 <- x[1: (sampleT - lagNum)]
    smp2 <- x[(lagNum + 1) : sampleT]
    curNumerator <- sum((smp1 - muX) * (smp2 -muX)) / (sampleT - lagNum)
    curdivisor <- (sd(smp1)* sd(smp2))* (sampleT - lagNum -1) / (sampleT - lagNum)
    curAcf <- curNumerator / curdivisor
    return(curAcf)
  }
  res <- sapply(lagList, FUN = smpAcfCalc)
  return(res)
}

#' @title Reversal function
#' @description  A simple algorithm is used to turn the numbers.
#' @importFrom Rcpp evalCpp
#' @importFrom stats sd
#' @importFrom utils tail
#' @useDynLib StatComp20076
#' @param x a vector of some positive integers
#' @return the result of turning
#' @examples
#' \dontrun{
#' char <- c('a','b','c','d')
#' rec_char(char)
#' }
#' @export
rec_char <- function(x){
  if (length(x)==1) return(x)
  return(c(tail(x,1),rec_char(x[-length(x)])))
}