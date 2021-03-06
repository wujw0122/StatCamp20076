% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myfunction.R
\name{GetAcf}
\alias{GetAcf}
\title{Auto- and Cross- Covariance and -Correlation Function Estimation}
\usage{
GetAcf(x, lag.max = 10)
}
\arguments{
\item{x}{a univariate or multivariate numeric time series object or a numeric vector or matrix}

\item{lag.max}{maximum lag at which to calculate the acf. Default is 10*log10(N/m) where N is the number of observations and m the number of series. Will be automatically limited to one less than the number of observations in the series.}
}
\value{
the value of ACF
}
\description{
The function acf computes (and by default plots) estimates of the autocovariance or autocorrelation function. Function pacf is the function used for the partial autocorrelations. Function ccf computes the cross-correlation or cross-covariance of two univariate series.
}
\examples{
\dontrun{
result <- GetAcf(tmp, lag.max = 10)
result
}
}
