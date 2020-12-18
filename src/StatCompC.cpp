#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;
//' @title The probability density of standard Laplace distribution using Rcpp
//' @description The probability density of standard Laplace distribution using Rcpp
//' @param x the value of the variable
//' @return the probability density of the value of x
//' @examples
//' \dontrun{
//' f(4)
//' }
//' @export
//[[Rcpp::export]]
double f(double x) {
  return exp(-abs(x));
}

//' @title An Rcpp function for  generating the standard Laplace distribution
//' @description An Rcpp function for  generating the standard Laplace distribution
//' @param x0 the initial value
//' @param sigma the param of Laplace distribution
//' @param N the sample size
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' rwMetropolis(.2,5,3000)
//' }
//' @export
//[[Rcpp::export]]
NumericVector rwMetropolis (double sigma, double x0, int N) {
  NumericVector x(N);
  x[0] = x0;
  NumericVector u = runif(N);
  for (int i = 1; i < N;i++ ) {
    NumericVector y = rnorm(1, x[i-1], sigma);
    if (u[i] <= (f(y[0]) / f(x[i-1]))){
      x[i] = y[0];
    }
    else {
      x[i] = x[i-1];
    }
  }
  return(x);
}
