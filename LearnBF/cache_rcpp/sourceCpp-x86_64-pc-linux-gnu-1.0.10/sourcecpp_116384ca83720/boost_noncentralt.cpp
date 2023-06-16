// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/math/distributions/non_central_t.hpp> 

using namespace boost::math;
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector dnct(const double x, const double df, const NumericVector ncp) {
  
  R_xlen_t n = ncp.length();
  NumericVector y(n);
  
  for (R_xlen_t i = 0; i < n; ++i) {
    non_central_t dist(df, ncp[i]);
    y[i] = pdf(dist, x);
  }
  
  return y;
}


#include <Rcpp.h>
#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// dnct
NumericVector dnct(const double x, const double df, const NumericVector ncp);
RcppExport SEXP sourceCpp_1_dnct(SEXP xSEXP, SEXP dfSEXP, SEXP ncpSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type x(xSEXP);
    Rcpp::traits::input_parameter< const double >::type df(dfSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type ncp(ncpSEXP);
    rcpp_result_gen = Rcpp::wrap(dnct(x, df, ncp));
    return rcpp_result_gen;
END_RCPP
}
