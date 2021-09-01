// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cor_by_trace
arma::vec cor_by_trace(arma::mat x, int dim);
RcppExport SEXP _dts_cor_by_trace(SEXP xSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(cor_by_trace(x, dim));
    return rcpp_result_gen;
END_RCPP
}
// cor_with_trace
arma::vec cor_with_trace(arma::mat x, arma::vec y, int dim);
RcppExport SEXP _dts_cor_with_trace(SEXP xSEXP, SEXP ySEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(cor_with_trace(x, y, dim));
    return rcpp_result_gen;
END_RCPP
}
// residual_variance_with_trace
arma::vec residual_variance_with_trace(arma::mat x, arma::vec y, int dim);
RcppExport SEXP _dts_residual_variance_with_trace(SEXP xSEXP, SEXP ySEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(residual_variance_with_trace(x, y, dim));
    return rcpp_result_gen;
END_RCPP
}
// diff_by_trace
arma::mat diff_by_trace(arma::mat x, int shift, int dim);
RcppExport SEXP _dts_diff_by_trace(SEXP xSEXP, SEXP shiftSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type shift(shiftSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    rcpp_result_gen = Rcpp::wrap(diff_by_trace(x, shift, dim));
    return rcpp_result_gen;
END_RCPP
}
// solve_arma
arma::mat solve_arma(const arma::mat x, const arma::mat y);
RcppExport SEXP _dts_solve_arma(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::mat >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(solve_arma(x, y));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dts_cor_by_trace", (DL_FUNC) &_dts_cor_by_trace, 2},
    {"_dts_cor_with_trace", (DL_FUNC) &_dts_cor_with_trace, 3},
    {"_dts_residual_variance_with_trace", (DL_FUNC) &_dts_residual_variance_with_trace, 3},
    {"_dts_diff_by_trace", (DL_FUNC) &_dts_diff_by_trace, 3},
    {"_dts_solve_arma", (DL_FUNC) &_dts_solve_arma, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_dts(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
