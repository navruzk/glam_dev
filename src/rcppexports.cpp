#include <Rcpp.h>

using namespace Rcpp;

// MatrixExample
NumericVector glam_add(intx int y);
RcppExport SEXP _rcpp_glam_add(SEXP origSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int x, int y>::type orig(origSEXP);
    rcpp_result_gen = Rcpp::wrap(glam_add(orig));
    return rcpp_result_gen;
END_RCPP
}
static const R_CallMethodDef CallEntries[] = {
    {"_rcpp_glam_add", (DL_FUNC) &_rcpp_glam_add, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_glam(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
