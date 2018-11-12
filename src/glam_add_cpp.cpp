#include <Rcpp.h>

using namespace Rcpp;
// [[Rcpp::export]]
NumericVector glam_add_cpp(int x, int y){
                int total;
                total = x + y;
                return total;}
