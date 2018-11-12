#include <Rcpp.h>

using namespace Rcpp;
// [[Rcpp::export]]
Numeric glam_add_cpp(int x, int y){
                int total;
                total = x + y;
                return total;}
