#include <Rcpp.h>

using namespace Rcpp;
// [[Rcpp::export]]
NumericVector glam_add(int x, int y){
                int total;
                total = x + y;
                return total;}
