#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
double fastLR_(double eps_f, double eps_g)
{
    double total = eps_f + eps_g;
    return total;
}
