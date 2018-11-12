#include <Rcpp.h>

using namespace Rcpp;
// [[Rcpp::export]]
NumericVector glam_add(int x, int y){
                int total;
                total = x + y;
                return total;}
// [[Rcpp::export]]
NumericVector glam_myhfunc1(NumericMatrix x, double y){
                int n=x.nrow();
                NumericVector total(n);
                for (int i=0; i<n; ++i){
                  total[i] = 1-pow((1-x(i,0)/(n+1)),y);
                }
                return total;}
// [[Rcpp::export]]
NumericVector glam_myhfunc2(NumericMatrix x, double y){
                int n=x.nrow();
                NumericVector total(n);
                for (int i=0; i<n; ++i){
                  total[i] = (exp(y*x(i,0)/(n+1)) - 1) / (exp(y) - 1);
                }
                return total;}
// [[Rcpp::export]]
NumericVector glam_myhfunc3(NumericMatrix x, double y){
                int n=x.nrow();
                NumericVector total(n);
                for (int i=0; i<n; ++i){
                  total[i] = pow(x(i,0)/(n+1),y);
                }
                return total;}
// [[Rcpp::export]]
NumericVector glam_myslopefunc(NumericMatrix x){
                  int n = x.nrow();
                  NumericVector total(n);
                  for (int i = 0; i < n-1; ++i) {total[i]  = 1/((n+1)*(x(i+1,0) - x(i,0)));}
                  return total;}
// [[Rcpp::export]]
NumericVector glam_mydiff1(NumericMatrix x, NumericMatrix y){
                  int n=x.nrow();
                  NumericVector total=(n);
                  for (int i=0; i<n-1; i++){
                    if (x(0,1) > y(i,0))
                      total[i]=0;
                  }
                  return total;}
// [[Rcpp::export]]
NumericVector glam_mydiff2(NumericMatrix x, NumericMatrix y){
                  int n=y.nrow();
                  int nrow=x.nrow();
                  NumericVector total=(n);
                  
                  for (int j=0; j<(n); j++){
                    total[j]=0;
                    for (int i=0; i<(nrow-1); i++){
                      if ( (x(i+1,1) > y(j,0) ) && ( x(i,1) <= y(j,0) ))
                        total[j]=y(j,0)-x(i,1);
                    }
                  }
                  return total;}
// [[Rcpp::export]]
NumericVector glam_mydiff3(NumericMatrix x, NumericMatrix y){
                  int n=y.nrow();
                  int nrow=x.nrow();
                  NumericVector total=(n);
                  
                  for (int j=0; j<(n); j++){
                    total[j]=0;
                    for (int i=0; i<(nrow-1); i++){
                      if ( (x(i+1,1) > y(j,0) ) && ( x(i,1) <= y(j,0) ))
                        total[j]=x(i,0) + (y(j,1) / x(i,2));
                    }
                  }
                  return total;}
// [[Rcpp::export]]
NumericVector glam_myfunc4(NumericMatrix x){
                  int n=x.nrow();
                  NumericVector nn(n);
                  for (int i=0; i<n; i++){
                    SubMatrix<REALSXP> yy = x(Range(0,i),Range(1,1));
                    NumericMatrix res = yy;
                    nn[i]=res.nrow()-1;
                  }
                  return nn;        
                }
// [[Rcpp::export]]
NumericVector glam_myfunc5(NumericMatrix x, double y){
                  int n=x.nrow();
                  NumericVector total(n);
                  for (int i=0; i<n; i++){
                    if (x(i,0) > y) 
                      total[i]=(1+x(i,2)/(n+1))/2;}
                  return total;
                }
// [[Rcpp::export]]
NumericVector glam_myfunc6(NumericMatrix x, double y){
                  int n=x.nrow();
                  NumericVector total(n);
                  for (int i=0; i<n; i++){
                    if (x(i,0) <= y) 
                      total[i]=(1-x(i,2)/(n+1))/2;}
                  return total;
                }
// // [[Rcpp::export]]
// NumericVector dist_fun(NumericMatrix x, double mu, double s){
//                   int n=x.nrow();
//                   NumericVector total(n);
//                   for (int i=0; i<n; i++){
//                     total[i] = 1 / (1 + exp(-(x(i,0) - mu) / s ) );} 
//                   return total;
//                 }
// // [[Rcpp::export]]
// NumericVector prob_fun(NumericMatrix x, double mu, double s){
//                   int n=x.nrow();
//                   NumericVector total(n);
//                   for (int i=0; i<n; i++){
//                     total[i]=exp( -(x(i,0)-mu)/s ) / (s *  pow((1 + exp(-(x(i,0)-mu)/s)),2));}
//                   return total;
//                 }
// [[Rcpp::export]]
NumericVector glam_scorefunctheta3(NumericMatrix x, double theta, double mu, double s){
                  int n=x.nrow();
                  NumericVector total(n);
                  for (int i=0; i<n; i++){
                    if ( x(i,0)!=0 ) 
                      total[i]=1/theta+(1/theta)*(log(x(i,0)));} 
                  return total;
                  }
// [[Rcpp::export]] 
NumericVector glam_scorefuncmu3(NumericMatrix x, double theta, double mu, double s){
                  int n=x.nrow();
                  NumericVector total(n);
                  for (int i=0; i<n; i++){
                    if ( x(i,0)!=0 ) 
                      total[i]=(1-pow(x(i,0), (1/theta)))*(theta-2) + 1/s;} 
                  return total;}
// [[Rcpp::export]]
NumericVector glam_scorefunctheta2(NumericMatrix x, double theta, double mu, double s){
                    int n=x.nrow();
                    NumericVector total(n);
                    for (int i=0; i<n; i++){
                      if ( x(i,0)!=0 )
                        total[i]=log(x(i,0)*(exp(theta-1)))/theta - exp(theta)/(exp(theta)-1) + 1/theta;}
                    return total;
                  }
// [[Rcpp::export]]
NumericVector glam_scorefuncmu2(NumericMatrix x, double theta, double mu, double s){
                    int n=x.nrow();
                    NumericVector total(n);
                    for (int i=0; i<n; i++){
                      if ( x(i,0)!=0 ) 
                        total[i]=1/s - (1/s) * (log(x(i,0)*(exp(theta-1)))/theta)*(1 - (log(x(i,0)*(exp(theta-1)))/theta)) * (theta + 2/(log(x(i,0)*(exp(theta-1)))/theta));}
                    return total;
                  }
// [[Rcpp::export]]
NumericVector glam_scorefunctheta1(NumericMatrix x, double theta, double mu, double s){
                    int n=x.nrow();
                    NumericVector total(n);
                    for (int i=0; i<n; i++){
                      if ( x(i,0)!=0 ) 
                        total[i]=1/theta+(1/theta)*(log(1-x(i,0)));} 
                    return total;
                  }
// [[Rcpp::export]]
NumericVector glam_scorefuncmu1(NumericMatrix x, double theta, double mu, double s){
                    int n=x.nrow();
                    NumericVector total(n);
                    for (int i=0; i<n; i++){
                      if ( x(i,0)!=0 ) 
                        total[i]=(1/s)*((3-theta)*(pow((1-x(i,0)),(1/theta))) -2 + theta);} 
                    return total;
                  }
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.

