rcpp_glam_add=function(x,y){
  val=x+y
  val
}
library(Rcpp)
library(RcppArmadillo)
sourceCpp('glam_add_cpp.cpp')
