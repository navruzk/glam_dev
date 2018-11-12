fastLR <- function(eps_f, eps_g)
{
    fastLR_(eps_f, eps_g)
}
library(Rcpp)
sourceCpp(glam_add_cpp.cpp)
rcpp_glam_add() = glam::glam_add_cpp()
