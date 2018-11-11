for (i in 1:1){
  library(doParallel)
  library(lubridate)
  library(dplyr)
  library(Rcpp)
  library(RcppArmadillo)
  library(RcppEigen)
  library(Smisc)
  library(glogis)
  # library(glam)
  library(parallel)
}
sourceCpp('glam_timestwo.cpp')

summary.optim_params <- function(object, ...)
{
  # se <- sqrt(diag(object$vcov))
  # tval <- coef(object) / se
  TAB <- cbind(Estimate = coef(object))
  #              , StdErr = se,
  #              t.value = tval,
  #              p.value = 2*pt(-abs(tval), df=object$df))
  res <- list(# call=object$call,
    coefficients=TAB)
  class(res) <- "summary.optim_params"
  # res
}

print.summary.optim_params <- function(x, ...)
{
  # cat("Call:\n")
  # print(x$call)
  # cat("\n")
  printCoefmat(x$coefficients)
}
# glam_func <- function(data, par, myscale, h_function_type) UseMethod("glam_func")
# glam_func.default=function(data, par, myscale, h_function_type){
# }
# glam_func=function(data,par,myscale,h_function_type){
#   data=data
#   par=par
#   h_function_type=h_function_type
#   myscale=myscale
#   lehman_function=function (data, par, myscale, h_function_type){
#     glam_myslopefunc=glam::glam_myslopefunc
#     glam_mydiff1=glam::glam_mydiff1
#     glam_mydiff2=glam::glam_mydiff2
#     glam_mydiff3=glam::glam_mydiff3
#     glam_myfunc4=glam::glam_myfunc4
#     glam_myfunc5=glam::glam_myfunc5
#     glam_myfunc6=glam::glam_myfunc6
#     
#     ## h function has three choices
#     ## F function always logistic
#     if (h_function_type == 'first') { ## u get first h function and score functions
#       glam_myhfunc=glam_myhfunc1
#       glam_scorefuncmu=glam_scorefuncmu1
#       glam_scorefunctheta=glam_scorefunctheta1} else
#         if (h_function_type == 'second') { ## u get second h function and score functions
#           glam_myhfunc=glam_myhfunc2
#           glam_scorefuncmu=glam_scorefuncmu2
#           glam_scorefunctheta=glam_scorefunctheta2} else
#             if (h_function_type == 'third') { ## u get third h function and score functions
#               glam_myhfunc=glam_myhfunc3
#               glam_scorefuncmu=glam_scorefuncmu3
#               glam_scorefunctheta=glam_scorefunctheta3}
#     myq=par[1]
#     myrr=par[2]
#     mytheta=myrr
#     myscale=myscale
#     temp_data=data
#     x=as.matrix(sort(temp_data))
#     myobs=sum(!is.na(temp_data))
#     
#     x=rbind(x,x[1,]-1/500) ## add smallest and biggest numbers to make it close to one
#     x=rbind(x,x[myobs,]+1/500)
#     x=as.matrix(sort(x[,1]))
#     myobs=sum(!is.na(x))
#     
#     newdata=list()
#     myfn=list()
#     
#     for (i in 1:(myobs-1)){ ## linearize and get respective step function value
#       
#       newdata[length(newdata)+1]=x[i,]
#       
#       xunit=abs((x[i+1,]-x[i,])/5)
#       newx=list()
#       for (k in 1:4){
#         newxx=xunit*k+x[i,]
#         newx[length(newx)+1]=newxx
#         newdata[length(newdata)+1]=newxx
#       }
#       newx=as.matrix(as.numeric(newx))
#       
#       myfn[length(myfn)+1]=i/(myobs+1)
#       for (k in 1:nrow(newx)) {
#         fn=(i/(myobs+1))+(newx[k,]-x[i,])/((myobs+1)*(x[i+1,]-x[i,]))
#         myfn[length(myfn)+1]=fn
#       }
#     }
#     newdata[length(newdata)+1]=x[nrow(x),]
#     myfn[length(myfn)+1]=(nrow(x)+1)/(myobs+1)
#     
#     myindex=as.matrix(seq(x))
#     myh=glam_myhfunc(myindex,myrr)  ### put in h function to get z values
#     
#     y=na.omit(myh)
#     myobsy = sum(!is.na(y))
#     y=as.matrix(y)
#     newpercentile=NULL
#     newdata=as.numeric(newdata)
#     myfn=as.numeric(myfn)
#     xx=cbind(newdata,myfn)
#     xx=as.matrix(xx)
#     myobsx=sum(!is.na(xx[,1]))
#     
#     xx=as.matrix(xx)
#     myslope=glam_myslopefunc(xx)
#     xx=cbind(xx,myslope)
#     
#     mydiff=glam_mydiff1(xx,y)
#     mydiff=glam_mydiff2(xx,y)
#     
#     y=cbind(y,mydiff)
#     
#     newy=glam_mydiff3(xx,y)
#     myfixed=as.matrix(cbind(newy, abs(newy-myq)))
#     myfixed1=myfixed[order(myfixed[,2], decreasing = FALSE),]
#     
#     mynumbers=glam_myfunc4(myfixed1)
#     myfixed1=cbind(myfixed1, mynumbers)
#     myrankdata=myfixed1
#     
#     myrankdata=as.matrix(myrankdata)
#     myrankdata=na.omit(myrankdata)
#     myobs = sum(!is.na(myrankdata[,1]))
#     
#     myt1=glam_myfunc5(myrankdata, myq)
#     myt2=glam_myfunc6(myrankdata, myq)
#     myt1=as.matrix(myt1)
#     myt2=as.matrix(myt2)
#     
#     scoremyt1=glam_scorefunctheta(myt1, mytheta, myq, myscale)
#     scoremyt2=glam_scorefunctheta(myt2, mytheta, myq, myscale)
#     myscorefunc=(sum(scoremyt1)+sum(scoremyt2))/myobs
#     
#     scoremytmu1=glam_scorefuncmu(myt1, mytheta, myq, myscale)
#     scoremytmu2=glam_scorefuncmu(myt2, mytheta, myq, myscale)
#     myscorefuncmu=(sum(scoremytmu1)+sum(scoremytmu2))/myobs
#     myscore=sum(abs(myscorefunc), abs(myscorefuncmu))
#     return (myscore)
#   }
#   optim_params=optim(data = data, par=par, myscale=myscale,h_function_type=h_function_type, 
#                      mylehmanfunction)
#   class(optim_params)='glam_func'
#   optim_params
# }
# glam_func <- function(data,par,myscale,h_function_type) UseMethod("glam_func")

glam_func=function(x,par,myscale,h_fun, f_fun){

  if(missing(x) || length(x) == 0L || mode(x) != "numeric" || x == 0)
    {stop("'x' must be a non-empty numeric vector")}
  if(any(!is.finite(x))) stop("'x' contains missing or infinite values")
  if(missing(h_fun) || !(is.function(h_fun) || is.character(h_fun)))
    stop("'h_function_type' must be supplied as a number")
  if(missing(f_fun) || !(is.function(f_fun) || is.character(f_fun)))
    stop("'f_function_type' must be supplied as a number")
  x=x
  par=par
  h_function_type=h_fun
  f_function_type=f_fun

  lehman_function=function (x, par, h_function_type, f_function_type){
    # glam_myslopefunc=glam::glam_myslopefunc
    # glam_mydiff1=glam::glam_mydiff1
    # glam_mydiff2=glam::glam_mydiff2
    # glam_mydiff3=glam::glam_mydiff3
    # glam_myfunc4=glam::glam_myfunc4
    # glam_myfunc5=glam::glam_myfunc5
    # glam_myfunc6=glam::glam_myfunc6
    ## h function has three choices
    ## F function always logistic
    if (h_function_type == 'first') { ## u get first h function and score functions
      glam_myhfunc=glam_myhfunc1
      glam_scorefuncmu=glam_scorefuncmu1
      glam_scorefunctheta=glam_scorefunctheta1} else
        if (h_function_type == 'second') { ## u get second h function and score functions
          glam_myhfunc=glam_myhfunc2
          glam_scorefuncmu=glam_scorefuncmu2
          glam_scorefunctheta=glam_scorefunctheta2} else
            if (h_function_type == 'third') { ## u get third h function and score functions
              glam_myhfunc=glam_myhfunc3
              glam_scorefuncmu=glam_scorefuncmu3
              glam_scorefunctheta=glam_scorefunctheta3}
    # # h function has three choices
    # # F function always logistic
    # h_function_type = 'first' ## u get first h function and score functions
    # glam_myhfunc=glam_myhfunc1
    # glam_scorefuncmu=glam_scorefuncmu1
    # glam_scorefunctheta=glam_scorefunctheta1
    if (f_function_type == 'logistic'){
      myscale=((( sqrt(3)*sqrt(var(as.numeric(x))) /pi )))
    }
    myq=par[1]
    myrr=par[2]
    mytheta=myrr

    temp_data=x
    x=as.matrix(sort(temp_data))
    myobs=sum(!is.na(temp_data))
    
    x=rbind(x,x[1,]-1/500) ## add smallest and biggest numbers to make it close to one
    x=rbind(x,x[myobs,]+1/500)
    x=as.matrix(sort(x[,1]))
    myobs=sum(!is.na(x))
    
    newdata=list()
    myfn=list()
    
    for (i in 1:(myobs-1)){ ## linearize and get respective step function value
      
      newdata[length(newdata)+1]=x[i,]
      
      xunit=abs((x[i+1,]-x[i,])/5)
      newx=list()
      for (k in 1:4){
        newxx=xunit*k+x[i,]
        newx[length(newx)+1]=newxx
        newdata[length(newdata)+1]=newxx
      }
      newx=as.matrix(as.numeric(newx))
      
      myfn[length(myfn)+1]=i/(myobs+1)
      for (k in 1:nrow(newx)) {
        fn=(i/(myobs+1))+(newx[k,]-x[i,])/((myobs+1)*(x[i+1,]-x[i,]))
        myfn[length(myfn)+1]=fn
      }
    }
    newdata[length(newdata)+1]=x[nrow(x),]
    myfn[length(myfn)+1]=(nrow(x)+1)/(myobs+1)
    
    myindex=as.matrix(seq(x))
    myh=glam_myhfunc(myindex,myrr)  ### put in h function to get z values
    
    y=na.omit(myh)
    myobsy = sum(!is.na(y))
    y=as.matrix(y)
    newpercentile=NULL
    newdata=as.numeric(newdata)
    myfn=as.numeric(myfn)
    xx=cbind(newdata,myfn)
    xx=as.matrix(xx)
    myobsx=sum(!is.na(xx[,1]))
    
    xx=as.matrix(xx)
    myslope=glam_myslopefunc(xx)
    xx=cbind(xx,myslope)
    
    mydiff=glam_mydiff1(xx,y)
    mydiff=glam_mydiff2(xx,y)
    
    y=cbind(y,mydiff)
    
    newy=glam_mydiff3(xx,y)
    myfixed=as.matrix(cbind(newy, abs(newy-myq)))
    myfixed1=myfixed[order(myfixed[,2], decreasing = FALSE),]
    
    mynumbers=glam_myfunc4(myfixed1)
    myfixed1=cbind(myfixed1, mynumbers)
    myrankdata=myfixed1
    
    myrankdata=as.matrix(myrankdata)
    myrankdata=na.omit(myrankdata)
    myobs = sum(!is.na(myrankdata[,1]))
    
    myt1=glam_myfunc5(myrankdata, myq)
    myt2=glam_myfunc6(myrankdata, myq)
    myt1=as.matrix(myt1)
    myt2=as.matrix(myt2)
    
    scoremyt1=glam_scorefunctheta(myt1, mytheta, myq, myscale)
    scoremyt2=glam_scorefunctheta(myt2, mytheta, myq, myscale)
    myscorefunc=(sum(scoremyt1)+sum(scoremyt2))/myobs
    
    scoremytmu1=glam_scorefuncmu(myt1, mytheta, myq, myscale)
    scoremytmu2=glam_scorefuncmu(myt2, mytheta, myq, myscale)
    myscorefuncmu=(sum(scoremytmu1)+sum(scoremytmu2))/myobs
    myscore=sum(abs(myscorefunc), abs(myscorefuncmu))
    return (myscore)
  }
  optim_params=optim(x = x, par=par,h_function_type=h_function_type, f_function_type=f_function_type, 
                     lehman_function)
  class(optim_params)='glam_func'
  optim_params
}
print.glam_func <- function(x, ...)
{
  cat("\nCoefficients:\n")
  cat("Location:")
  print(x$par[1])
  cat("Asymmetry:")
  print(x$par[2])
}
result
x=rnorm(100,0,1)
starting_value=c(0,1)

result=glam_func(x, par=starting_value,h_fun='first',f_fun = 'logistic',myscale = 0.01)
result
result$par[1]
summary(result)
