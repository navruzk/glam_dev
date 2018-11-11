\name{mylehmanfunction}
\alias{mylehmanfunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mylehmanfunction(data, par, myscale, h_function_type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{par}{
%%     ~~Describe \code{par} here~~
}
  \item{myscale}{
%%     ~~Describe \code{myscale} here~~
}
  \item{h_function_type}{
%%     ~~Describe \code{h_function_type} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, par, myscale, h_function_type) 
{
    glam_myslopefunc = glam::glam_myslopefunc
    glam_mydiff1 = glam::glam_mydiff1
    glam_mydiff2 = glam::glam_mydiff2
    glam_mydiff3 = glam::glam_mydiff3
    glam_myfunc4 = glam::glam_myfunc4
    glam_myfunc5 = glam::glam_myfunc5
    glam_myfunc6 = glam::glam_myfunc6
    if (h_function_type == "first") {
        glam_myhfunc = glam_myhfunc1
        glam_scorefuncmu = glam_scorefuncmu1
        glam_scorefunctheta = glam_scorefunctheta1
    }
    else if (h_function_type == "second") {
        glam_myhfunc = glam_myhfunc2
        glam_scorefuncmu = glam_scorefuncmu2
        glam_scorefunctheta = glam_scorefunctheta2
    }
    else if (h_function_type == "third") {
        glam_myhfunc = glam_myhfunc3
        glam_scorefuncmu = glam_scorefuncmu3
        glam_scorefunctheta = glam_scorefunctheta3
    }
    myq = par[1]
    myrr = par[2]
    mytheta = myrr
    myscale = myscale
    temp_data = data
    x = as.matrix(sort(temp_data))
    myobs = sum(!is.na(temp_data))
    x = rbind(x, x[1, ] - 1/500)
    x = rbind(x, x[myobs, ] + 1/500)
    x = as.matrix(sort(x[, 1]))
    myobs = sum(!is.na(x))
    newdata = list()
    myfn = list()
    for (i in 1:(myobs - 1)) {
        newdata[length(newdata) + 1] = x[i, ]
        xunit = abs((x[i + 1, ] - x[i, ])/5)
        newx = list()
        for (k in 1:4) {
            newxx = xunit * k + x[i, ]
            newx[length(newx) + 1] = newxx
            newdata[length(newdata) + 1] = newxx
        }
        newx = as.matrix(as.numeric(newx))
        myfn[length(myfn) + 1] = i/(myobs + 1)
        for (k in 1:nrow(newx)) {
            fn = (i/(myobs + 1)) + (newx[k, ] - x[i, ])/((myobs + 
                1) * (x[i + 1, ] - x[i, ]))
            myfn[length(myfn) + 1] = fn
        }
    }
    newdata[length(newdata) + 1] = x[nrow(x), ]
    myfn[length(myfn) + 1] = (nrow(x) + 1)/(myobs + 1)
    myindex = as.matrix(seq(x))
    myh = glam_myhfunc(myindex, myrr)
    y = na.omit(myh)
    myobsy = sum(!is.na(y))
    y = as.matrix(y)
    newpercentile = NULL
    newdata = as.numeric(newdata)
    myfn = as.numeric(myfn)
    xx = cbind(newdata, myfn)
    xx = as.matrix(xx)
    myobsx = sum(!is.na(xx[, 1]))
    xx = as.matrix(xx)
    myslope = glam_myslopefunc(xx)
    xx = cbind(xx, myslope)
    mydiff = glam_mydiff1(xx, y)
    mydiff = glam_mydiff2(xx, y)
    y = cbind(y, mydiff)
    newy = glam_mydiff3(xx, y)
    myfixed = as.matrix(cbind(newy, abs(newy - myq)))
    myfixed1 = myfixed[order(myfixed[, 2], decreasing = FALSE), 
        ]
    mynumbers = glam_myfunc4(myfixed1)
    myfixed1 = cbind(myfixed1, mynumbers)
    myrankdata = myfixed1
    myrankdata = as.matrix(myrankdata)
    myrankdata = na.omit(myrankdata)
    myobs = sum(!is.na(myrankdata[, 1]))
    myt1 = glam_myfunc5(myrankdata, myq)
    myt2 = glam_myfunc6(myrankdata, myq)
    myt1 = as.matrix(myt1)
    myt2 = as.matrix(myt2)
    scoremyt1 = glam_scorefunctheta(myt1, mytheta, myq, myscale)
    scoremyt2 = glam_scorefunctheta(myt2, mytheta, myq, myscale)
    myscorefunc = (sum(scoremyt1) + sum(scoremyt2))/myobs
    scoremytmu1 = glam_scorefuncmu(myt1, mytheta, myq, myscale)
    scoremytmu2 = glam_scorefuncmu(myt2, mytheta, myq, myscale)
    myscorefuncmu = (sum(scoremytmu1) + sum(scoremytmu2))/myobs
    myscore = sum(abs(myscorefunc), abs(myscorefuncmu))
    return(myscore)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
