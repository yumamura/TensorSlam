\name{Refold}
\alias{Refold}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Refold(mat, dimToRecover, mode)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{dimToRecover}{
%%     ~~Describe \code{dimToRecover} here~~
}
  \item{mode}{
%%     ~~Describe \code{mode} here~~
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
function (mat, dimToRecover, mode) 
{
    recoverOrder <- c(dimToRecover[mode], dimToRecover[-mode])
    mode.num <- c(mode, (1:length(dimToRecover))[-mode])
    index <- mat$i
    cumdim <- c(recoverOrder)[2:(length(dimToRecover))]
    cumdim <- cumprod(cumdim)
    for (i in 1:(length(cumdim))) {
        if (i == 1) {
            tmp <- (index[, 2])\%\%cumdim[i]
            tmp[which(tmp == 0)] <- cumdim[i]
        }
        else {
            tmp <- (index[, 2] - 1)\%/\%cumdim[i - 1] + 1
        }
        index <- cbind(index, tmp)
    }
    index <- index[, c(-2)]
    if (class(index) == "numeric") 
        index <- t(as.matrix(index))
    tmp <- index[, 1]
    index <- index[, -1]
    ncolIndex <- ncol(index)
    if (is.null(ncolIndex)) 
        ncolIndex <- length(index)
    if (mode == 1) {
        index <- cbind(tmp, index)
    }
    else if (mode == (ncolIndex + 1)) {
        index <- cbind(index, tmp)
    }
    else {
        if (class(index) == "numeric") 
            index <- t(as.matrix(index))
        index <- cbind(index[, 1:(mode - 1)], tmp, index[, mode:ncolIndex])
    }
    tnsr <- mat
    tnsr$i <- index
    tnsr$v <- mat$v
    tnsr$dim <- dimToRecover
    return(tnsr)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
