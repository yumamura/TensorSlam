\name{checkDiff}
\alias{checkDiff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
checkDiff(Xnew, Xold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Xnew}{
%%     ~~Describe \code{Xnew} here~~
}
  \item{Xold}{
%%     ~~Describe \code{Xold} here~~
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
function (Xnew, Xold) 
{
    if (0) {
        Xnew <- array(1:8, dim = c(2, 2, 2))
        Xnew <- array(2, dim = c(2, 2, 2))
        Xold <- array(2, dim = c(2, 2, 2))
    }
    if (class(Xnew) == "simple_sparse_array") 
        Xnew <- as.array(Xnew)
    if (class(Xold) == "simple_sparse_array") 
        Xold <- as.array(Xold)
    if (class(Xnew) == "simple_triplet_matrix") 
        Xnew <- as.matrix(Xnew)
    if (class(Xnew) == "simple_triplet_matrix") 
        Xnew <- as.matrix(Xnew)
    XDiff <- abs(Xnew - Xold)
    child <- sqrt(sum(XDiff^2))
    mother <- sqrt(sum(Xold^2))
    rate <- child/mother
    return(rate)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
