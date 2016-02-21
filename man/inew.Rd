\name{inew}
\alias{inew}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inew(C, old_ind)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{C}{
%%     ~~Describe \code{C} here~~
}
  \item{old_ind}{
%%     ~~Describe \code{old_ind} here~~
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
function (C, old_ind) 
{
    n <- dim(C)[1]
    vec <- rep(0, n)
    for (i in 1:n) {
        vec[i] <- sqrt(sum((C[i, ]$v)^2))
    }
    ind <- order(abs(vec), decreasing = T)
    if (!any(old_ind == ind[1])) {
        ind <- ind[1]
    }
    else if (!any(old_ind == ind[2])) {
        ind <- ind[2]
    }
    else if (!any(old_ind == ind[3])) {
        ind <- ind[3]
    }
    else if (!any(old_ind == ind[4])) {
        ind <- ind[4]
    }
    else if (!any(old_ind == ind[5])) {
        ind <- ind[5]
    }
    else if (!any(old_ind == ind[6])) {
        ind <- ind[6]
    }
    else if (!any(old_ind == ind[7])) {
        ind <- ind[7]
    }
    else if (!any(old_ind == ind[8])) {
        ind <- ind[8]
    }
    else if (!any(old_ind == ind[9])) {
        ind <- ind[9]
    }
    else if (!any(old_ind == ind[10])) {
        ind <- ind[10]
    }
    else {
        ind <- ind[11]
    }
    return(ind)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
