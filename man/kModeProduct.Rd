\name{kModeProduct}
\alias{kModeProduct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kModeProduct(tnsr, mat, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tnsr}{
%%     ~~Describe \code{tnsr} here~~
}
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{m}{
%%     ~~Describe \code{m} here~~
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
function (tnsr, mat, m) 
{
    tnsr.mat <- kModeUnfold(tnsr, m)
    tnsr.mat <- as.simple_triplet_matrix(tnsr.mat)
    if (class(mat) != "simple_triplet_matrix") 
        mat <- as.simple_triplet_matrix(mat)
    res.mat <- as.simple_sparse_array(crossprod_simple_triplet_matrix(t(mat), 
        tnsr.mat))
    res.tnsr.dim <- dim(tnsr)
    res.tnsr.dim[m] <- nrow(mat)
    res <- Refold(res.mat, res.tnsr.dim, m)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
