\name{HOSVD}
\alias{HOSVD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HOSVD(tnsr, ranks = ceiling(dim(tnsr)/10), doMC = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tnsr}{
%%     ~~Describe \code{tnsr} here~~
}
  \item{ranks}{
%%     ~~Describe \code{ranks} here~~
}
  \item{doMC}{
%%     ~~Describe \code{doMC} here~~
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
function (tnsr, ranks = ceiling(dim(tnsr)/10), doMC = T) 
{
    if (1 == 0) {
        hosvd_res <- HOSVD(y, core_dims)
        res <- hosvd(as.tensor(as.array(y)), core_dims)
        ttm(as.tensor(as.array(g)), t(As[[1]]), 1)@data
    }
    if (doMC == F) {
        As <- lapply(1:length(dim(tnsr)), function(mode.n) {
            X.unfolded <- kModeUnfold(tnsr, mode.n)
            X.unfolded <- as.sparseMatrix.simple_sparse_array(X.unfolded)
            A <- smartsvd(X.unfolded, nu = ranks[mode.n])$u
            A <- as.matrix(A[, 1:ranks[mode.n]])
            return(A)
        })
    }
    else if (doMC == T) {
        registerDoMC(3)
        As <- foreach(mode.n = 1:length(dim(tnsr))) \%dopar\% {
            X.unfolded <- kModeUnfold(tnsr, mode.n)
            X.unfolded <- as.sparseMatrix.simple_sparse_array(X.unfolded)
            A <- smartsvd(X.unfolded, nu = ranks[mode.n])$u
            A <- as.matrix(A[, 1:ranks[mode.n]])
            return(A)
        }
    }
    g <- tnsr
    for (n in 1:length(dim(tnsr))) {
        g <- kModeProduct(tnsr = g, mat = t(As[[n]]), m = n)
    }
    return(list(g = g, As = As))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
