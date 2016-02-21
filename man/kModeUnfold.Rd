\name{kModeUnfold}
\alias{kModeUnfold}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kModeUnfold(tnsr, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tnsr}{
%%     ~~Describe \code{tnsr} here~~
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
function (tnsr, m) 
{
    if (class(tnsr) == "array") {
        mat <- t(apply(tnsr, m, cBind))
        return(mat)
    }
    if (class(tnsr) == "simple_sparse_array") {
        if (prod(dim(tnsr)) == 1) {
            if (as.array(tnsr)[1] != 0) {
                mat <- matrix(tnsr$v)
            }
            else {
                mat <- matrix(0)
            }
            return(mat)
        }
        else {
            if (ncol(tnsr$i) != 3) {
                tnsr <- as.simple_sparse_array(as.array(tnsr))
            }
            mat.nrow <- dim(tnsr)[m]
            mat.ncol <- prod(dim(tnsr)[-m])
            mat.rowIdx <- tnsr$i[, m]
            mat.colIdx <- tnsr$i[, -m]
            if (class(mat.colIdx) == "integer") {
                mat.colIdx <- t(as.matrix(mat.colIdx))
            }
            if (class(mat.colIdx) == "numeric") {
                mat.colIdx <- t(as.matrix(mat.colIdx))
            }
            mat.colIdx[, 2:ncol(mat.colIdx)] <- mat.colIdx[, 
                2:ncol(mat.colIdx)] - 1
            shift.index <- dim(tnsr)[-m]
            shift.index <- c(1, cumprod(shift.index))[1:(length(dim(tnsr)) - 
                1)]
            mat.colIdx <- mat.colIdx \%*\% shift.index
            tnsr$i <- cbind(mat.rowIdx, mat.colIdx)
            tnsr$dim <- c(mat.nrow, mat.ncol)
            tnsr$dimnames <- NULL
            return(tnsr)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
