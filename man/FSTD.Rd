\name{FSTD}
\alias{FSTD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FSTD(Y, k, eps = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{eps}{
%%     ~~Describe \code{eps} here~~
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
function (Y, k, eps = NULL) 
{
    I <- dim(Y)
    N <- length(I)
    index <- lapply(1:N, function(n) {
        ceiling(runif(1, min = 0, max = 1) * I[n])
    })
    ssub <- rep(1, N)
    p <- 2
    completed <- rep(0, N)
    while (p <= k && (prod(completed) == 0)) {
        for (n in 1:N) {
            if (completed[n] == 0) {
                if (p == 2 && n == 1) {
                  index[[n]] <- c(index[[n]], ceiling(runif(1, 
                    min = 0, max = 1) * I[n]))
                  ssub[n] <- ssub[n] + 1
                }
                else {
                  YresNorm <- sqrt(sum((Yres$v)^2))
                  FIBredNorm <- sqrt(sum((FIBred[[nextInd]]$v)^2))
                  if ((YresNorm/FIBredNorm) > eps) {
                    index[[n]] <- c(index[[n]], inew(Yres, index[[n]]))
                    ssub[n] <- ssub[n] + 1
                  }
                  else {
                  }
                }
                W <- Y[index[[1]], index[[2]], index[[3]]]
                Wpinv <- list()
                FIB <- list()
                for (m in 1:N) {
                  Wpinv[[m]] <- ginv(as.simple_triplet_matrix(kModeUnfold(tnsr = W, 
                    m = m)))
                  ind <- index
                  ind[[m]] <- 1:I[m]
                  FIB[[m]] <- as.simple_triplet_matrix(kModeUnfold(Y[ind[[1]], 
                    ind[[2]], ind[[3]]], m))
                }
                U <- reconstTucker(core = W, Wlist = Wpinv)
            }
            if (n == N) {
                nextInd <- 1
            }
            else {
                nextInd <- n + 1
            }
            if (completed[nextInd] == 0) {
                FIBred <- list()
                for (m in 1:N) {
                  FIBred[[m]] <- FIB[[m]][index[[m]], ]
                }
                ind <- index
                ind[[nextInd]] <- 1:I[nextInd]
                FIBred[[nextInd]] <- FIB[[nextInd]]
                smat <- ssub
                smat[nextInd] <- I[nextInd]
                mother <- Y[ind[[1]], ind[[2]], ind[[3]]]
                child <- reconstTucker(core = U, Wlist = FIBred)
                mother <- as.array(mother)
                child <- as.array(child)
                tnsrDiff <- as.simple_sparse_array(mother - child)
                Yres <- kModeUnfold(tnsrDiff, nextInd)
            }
        }
        p <- p + 1
    }
    res <- list(g = U, As = FIB)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
