\name{allPerms}
\alias{allPerms}
\alias{print.allPerms}
\alias{summary.allPerms}
\alias{print.summary.allPerms}

\title{Complete enumeration of all possible permutations}
\description{
  \code{allPerms} is a utility function to return the set of
  permutations for a given R object and a specified permutation design.
}
\usage{
allPerms(n, control = permControl(), max = 9999,
         observed = FALSE)

\method{summary}{allPerms}(object, \dots)
}
\arguments{
  \item{n}{the number of observations or an 'object' from which the
    number of observations can be determined via \code{getNumObs}.}
  \item{control}{a list of control values describing properties of the
    permutation design, as returned by a call to
    \code{\link{permControl}}.}
  \item{max}{the maximum number of permutations, below which complete
    enumeration will be attempted. See Details.}
  \item{observed}{logical, should the observed ordering of samples be
    returned as part of the complete enumeration? Default is
    \code{FALSE} to facilitate usage in higher level functions.}
  \item{object}{an object of class \code{"allPerms"}.}
  \item{\dots}{arguments to other methods.}
}
\details{
  Function \code{allPerms} enumerates all possible permutations for the 
  number of observations and the selected permutation scheme. It has
  \code{\link{print}} and \code{\link{summary}} methods. \code{allPerms}
  returns a matrix containing all possible permutations, possibly
  containing the observed ordering (if argument \code{observed} is
  \code{TRUE}). The rows of this matrix are the various permutations and
  the columns reflect the number of samples.

  With free permutation designs, and restricted permutation schemes with
  large numbers of observations, there are a potentially huge number of
  possible permutations of the samples. It would be inefficient, not to
  mention incredibly time consuming, to enumerate them all. Storing all
  possible permutations would also become problematic in such cases. To
  control this and guard against trying to evaluate too large a number
  of permutations, if the number of possible permutations is larger than
  \code{max}, \code{allPerms} exits with an error.
}
\value{
  For \code{allPerms}, and object of class \code{"allPerms"}, a matrix
  whose rows are the set of all possible permutations for the supplies
  number of observations and permutation scheme selected. The matrix has
  two additional attributes \code{control} and
  \code{observed}. Attribute \code{control} contains the argument
  \code{control} (possibly updated via \code{check}). Attribute 
  \code{observed} contains argument \code{observed}.
}
\section{Warning}{
  This function currently assumes that any strata are represented by
  contiguous blocks in the order of the levels of \code{strata}. This is
  unlike \code{\link{shuffle}} and \code{\link{shuffleSet}}.

  If permuting the strata themselves, a balanced design is required (the
  same number of observations in each level of \code{strata}. This is
  common to all functions in the package.
}
\author{Gavin Simpson}
\examples{
## allPerms can work with a vector
vec <- c(3,4,5)
allPerms(vec) ## free permutation

## enumerate all possible permutations for a more complicated
## design
fac <- gl(2,6)
##ctrl <- permControl(type = "grid", mirror = FALSE, strata = fac,
##                    constant = TRUE, nrow = 3, ncol = 2)
ctrl <- permControl(strata = fac,
                    within = Within(type = "grid", mirror = FALSE,
                                    constant = TRUE, nrow = 3, ncol = 2))
Nobs <- length(fac)
numPerms(seq_len(Nobs), control = ctrl)
(tmp <- allPerms(Nobs, control = ctrl, observed = TRUE))
(tmp2 <- allPerms(Nobs, control = ctrl))

## turn on mirroring
ctrl$within$mirror <- TRUE
numPerms(seq_len(Nobs), control = ctrl)
(tmp3 <- allPerms(Nobs, control = ctrl, observed = TRUE))
(tmp4 <- allPerms(Nobs, control = ctrl))

%\dontrun{
## prints out details of the permutation scheme as
## well as the matrix of permutations
% FIXME: uncomment the two lines below when we remove old permute
% code from vegan and have vegan depend on permute
summary(tmp3)
summary(tmp4)
%}
}
