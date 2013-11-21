\name{check}
\alias{check}
\alias{print.check}
\alias{print.summary.check}
\alias{summary.check}

\title{Utility functions for permutation schemes}
\description{
  \code{check} provides checking of permutation schemes for
  validity. \code{permuplot} produces a graphical representation of the
  selected permutation design.
}
\usage{
check(object, control = how(), quietly = FALSE)

\method{summary}{check}(object, \dots)
}

\arguments{
  \item{object}{an R object. See Details for a complete description,
    especially for \code{\link{numPerms}}. For
    \code{\link{summary.check}} an object of class \code{"check"}.}
  \item{control}{a list of control values describing properties of the
    permutation design, as returned by a call to \code{\link{how}}.}
  \item{quietly}{logical; should messages by suppressed?}
  \item{\dots}{arguments to other methods.}
}
\details{
  \code{check} is a utility functions for working with the new
  permutation schemes available in \code{\link{shuffle}}.

  \code{check} is used to check the current permutation schemes
  against the object to which it will be applied. It calculates the
  maximum number of possible permutations for the number of observations
  in \code{object} and the permutation scheme described by
  \code{control}. The returned object contains component \code{control},
  an object of class \code{"how"} suitably modified if
  \code{check} identifies a problem.

  The main problem is requesting more permutations than is possible with
  the number of observations and the permutation design. In such cases,
  \code{nperm} is reduced to equal the number of possible permutations,
  and complete enumeration of all permutations is turned on
  (\code{control$complete} is set to \code{TRUE}). 

  Alternatively, if the number of possible permutations is low, and less
  than \code{control$minperm}, it is better to enumerate all possible
  permutations, and as such complete enumeration of all permutations is
  turned  on (\code{control$complete} is set to \code{TRUE}).
}
\value{
  For \code{check} a list containing the maximum number of
  permutations possible and an object of class \code{"\link{how}"}.
}
\author{Gavin L. Simpson}
\seealso{\code{\link{shuffle}} and \code{\link{how}}.}

\examples{
## use example data from ?pyrifos in package vegan
require(vegan)
example(pyrifos)

## Demonstrate the maximum number of permutations for the pyrifos data
## under a series of permutation schemes

## no restrictions - lots of perms
CONTROL <- how(within = Within(type = "free"))
(check1 <- check(pyrifos, CONTROL))
##summary(check1)

## no strata but data are series with no mirroring, so 132 permutations
CONTROL <- how(within = Within(type = "series", mirror = FALSE))
check(pyrifos, CONTROL)

## no strata but data are series with mirroring, so 264 permutations
CONTROL <- how(within = Within(type = "series", mirror = TRUE))
check(pyrifos, control = CONTROL)

## unrestricted within strata
check(pyrifos, control = how(plots = Plots(strata = ditch),
                             within = Within(type = "free")))

## time series within strata, no mirroring
check(pyrifos,
      control = how(plots = Plots(strata = ditch),
                    within = Within(type = "series", mirror = FALSE)))

## time series within strata, with mirroring
check(pyrifos,
      control = how(plots = Plots(strata = ditch),
                    within = Within(type = "series", mirror = TRUE)))

## time series within strata, no mirroring, same permutation
## within strata
check(pyrifos,
      control = how(plots = Plots(strata = ditch),
                    within = Within(type = "series", constant = TRUE)))

## time series within strata, with mirroring, same permutation
## within strata
check(pyrifos,
      control = how(plots = Plots(strata = ditch),
                    within = Within(type = "series", mirror = TRUE,
                                    constant = TRUE)))
## permute strata
check(pyrifos, how(plots = Plots(strata = ditch, type = "free"),
                   within = Within(type = "none")))

## this should also also for arbitrary vectors
vec1 <- check(1:100)
vec2 <- check(1:100, how())
all.equal(vec1, vec2)
vec3 <- check(1:100, how(within = Within(type = "series")))
all.equal(100, vec3$n)
vec4 <- check(1:100, how(within = Within(type= "series", mirror = TRUE)))
all.equal(vec4$n, 200)

## enumerate all possible permutations
fac <- gl(2,6)
ctrl <- how(plots = Plots(strata = fac),
            within = Within(type = "grid", mirror = FALSE,
                            constant = TRUE, nrow = 3, ncol = 2))
check(1:12, ctrl)

numPerms(1:12, control = ctrl)
(tmp <- allPerms(12, control = update(ctrl, observed = TRUE)))
(tmp2 <- allPerms(12, control = ctrl))

## turn on mirroring
ctrl <- update(ctrl, within = update(getWithin(ctrl), mirror = TRUE))
numPerms(1:12, control = ctrl)
(tmp3 <- allPerms(12, control = update(ctrl, observed = TRUE)))
(tmp4 <- allPerms(12, control = ctrl))
## prints out details of the permutation scheme as
## well as the matrix of permutations
summary(tmp)
summary(tmp2)

## different numbers of observations per level of strata
fac <- factor(rep(1:3, times = c(3,2,2)))
## free permutations in levels of strata
numPerms(7, how(within = Within(type = "free"),
                plots = Plots(strata = fac, type = "none")))
allPerms(7, how(within = Within(type = "free"),
                plots = Plots(strata = fac)))
## series permutations in levels of strata
ctrl <- how(within = Within(type = "series"), plots = Plots(strata = fac))
numPerms(7, control = ctrl)
allPerms(7, control = ctrl)
}
\keyword{ utilities }
\keyword{ design }
\keyword{ methods }
