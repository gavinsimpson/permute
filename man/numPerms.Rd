\name{numPerms}
\alias{numPerms}

\title{Number of possible permutations for a given object}
\description{
  \code{numPerms} calculates the maximum number of permutations possible
  under the current permutation scheme.
}
\usage{
numPerms(object, control = how())
}
\arguments{
  \item{object}{any object handled by \code{\link{nobs}}.}
  \item{control}{a list of control values describing properties of the
    permutation design, as returned by a call to
    \code{\link{how}}.}
}
\details{
  Function \code{numPerms} returns the number of permutations for the
  passed \code{object} and the selected permutation
  scheme. \code{object} can be one of a data frame, matrix, an object
  for which a scores method exists, or a numeric or integer vector. In
  the case of a numeric or integer vector, a vector of length 1 can be
  used and it will be expanded to a vector of length \code{object}
  (i.e., \code{1:object}) before computing the number of
  permutations. As such, \code{object} can be the number of observations
  not just the object containing the observations. 
}
\value{
  The (numeric) number of possible permutations of observations in
  \code{object}.
}
\note{
  In general, mirroring \code{"series"} or \code{"grid"} designs doubles
  or quadruples, respectively, the number of permutations without
  mirroring (within levels of strata if present). This is \strong{not}
  true in two special cases:
  \enumerate{
    \item In \code{"grid"} designs where the number of columns is equal
    to 2, and
    \item In \code{"series"} designs where the number of observations in
    a series is equal to 2.
  }

  For example, with 2 observations there are 2 permutations for
  \code{"series"} designs:
  \enumerate{
    \item 1-2, and
    \item 2-1.
  }
  If these two permutations were mirrored, we would have:
  \enumerate{
    \item 2-1, and
    \item 1-2.
  }
  It is immediately clear that this is the same set of permutations
  without mirroring (if one reorders the rows). A similar situation
  arises in \code{"grid"} designs where the number of \strong{columns}
  per \emph{grid} is equal to 2. Note that the number of rows per
  \emph{grid} is not an issue here.
}
\author{Gavin Simpson}
\seealso{\code{\link{shuffle}} and
  \code{\link{how}}. Additional \code{\link{nobs}} methods are
  provide, see \code{\link{nobs-methods}}.}
\examples{
## permutation design --- see ?how
ctrl <- how() ## defaults to freely exchangeable

## vector input
v <- 1:10
(obs <- nobs(v))
numPerms(v, control = ctrl)

## integer input
len <- length(v)
(obs <- nobs(len))
numPerms(len, control = ctrl)

## new design, objects are a time series
ctrl <- how(within = Within(type = "series"))
numPerms(v, control = ctrl)
## number of permutations possible drastically reduced...
## ...turn on mirroring
ctrl <- how(within = Within(type = "series", mirror = TRUE))
numPerms(v, control = ctrl)

## Try blocking --- 2 groups of 5
bl <- numPerms(v, control = how(blocks = gl(2,5)))
bl

## should be same as
pl <- numPerms(v, control = how(plots = Plots(strata = gl(2,5))))
pl
stopifnot(all.equal(bl, pl))
}
