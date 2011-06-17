\name{getNumObs}
\alias{getNumObs}
\alias{getNumObs.default}
\alias{getNumObs.integer}
\alias{getNumObs.numeric}
\alias{nobs.numeric}
\alias{nobs.integer}
\alias{nobs.matrix}
\alias{nobs.cca}

\title{Number of observations in a given object}
\description{
  \code{getNumObs} is a utility function to return the number of
  observations for a range of R and ordination objects.
}
\usage{
getNumObs(object, \dots)

\method{getNumObs}{default}(object, \dots)

\method{getNumObs}{numeric}(object, \dots)

\method{getNumObs}{integer}(object, \dots)
}
\arguments{
  \item{object}{any R object handled by \code{\link[vegan]{scores}},
    data frames, matrices, and numeric and integer vectors.}
  \item{\dots}{arguments to other methods.}
}
\details{
  Function \code{getNumObs} is a simple generic function to return the
  number of observations in a range of R objects. The default method
  will work for any object for which a \code{\link[vegan]{scores}}
  method exists. This includes matrices and data frames, as well as
  specific methods for numeric or integer vectors.
}
\value{
  The (numeric) number of observations in \code{object}.
}
\author{Gavin Simpson}
\examples{
## vector
len <- sample(1:10, 1)
v <- numeric(sample(1:100, len))
len
obs <- getNumObs(v)
isTRUE(all.equal(len, obs))

## integer
len <- sample(1:10, 1)
obs <- getNumObs(len)
isTRUE(all.equal(len, obs))

}
