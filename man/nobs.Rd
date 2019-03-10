\name{nobs-methods}
\alias{nobs-methods}
\alias{nobs.numeric}
\alias{nobs.integer}
\alias{nobs.matrix}
\alias{nobs.data.frame}
\alias{nobs.factor}
\alias{nobs.character}

\title{Number of observations in a given object}
\description{
  \code{\link{nobs}} is a generic function to return the number of
  observations from a model. \code{shuffle} provides a few methods for
  other types of data object in R.
}
\usage{

\method{nobs}{numeric}(object, \dots)

\method{nobs}{integer}(object, \dots)

\method{nobs}{matrix}(object, \dots)

\method{nobs}{data.frame}(object, \dots)

\method{nobs}{character}(object, \dots)

\method{nobs}{factor}(object, \dots)
}
\arguments{
  \item{object}{a data frame or matrix, or a numeric, integer,
    character, or factor vector.}
  \item{\dots}{arguments to other methods.}
}
\details{
  Function \code{nobs} is a simple generic function to return the
  number of observations in a range of R model objects. Methods are
  provided to work with a variety of R objects.
}
\value{
  The (numeric) number of observations in \code{object}.
}
\author{Gavin Simpson}
\examples{
\dontshow{suppressWarnings(RNGversion("3.5.0"))}
set.seed(1)
## numeric vector
len <- sample(1:10, 1)
v <- as.numeric(sample(1:100, len))
len
obs <- nobs(v)
isTRUE(all.equal(len, obs))

## integer
len <- sample(1L:10L, 1)
obs <- nobs(len)
isTRUE(all.equal(len, obs))

}
