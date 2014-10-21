\name{blocks}
\alias{blocks}
\alias{blocks.default}
\alias{blocks.how}
\alias{blocks<-}
\alias{blocks<-.default}
\alias{blocks<-.how}

\title{Blocking factors}
\description{
  Extract or replace the blocking factor of an existing restricted
  permutation design created by \code{\link{how}}.
}
\usage{
blocks(object, ...)
blocks(object) <- value

\method{blocks}{how}(object, ...)

}

\arguments{
  \item{object}{An R object to dispatch on.}
  \item{value}{The replacement value/object.}
  \item{\dots}{Arguments passed on to other methods.}
}
\value{
  The \code{blocks} component of \code{object} is returned.
}
\author{Gavin L. Simpson}
\seealso{
  Define permutation schemes with blocking using \code{\link{how}}.
}

\examples{
## extract blocking factor from a "how" object
hh <- how(blocks = gl(4,5))
blocks(hh)
}
\keyword{ methods }
\keyword{ utils }