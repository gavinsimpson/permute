\name{plots}
\alias{plots}
\alias{plots.default}
\alias{plots.how}
\alias{plots<-}
\alias{plots<-.default}
\alias{plots<-.how}

\title{Plot factors}
\description{
  Extract or replace the plot-level factor of an existing restricted
  permutation design created by \code{\link{how}}.
}
\usage{
plots(object, ...)
plots(object) <- value

\method{plots}{how}(object, ...)

}

\arguments{
  \item{object}{An R object to dispatch on.}
  \item{value}{The replacement value/object.}
  \item{\dots}{Arguments passed on to other methods.}
}
\value{
  The \code{plots} component of \code{object} is returned.
}
\author{Gavin L. Simpson}
\seealso{
  Define permutation schemes with plots using \code{\link{how}}.
}

\examples{
## extract plots factor from a "how" object
hh <- how(plots = Plots(strata = gl(4,5)))
plots(hh)
}
\keyword{ methods }
\keyword{ utils }