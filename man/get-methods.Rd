\name{get-methods}
\alias{get-methods}
\alias{getBlocks}
\alias{getBlocks.default}
\alias{getBlocks.permControl}
\alias{getWithin}
\alias{getWithin.default}
\alias{getWithin.permControl}
\alias{getStrata}
\alias{getStrata.default}
\alias{getStrata.permControl}
\alias{getType}
\alias{getType.default}
\alias{getType.permControl}
\alias{getMirror}
\alias{getMirror.default}
\alias{getMirror.permControl}
\alias{getConstant}
\alias{getConstant.default}
\alias{getConstant.permControl}
\alias{getPlots}
\alias{getPlots.default}
\alias{getPlots.permControl}

\title{Extractor functions to access components of a permutation design}
\description{
  Simple functions to allow abstracted access to components of a
  permutation design, for example as returned by
  \code{\link{permControl}}. Whilst many of these are very simple index
  opertations on a list, using these rather than directly accessing that
  list allows the internal representation of the permutation design to
  change without breaking code.
}
\usage{

getWithin(object, ...)

\method{getWithin}{permControl}(object, ...)

getPlots(object, ...)

\method{getPlots}{permControl}(object, ...)

getBlocks(object, ...)

\method{getBlocks}{permControl}(object, ...)

getStrata(object, ...)

\method{getStrata}{permControl}(object, which = c("plots", "blocks"),
drop = TRUE, ...)

getType(object, ...)

\method{getType}{permControl}(object, which = c("plots", "within"), ...)

getMirror(object, ...)

\method{getMirror}{permControl}(object, which = c("plots", "within"), ...)

getConstant(object, ...)

\method{getConstant}{permControl}(object, ...)
}

\arguments{
  \item{object}{An R object to dispatch on.}
  \item{which}{character; which level of restriction to extract
    information for.}
  \item{drop}{logical; should un-used factor levels be dropped?}
  \item{\dots}{Arguments passed on to other methods.}
}
\details{
  TODO
}
\value{
  TODO
}
\author{Gavin Simpson}
\seealso{\code{\link{check}}, a utility function for checking
  permutation scheme described by \code{\link{permControl}}.
}

\examples{
set.seed(1234)

}
\keyword{ methods }
\keyword{ utils }