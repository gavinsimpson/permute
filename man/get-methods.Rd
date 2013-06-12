\name{get-methods}
\alias{get-methods}
\alias{getBlocks}
\alias{getBlocks.default}
\alias{getBlocks.how}
\alias{getBlocks.permControl}
\alias{getWithin}
\alias{getWithin.default}
\alias{getWithin.how}
\alias{getWithin.permControl}
\alias{getStrata}
\alias{getStrata.default}
\alias{getStrata.how}
\alias{getStrata.permControl}
\alias{getType}
\alias{getType.default}
\alias{getType.how}
\alias{getType.permControl}
\alias{getMirror}
\alias{getMirror.default}
\alias{getMirror.how}
\alias{getMirror.permControl}
\alias{getConstant}
\alias{getConstant.default}
\alias{getConstant.how}
\alias{getConstant.permControl}
\alias{getPlots}
\alias{getPlots.default}
\alias{getPlots.how}
\alias{getPlots.permControl}
\alias{getRow}
\alias{getRow.default}
\alias{getRow.how}
\alias{getRow.permControl}
\alias{getCol}
\alias{getCol.default}
\alias{getCol.how}
\alias{getCol.permControl}
\alias{getDim}
\alias{getDim.default}
\alias{getDim.how}
\alias{getDim.permControl}
\alias{getNperm}
\alias{getNperm.default}
\alias{getNperm.how}
\alias{getNperm.permControl}
\alias{getMaxperm}
\alias{getMaxperm.default}
\alias{getMaxperm.how}
\alias{getMaxperm.permControl}
\alias{getMinperm}
\alias{getMinperm.default}
\alias{getMinperm.how}
\alias{getMinperm.permControl}
\alias{getComplete}
\alias{getComplete.default}
\alias{getComplete.how}
\alias{getComplete.permControl}

\title{Extractor functions to access components of a permutation design}
\description{
  Simple functions to allow abstracted access to components of a
  permutation design, for example as returned by
  \code{\link{how}}. Whilst many of these are very simple index
  opertations on a list, using these rather than directly accessing that
  list allows the internal representation of the permutation design to
  change without breaking code.
}
\usage{

getWithin(object, ...)
getPlots(object, ...)
getBlocks(object, ...)
getStrata(object, ...)
getType(object, ...)
getMirror(object, ...)
getConstant(object, ...)
getConstant(object, ...)
getCol(object, ...)
getRow(object, ...)
getDim(object, ...)
getNperm(object, ...)
getMaxperm(object, ...)
getMinperm(object, ...)
getComplete(object, ...)

\method{getWithin}{how}(object, ...)

\method{getPlots}{how}(object, ...)

\method{getBlocks}{how}(object, ...)

\method{getStrata}{how}(object, which = c("plots", "blocks"),
         drop = TRUE, ...)

\method{getType}{how}(object, which = c("plots", "within"), ...)

\method{getMirror}{how}(object, which = c("plots", "within"), ...)

\method{getConstant}{how}(object, ...)

\method{getCol}{how}(object, which = c("plots", "within"), ...)

\method{getRow}{how}(object, which = c("plots", "within"), ...)

\method{getDim}{how}(object, which = c("plots", "within"), ...)

\method{getNperm}{how}(object, ...)

\method{getMaxperm}{how}(object, ...)

\method{getMinperm}{how}(object, ...)

\method{getComplete}{how}(object, ...)

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
  permutation scheme described by \code{\link{how}}.
}

\examples{
set.seed(1234)

}
\keyword{ methods }
\keyword{ utils }