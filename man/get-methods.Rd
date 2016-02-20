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
\alias{getStrata.Plots}
\alias{getType}
\alias{getType.default}
\alias{getType.how}
\alias{getType.permControl}
\alias{getType.Plots}
\alias{getType.Within}
\alias{getMirror}
\alias{getMirror.default}
\alias{getMirror.how}
\alias{getMirror.permControl}
\alias{getMirror.Plots}
\alias{getMirror.Within}
\alias{getConstant}
\alias{getConstant.default}
\alias{getConstant.how}
\alias{getConstant.permControl}
\alias{getConstant.Within}
\alias{getPlots}
\alias{getPlots.default}
\alias{getPlots.how}
\alias{getPlots.permControl}
\alias{getRow}
\alias{getRow.default}
\alias{getRow.how}
\alias{getRow.permControl}
\alias{getRow.Plots}
\alias{getRow.Within}
\alias{getCol}
\alias{getCol.default}
\alias{getCol.how}
\alias{getCol.permControl}
\alias{getCol.Plots}
\alias{getCol.Within}
\alias{getDim}
\alias{getDim.default}
\alias{getDim.how}
\alias{getDim.permControl}
\alias{getDim.Plots}
\alias{getDim.Within}
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
\alias{getMake}
\alias{getMake.default}
\alias{getMake.how}
\alias{getObserved}
\alias{getObserved.default}
\alias{getObserved.how}
\alias{getAllperms}
\alias{getAllperms.default}
\alias{getAllperms.how}
\alias{getControl}
\alias{getHow}
\alias{getControl.default}
\alias{getControl.allPerms}

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

getAllperms(object, ...)
getBlocks(object, ...)
getComplete(object, ...)
getConstant(object, ...)
getCol(object, ...)
getDim(object, ...)
getMake(object, ...)
getMaxperm(object, ...)
getMinperm(object, ...)
getMirror(object, ...)
getNperm(object, ...)
getObserved(object, ...)
getPlots(object, ...)
getRow(object, ...)
getStrata(object, ...)
getType(object, ...)
getWithin(object, ...)
getControl(object, ...)
getHow(object, ...)

\method{getAllperms}{how}(object, ...)

\method{getBlocks}{how}(object, ...)

\method{getCol}{how}(object, which = c("plots", "within"), ...)
\method{getCol}{Plots}(object, ...)
\method{getCol}{Within}(object, ...)

\method{getComplete}{how}(object, ...)

\method{getConstant}{how}(object, ...)
\method{getConstant}{Within}(object, ...)

\method{getDim}{how}(object, which = c("plots", "within"), ...)
\method{getDim}{Plots}(object, ...)
\method{getDim}{Within}(object, ...)

\method{getMake}{how}(object, ...)

\method{getMaxperm}{how}(object, ...)

\method{getMinperm}{how}(object, ...)

\method{getMirror}{how}(object, which = c("plots", "within"), ...)
\method{getMirror}{Plots}(object, ...)
\method{getMirror}{Within}(object, ...)

\method{getNperm}{how}(object, ...)

\method{getObserved}{how}(object, ...)

\method{getPlots}{how}(object, ...)

\method{getRow}{how}(object, which = c("plots", "within"), ...)
\method{getRow}{Plots}(object, ...)
\method{getRow}{Within}(object, ...)

\method{getStrata}{how}(object, which = c("plots", "blocks"),
          drop = TRUE, ...)
\method{getStrata}{Plots}(object, drop = TRUE, ...)

\method{getType}{how}(object, which = c("plots", "within"), ...)
\method{getType}{Plots}(object, ...)
\method{getType}{Within}(object, ...)

\method{getWithin}{how}(object, ...)

\method{getControl}{allPerms}(object, ...)
}

\arguments{
  \item{object}{An R object to dispatch on.}
  \item{which}{character; which level of restriction to extract
    information for.}
  \item{drop}{logical; should un-used factor levels be dropped?}
  \item{\dots}{Arguments passed on to other methods.}
}
\details{
  These are extractor functions for working with permutation design
  objects created by \code{\link{how}}. They should be used in
  preference to directly subsetting the permutation design in case the
  internal structure of object changes as \pkg{permute} is developed.

  \code{getHow} is an alias for \code{getControl}; specific methods are
  implemented for \code{getControl} if you are debugging.
}
\value{
  These are simple extractor functions and return the contents of the
  corresponding components of \code{object}.
}
\author{Gavin Simpson}
\seealso{\code{\link{check}}, a utility function for checking
  permutation scheme described by \code{\link{how}}.
}

\examples{
## extract components from a "how" object
hh <- how()
getWithin(hh)
getNperm(hh)
}
\keyword{ methods }
\keyword{ utils }