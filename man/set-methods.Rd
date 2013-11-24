\name{set-methods}
\alias{set-methods}
\alias{setBlocks<-}
\alias{setBlocks<-.default}
\alias{setBlocks<-.how}
\alias{setBlocks<-.permControl}
%% \alias{getWithin}
%% \alias{getWithin<-.default}
%% \alias{getWithin<-.how}
%% \alias{getWithin<-.permControl}
%% \alias{getStrata}
%% \alias{getStrata<-.default}
%% \alias{getStrata<-.how}
%% \alias{getStrata<-.permControl}
%% \alias{getStrata<-.Plots}
%% \alias{getType}
%% \alias{getType<-.default}
%% \alias{getType<-.how}
%% \alias{getType<-.permControl}
%% \alias{getType<-.Plots}
%% \alias{getType<-.Within}
%% \alias{getMirror}
%% \alias{getMirror<-.default}
%% \alias{getMirror<-.how}
%% \alias{getMirror<-.permControl}
%% \alias{getMirror<-.Plots}
%% \alias{getMirror<-.Within}
%% \alias{getConstant}
%% \alias{getConstant<-.default}
%% \alias{getConstant<-.how}
%% \alias{getConstant<-.permControl}
%% \alias{getConstant<-.Within}
%% \alias{getPlots}
%% \alias{getPlots<-.default}
%% \alias{getPlots<-.how}
%% \alias{getPlots<-.permControl}
%% \alias{getRow}
%% \alias{getRow<-.default}
%% \alias{getRow<-.how}
%% \alias{getRow<-.permControl}
%% \alias{getRow<-.Plots}
%% \alias{getRow<-.Within}
%% \alias{getCol}
%% \alias{getCol<-.default}
%% \alias{getCol<-.how}
%% \alias{getCol<-.permControl}
%% \alias{getCol<-.Plots}
%% \alias{getCol<-.Within}
%% \alias{getDim}
%% \alias{getDim<-.default}
%% \alias{getDim<-.how}
%% \alias{getDim<-.permControl}
%% \alias{getDim<-.Plots}
%% \alias{getDim<-.Within}
\alias{setNperm<-}
\alias{setNperm<-.default}
\alias{setNperm<-.how}
\alias{setNperm<-.permControl}
\alias{setAllperms<-}
\alias{setAllperms<-.default}
\alias{setAllperms<-.how}
\alias{setAllperms<-.permControl}
\alias{setMaxperm<-}
\alias{setMaxperm<-.default}
\alias{setMaxperm<-.how}
\alias{setMaxperm<-.permControl}
\alias{setMinperm<-}
\alias{setMinperm<-.default}
\alias{setMinperm<-.how}
\alias{setMinperm<-.permControl}
\alias{setComplete<-}
\alias{setComplete<-.default}
\alias{setComplete<-.how}
\alias{setComplete<-.permControl}
\alias{setMake<-}
\alias{setMake<-.default}
\alias{setMake<-.how}
\alias{setObserved<-}
\alias{setObserved<-.default}
\alias{setObserved<-.how}

\title{Replacement functions to set components of a permutation design}
\description{
  Simple functions to allow abstracted replacement of components of a
  permutation design, for example as returned by \code{\link{how}}. In
  addition to performing replacement of components of the list returned
  by \code{\link{how}}, these replacement function also update the
  matched calls stored within the list to facilitate the use of
  \code{\link{update}} by users.
}
\usage{

setBlocks(object) <- value
setNperm(object) <- value
setAllperms(object) <- value
setMaxperm(object) <- value
setMinperm(object) <- value
setComplete(object) <- value
setMake(object) <- value
setObserved(object) <- value

\method{setBlocks}{how}(object) <- value

\method{setNperm}{how}(object) <- value

\method{setAllperms}{how}(object) <- value

\method{setMaxperm}{how}(object) <- value

\method{setMinperm}{how}(object) <- value

\method{setComplete}{how}(object) <- value

\method{setMake}{how}(object) <- value

\method{setObserved}{how}(object) <- value

}

\arguments{
  \item{object}{An R object to dispatch on.}
  \item{value}{The replacement value/object.}
}
\details{
  These are replacement functions for working with permutation design
  objects created by \code{\link{how}}. They should be used in
  preference to directly updating the permutation design in case the
  internal structure of object changes as \pkg{permute} is developed and
  because the matched call also needs to be updated to facilitate use of
  \code{\link{update}} on the \code{\link{how}} object.
}
\value{
  These replacement functions return \code{object} suitably modified.
}
\author{Gavin Simpson}
\seealso{\code{\link{check}}, a utility function for checking
  permutation scheme described by \code{\link{how}}. Comparable
  extractor functions are also available; see
  \code{\link{get-methods}}.
}

\examples{
## extract components from a "how" object
hh <- how()
getNperm(hh)
setNperm(hh) <- 999
getNperm(hh)
}
\keyword{ methods }
\keyword{ utils }