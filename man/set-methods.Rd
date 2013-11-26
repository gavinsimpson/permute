\name{set-methods}
\alias{set-methods}
\alias{setBlocks<-}
\alias{setBlocks<-.default}
\alias{setBlocks<-.how}
\alias{setBlocks<-.permControl}
\alias{setWithin<-}
\alias{setWithin<-.default}
\alias{setWithin<-.how}
\alias{setStrata<-}
\alias{setStrata<-.default}
\alias{setStrata<-.how}
\alias{setStrata<-.Plots}
\alias{setType<-}
\alias{setType<-.default}
\alias{setType<-.how}
\alias{setType<-.Plots}
\alias{setType<-.Within}
\alias{setMirror<-}
\alias{setMirror<-.default}
\alias{setMirror<-.how}
\alias{setMirror<-.Plots}
\alias{setMirror<-.Within}
\alias{setConstant<-}
\alias{setConstant<-.default}
\alias{setConstant<-.how}
\alias{setConstant<-.Plots}
\alias{setConstant<-.Within}
\alias{setPlots<-}
\alias{setPlots<-.default}
\alias{setPlots<-.how}
\alias{setRow<-}
\alias{setRow<-.default}
\alias{setRow<-.how}
\alias{setRow<-.Plots}
\alias{setRow<-.Within}
\alias{setCol<-}
\alias{setCol<-.default}
\alias{setCol<-.how}
\alias{setCol<-.Plots}
\alias{setCol<-.Within}
\alias{setDim<-}
\alias{setDim<-.default}
\alias{setDim<-.how}
\alias{setDim<-.Plots}
\alias{setDim<-.Within}
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
setPlots(object) <- value
setWithin(object) <- value
setStrata(object) <- value
setNperm(object) <- value
setAllperms(object) <- value
setMaxperm(object) <- value
setMinperm(object) <- value
setComplete(object) <- value
setMake(object) <- value
setObserved(object) <- value
setRow(object) <- value
setCol(object) <- value
setDim(object) <- value
setType(object) <- value
setMirror(object) <- value
setConstant(object) <- value
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
\section{Note}{
  \code{setStrata<-} has methods for objects of class \code{"how"} and
  \code{"Plots"}. The former sets the \code{blocks} component of the
  \code{\link{how}} object, whilst the latter sets the \code{strata}
  component of the \code{\link{Plots}} object.

  \code{setDim<-}, \code{setRow<-}, and \code{setCol<-} cannot be used
  on an object of class \code{"how"}. Instead, extract the \code{Plots}
  or \code{Within} components with \code{\link{getPlots}} or
  \code{\link{getWithin}} and alter those components, then use the
  resulting object to replace the \code{plots} or \code{within}
  components using \code{setPlots} or \code{setWithin}.
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