\name{permCheck-deprecated}
\alias{permCheck} % for the deprecated function
\alias{permCheck-deprecated} % for the deprecated function
\alias{print.permCheck}
\alias{print.summary.permCheck}
\alias{summary.permCheck}

\title{Utility functions for permutation schemes}
\description{
  \code{permCheck} provides checking of permutation schemes for
  validity. \code{permuplot} produces a graphical representation of the
  selected permutation design.
}
\usage{
permCheck(object, control = how())

\method{summary}{permCheck}(object, \dots)
}

\arguments{
  \item{object}{an R object. See Details for a
    complete description, especially for \code{numPerms}. For
    \code{\link{summary.permCheck}} an object of class
    \code{"permCheck"}.} 
  \item{control}{a list of control values describing properties of the
    permutation design, as returned by a call to
    \code{\link{how}}.}
  \item{\dots}{arguments to other methods. For \code{permuplot}
    graphical parameters can be passed to plotting functions, though
    note that not all parameters will be accepted gracefully at the
    moment.}
}
\details{
  \code{permCheck} is a utility functions for working
  with the new permutation schemes available in \code{\link{shuffle}}.

  \code{permCheck} is used to check the current permutation schemes
  against the object to which it will be applied. It calculates the
  maximum number of possible permutations for the number of observations
  in \code{object} and the permutation scheme described by
  \code{control}. The returned object contains component \code{control},
  an object of class \code{"how"} suitably modified if
  \code{permCheck} identifies a problem.

  The main problem is requesting more permutations than possible with
  the number of observations and the permutation design. In such cases,
  \code{nperm} is reduced to equal the number of possible permutations,
  and complete enumeration of all permutations is turned on
  (\code{control$complete} is set to \code{TRUE}). 

  Alternatively, if the number of possible permutations is low, and less
  than \code{control$minperm}, it is better to enumerate all possible
  permutations, and as such complete enumeration of all permutations is
  turned  on (\code{control$complete} is set to \code{TRUE}).
}
\value{
  For \code{permCheck} a list containing the maximum number of
  permutations possible and an object of class
  \code{"\link{how}"}.

  For \code{permuplot}, a plot on the currently active device.
}
\author{Gavin L. Simpson}
\seealso{\code{\link{shuffle}} and \code{\link{how}}.}
\keyword{ utilities }
\keyword{ methods }
