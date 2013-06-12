\name{allUtils}
\alias{allFree}
\alias{allSeries}
\alias{allGrid}
\alias{allStrata}

\title{Utility functions for complete enumeration of all possible
  permutations}

\description{
  Utility functions to return the set of all permutations under
  different designs. For most practical applications, i.e. to combine
  designs permuting blocks and/or within blocks function
  \code{\link{allPerms}} will be required.
}

\usage{
allFree(n, v = seq_len(n))

allSeries(n, nperms, mirror = FALSE)

allGrid(n, nperms, nr, nc, mirror, constant)

allStrata(n, control)
}

\arguments{
  \item{n}{the number of observations.}
  \item{v}{numeric; vector of indices. Default is \code{1:n}.}
  \item{nperms}{numeric; number of possible permutations.}
  \item{mirror}{logical; mirroring of permutations allowed?}
  \item{nr,nc}{integer; number of rows and columns of grid designs.}
  \item{constant}{logical; same permutation within each block?}
  \item{control}{a list of control values describing properties of the
    permutation design, as returned by a call to \code{\link{how}}.}
}

\details{
  These are utility functions and aren't designed for casual
  use. \code{\link{allPerms}} should be used instead.

  Details on usage of these functions can be found in
  \code{\link{allPerms}}.
}

\value{
  A matrix of all possible permutations of \code{n} observations or of
  \code{v}, given the provided options.
}

\author{Gavin Simpson}

