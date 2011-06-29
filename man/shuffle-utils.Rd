\name{shuffle-utils}
\alias{shuffle-utils}
\alias{shuffleFree}
\alias{shuffleGrid}
\alias{shuffleSeries}
\alias{shuffleStrata}

\title{Utility functions for Unrestricted and restricted permutations}
\description{
  Unrestricted and restricted permutations for time series,
  line transects, spatial grids and blocking factors.
}
\usage{
shuffleFree(x, size)

shuffleSeries(x, mirror = FALSE, start = NULL, flip = NULL)

shuffleGrid(nrow, ncol, mirror = FALSE, start.row = NULL,
            start.col = NULL, flip = NULL)

shuffleStrata(strata, type, mirror = FALSE, start = NULL, flip = NULL,
              nrow, ncol, start.row = NULL, start.col = NULL)
}

\arguments{
  \item{x}{vector of indices to permute.}
  \item{size}{number of random permutations required}
  \item{mirror}{logical; should mirroring of sequences be allowed?}
  \item{start}{integer; the starting point for time series
    permutations. If missing, a random starting point is determined.}
  \item{flip}{logical, length 1 (\code{shuffleSeries}) or length 2
    (\code{shuffleGrid}); force mirroring of permutation. This will
    always return the reverse of the computed permutation. For
    \code{shuffleGrid}, the first element pertains to flipping rows, the
    second to flipping columns of the grid.}
  \item{nrow, ncol}{numeric; the number of rows and columns in the grid.}
  \item{start.row, start.col}{numeric; the starting row and column for
    the shifted grid permutation. If non supplied, a random starting row
    and column will be selected.}
  \item{strata}{factor; the blocks to permute.}
  \item{type}{character; the type of permutation used to shuffle the
    \code{strata}. One of \code{"free"}, \code{"grid"} or
    \code{"series"}.}
}
\details{
}
\value{
}
%\note{
%  \code{shuffle} is currently used in one Vegan function;
%  \code{\link[vegan]{permutest.betadisper}}. Over time, the other functions
%  that currently use the older \code{\link[vegan]{shuffle}} will be
%  updated to use \code{shuffle}.
%}
%\references{
%}
\author{Gavin Simpson}
\seealso{\code{\link{permCheck}}, a utility function for checking
  permutation scheme described by
  \code{\link{permControl}}. \code{\link{shuffle}} as a user-oriented
  wrapper to these functions.}

\examples{
set.seed(3)

## draw 1 value at random from the set 1:10
shuffleFree(1:10, 1)

## permute the series 1:10
x <- 1:10
shuffleSeries(x)                ## with random starting point
shuffleSeries(x, start = 5L)    ## known starting point
shuffleSeries(x, flip = TRUE)   ## random start, forced mirror
shuffleSeries(x, mirror = TRUE) ## random start, possibly mirror

## permute a grid of size 3x3
shuffleGrid(3, 3)                      ## random starting row/col
shuffleGrid(3, 3, start.row = 2,
            start.col = 3)             ## with known row/col
shuffleGrid(3, 3, flip = rep(TRUE, 2)) ## random start, forced mirror
}
\keyword{ htest }
\keyword{ design }
