\name{shuffleSet}
\alias{shuffleSet}

\title{
  Generate a set of permutations from the specified design.
}
\description{
  \code{shuffleSet} returns a set of \code{nset} permutations from the
  specified design. The main purpose of the function is to circumvent
  the overhead of repeatedly calling \code{\link{shuffle}} to generate a
  set of permutations.
}
\usage{
shuffleSet(n, nset = 1, control = permControl())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
    numeric; the number of observations in the sample set.
  }
  \item{nset}{
    numeric; the number of permutations to generate for the set
  }
  \item{control}{
    an object of class \code{"permControl"} describing a valid
    permutation design.
  }
}
\details{
  Currently, only the simple case of permutations not in the presence of
  blocks (strata) is implemented.
}
\value{
  Returns a matrix of permutations, where each row is a separate
  permutation. As such, the returned matrix has \code{nset} rows and
  \code{n} columns.
}
\author{
  Gavin L. Simpson
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  See \code{\link{shuffle}} for generating a single permutation, and
  \code{\link{permControl}} for setting up permutation designs.
}
\examples{
## simple random permutations, 5 permutations in set
shuffleSet(n = 10, nset = 10)

## series random permutations, 10 permutations in set
shuffleSet(10, 10, permControl(within = Within(type = "series")))

## series random permutations, 10 permutations in set,
## with possible mirroring
CTRL <- permControl(within = Within(type = "series", mirror = TRUE))
shuffleSet(10, 10, CTRL)

## Permuting strata
grp <- gl(4,5) ## 4 groups of 5 observations
CTRL <- permControl(strata = grp, within = Within(type = "none"),
                                  blocks = Blocks(type = "free"))
shuffleSet(20, 10, control = CTRL)

## 10 random permutations in presence of strata
block <- gl(4, 5)
CTRL <- permControl(strata = block,
                    within = Within(type = "free"))
shuffleSet(20, 10, CTRL)
## as above but same random permutation within strata
CTRL <- permControl(strata = block,
                    within = Within(type = "free", constant = TRUE))
shuffleSet(20, 10, CTRL)

## time series within each level of block
CTRL <- permControl(strata = block,
                    within = Within(type = "series"))
shuffleSet(20, 10, CTRL)
## as above, but  with same permutation for each level
CTRL <- permControl(strata = block,
                    within = Within(type = "series",
                                    constant = TRUE))
shuffleSet(20, 10, CTRL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ design }% __ONLY ONE__ keyword per line
