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
shuffleSet(n, nset, control = how())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
    numeric; the number of observations in the sample set.
  }
  \item{nset}{
    numeric; the number of permutations to generate for the set. Can be
    missing, the default, in which case \code{nset} is determined from
    \code{control}.
  }
  \item{control}{
    an object of class \code{"how"} describing a valid
    permutation design.
  }
}
\details{
  TODO
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
\references{
  \code{shuffle()} is modelled after the permutation schemes of Canoco
  3.1 (ter Braak, 1990); see also Besag & Clifford (1989).

  Besag, J. and Clifford, P. (1989) Generalized Monte Carlo significance
  tests. \emph{Biometrika} \strong{76}; 633--642.

  ter Braak, C. J. F. (1990). \emph{Update notes: CANOCO version
    3.1}. Wageningen: Agricultural Mathematics Group. (UR).
}
\seealso{
  See \code{\link{shuffle}} for generating a single permutation, and
  \code{\link{how}} for setting up permutation designs.
}
\examples{
## simple random permutations, 5 permutations in set
shuffleSet(n = 10, nset = 10)

## series random permutations, 10 permutations in set
shuffleSet(10, 10, how(within = Within(type = "series")))

## series random permutations, 10 permutations in set,
## with possible mirroring
CTRL <- how(within = Within(type = "series", mirror = TRUE))
shuffleSet(10, 10, CTRL)

## Permuting strata
## 4 groups of 5 observations
CTRL <- how(within = Within(type = "none"),
            plots = Plots(strata = gl(4,5), type = "free"))
shuffleSet(20, 10, control = CTRL)

## 10 random permutations in presence of Plot-level strata
plotStrata <- Plots(strata = gl(4,5))
CTRL <- how(plots = plotStrata,
            within = Within(type = "free"))
shuffleSet(20, 10, control = CTRL)
## as above but same random permutation within Plot-level strata
CTRL <- how(plots = plotStrata,
            within = Within(type = "free", constant = TRUE))
shuffleSet(20, 10, CTRL)

## time series within each level of Plot strata
CTRL <- how(plots = plotStrata,
            within = Within(type = "series"))
shuffleSet(20, 10, CTRL)
## as above, but  with same permutation for each Plot-level stratum
CTRL <- how(plots = plotStrata,
            within = Within(type = "series", constant = TRUE))
shuffleSet(20, 10, CTRL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ design }
