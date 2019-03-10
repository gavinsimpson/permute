\name{shuffleSet}
\alias{shuffleSet}
\alias{as.matrix.permutationMatrix}

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
shuffleSet(n, nset, control = how(), check = TRUE, quietly = FALSE)

\method{as.matrix}{permutationMatrix}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
    numeric; the number of observations in the sample set. May also be
    any object that \code{nobs} knows about; see
    \code{\link{nobs-methods}}.
  }
  \item{nset}{
    numeric; the number of permutations to generate for the set. Can be
    missing, the default, in which case \code{nset} is determined from
    \code{control}.
  }
  \item{control}{
    an object of class \code{"how"} describing a valid permutation
    design.
  }
  \item{check}{
    logical; should the design be checked for various problems via
    function \code{\link{check}}? The default is to check the design for
    the stated number of observations and update \code{control}
    accordingly. See Details.
  }
  \item{quietly}{
    logical; should messages by suppressed?
  }
  \item{x}{
    an object of class \code{"permutationMatrix"}, as returned by
    \code{shuffleSet}.
  }
  \item{\dots}{
    arguments passed to other methods. For the \code{as.matrix} method
    only.
  }
}
\details{
  \code{shuffleSet} is designed to generate a set of \code{nset}
  permutation indices over which a function can iterate as part of a
  permutation test. It is only slightly more efficient than calling
  \code{\link{shuffle}} \code{nset} times, but it is far more practical
  than the simpler function because a set of permutations can be worked
  on by applying a function to the rows of the returned object. This
  simplifies the function applied, and facilitates the use of parallel
  processing functions, thus enabling a larger number of permutations to
  be evaluated in reasonable time.

  By default, \code{shuffleSet} will check the permutations design
  following a few simple heuristics. See \code{\link{check}} for details
  of these. Whether some of the heuristics are activiated or not can be
  controlled via \code{\link{how}}, essentialy via its argument
  \code{minperm}. In particular, if there are fewer than \code{minperm}
  permutations, \code{shuffleSet} will generate and return \strong{all
    possible permutations}, which may differ from the number requested via
  argument \code{nset}.

  The \code{check} argument to \code{shuffleSet} controls whether
  checking is performed in the permutation design. If you set
  \code{check = FALSE} then exactly \code{nset} permutations will be
  returned. However, do be aware that there is no guarantee that the set
  of permutations returned will be unique, especially so for designs and
  data sets where there are few possible permutations relative to the
  number requested.

  The \code{as.matrix} method sets the \code{control} and \code{seed}
  attributes to \code{NULL} and removes the \code{"permutationMatrix"}
  class, resulting in a standard matrix object.
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
  \code{shuffleSet()} is modelled after the permutation schemes of Canoco
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
\dontshow{suppressWarnings(RNGversion("3.5.0"))}
set.seed(1)
## simple random permutations, 5 permutations in set
shuffleSet(n = 10, nset = 5)

## series random permutations, 5 permutations in set
shuffleSet(10, 5, how(within = Within(type = "series")))

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
numPerms(20, control = CTRL)
shuffleSet(20, 10, control = CTRL)
## as above but same random permutation within Plot-level strata
CTRL <- how(plots = plotStrata,
            within = Within(type = "free", constant = TRUE))
numPerms(20, control = CTRL)
shuffleSet(20, 10, CTRL) ## check this.

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
