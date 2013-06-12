\name{permControl-deprecated}
\alias{permControl}
\alias{print.permControl}
\alias{print.permControl-deprecated}

\title{How to define a permutation design?}

\description{
  Utility functions to describe unrestricted and restricted permutation
  designs for time series, line transects, spatial grids and blocking
  factors.
}

\usage{
permControl(within = Within(), plots = Plots(), blocks = NULL,
            nperm = 199, complete = FALSE, maxperm = 9999,
            minperm = 99, all.perms = NULL, observed = FALSE)
}

\arguments{
  \item{within, plots, blocks}{Permutation designs for samples within the
    levels of \code{plots} (\code{within}), permutation of \code{plots}
    themselves, or for the definition of blocking structures which
    further restrict permutations (\code{blocks}). \code{within} and
    \code{plots} each require a named list as produced by \code{Within}
    and \code{Plots} respectively. \code{blocks} takes a factor, the
    levels of which define the blocking structure.}
  \item{nperm}{the number of permutations.}
  \item{complete}{logical; should complete enumeration of all
    permutations be performed?}
  \item{maxperm}{the maximum number of permutations to
    perform. Currently unused.}
  \item{minperm}{the lower limit to the number of possible permutations
    at which complete enumeration is performed. See argument
    \code{complete} and Details, below.}
  \item{all.perms}{an object of class \code{allPerms}, the result of a
    call to \code{\link{allPerms}}.}
  \item{observed}{logical; should the observed permutation be returned
    as part of the set of all permutations?}
}
\details{
  Argument \code{mirror} determines whether grid or series permutations
  can be mirrored. Consider the sequence 1,2,3,4. The relationship
  between consecutive observations is preserved if we reverse the
  sequence to 4,3,2,1. If there is no inherent direction in your
  experimental design, mirrored permutations can be considered
  part of the Null model, and as such increase the number of possible
  permutations. The default is to not use mirroring so you must
  explicitly turn this on using \code{mirror = TRUE} in
  \code{permControl}.

  To permute plots rather than the observations within plots (the
  levels of \code{strata}), use \code{Within(type = "none")} and
  \code{Plots(type = foo)}, where \code{foo} is how you want the plots
  to be permuted. However, note that the number of observations within
  each plot \strong{must} be equal! 

  For some experiments, such as BACI designs, one might wish to use the
  same permutation within each plot. This is controlled by
  argument \code{constant}. If \code{constant = TRUE} then the same
  permutation will be generated for each level of \code{strata}. The
  default is \code{constant = FALSE}.
}
\value{  
  For \code{permControl} a list with components for each
  of the possible arguments.
}
\references{
  \code{shuffle()} is modelled after the permutation schemes of Canoco
  3.1 (ter Braak, 1990); see also Besag & Clifford (1989).

  Besag, J. and Clifford, P. (1989) Generalized Monte Carlo significance
  tests. \emph{Biometrika} \strong{76}; 633--642.

  ter Braak, C. J. F. (1990). \emph{Update notes: CANOCO version
    3.1}. Wageningen: Agricultural Mathematics Group. (UR).
}
\author{Gavin L. Simpson}
\seealso{\code{\link{shuffle}} for permuting from a design,
  \code{\link{check}}, a utility function for checking permutation
  schemedesign described by \code{\link{how}}.}
\keyword{ utils  }