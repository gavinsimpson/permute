\name{check}
\alias{check}
\alias{print.check}
\alias{print.summary.check}
\alias{summary.check}
\alias{permuplot}

\title{Utility functions for permutation schemes}
\description{
  \code{check} provides checking of permutation schemes for
  validity. \code{permuplot} produces a graphical representation of the
  selected permutation design.
}
\usage{
check(object, control = how(), make.all = TRUE)

\method{summary}{check}(object, \dots)

permuplot(n, control = how(), col = par("col"),
          hcol = "red", shade = "lightgrey", xlim = NULL, ylim = NULL,
          inset = 0.1, main = NULL, sub = NULL, ann = par("ann"),
          cex = par("cex"), \dots)
}

\arguments{
  \item{object}{an R object. See Details for a
    complete description, especially for \code{numPerms}. For
    \code{\link{summary.check}} an object of class
    \code{"check"}.} 
  \item{control}{a list of control values describing properties of the
    permutation design, as returned by a call to
    \code{\link{how}}.}
  \item{make.all}{logical; should \code{permCheck} generate all
    possible permutations? Useful if want to check permutation design
    but not produce the matrix of all permutations.}
  \item{n}{the number of observations or an 'object' from which the
    number of observations can be determined via \code{getNumObs}.}
  \item{col, xlim, ylim, main, sub, ann, cex}{Graphical parameters.}
  \item{hcol}{Colour to use for highlighting observations and the border
    colour of the polygons drawn when \code{type = "strata"}.}
  \item{shade}{The polygon shading colour (passed to argument \code{col}
    of function \code{\link{polygon}}) when \code{type = "strata"}.}
  \item{inset}{Proportion of range of x and y coordinates to add to the
    plot x and y limits. Used to create a bit of extra space around the
    margin of each plot.}
  \item{\dots}{arguments to other methods. For \code{permuplot}
    graphical parameters can be passed to plotting functions, though
    note that not all parameters will be accepted gracefully at the
    moment.}
}
\details{
  \code{check} and \code{permuplot} are utility functions for working
  with the new permutation schemes available in \code{\link{shuffle}}.

  \code{check} is used to check the current permutation schemes
  against the object to which it will be applied. It calculates the
  maximum number of possible permutations for the number of observations
  in \code{object} and the permutation scheme described by
  \code{control}. The returned object contains component \code{control},
  an object of class \code{"how"} suitably modified if
  \code{check} identifies a problem.

  The main problem is requesting more permutations than possible with
  the number of observations and the permutation design. In such cases,
  \code{nperm} is reduced to equal the number of possible permutations,
  and complete enumeration of all permutations is turned on
  (\code{control$complete} is set to \code{TRUE}). 

  Alternatively, if the number of possible permutations is low, and less
  than \code{control$minperm}, it is better to enumerate all possible
  permutations, and as such complete enumeration of all permutations is
  turned  on (\code{control$complete} is set to \code{TRUE}).

  \code{permuplot} is a graphical utility function, which produces a
  graphical representation of a permutation design. It takes the number
  of observations and an object returned by \code{\link{how}} as
  arguments and produces a plot on the currently active device. If
  strata are present in the design, the plotting region is split into
  sufficient plotting regions (one for each stratum), and the design in
  each stratum plotted.

  Free permutation designs are represented by plotting the observation
  number at random x and y coordinates. Series designs (time series or
  line transects) are represented by plotting the observation numbers
  comprising the series in a circle and the start of the permuted series
  is highlighted using colour \code{hcol}. Grid designs are drawn on a
  regular grid and the top left observation in the original grid is
  highlighted using colour \code{hcol}. Note the ordering used is R's
  standard ordering for matrices - columns are filled first.
}
\value{
  For \code{check} a list containing the maximum number of
  permutations possible and an object of class \code{"\link{how}"}.
}
\author{Gavin L. Simpson}
\seealso{\code{\link{shuffle}} and \code{\link{how}}.}\examples{

## use example data from ?pyrifos in package vegan
require(vegan)
example(pyrifos)

## Demonstrate the maximum number of permutations for the pyrifos data
## under a series of permutation schemes

## no restrictions - lots of perms
CONTROL <- how(within = Within(type = "free"))
(check1 <- check(pyrifos, CONTROL))
##summary(check1)

## no strata but data are series with no mirroring, so 132 permutations
CONTROL <- how(within = Within(type = "series", mirror = FALSE))
check(pyrifos, CONTROL)

## no strata but data are series with mirroring, so 264 permutations
CONTROL <- how(within = Within(type = "series", mirror = TRUE))
check(pyrifos, control = CONTROL)

## unrestricted within strata
check(pyrifos, control = how(plots = Plots(strata = ditch),
                             within = Within(type = "free")))

## time series within strata, no mirroring
check(pyrifos,
      control = how(plots = Plots(strata = ditch),
                    within = Within(type = "series", mirror = FALSE)))

## time series within strata, with mirroring
check(pyrifos,
      control = how(plots = Plots(strata = ditch),
                    within = Within(type = "series", mirror = TRUE)))

## time series within strata, no mirroring, same permutation
## within strata
check(pyrifos,
      control = how(plots = Plots(strata = ditch),
                    within = Within(type = "series", constant = TRUE)))

## time series within strata, with mirroring, same permutation
## within strata
check(pyrifos,
      control = how(plots = Plots(strata = ditch),
                    within = Within(type = "series", mirror = TRUE,
                                    constant = TRUE)))
## permute strata
check(pyrifos, how(plots = Plots(strata = ditch, type = "free"),
                   within = Within(type = "none")))

## this should also also for arbitrary vectors
vec1 <- check(1:100)
vec2 <- check(1:100, how())
all.equal(vec1, vec2)
vec3 <- check(1:100, how(within = Within(type = "series")))
all.equal(100, vec3$n)
vec4 <- check(1:100, how(within = Within(type= "series", mirror = TRUE)))
all.equal(vec4$n, 200)

## enumerate all possible permutations
fac <- gl(2,6)
ctrl <- how(plots = Plots(strata = fac),
            within = Within(type = "grid", mirror = FALSE,
                            constant = TRUE, nrow = 3, ncol = 2))
check(1:12, ctrl)

numPerms(1:12, control = ctrl)
(tmp <- allPerms(12, control = ctrl, observed = TRUE))
(tmp2 <- allPerms(12, control = ctrl))

## turn on mirroring %%FIXME needs a proper method to do this
ctrl$within$mirror <- TRUE
numPerms(1:12, control = ctrl)
(tmp3 <- allPerms(12, control = ctrl, observed = TRUE))
(tmp4 <- allPerms(12, control = ctrl))
## prints out details of the permutation scheme as
## well as the matrix of permutations
summary(tmp) %% FIXME these don't print the scheme
summary(tmp2)

## different numbers of observations per level of strata
fac <- factor(rep(1:3, times = c(3,2,2)))
## free permutations in levels of strata
numPerms(7, how(within = Within(type = "free"),
                plots = Plots(strata = fac, type = "none")))
allPerms(7, how(within = Within(type = "free"),
                plots = Plots(strata = fac)))
## series permutations in levels of strata
ctrl <- how(within = Within(type = "series"), plots = Plots(strata = fac))
numPerms(7, control = ctrl)
allPerms(7, control = ctrl)

%% FIXME - need all these updating to new API in how...
%% moved permuplot examples out of package until I can fix it
%% properly

}
\keyword{ utilities }
\keyword{ design }
\keyword{ methods }
