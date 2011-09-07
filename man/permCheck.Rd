\name{check}
\alias{check}
\alias{permCheck} % for the deprecated function
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
check(object, control = permControl(), make.all = TRUE)

\method{summary}{check}(object, \dots)

permuplot(n, control = permControl(), col = par("col"),
          hcol = "red", shade = "lightgrey", xlim = NULL, ylim = NULL,
          inset = 0.1, main = NULL, sub = NULL, ann = par("ann"),
          cex = par("cex"), \dots)
}

\arguments{
  \item{object}{an R object. See Details for a
    complete description, especially for \code{numPerms}. For
    \code{\link{summary.check}} an object of class
    \code{"check"}. For \code{\link{summary.allPerms}} an object of
    class \code{"allPerms"}.} 
  \item{control}{a list of control values describing properties of the
    permutation design, as returned by a call to
    \code{\link{permControl}}.}
  \item{make.all}{logical; should \code{check} generate all
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
  an object of class \code{"permControl"} suitably modified if
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

  % Function \code{getNumObs} is a simple generic function to return the
  % number of observations in a range of R objects. The default method
  % will work for any object for which a \code{\link[vegan]{scores}}
  % method exists. This includes matrices and data frames, as well as
  % specific methods for numeric or integer vectors.

  \code{permuplot} is a graphical utility function, which produces a
  graphical representation of a permutation design. It takes the number
  of observations and an object returned by \code{\link{permControl}} as
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
  permutations possible and an object of class
  \code{"\link{permControl}"}.

  For \code{permuplot}, a plot on the currently active device.
}
%\references{
%}
\author{Gavin Simpson}
\seealso{\code{\link{shuffle}} and \code{\link{permControl}}.}

\examples{
%\dontrun{

## Only for vegan >= 2.0-0
vers <- packageDescription("vegan")$Version

if(vers >= "2.0-0") {
## use example data from ?pyrifos in package vegan
require(vegan)
example(pyrifos)

## Demonstrate the maximum number of permutations for the pyrifos data
## under a series of permutation schemes

## no restrictions - lots of perms
CONTROL <- permControl(within = Within(type = "free"))
(check1 <- check(pyrifos, CONTROL))
##summary(check1)

## no strata but data are series with no mirroring, so 132 permutations
CONTROL <- permControl(within = Within(type = "series",
                                       mirror = FALSE))
check(pyrifos, CONTROL)

## no strata but data are series with mirroring, so 264 permutations
CONTROL <- permControl(within = Within(type = "series",
                                       mirror = TRUE))
check(pyrifos, control = CONTROL)

## unrestricted within strata
check(pyrifos, control = permControl(strata = ditch,
                   within = Within(type = "free")))

## time series within strata, no mirroring
check(pyrifos, control = permControl(strata = ditch,
                   within = Within(type = "series",
                                                  mirror = FALSE)))

## time series within strata, with mirroring
check(pyrifos, control = permControl(strata = ditch,
                   within = Within(type = "series",
                                                  mirror = TRUE)))

## time series within strata, no mirroring, same permutation
## within strata
check(pyrifos, control = permControl(strata = ditch,
                   within = Within(type = "series",
                                   constant = TRUE)))

## time series within strata, with mirroring, same permutation
## within strata
check(pyrifos, control = permControl(strata = ditch,
                   within = Within(type = "series",
                                                  mirror = TRUE,
                                                  constant = TRUE)))

## permute strata
check(pyrifos, permControl(strata = ditch,
                               within = Within(type = "none"),
                               blocks = Blocks(type = "free")))
%}

## this should also also for arbitrary vectors
vec1 <- check(1:100)
vec2 <- check(1:100, permControl())
all.equal(vec1, vec2)
vec3 <- check(1:100, permControl(within = Within(type = "series")))
all.equal(100, vec3$n)
vec4 <- check(1:100, permControl(within =
                                     Within(type= "series",
                                                mirror = TRUE)))
all.equal(vec4$n, 200)

## enumerate all possible permutations
fac <- gl(2,6)
ctrl <- permControl(strata = fac,
                    within = Within(type = "grid", mirror = FALSE,
                                    constant = TRUE, nrow = 3,
                                    ncol = 2))
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
##summary(tmp) %% FIXME these don't print the scheme
##summary(tmp2)
}

%% FIXME - need all these updating to new API in permControl...
%% Fixed one, above, but there may still be problems with some of
%% the code below:
\dontrun{
## different numbers of observations per level of strata
fac <- factor(rep(1:3, times = c(3,2,2)))
## free permutations in levels of strata
numPerms(7, permControl(type = "free", strata = fac))
allPerms(7, permControl(type = "free", strata = fac))
## series permutations in levels of strata
numPerms(7, permControl(type = "series", strata = fac))
allPerms(7, permControl(type = "series", strata = fac))

## allPerms can work with a vector
vec <- c(3,4,5)
allPerms(vec)

## Tests for permuplot
n <- 25
## standard permutation designs
permuplot(n, permControl(type = "free"))
permuplot(n, permControl(type = "series"))
permuplot(n, permControl(type = "grid", nrow = 5, ncol = 5))

## restricted perms with mirroring
permuplot(n, permControl(type = "series", mirror = TRUE))
permuplot(n, permControl(type = "grid", nrow = 5, ncol = 5,
                             mirror = TRUE))

## perms within strata
fac <- gl(6, 20)
control <- permControl(type = "free", strata = fac)
permuplot(120, control = control, cex = 0.8)
control <- permControl(type = "series", strata = fac)
permuplot(120, control = control, cex = 0.8)
fac <- gl(6, 25)
control <- permControl(type = "grid", strata = fac,
                       nrow = 5, ncol = 5)
permuplot(150, control = control, cex = 0.8)

## perms within strata with mirroring
fac <- gl(6, 20)
control <- permControl(type = "series", strata = fac,
                       mirror = TRUE)
permuplot(120, control = control, cex = 0.8)
fac <- gl(6, 25)
control <- permControl(type = "grid", strata = fac,
                       nrow = 5, ncol = 5, mirror = TRUE)
permuplot(150, control = control, cex = 0.8)

## same perms within strata
fac <- gl(6, 20)
control <- permControl(type = "free", strata = fac,
                       constant = TRUE)
permuplot(120, control = control, cex = 0.8)
control <- permControl(type = "series", strata = fac,
                       constant = TRUE)
permuplot(120, control = control, cex = 0.8)
fac <- gl(6, 25)
control <- permControl(type = "grid", strata = fac,
                       nrow = 5, ncol = 5, constant = TRUE)
permuplot(150, control = control, cex = 0.8)

## same perms within strata with mirroring
fac <- gl(6, 20)
control <- permControl(type = "series", strata = fac,
                       mirror = TRUE, constant = TRUE)
permuplot(120, control = control, cex = 0.8)
fac <- gl(6, 25)
control <- permControl(type = "grid", strata = fac,
                       nrow = 5, ncol = 5, mirror = TRUE,
                       constant = TRUE)
permuplot(150, control = control, cex = 0.8)
}
}
\keyword{ utilities }
\keyword{ design }
\keyword{ methods }
