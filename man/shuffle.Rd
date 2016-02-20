\name{shuffle}
\alias{shuffle}
\alias{permute}

\title{Unrestricted and restricted permutations}
\description{
  Unrestricted and restricted permutation designs for time series,
  line transects, spatial grids and blocking factors.
}
\usage{
shuffle(n, control = how())

permute(i, n, control)
}

\arguments{
  \item{n}{numeric; the length of the returned vector of permuted
    values. Usually the number of observations under consideration. May
    also be any object that \code{nobs} knows about; see
    \code{\link{nobs-methods}}.}
  \item{control}{a list of control values describing properties of the
    permutation design, as returned by a call to \code{how}.}
  \item{i}{integer; row of \code{control$all.perms} to return.}
}
\details{
  \code{shuffle} can generate permutations for a wide range of
  restricted permutation schemes. A small selection of the available
  combinations of options is provided in the Examples section below.

  \code{permute} is a higher level utility function for use in a loop
  within a function implementing a permutation test. The main purpose of
  \code{permute} is to return the correct permutation in each iteration
  of the loop, either a random permutation from the current design or
  the next permutation from \code{control$all.perms} if it is not
  \code{NULL} and \code{control$complete} is \code{TRUE}.
}
\value{
  For \code{shuffle} a vector of length \code{n} containing a
  permutation of the observations 1, \ldots, n using the permutation
  scheme described by argument \code{control}.
  
  For \code{permute} the \code{i}th permutation from the set of all
  permutations, or a random permutation from the design.
}
%\note{
%  \code{shuffle} is currently used in one Vegan function;
%  \code{\link[vegan]{permutest.betadisper}}. Over time, the other functions
%  that currently use the older \code{\link[vegan]{shuffle}} will be
%  updated to use \code{shuffle}.
%}
\references{
  \code{shuffle()} is modelled after the permutation schemes of Canoco
  3.1 (ter Braak, 1990); see also Besag & Clifford (1989).

  Besag, J. and Clifford, P. (1989) Generalized Monte Carlo significance
  tests. \emph{Biometrika} \strong{76}; 633--642.

  ter Braak, C. J. F. (1990). \emph{Update notes: CANOCO version
    3.1}. Wageningen: Agricultural Mathematics Group. (UR).
}
\author{Gavin Simpson}
\seealso{\code{\link{check}}, a utility function for checking
  permutation scheme described by \code{\link{how}}.}

\examples{
set.seed(1234)

## unrestricted permutations
shuffle(20)

## observations represent a time series of line transect
CTRL <- how(within = Within(type = "series"))
shuffle(20, control = CTRL)

## observations represent a time series of line transect
## but with mirroring allowed
CTRL <- how(within = Within(type = "series", mirror = TRUE))
shuffle(20, control = CTRL)

## observations represent a spatial grid, 5rx4c
nr <- 5
nc <- 4
CTRL <- how(within = Within(type = "grid", ncol = nc, nrow = nr))
perms <- shuffle(20, control = CTRL)
## view the permutation as a grid
matrix(matrix(1:20, nrow = nr, ncol = nc)[perms],
       ncol = nc, nrow = nr)

## random permutations in presence of strata
plots <- Plots(strata = gl(4, 5))
CTRL <- how(plots = plots, within = Within(type = "free"))
shuffle(20, CTRL)
## as above but same random permutation within strata
CTRL <- how(plots = plots, within = Within(type = "free",
            constant = TRUE))
shuffle(20, CTRL)

## time series within each level of block
CTRL <- how(plots = plots, within = Within(type = "series"))
shuffle(20, CTRL)
## as above, but  with same permutation for each level
CTRL <- how(plots = plots, within = Within(type = "series",
            constant = TRUE))
shuffle(20, CTRL)

## spatial grids within each level of block, 4 x (5r x 5c)
nr <- 5
nc <- 5
nb <- 4 ## number of blocks
plots <- Plots(gl(nb, 25))
CTRL <- how(plots = plots,
            within = Within(type = "grid", ncol = nc, nrow = nr))
shuffle(100, CTRL)
## as above, but with same permutation for each level
CTRL <- how(plots = plots,
            within = Within(type = "grid", ncol = nc, nrow = nr,
                            constant = TRUE))
shuffle(100, CTRL)

## permuting levels of plots instead of observations
CTRL <- how(plots = Plots(gl(4, 5), type = "free"),
            within = Within(type = "none"))
shuffle(20, CTRL)
## permuting levels of plots instead of observations
## but plots represent a time series
CTRL <- how(plots = Plots(gl(4, 5), type = "series"),
            within = Within(type = "none"))
shuffle(20, CTRL)

## permuting levels of plots but plots represent a time series
## free permutation within plots
CTRL <- how(plots = Plots(gl(4, 5), type = "series"),
            within = Within(type = "free"))
shuffle(20, CTRL)

## permuting within blocks
grp <- gl(2, 10) # 2 groups of 10 samples each
CTRL <- how(blocks = grp)
shuffle(length(grp), control = CTRL)

## Simple function using permute() to assess significance
## of a t.test  
pt.test <- function(x, group, control) {
    ## function to calculate t
    t.statistic <- function(x, y) {
        m <- length(x)
        n <- length(y)
        ## means and variances, but for speed
        xbar <- mean(x)
        ybar <- mean(y)
        xvar <- var(x)
        yvar <- var(y)
        pooled <- sqrt(((m-1)*xvar + (n-1)*yvar) / (m+n-2))
        (xbar - ybar) / (pooled * sqrt(1/m + 1/n))
    }
    ## check the control object
    #control <- check(x, control)$control ## FIXME
    ## number of observations
    Nobs <- nobs(x)
    ## group names
    lev <- names(table(group))
    ## vector to hold results, +1 because of observed t
    t.permu <- numeric(length = control$nperm) + 1
    ## calculate observed t
    t.permu[1] <- t.statistic(x[group == lev[1]], x[group == lev[2]])
    ## generate randomisation distribution of t
    for(i in seq_along(t.permu)) {
        ## return a permutation
        want <- permute(i, Nobs, control)
        ## calculate permuted t
        t.permu[i+1] <- t.statistic(x[want][group == lev[1]],
                                    x[want][group == lev[2]])
    }
    ## pval from permutation test
    pval <- sum(abs(t.permu) >= abs(t.permu[1])) / (control$nperm + 1)
    ## return value
    return(list(t.stat = t.permu[1], pval = pval))
}

## generate some data with slightly different means
set.seed(1234)
gr1 <- rnorm(20, mean = 9)
gr2 <- rnorm(20, mean = 10)
dat <- c(gr1, gr2)
## grouping variable
grp <- gl(2, 20, labels = paste("Group", 1:2))
## create the permutation design
control <- how(nperm = 999, within = Within(type = "free"))
## perform permutation t test
perm.val <- pt.test(dat, grp, control)
perm.val

## compare perm.val with the p-value from t.test()
t.test(dat ~ grp, var.equal = TRUE)
}
\keyword{ htest }
\keyword{ design }
