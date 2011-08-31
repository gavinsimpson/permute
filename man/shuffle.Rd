\name{shuffle}
\alias{shuffle}
\alias{permControl}
\alias{Blocks}
\alias{Within}
\alias{print.permControl}
\alias{permute}
\alias{getBlocks}
\alias{getBlocks.default}
\alias{getBlocks.permControl}
\alias{getWithin}
\alias{getWithin.default}
\alias{getWithin.permControl}
\alias{getStrata}
\alias{getStrata.default}
\alias{getStrata.permControl}

\title{Unrestricted and restricted permutations}
\description{
  Unrestricted and restricted permutation designs for time series,
  line transects, spatial grids and blocking factors.
}
\usage{
shuffle(n, control = permControl())

permControl(strata = NULL, nperm = 199, complete = FALSE,
            within = Within(),
            blocks = Blocks(),
            maxperm = 9999, minperm = 99,
            all.perms = NULL, observed = FALSE)

Within(type = c("free","series","grid","none"),
       constant = FALSE, mirror = FALSE,
       ncol = NULL, nrow = NULL)

Blocks(type = c("free","series","grid","none"),
       mirror = FALSE, ncol = NULL, nrow = NULL)

permute(i, n, control)

getWithin(object, ...)

\method{getWithin}{permControl}(object, ...)

getBlocks(object, ...)

\method{getBlocks}{permControl}(object, ...)

getStrata(object, ...)

\method{getStrata}{permControl}(object, ...)
}

\arguments{
  \item{n}{numeric; the length of the returned vector of permuted
    values. Usually the number of observations under consideration.}
  \item{control}{a list of control values describing properties of the
    permutation design, as returned by a call to \code{permControl}.}
  \item{strata}{An integer vector or factor specifying the strata for
    permutation. If supplied, observations are permuted only within the
    specified strata.}
  \item{nperm}{the number of permutations.}
  \item{complete}{logical; should complete enumeration of all
    permutations be performed?}
  \item{within, blocks}{Permutation designs for samples within the
    levels of \code{strata} (\code{within}) or for the blocks (strata)
    themselves.}
  \item{type}{the type of permutations required. One of \code{"free"},
    \code{"series"}, \code{"grid"} or \code{"none"}. See Details.}
  \item{maxperm}{the maximum number of permutations to
    perform. Currently unused.}
  \item{minperm}{the lower limit to the number of possible permutations
    at which complete enumeration is performed. See argument
    \code{complete} and Details, below.}
  \item{all.perms}{an object of class \code{allPerms}, the result of a
    call to \code{\link{allPerms}}.}
  \item{observed}{logical; should the observed permutation be returned
    as part of the set of all permutations?}
  \item{constant}{logical; should the same permutation be used within
    each level of strata? If \code{FALSE} a separate, possibly restricted,
    permutation is produced for each level of \code{strata}.}
  \item{mirror}{logical; should mirroring of sequences be allowed?}
  \item{ncol, nrow}{numeric; the number of columns and rows of samples
    in the spatial grid respectively.}
  \item{i}{integer; row of \code{control$all.perms} to return.}
  \item{object}{An R object to dispatch on.}
  \item{\dots}{Arguments passed on to other methods.}
}
\details{
  \code{shuffle} can generate permutations for a wide range of
  restricted permutation schemes. A small selection of the available
  combinations of options is provided in the Examples section below.

  Argument \code{mirror} determines whether grid or series permutations
  can be mirrored. Consider the sequence 1,2,3,4. The relationship
  between consecutive observations is preserved if we reverse the
  sequence to 4,3,2,1. If there is no inherent direction in your
  experimental design, mirrored permutations can be considered
  part of the Null model, and as such increase the number of possible
  permutations. The default is to not use mirroring so you must
  explicitly turn this on using \code{mirror = TRUE} in
  \code{permControl}.

  To permute \code{strata} rather than the observations within the
  levels of \code{strata}, use \code{permute.strata = TRUE}. However, note
  that the number of observations within each level of strata
  \strong{must} be equal! 

  For some experiments, such as BACI designs, one might wish to use the
  same permutation within each level of strata. This is controlled by
  argument \code{constant}. If \code{constant = TRUE} then the same
  permutation will be generated for each level of \code{strata}. The
  default is \code{constant = FALSE}.

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
  
  For \code{permControl} a list with components for each of the possible
  arguments.
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
\seealso{\code{\link{check}}, a utility function for checking
  permutation scheme described by \code{\link{permControl}}.}

\examples{
set.seed(1234)

## unrestricted permutations
shuffle(20)

## observations represent a time series of line transect
CTRL <- permControl(within = Within(type = "series"))
shuffle(20, control = CTRL)

## observations represent a time series of line transect
## but with mirroring allowed
CTRL <- permControl(within = Within(type = "series",
                                    mirror = TRUE))
shuffle(20, control = CTRL)

## observations represent a spatial grid, 5rx4c
nr <- 5
nc <- 4
CTRL <- permControl(within = Within(type = "grid", ncol = nc,
                                     nrow = nr))
perms <- shuffle(20, control = CTRL)
## view the permutation as a grid
matrix(matrix(1:20, nrow = nr, ncol = nc)[perms],
       ncol = nc, nrow = nr)

## random permutations in presence of strata
block <- gl(4, 5)
CTRL <- permControl(strata = block,
                    within = Within(type = "free"))
shuffle(20, CTRL)
## as above but same random permutation within strata
CTRL <- permControl(strata = block,
                    within = Within(type = "free", constant = TRUE))
shuffle(20, CTRL)

## time series within each level of block
CTRL <- permControl(strata = block,
                    within = Within(type = "series"))
shuffle(20, CTRL)
## as above, but  with same permutation for each level
CTRL <- permControl(strata = block,
                    within = Within(type = "series",
                                    constant = TRUE))
shuffle(20, CTRL)

## spatial grids within each level of block, 4 x (5r x 5c)
nr <- 5
nc <- 5
nb <- 4 ## number of blocks
block <- gl(nb, 25)
CTRL <- permControl(strata = block,
                    within = Within(type = "grid",
                                    ncol = nc,
                                    nrow = nr))
shuffle(100, CTRL)
## as above, but with same permutation for each level
CTRL <- permControl(strata = block,
                    within = Within(type = "grid",
                                    ncol = nc,
                                    nrow = nr,
                                    constant = TRUE))
shuffle(100, CTRL)

## permuting levels of block instead of observations
block <- gl(4, 5)
CTRL <- permControl(strata = block,
                    blocks = Blocks(type = "free"),
                    within = Within(type = "none"))
shuffle(20, CTRL)
## permuting levels of block instead of observations
## but blocks represent a time series
CTRL <- permControl(strata = block,
                    blocks = Blocks(type = "series"),
                    within = Within(type = "none"))
shuffle(20, CTRL)

## permuting levels of block but blocks represent a time series
## free permutation within blocks
CTRL <- permControl(strata = block,
                    blocks = Blocks(type = "series"),
                    within = Within(type = "free"))
shuffle(20, CTRL)

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
    control <- check(x, control)$control
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
control <- permControl(nperm = 999,
                       within = Within(type = "free"))
## perform permutation t test
perm.val <- pt.test(dat, grp, control)
perm.val

## compare perm.val with the p-value from t.test()
t.test(dat ~ grp, var.equal = TRUE)
}
\keyword{ htest }
\keyword{ design }
