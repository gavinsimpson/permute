##' Utility functions for complete enumeration of all possible permutations
##'
##' Utility functions to return the set of all permutations under different
##' designs. For most practical applications, i.e. to combine designs permuting
##' blocks and/or within blocks function \code{\link{allPerms}} will be
##' required.
##'
##' These are utility functions and aren't designed for casual use.
##' \code{\link{allPerms}} should be used instead.
##'
##' Details on usage of these functions can be found in \code{\link{allPerms}}.
##'
##' @alias allUtils
##'
##' @export
##'
##' @name all-utils
##'
##' @param n the number of observations.
##' @param v numeric; vector of indices. Default is \code{1:n}.
##' @param nperms numeric; number of possible permutations.
##' @param mirror logical; mirroring of permutations allowed?
##' @param nr,nc integer; number of rows and columns of grid designs.
##' @param constant logical; same permutation within each block?
##' @param control a list of control values describing properties of the
##' permutation design, as returned by a call to \code{\link{how}}.
##'
##' @return A matrix of all possible permutations of \code{n} observations or
##' of \code{v}, given the provided options.
##'
##' @author Gavin L. Simpson
`allFree` <- function(n, v = seq_len(n)) {
    ## Modified version of allFree() provided by Doug Bates
    ## via personal email on 19 Jan 2012
    if(n == 1L) return(array(v, c(1L, 1L)))
    do.call(rbind,
            lapply(seq_len(n),
                   function(i) cbind(v[i], allFree(n - 1L, v[-i]))))
}

##' @rdname all-utils
##'
##' @export
`allGrid` <- function(n, nperms, nr, nc, mirror, constant) {
    v <- seq_len(n)
    X <- matrix(nrow = nperms, ncol = n)
    idx <- 1
    ## ncol == 2 is special case
    if(nc == 2) {
        X <- allSeries(n, nperms = nperms, mirror = mirror)
    } else {
        for(i in seq_len(nr)) {
            for(j in seq_len(nc)) {
                ir <- seq(i, length = nr)%%nr
                ic <- seq(j, length = nc)%%nc
                ## block 1 - no reversals
                X[idx, ] <- rep(ic, each = nr) * nr +
                    rep(ir, len = nr * nc) + 1
                if(mirror) {
                    ## block 2 - rev rows but not columns
                    X[idx + n, ] <- rep(ic, each = nr) * nr +
                        rep(rev(ir), len = nr * nc) + 1
                    ## block 3 - rev columns but not rows
                    X[idx + (2*n), ] <- rep(rev(ic), each = nr) *
                        nr + rep(ir, len = nr * nc) + 1
                }
                idx <- idx + 1
            }
        }
        if(mirror) {
            ## rev columns and rows
            ## no calculations, just rev cols of block 1
            v <- seq_len(n)
            X[((3*n)+1):(4*n), ] <- X[v, rev(v)]
        }
    }
    X
}

##' @rdname all-utils
##'
##' @export
`allSeries` <- function(n, nperms, mirror = FALSE)
{
    v <- seq_len(n)
    X <- matrix(nrow = nperms, ncol = n)
    for(i in v) {
        X[i,] <- seq(i, length = n)%%n + 1
    }
    ## if mirroring, rev the cols of X[v,]
    ## but only if nperms > 2
    if(mirror && (nperms > 2))
        X[(n+1):(2*n),] <- X[v, rev(v)]
    X
}

##' @rdname all-utils
##'
##' @export
`allStrata` <- function(n, control) {
    ## seq vector of observation indices
    v <- seq_len(n)
    ## number of groups
    strata <- getStrata(control, which = "plots")
    lev <- length(levels(strata))
    ## compute nperms on number of levels - for this need Within()
    ## and type == typeP
    type <- getType(control, which = "plots")
    newControl <- how(within = Within(type = type))
    nperms <- numPerms(lev, newControl)
    ## result object
    X <- matrix(nrow = nperms, ncol = length(strata))
    ## store the type
    type <- getType(control, which = "plots")
    mirror <- getMirror(control, which = "plots")
    perms <- if(type == "free") {
        allFree(lev)
    } else if(type == "series") {
        allSeries(lev, nperms = nperms, mirror = mirror)
    } else if(type == "grid") {
        nr <- getRow(control, which = "plots")
        nc <- getCol(control, which = "plots")
        constant <- getConstant(control)
        allGrid(lev, nperms = nperms, nr = nr, nc = nc,
                mirror = mirror, constant = constant)
    } else {
        ## if in here, must have both types == "none"
        ## this is here just in case - need to check if this
        ## is possible given calling function...
        return(v)
    }
    sp <- split(v, strata)
    ## build permutations by concatenating components of sp
    ## for each row of level permutations
    for(i in seq_len(nrow(perms)))
        X[i,] <- unname(do.call(c, sp[perms[i,]]))
    X
}
