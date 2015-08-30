##' Utility functions for unrestricted and restricted permutations
##'
##' Unrestricted and restricted permutations for time series, line transects,
##' spatial grids and blocking factors.
##'
##' These are developer-level functions for generating permuted indexes from
##' one of several restricted and unrestricted designs.
##'
##' \code{shuffleFree} is a wrapper to code underlying \code{\link{sample}},
##' but without the extra over head of sanity checks. It is defined as
##' \code{sample.int(x, size, replace = FALSE)}.  You must arrange for the
##' correct values to be supplied, where \code{x} is a vector of indices to
##' sample from, and \code{size} is the number of indices to sample. Sampling
##' is done without replacement and without regard to prior probabilities.
##' Argument \code{size} is allowed so that one can draw a single observation
##' at random from the indices \code{x}. In general use, \code{size} would be
##' set equal to \code{length{x}}.
##'
##' @aliases shuffle-utils
##'
##' @name shuffle-utils
##'
##' @param x vector of indices to permute.
##' @param size number of random permutations required
##' @param mirror logical; should mirroring of sequences be allowed?
##' @param start integer; the starting point for time series permutations. If
##' missing, a random starting point is determined.
##' @param flip logical, length 1 (\code{shuffleSeries}) or length 2
##' (\code{shuffleGrid}); force mirroring of permutation. This will always
##' return the reverse of the computed permutation. For \code{shuffleGrid}, the
##' first element pertains to flipping rows, the second to flipping columns of
##' the grid.
##' @param nrow,ncol numeric; the number of rows and columns in the grid.
##' @param start.row,start.col numeric; the starting row and column for the
##' shifted grid permutation. If non supplied, a random starting row and column
##' will be selected.
##' @param strata factor; the blocks to permute.
##' @param type character; the type of permutation used to shuffle the
##' \code{strata}. One of \code{"free"}, \code{"grid"} or \code{"series"}.
##'
##' @return A integer vector of permuted indices.
##'
##' @author Gavin L. Simpson
##'
##' @seealso \code{\link{check}}, a utility function for checking permutation
##' scheme described by \code{\link{how}}. \code{\link{shuffle}} as a
##' user-oriented wrapper to these functions.
##'
##' @keywords htest design
##'
##' @export
##'
##' @examples
##'
##' set.seed(3)
##'
##' ## draw 1 value at random from the set 1:10
##' shuffleFree(1:10, 1)
##'
##' ## permute the series 1:10
##' x <- 1:10
##' shuffleSeries(x)                ## with random starting point
##' shuffleSeries(x, start = 5L)    ## known starting point
##' shuffleSeries(x, flip = TRUE)   ## random start, forced mirror
##' shuffleSeries(x, mirror = TRUE) ## random start, possibly mirror
##'
##' ## permute a grid of size 3x3
##' shuffleGrid(3, 3)                      ## random starting row/col
##' shuffleGrid(3, 3, start.row = 2,
##'             start.col = 3)             ## with known row/col
##' shuffleGrid(3, 3, flip = rep(TRUE, 2)) ## random start, forced mirror
##'
`shuffleStrata` <- function(strata, type, mirror = FALSE, start = NULL,
                            flip = NULL, nrow, ncol, start.row = NULL,
                            start.col = NULL) {
    ## drop unused levels
    strata <- droplevels(strata)
    LEVS <- levels(strata)
    lev <- nlevels(strata)
    ngr <- length(strata) / lev
    SEQ <- seq_len(lev)
    sp <- split(out <- seq_along(strata), strata)
    perm <- if(type == "free") {
        shuffleFree(lev, lev)
    } else if (type == "series") {
        shuffleSeries(SEQ, mirror = mirror, start = start,
                      flip = flip)
    } else if (type == "grid") {
        shuffleGrid(nrow = nrow, ncol = ncol, mirror = mirror,
                    start.row = start.row, start.col = start.col,
                    flip = flip)
    } else {
        stop("Invalid permutation type.")
    }
    for(i in SEQ) {
        want <- which(strata == LEVS[i])
        out[want] <- sp[[perm[i]]]
    }
    out
}

##' @rdname shuffle-utils
##'
##' @export
`shuffleGrid` <- function(nrow, ncol, mirror = FALSE, start.row = NULL,
                          start.col = NULL, flip = NULL) {
    if(is.null(start.row))
        start.row <- shuffleFree(nrow, 1L)
    if(is.null(start.col))
        start.col <- shuffleFree(ncol, 1L)
    ir <- seq(start.row, length=nrow) %% nrow
    ic <- seq(start.col, length=ncol) %% ncol
    if(!is.null(flip) && mirror) {
        if(any(flip)) {
            if(flip[1L])
                ir <- rev(ir)
            if(flip[2L])
                ic <- rev(ic)
        }
    } else {
        if (mirror) {
            if (runif(1L) < 0.5)
                ir <- rev(ir)
            if (runif(1L) < 0.5)
                ic <- rev(ic)
        }
    }
    rep(ic, each=nrow) * nrow + rep(ir, len=nrow*ncol) + 1L
}

##' @rdname shuffle-utils
##'
##' @export
`shuffleSeries` <- function(x, mirror = FALSE, start = NULL,
                            flip = NULL) {
    n <- length(x)
    if(is.null(start))
        start <- shuffleFree(n, 1L)
    out <- seq(start, length = n) %% n + 1L
    if(!is.null(flip) && mirror) {
        if(flip)
            out <- rev(out)
    } else {
        if(mirror && runif(1L) < 0.5)
            out <- rev(out)
    }
    x[out]
}

##' @rdname shuffle-utils
##'
##' @export
`shuffleFree` <- function(x, size) {
    sample.int(x, size, replace = FALSE)
}

##' @rdname shuffle-utils
##'
##' @export
`shuffleNoStrata` <- function(n, control) {
    ## wrapper function when shuffling without any strata at all at any level
    type <- control$within$type
    switch(type,
           "free" = shuffleFree(n, n),
           "series" = shuffleSeries(seq_len(n), mirror = control$within$mirror),
           "grid" = shuffleGrid(nrow = control$within$nrow,
           ncol = control$within$ncol, mirror = control$within$mirror),
           "none" = seq_len(n)
           )
}
