#' Utility functions for unrestricted and restricted permutations
#'
#' Unrestricted and restricted permutations for time series, line transects,
#' spatial grids, and blocking factors.
#'
#' These are developer-level functions for generating permuted indices from
#' restricted and unrestricted designs.
#'
#' `shuffleFree()` is a lightweight wrapper around the code underlying
#' [base::sample()]. It calls `base::sample.int(x, size, replace = FALSE)`
#' without additional checks. Sampling is without replacement and without
#' regard to prior probabilities. `size` can be one to draw a single index; in
#' general use it is set equal to `length(x)`.
#'
#' With `type = "partition"`, `shuffleStrata()` returns one canonical index
#' permutation for a random arrangement of the labels in `strata`; indices
#' carrying the same original label retain their relative order.
#'
#' @param x A vector of indices to permute.
#' @param size The number of indices required.
#' @param mirror Logical; should mirroring of sequences be allowed?
#' @param start Integer; the starting point for time-series permutations. If
#'   missing, a random starting point is determined.
#' @param flip Logical of length one for `shuffleSeries()` or length two for
#'   `shuffleGrid()`; force mirroring of the permutation. For `shuffleGrid()`,
#'   the first element flips rows and the second flips columns.
#' @param nrow,ncol Numeric; the number of rows and columns in the grid.
#' @param start.row,start.col Numeric; the starting row and column for the
#'   shifted grid permutation. If not supplied, they are selected randomly.
#' @param strata A factor containing the blocks to permute.
#' @param type Character; the permutation type used to shuffle `strata`. One
#'   of `"free"`, `"grid"`, `"series"`, or `"partition"`.
#'
#' @returns An integer vector of permuted indices.
#' @author Gavin Simpson
#' @seealso [check()] checks a permutation design, [how()] describes a design,
#'   and [shuffle()] is the user-oriented wrapper around these functions.
#' @keywords htest design
#' @name shuffle-utils
#' @order 0
#' @examples
#' \dontshow{suppressWarnings(RNGversion("3.5.0"))}
#' set.seed(3)
#'
#' ## draw 1 value at random from the set 1:10
#' shuffleFree(1:10, 1)
#'
#' ## permute the series 1:10
#' x <- 1:10
#' shuffleSeries(x)                ## with random starting point
#' shuffleSeries(x, start = 5L)    ## known starting point
#' shuffleSeries(x, flip = TRUE)   ## random start, forced mirror
#' shuffleSeries(x, mirror = TRUE) ## random start, possibly mirror
#'
#' ## permute a grid of size 3x3
#' shuffleGrid(3, 3)                      ## random starting row/col
#' shuffleGrid(3, 3, start.row = 2,
#'             start.col = 3)             ## with known row/col
#' shuffleGrid(3, 3, flip = rep(TRUE, 2)) ## random start, forced mirror
NULL

#' @rdname shuffle-utils
#' @order 4
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
    } else if (type == "partition") {
        return(doShufflePartition(strata))
    } else {
        stop("Invalid permutation type.")
    }
    for(i in SEQ) {
        want <- which(strata == LEVS[i])
        out[want] <- sp[[perm[i]]]
    }
    out
}

#' @rdname shuffle-utils
#' @order 3
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

#' @rdname shuffle-utils
#' @order 2
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

#' @rdname shuffle-utils
#' @order 1
`shuffleFree` <- function(x, size) {
    # need to ensure x is length 1 when passed to sample.int
    # documented behaviour is to allow x = 10:20 say
    # so we need to allow this now even though I never use it like this in
    # the package code, except for a single example
    if (length(x) > 1L) {
        p <- sample.int(length(x), size, replace = FALSE)
        out <- x[p]
    } else {
        out <- sample.int(x, size, replace = FALSE)
    }
    out
}

## wrapper function when shuffling without any strata at all at any level
`shuffleNoStrata` <- function(n, control) {
    type <- control$within$type
    switch(type,
           "free" = shuffleFree(n, n),
           "series" = shuffleSeries(seq_len(n), mirror = control$within$mirror),
           "grid" = shuffleGrid(nrow = control$within$nrow,
           ncol = control$within$ncol, mirror = control$within$mirror),
           "none" = seq_len(n)
           )
}
