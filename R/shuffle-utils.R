
`shuffleStrata` <- function(strata, type, mirror = FALSE, start = NULL,
                            flip = NULL, nrow, ncol, start.row = NULL,
                            start.col = NULL) {
    lev <- length(levels(strata))
    ngr <- length(strata) / lev
    sp <- split(seq(along = strata), strata)
    if(type == "free") {
        unname(do.call(c, sp[shuffleFree(lev, lev)]))
    } else if(type == "series") {
        unname(do.call(c,
                       sp[shuffleSeries(seq_len(lev),
                                        mirror = mirror,
                                        start = start,
                                        flip = flip)]))
    } else if(type == "grid") {
        unname(do.call(c,
                       sp[shuffleGrid(nrow = nrow, ncol = ncol,
                                      mirror = mirror,
                                      start.row = start.row,
                                      start.col = start.col,
                                      flip = flip)]))
    } else {
        stop("Invalid permutation type.")
    }
}

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

`shuffleFree` <- function(x, size) {
    .Internal(sample(x, size, FALSE, NULL))
}
