`shuffle` <-
    function (n, control = permControl())
{
    `pStrata` <- function(strata, type, mirror = FALSE, start = NULL,
                          flip = NULL, nrow, ncol, start.row = NULL,
                          start.col = NULL) {
            lev <- length(levels(strata))
            ngr <- length(strata) / lev
            sp <- split(seq(along = strata), strata)
            if(type == "free") {
                unname(do.call(c, sp[pFree(lev, lev)]))
            } else if(type == "series") {
                unname(do.call(c,
                               sp[pSeries(seq_len(lev),
                                          mirror = mirror,
                                          start = start,
                                          flip = flip)]))
            } else if(type == "grid") {
                unname(do.call(c,
                               sp[pGrid(nrow = nrow, ncol = ncol,
                                        mirror = mirror,
                                        start.row = start.row,
                                        start.col = start.col,
                                        flip = flip)]))
            } else {
                stop("Invalid permutation type.")
            }
        }
    `pGrid` <- function(nrow, ncol, mirror = FALSE, start.row = NULL,
                        start.col = NULL, flip = NULL) {
            if(is.null(start.row))
                start.row <- pFree(nrow, 1L)
            if(is.null(start.col))
                start.col <- pFree(ncol, 1L)
            ir <- seq(start.row, length=nrow) %% nrow
            ic <- seq(start.col, length=ncol) %% ncol
            if(!is.null(flip)) {
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
    `pSeries` <- function(inds, mirror = FALSE, start = NULL,
                          flip = NULL) {
        n <- length(inds)
        if(is.null(start))
            start <- pFree(n, 1L)
        out <- seq(start, length = n) %% n + 1L
        if(!is.null(flip)) {
            if(flip)
                out <- rev(out)
        } else {
            if(mirror && runif(1L) < 0.5)
                out <- rev(out)
        }
        inds[out]
    }
    `pFree` <- function(x, size)
        .Internal(sample(x, size, FALSE, NULL))
    ## END in-line Functions ##

    ## If no strata then permute all samples using stated scheme
    if(is.null(control$strata)) {
        out <-
            switch(control$within$type,
                   "free" = pFree(n, n),
                   "series" =
                   pSeries(1:n, mirror = control$within$mirror),
                   "grid" =
                   pGrid(nrow = control$within$nrow,
                         ncol = control$within$ncol,
                         mirror = control$within$mirror)
                   )
    } else {
        ## If strata present, either permute samples, strata or both

        ## permute strata?
        if(control$blocks$type == "none") {
            out <- 1:n
        } else {
            flip <- runif(1L) < 0.5
            out <- pStrata(control$strata,
                           type = control$blocks$type,
                           mirror = control$blocks$mirror,
                           flip = flip,
                           nrow = control$blocks$nrow,
                           ncol = control$blocks$ncol)
        }
        ## permute the samples within strata?
        if(control$within$type != "none") {
            tab <- table(control$strata[out])
            ## the levels of the strata
            inds <- names(tab)
            ## same permutation within each level of strata?
            if(control$within$constant) {
                if(control$within$type == "free") {
                    n <- unique(tab)[1L]
                    same.rand <- pFree(n, n)
                } else if(control$within$type == "series") {
                    start <- pFree(n / length(inds), 1L)
                    flip <- runif(1L) < 0.5
                } else if(control$within$type == "grid") {
                    start.row <- pFree(control$within$nrow, 1L)
                    start.col <- pFree(control$within$ncol, 1L)
                    flip <- runif(2L) < 0.5
                }
            } else {
                start <- start.row <- start.col <- flip <- NULL
            }
            tmp <- out
            ## for each level of strata, permute
            for (is in inds) {
                ## must re-order strata here on basis of out as they
                ## may have been permuted above
                MATCH <- control$strata[out] == is
                gr <- out[MATCH]
                if ((n.gr <- length(gr)) > 1) {
                    tmp[which(MATCH)] <-
                        switch(control$within$type,
                               "free" =
                               if(control$within$constant) {
                                   gr[same.rand]
                               } else {
                                   out[gr][pFree(n.gr, n.gr)]
                               },
                               "series" =
                               pSeries(gr,
                                       mirror = control$within$mirror,
                                       start = start, flip = flip),
                               "grid" =
                               gr[pGrid(nrow = control$within$nrow,
                                        ncol = control$within$ncol,
                                        mirror = control$within$mirror,
                                        start.row = start.row,
                                        start.col = start.col,
                                        flip = flip)]
                               )
                }
            }
            out <- tmp
        }
    }
    out
}
