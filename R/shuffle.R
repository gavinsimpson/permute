`shuffle2` <- function (n, control = how()) {
    ## capture strata data
    Pstrata <- getStrata(control, which = "plots")
    Bstrata <- getStrata(control, which = "blocks")
    ## if no strata at all permute all samples using stated scheme
    if(is.null(Pstrata) && is.null(Bstrata)) {
        out <- shuffleNoStrata(n, control)
    } else {
        ## If strata present, either permute samples, strata or both

        ## permute strata?
        if(control$blocks$type == "none") {
            out <- seq_len(n)
        } else {
            flip <- runif(1L) < 0.5 ## why are we doing this? Null better?
            out <- shuffleStrata(control$strata,
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
                    same.rand <- shuffleFree(n, n)
                } else if(control$within$type == "series") {
                    start <- shuffleFree(n / length(inds), 1L)
                    flip <- runif(1L) < 0.5
                } else if(control$within$type == "grid") {
                    start.row <- shuffleFree(control$within$nrow, 1L)
                    start.col <- shuffleFree(control$within$ncol, 1L)
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
                                   out[gr][shuffleFree(n.gr, n.gr)]
                               },
                               "series" =
                               gr[shuffleSeries(seq_len(n.gr),
                                                mirror = control$within$mirror,
                                                start = start, flip = flip)],
                               "grid" =
                               gr[shuffleGrid(nrow = control$within$nrow,
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
