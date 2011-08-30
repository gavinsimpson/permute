`shuffleSet` <- function(n, nset = 1, control = permControl()) {
    Set <- matrix(nrow = nset, ncol = n)
    WI <- getWithin(control)
    strata <- getStrata(control)
    if(is.null(strata)) {
        ## If no strata then permute all samples using stated scheme
        Args <- switch(WI$type,
                       "free" = list(x = n, size = n),
                       "series" = list(x = seq_len(n), mirror = WI$mirror),
                       "grid" = list(nrow = WI$nrow, ncol = WI$ncol,
                       mirror = WI$mirror))
        FUN <- switch(WI$type,
                      "free" = shuffleFree,
                      "series" = shuffleSeries,
                      "grid" = shuffleGrid)
        if(WI$type == "none") {
            Set[] <- rep(seq_len(n), each = nset)
        } else {
            for(i in seq_len(nset)) {
                Set[i,] <- do.call(FUN, Args)
            }
        }
    } else {
        ## If strata present, either permute samples, strata or both
        BL <- getBlocks(control)

        ## permute strata?
        if(BL$type == "none") {
            Set[] <- rep(seq_len(n), each = nset)
        } else {
            for(i in seq_len(nset)) {
                Set[i,] <- do.call(shuffleStrata,
                                   list(strata = strata, type = BL$type,
                                        mirror = BL$mirror, flip = NULL,
                                        nrow = BL$nrow, ncol = BL$ncol))
            }
        }

        tmp <- Set
        ## permute the samples within strata?
        if(WI$type != "none") {
            for(i in seq_len(nset)) {
                tab <- table(strata[Set[i,]])
                ## the levels of the strata
                inds <- names(tab)
                ## same permutation within each level of strata?
                if(WI$constant) {
                    if(WI$type == "free") {
                        n <- unique(tab)[1L]
                        same.rand <- shuffleFree(n, n)
                    } else if(WI$type == "series") {
                        start <- shuffleFree(n / length(inds), 1L)
                        flip <- runif(1L) < 0.5
                    } else if(WI$type == "grid") {
                        start.row <- shuffleFree(WI$nrow, 1L)
                        start.col <- shuffleFree(WI$ncol, 1L)
                        flip <- runif(2L) < 0.5
                    }
                } else {
                    start <- start.row <- start.col <- flip <- NULL
                }
                ## for each level of strata, permute
                for(is in inds) {
                    ## must re-order strata here on basis of Ser as they
                    ## may have been permuted above
                    MATCH <- strata[Set[i,]] == is
                    gr <- Set[i,][MATCH]
                    if ((n.gr <- length(gr)) > 1) {
                        if(WI$constant && WI$type == "free") {
                            tmp[i,][which(MATCH)] <- gr[same.rand]
                        } else {
                            Args <-
                                switch(WI$type,
                                       "free" = list(x = n.gr, size = n.gr),
                                       "series" = list(x = seq_len(n.gr),
                                       mirror = WI$mirror,
                                       start = start,
                                       flip = flip),
                                       "grid" = list(nrow = WI$nrow,
                                       ncol = WI$ncol,
                                       mirror = WI$mirror,
                                       start.row = start.row,
                                       start.col = start.col,
                                       flip = flip))
                            FUN <-
                                switch(WI$type,
                                       "free" = shuffleFree,
                                       "series" = shuffleSeries,
                                       "grid" = shuffleGrid)
                            tmp[i,][which(MATCH)] <- gr[do.call(FUN, Args)]
                        }
                    }
                }
            }
            Set <- tmp
        }
    }
    Set
}
