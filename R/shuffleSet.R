`shuffleSet` <- function(n, nset = 1, control = permControl()) {
    Set <- matrix(nrow = nset, ncol = n)
    WI <- getWithin(control)
    if(is.null(getStrata(control))) {
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
            Set <- rep(seq_len(n), each = nset)
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
            Set <- rep(seq_len(n), each = nset)
        } else {
            for(i in seq_len(nset)) {
                Set[i,] <- do.call(shuffleStrata,
                                   list(strata = control$strata, type = BL$type,
                                        mirror = BL$mirror, flip = NULL,
                                        nrow = BL$nrow, ncol = BL$ncol))
            }
        }

        ## permute the samples within strata?
        if(WI$type != "none") {
            tab <- table(getStrat(control)[out])
            ## the levels of the strata
            inds <- names(tab)
            ## same permutation within each level of strata?
            for(i in seq_len(nset)) {

            }
            .NotYetImplemented()
        }
    }
    Set
}
