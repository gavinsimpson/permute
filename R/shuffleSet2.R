## new version of shuffleSet() that allows for blocking
`shuffleSet` <- function(n, nset, control = how()) {
    ## handle missing nset - take from control if can
    if(missing(nset)) {
        np <- getNperm(control)
        if(is.null(np)) ## something wrong, default back to 1
            nset <- 1
        else
            nset <- np
    }

    sn <- seq_len(n) ## sequence of samples in order of input

    ## need to check number of permutations won't blow up
    pcheck <- check(sn, control = control, make.all = TRUE)
    ## control possibly now updated
    control <- pcheck$control

    if(is.null(control$all.perms)) {
        ## get blocking, if any
        Block <- getStrata(control, which = "blocks")
        if(is.null(Block))
            Block <- factor(rep(1, n))

        ## split sn on basis of Block
        spln <- split(sn, Block)
        nb <- length(spln) ## number of blocks

        ## result list
        out <- vector(mode = "list", length = nb)

        ## loop over spln and shuffle in each split
        for(i in seq_len(nb)) {
            out[[i]] <- doShuffleSet(spln[[i]], nset = nset, control)
        }
        ##out <- do.call(cbind, out) ## undo the original splitting
        out <- unsplit(out, Block) ## undo the original splitting
    } else {
        ## if we have all.perms now then we must have generated it
        ## during checking or user passed it with control
        ## Use that instead of a ranodm set
        out <- control$all.perms
    }
    out
}

`doShuffleSet` <- function(ind, nset = 1, control) {
    ## collect strata at Plot level
    Pstrata <- getStrata(control, which = "plots", drop = TRUE)
    plotCTRL <- getPlots(control)
    typeP <- getType(control, which = "plots")

    ## collect the within control object
    withinCTRL <- getWithin(control)
    typeW <- getType(control, which = "within")

    n <- length(ind)
    sn <- seq_len(n)

    ## result object
    Set <- matrix(nrow = nset, ncol = n)

    ## if no strata at Plot level permute all samples using stated scheme
    if(is.null(Pstrata)) {
        ## If no strata at plot then permute all samples using stated scheme
        Args <- switch(typeW,
                       "free" = list(x = n, size = n),
                       "series" = list(x = seq_len(n),
                           mirror = withinCTRL$mirror),
                       "grid" = list(nrow = withinCTRL$nrow,
                           ncol = withinCTRL$ncol,
                           mirror = withinCTRL$mirror))
        FUN <- switch(typeW,
                      "free" = shuffleFree,
                      "series" = shuffleSeries,
                      "grid" = shuffleGrid)
        if(withinCTRL$type == "none") {
            Set[] <- rep(sn, each = nset)
        } else {
            for(i in seq_len(nset)) {
                Set[i,] <- do.call(FUN, Args)
            }
        }
    } else {
        ## If strata at Plot level present, either permute samples, Plots or both

        ## permute strata at Plot level?
        if(isTRUE(all.equal(typeP, "none"))) {
            Set[] <- rep(sn, each = nset)
        } else {
            for(i in seq_len(nset)) {
                Set[i,] <- do.call(shuffleStrata,
                                   list(strata = Pstrata,
                                        type = typeP,
                                        mirror = plotCTRL$mirror,
                                        flip = NULL, ## runif(1L) < 0.5 ??
                                        nrow = plotCTRL$nrow,
                                        ncol = plotCTRL$ncol))
            }
        }

        tmp <- Set

        ## permute the samples within Plot strata
        if(!isTRUE(all.equal(typeW, "none"))) {
            for(i in seq_len(nset)) {
                tab <- table(Pstrata[ind][Set[i,]])
                ## the levels of the Plot strata
                levs <- names(tab)

                ## same permutation within each level of the Plot strata?
                if(withinCTRL$constant) {
                    if(isTRUE(all.equal(typeW, "free"))) {
                        n <- unique(tab)[1L]
                        same.rand <- shuffleFree(n, n)
                    } else if(isTRUE(all.equal(typeW, "series"))) {
                        start <- shuffleFree(n / length(levs), 1L)
                        flip <- runif(1L) < 0.5 ## FIXME this should be moved out of the loop
                    } else if(isTRUE(all.equal(typeW, "grid"))) {
                        start.row <- shuffleFree(withinCTRL$nrow, 1L)
                        start.col <- shuffleFree(withinCTRL$ncol, 1L)
                        flip <- runif(2L) < 0.5 ## FIXME this should be moved out of the loop
                    }
                } else {
                    start <- start.row <- start.col <- flip <- NULL
                }

                ## for each level of strata, permute
                for(lv in levs) {
                    ## must re-order strata here on basis of Ser as they
                    ## may have been permuted above
                    MATCH <- Pstrata[ind][Set[i,]] == lv
                    gr <- Set[i,][MATCH]
                    if((n.gr <- length(gr)) > 1) {
                        if(withinCTRL$constant && isTRUE(all.equal(typeW, "free"))) {
                            tmp[i,][which(MATCH)] <- gr[same.rand]
                        } else {
                            Args <-
                                switch(typeW,
                                       "free" = list(x = n.gr, size = n.gr),
                                       "series" = list(x = seq_len(n.gr),
                                           mirror = withinCTRL$mirror,
                                           start = start,
                                           flip = flip),
                                       "grid" = list(nrow = withinCTRL$nrow,
                                           ncol = withinCTRL$ncol,
                                           mirror = withinCTRL$mirror,
                                           start.row = start.row,
                                           start.col = start.col,
                                           flip = flip))
                            FUN <-
                                switch(typeW,
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
    out <- Set ## have to copy or next line fails
    out[] <- ind[Set]
    out
}
