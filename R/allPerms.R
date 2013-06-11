`allPerms` <- function(n, control = permControl(), max = 9999,
                       observed = FALSE) {
    ## start
    v <- n
    ## expand n if a numeric or integer vector of length 1
    if((is.numeric(n) || is.integer(n)) && (length(n) == 1))
         v <- seq_len(n)
    ## number of observations in data
    n <- nobs(v)
    ## check permutation scheme and update control
    ## pcheck <- check(v, control = control, make.all = FALSE)
    ## ctrl <- pcheck$control

    ## get max number of permutations
    nperms <- numPerms(v, control = control)

    ## sanity check - don't let this run away to infinity
    ## esp with type = "free"
    if(nperms > max)
        stop("Number of possible permutations too large (> 'max')")

    WI <- getWithin(control)
    strataP <- getStrata(control, which = "plots")
    typeW <- getType(control, which = "within")
    typeP <- getType(control, which = "plot")
    BLOCKS <- getBlocks(control)
    dimW <- getDim(control, which = "within")
    dimP <- getDim(control, which = "plots")
    mirrorW <- getMirror(control, which = "within")
    mirrorP <- getMirror(control, which = "plots")
    constantW <- getConstant(control)

    ## give a BLOCKS if non supplied - i.e. one block
    if(is.null(BLOCKS))
        BLOCKS <- factor(rep(1, n))

    ## split v by blocks
    spl <- split(v, BLOCKS)
    nb <- length(spl) # number of blocks

    ## result object
    out <- vector(mode = "list", length = nb)

    ## loop over blocks and return allPerms on each block
    for (i in seq_along(spl)) {
        out[[i]] <-
            doAllPerms(spl[[i]], strataP, typeW, typeP, mirrorW,
                       mirrorP, constantW, dimW, dimP, control)
    }

    ## bind all the blocks together
    out <- do.call(rbind, out) ## hmm are any of these the same shape?

    if(!observed) {
        obs.v <- seq_len(n)
        obs.row <- apply(out, 1, function(x, obs.v) all(x == obs.v), obs.v)
        out <- out[!obs.row, ]
        ## reduce the number of permutations to get rid of the
        ## observed ordering
        control$nperm <- control$nperm - 1
    }
    class(out) <- "allPerms"
    attr(out, "observed") <- observed
    out
}

`doAllPerms` <- function(obs, strataP, typeW, typeP, mirrorW, mirrorP,
                         constantW, dimW, dimP, control) {
    ## replicate a matrix by going via a list and bind together
    repMat <- function(mat, n) {
        res <- rep(list(mat), n)
        do.call(rbind, res)
    }

    n <- length(obs)

    ## permuting within?
    if (typeW != "none") {
        if(is.null(strataP)) { ## no plot-level permutations
            res <- switch(typeW,
                          free = allFree(n),
                          series = allSeries(n, nperms, mirrorW),
                          grid = allGrid(n, nperms, dimW[1],
                          dimW[2], mirrorW, constantW))
        } else {
            ## permuting within plots
            tab <- table(strataP)
            pg <- unique(tab)
            if(constantW) {
                ## same permutation in each plot
                ##pg <- unique(tab)
                controlW <- permControl(within = getWithin(control))
                nperms <- numPerms(pg, controlW)
                ord <- switch(typeW,
                              free = allFree(pg),
                              series = allSeries(pg, nperms, mirrorW),
                              grid = allGrid(pg, nperms, dimW[1],
                              dimW[2], mirrorW, constantW))
                permW <- nrow(ord)
                sp <- split(obs, strataP)
                res <- matrix(nrow = nperms, ncol = n)
                for(i in seq_len(permW)) {
                    res[i,] <- sapply(sp,
                                      function(x, ord) x[ord[i,]], ord = ord)
                }
            } else {
                ## different permutations within blocks
                ng <- length(tab)
                ##pg <- unique(tab)
                if(length(pg) > 1) {
                    ## different number of observations per level of strata
                    if(typeW == "grid")
                        ## FIXME: this should not be needed once all checks are
                        ## in place in check()
                        stop("Unbalanced grid designs are not supported")
                    controlW <- permControl(within = getWithin(control))
                    sp <- split(obs, strataP)
                    res <- vector(mode = "list", length = ng)
                    add <- c(0, cumsum(tab)[1:(ng-1)])
                    for(j in seq_along(tab)) {
                        np <- numPerms(tab[j], controlW)
                        ord <- switch(typeW,
                                      free = allFree(tab[j]),
                                      series = allSeries(tab[j], np, mirrorW))
                        permW <- nrow(ord)
                        if(j == 1) {
                            a <- 1
                            b <- np / permW
                        } else {
                            b <- b/permW
                            a <- np / (b*permW)
                        }
                        res[[j]] <- matrix(rep(repMat(ord+add[j], a),
                                               each = b),
                                           ncol = tab[j])
                    }
                    res <- do.call(cbind, res)
                    sp <- split(obs, strataP)
                    res <- t(apply(res, 1,
                                   function(x, inds, o) {o[inds] <- inds[x]; o},
                                   unlist(sp), obs))
                } else {
                    ## same number of observations per level of strata
                    controlW <- permControl(within = getWithin(control))
                    np <- numPerms(pg, controlW)
                    ord <-
                        switch(typeW,
                               free = allFree(pg),
                               series = allSeries(pg, np, mirrorW),
                               grid = allGrid(pg, np, dimW[1],
                               dimW[2], mirrorW, constantW))
                    permW <- nrow(ord)
                    add <- seq(from = 0, by = pg, length.out = ng)
                    res <- vector(mode = "list", length = ng)
                    a <- 1
                    b <- np / permW
                    for(i in seq_len(ng)) {
                        res[[i]] <- matrix(rep(repMat(ord+add[i], a),
                                               each = b),
                                           ncol = pg)
                        a <- a*permW
                        b <- b/permW
                    }
                    res <- do.call(cbind, res)
                    sp <- split(obs, strataP)
                    res <- t(apply(res, 1,
                                   function(x, inds, o) {o[inds] <- inds[x]; o},
                                   unlist(sp), obs))
                }
            }
        }
    }
    ## Do we need to permute plots?
    if (!is.null(strataP)) {
        ## permuting plots ONLY
        if(typeW == "none") {
            res <- allStrata(n, control = control)
        } else {
            ## FIXME - this need updating to work with the new code
            ## permuting blocks AND within blocks
            ## need a local CONTROL that just permutes blocks
            controlP <- permControl(plots = Plots(strata = strataP),
                                    within = Within(type = "none"))
            ## number of permutations for just the block level
            permP <- numPerms(n, control = controlP)
            ## get all permutations for the block level
            shuffP <- allStrata(n, control = controlP)
            ## copy the set of permutations for within blocks
            ## permP times - results is a list
            resP <- rep(list(res), permP)
            resP <- lapply(seq_along(resP),
                            function(i, wi, bl) {
                                t(apply(wi[[i]], 1,
                                        function(x, bl, i) {
                                            x[bl[i,]]
                                        }, bl = bl, i = i))
                            },
                            wi = resP, bl = shuffP)
            res <- do.call(rbind, resP)
        }
    }
    ## some times storage.mode of res is numeric, sometimes
    ## it is integer, set to "integer" for comparisons using
    ## identical to match the observed ordering
    storage.mode(res) <- "integer"

    ## return
    res
}

## enumerate all possible permutations for a more complicated
## design
## fac <- gl(2,6)
##ctrl <- permControl(type = "grid", mirror = FALSE, strata = fac,
##                    constant = TRUE, nrow = 3, ncol = 2)
## ctrl <- permControl(strata = fac,
##                     within = Within(type = "grid", mirror = FALSE,
##                     constant = TRUE, nrow = 3, ncol = 2),
##                     blocks = Blocks(type = "free"))
## Nobs <- length(fac)
## numPerms(seq_len(Nobs), control = ctrl)
## numPerms(Nobs, control = ctrl) ## works just as well
## (tmp <- allPerms(Nobs, control = ctrl, observed = TRUE))
## (tmp2 <- allPerms(Nobs, control = ctrl))

## just in case, keep this for now so I have something to look at before comitting


    ## if(typeW != "none") {
    ##     if(is.null(strataP)) {
    ##         res <- switch(type.wi,
    ##                       free = allFree(n),
    ##                       series = allSeries(n, nperms, WI$mirror),
    ##                       grid = allGrid(n, nperms, WI$nrow,
    ##                       WI$ncol, WI$mirror, WI$constant))
    ##     } else {
    ##         ## permuting within blocks
    ##         tab <- table(STRATA)
    ##         if(WI$constant) {
    ##             ## same permutation in each block
    ##             pg <- unique(tab)
    ##             control.wi <- permControl(within = WI)
    ##             nperms <- numPerms(pg, control.wi)
    ##             ord <- switch(type.wi,
    ##                           free = allFree(pg),
    ##                           series = allSeries(pg, nperms, WI$mirror),
    ##                           grid = allGrid(pg, nperms, WI$nrow,
    ##                           WI$ncol, WI$mirror,
    ##                           WI$constant))
    ##             perm.wi <- nrow(ord)
    ##             sp <- split(v, STRATA)
    ##             res <- matrix(nrow = nperms, ncol = n)
    ##             for(i in seq_len(perm.wi))
    ##                 #res[i,] <- t(sapply(sp, function(x) x[ord[i,]]))
    ##                 res[i,] <- sapply(sp, function(x) x[ord[i,]])
    ##         } else {
    ##             ## different permutations within blocks
    ##             tab <- table(STRATA)
    ##             ng <- length(tab)
    ##             pg <- unique(tab)
    ##             if(length(pg) > 1) {
    ##                 ## different number of observations per level of strata
    ##                 if(type.wi == "grid")
    ##                     ## FIXME: this should not be needed once all checks are
    ##                     ## in place in check()
    ##                     stop("Unbalanced grid designs are not supported")
    ##                 control.wi <- permControl(within = WI)
    ##                 sp <- split(v, STRATA)
    ##                 res <- vector(mode = "list", length = ng)
    ##                 add <- c(0, cumsum(tab)[1:(ng-1)])
    ##                 for(j in seq_along(tab)) {
    ##                     np <- numPerms(tab[j], control.wi)
    ##                     ord <- switch(type.wi,
    ##                                   free = allFree(tab[j]),
    ##                                   series = allSeries(tab[j], np, WI$mirror))
    ##                     perm.wi <- nrow(ord)
    ##                     if(j == 1) {
    ##                         a <- 1
    ##                         b <- np / perm.wi
    ##                     } else {
    ##                         b <- b/perm.wi
    ##                         a <- np / (b*perm.wi)
    ##                     }
    ##                     res[[j]] <- matrix(rep(repMat(ord+add[j], a),
    ##                                            each = b),
    ##                                        ncol = tab[j])
    ##                 }
    ##                 res <- do.call(cbind, res)
    ##                 sp <- split(v, STRATA)
    ##                 res <- t(apply(res, 1,
    ##                                function(x, inds, v) {v[inds] <- inds[x]; v},
    ##                                unlist(sp), v))
    ##             } else {
    ##                 ## same number of observations per level of strata
    ##                 control.wi <- permControl(within = WI)
    ##                 np <- numPerms(pg, control.wi)
    ##                 ord <-
    ##                     switch(type.wi,
    ##                            free = allFree(pg),
    ##                            series = allSeries(pg, np, WI$mirror),
    ##                            grid = allGrid(pg, np, WI$nrow,
    ##                            WI$ncol, WI$mirror,
    ##                            WI$constant))
    ##                 perm.wi <- nrow(ord)
    ##                 add <- seq(from = 0, by = pg, length.out = ng)
    ##                 res <- vector(mode = "list", length = ng)
    ##                 a <- 1
    ##                 b <- np / perm.wi
    ##                 for(i in seq_len(ng)) {
    ##                     res[[i]] <- matrix(rep(repMat(ord+add[i], a),
    ##                                            each = b),
    ##                                        ncol = pg)
    ##                     a <- a*perm.wi
    ##                     b <- b/perm.wi
    ##                 }
    ##                 res <- do.call(cbind, res)
    ##                 sp <- split(v, STRATA)
    ##                 res <- t(apply(res, 1,
    ##                                function(x, inds, v) {v[inds] <- inds[x]; v},
    ##                                unlist(sp), v))
    ##             }
    ##         }
    ##     }
    ## }
    ## ## Do we need to permute blocks?
    ## if ((type.b <- control$blocks$type) != "none") {
    ##     ## permuting blocks ONLY
    ##     if(type.wi == "none") {
    ##         res <- allStrata(n, control = control)
    ##     } else {
    ##       ## FIXME - this need updating to work with the new code
    ##         ## permuting blocks AND within blocks
    ##         ## need a local CONTROL that just permutes blocks
    ##         control.b <- permControl(strata = STRATA,
    ##                               within = Within(type = "none"),
    ##                               blocks = getBlocks(control))
    ##         ## number of permutations for just the block level
    ##         perm.b <- numPerms(n, control = control.b)
    ##         ## get all permutations for the block level
    ##         shuff.b <- allStrata(n, control = control.b)
    ##         ## copy the set of permutations for within blocks
    ##         ## perm.b times - results is a list
    ##         res.b <- rep(list(res), perm.b)
    ##         res.b <- lapply(seq_along(res.b),
    ##                         function(i, wi, bl) {
    ##                             t(apply(wi[[i]], 1,
    ##                                     function(x, bl, i) {
    ##                                         x[bl[i,]]
    ##                                     }, bl = bl, i = i))
    ##                         },
    ##                         wi = res.b, bl = shuff.b)
    ##         res <- do.call(rbind, res.b)
    ##     }
    ## }
    ## ## some times storage.mode of res is numeric, sometimes
    ## ## it is integer, set to "integer" for comparisons using
    ## ## identical to match the observed ordering
    ## storage.mode(res) <- "integer"
