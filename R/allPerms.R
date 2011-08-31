`allPerms` <- function(n, control = permControl(), max = 9999,
                       observed = FALSE) {
    ## replicate a matrix by going via a list and bind together
    repMat <- function(mat, n) {
        res <- rep(list(mat), n)
        do.call(rbind, res)
    }
    ## start
    v <- n
    ## expand n if a numeric or integer vector of length 1
    if((is.numeric(n) || is.integer(n)) && (length(n) == 1))
         v <- seq_len(n)
    ## number of observations in data
    n <- nobs(v)
    ## check permutation scheme and update control
    pcheck <- check(v, control = control, make.all = FALSE)
    ctrl <- pcheck$control
    ## get max number of permutations
    nperms <- pcheck$n
    ## sanity check - don't let this run away to infinity
    ## esp with type = "free"
    if(nperms > max)
        stop("Number of possible permutations too large (> 'max')")
    WI <- getWithin(ctrl)
    STRATA <- getStrata(ctrl)
    type.wi <- WI$type
    if(type.wi != "none") {
        if(is.null(STRATA)) {
            res <- switch(type.wi,
                          free = allFree(n),
                          series = allSeries(n, nperms, WI$mirror),
                          grid = allGrid(n, nperms, WI$nrow,
                          WI$ncol, WI$mirror, WI$constant))
        } else {
            ## permuting within blocks
            tab <- table(STRATA)
            if(WI$constant) {
                ## same permutation in each block
                pg <- unique(tab)
                ctrl.wi <- permControl(strata = NULL, within = WI)
                nperms <- numPerms(pg, ctrl.wi)
                ord <- switch(type.wi,
                              free = allFree(pg),
                              series = allSeries(pg, nperms, WI$mirror),
                              grid = allGrid(pg, nperms, WI$nrow,
                              WI$ncol, WI$mirror,
                              WI$constant))
                perm.wi <- nrow(ord)
                sp <- split(v, STRATA)
                res <- matrix(nrow = nperms, ncol = n)
                for(i in seq_len(perm.wi))
                    #res[i,] <- t(sapply(sp, function(x) x[ord[i,]]))
                    res[i,] <- sapply(sp, function(x) x[ord[i,]])
            } else {
                ## different permutations within blocks
                tab <- table(STRATA)
                ng <- length(tab)
                pg <- unique(tab)
                if(length(pg) > 1) {
                    ## different number of observations per level of strata
                    if(type.wi == "grid")
                        ## FIXME: this should not be needed once all checks are
                        ## in place in check()
                        stop("Unbalanced grid designs are not supported")
                    ctrl.wi <- permControl(strata = NULL, within = WI)
                    sp <- split(v, STRATA)
                    res <- vector(mode = "list", length = ng)
                    add <- c(0, cumsum(tab)[1:(ng-1)])
                    for(j in seq_along(tab)) {
                        np <- numPerms(tab[j], ctrl.wi)
                        ord <- switch(type.wi,
                                      free = allFree(tab[j]),
                                      series = allSeries(tab[j], np, WI$mirror))
                        perm.wi <- nrow(ord)
                        if(j == 1) {
                            a <- 1
                            b <- np / perm.wi
                        } else {
                            b <- b/perm.wi
                            a <- np / (b*perm.wi)
                        }
                        res[[j]] <- matrix(rep(repMat(ord+add[j], a),
                                               each = b),
                                           ncol = tab[j])
                    }
                    res <- do.call(cbind, res)
                    sp <- split(v, STRATA)
                    res <- t(apply(res, 1,
                                   function(x, inds, v) {v[inds] <- inds[x]; v},
                                   unlist(sp), v))
                } else {
                    ## same number of observations per level of strata
                    ctrl.wi <- permControl(strata = NULL, within = WI)
                    np <- numPerms(pg, ctrl.wi)
                    ord <-
                        switch(type.wi,
                               free = allFree(pg),
                               series = allSeries(pg, np, WI$mirror),
                               grid = allGrid(pg, np, WI$nrow,
                               WI$ncol, WI$mirror,
                               WI$constant))
                    perm.wi <- nrow(ord)
                    add <- seq(from = 0, by = pg, length.out = ng)
                    res <- vector(mode = "list", length = ng)
                    a <- 1
                    b <- np / perm.wi
                    for(i in seq_len(ng)) {
                        res[[i]] <- matrix(rep(repMat(ord+add[i], a),
                                               each = b),
                                           ncol = pg)
                        a <- a*perm.wi
                        b <- b/perm.wi
                    }
                    res <- do.call(cbind, res)
                    sp <- split(v, STRATA)
                    res <- t(apply(res, 1,
                                   function(x, inds, v) {v[inds] <- inds[x]; v},
                                   unlist(sp), v))
                }
            }
        }
    }
    ## Do we need to permute blocks?
    if ((type.b <- control$blocks$type) != "none") {
        ## permuting blocks ONLY
        if(type.wi == "none") {
            res <- allStrata(n, control = control)
        } else {
            ## permuting blocks AND within blocks
            ## need a local CTRL that just permutes blocks
            ctrl.b <- permControl(strata = STRATA,
                                  within = Within(type = "none"),
                                  blocks = getBlocks(ctrl))
            ## number of permutations for just the block level
            perm.b <- numPerms(n, control = ctrl.b)
            ## get all permutations for the block level
            shuff.b <- allStrata(n, control = ctrl.b)
            ## copy the set of permutations for within blocks
            ## perm.b times - results is a list
            res.b <- rep(list(res), perm.b)
            res.b <- lapply(seq_along(res.b),
                            function(i, wi, bl) {
                                t(apply(wi[[i]], 1,
                                        function(x, bl, i) {
                                            x[bl[i,]]
                                        }, bl = bl, i = i))
                            },
                            wi = res.b, bl = shuff.b)
            res <- do.call(rbind, res.b)
        }
    }
    ## some times storage.mode of res is numeric, sometimes
    ## it is integer, set to "integer" for comparisons using
    ## identical to match the observed ordering
    storage.mode(res) <- "integer"
    if(!observed) {
        obs.v <- seq_len(n)
        ##obs.row <- apply(res, 1, function(x, v) {identical(x, v)}, obs.v)
        obs.row <- apply(res, 1, function(x, obs.v) all(x == obs.v), obs.v)
        res <- res[!obs.row, ]
        ## reduce the number of permutations to get rid of the
        ## observed ordering
        control$nperm <- control$nperm - 1
    }
    class(res) <- "allPerms"
    ##attr(res, "control") <- control
    attr(res, "observed") <- observed
    return(res)
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
