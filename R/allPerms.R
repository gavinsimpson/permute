`allPerms` <- function(n, control = permControl(), max = 9999,
                       observed = FALSE)
{
    ## what does this do - used below to generate the
    ## permutations when constant == FALSE
    bar <- function(mat, n) {
        res <- vector(mode = "list", length = n)
        for(i in seq_len(n))
            res[[i]] <- mat
        do.call(rbind, res)
    }
    BAR <- function(mat, n) {
        res <- list()
        res[[1]] <- mat
        res <- rep(res, n)
        do.call(rbind, res)
    }
    ## start
    v <- n
    ## expand n if a numeric or integer vector of length 1
    if((is.numeric(n) || is.integer(n)) && (length(n) == 1))
         v <- seq_len(n)
    ## number of observations in data
    n <- getNumObs(v)
    ## check permutation scheme and update control
    pcheck <- permCheck(v, control = control, make.all = FALSE)
    ctrl <- pcheck$control
    ## get max number of permutations
    Nperms <- pcheck$n
    ## sanity check - don't let this run away to infinity
    ## esp with type = "free"
    if(Nperms > max)
        stop("Number of possible permutations too large (> 'max')")
    type.wi <- ctrl$within$type
    if(type.wi != "none") {
        if(is.null(ctrl$strata)) {
            res <- switch(type.wi,
                          free = allFree(n),
                          series = allSeries(n, nperms, ctrl$within$mirror),
                          grid = allGrid(n, nperms, ctrl$within$nrow,
                          ctrl$within$ncol, ctrl$within$mirror,
                          ctrl$within$constant))
        } else {
            ## permuting within blocks
            tab <- table(ctrl$strata)
            if(ctrl$within$constant) {
                ## same permutation in each block
                pg <- unique(tab)
                ctrl.wi <- permControl(strata = NULL, within = ctrl$within)
                nperms <- numPerms(pg, ctrl.wi)
                ord <- switch(type.wi,
                              free = allFree(pg),
                              series = allSeries(pg, nperms, ctrl$within$mirror),
                              grid = allGrid(pg, nperms, ctrl$within$nrow,
                              ctrl$within$ncol, ctrl$within$mirror,
                              ctrl$within$constant))
                perm.wi <- nrow(ord)
                sp <- split(v, ctrl$strata)
                res <- matrix(nrow = nperms, ncol = n)
                for(i in seq_len(perm.wi))
                    #res[i,] <- t(sapply(sp, function(x) x[ord[i,]]))
                    res[i,] <- sapply(sp, function(x) x[ord[i,]])
            } else {
                ## different permutations within blocks
                tab <- table(ctrl$strata)
                ng <- length(tab)
                pg <- unique(tab)
                if(length(pg) > 1) {
                    ## different number of observations per level of strata
                    if(type.wi == "grid")
                        ## FIXME: this should not be needed once all checks are
                        ## in place in permCheck()
                        stop("Unbalanced grid designs are not supported")
                    ctrl.wi <- permControl(strata = NULL, within = ctrl$within)
                    sp <- split(v, ctrl$strata)
                    res <- vector(mode = "list", length = ng)
                    add <- c(0, cumsum(tab)[1:(ng-1)])
                    for(j in seq_along(tab)) {
                        nperms <- numPerms(tab[j], ctrl.wi)
                        ord <- switch(type.wi,
                                      free = allFree(tab[j]),
                                      series = allSeries(tab[j], nperms, ctrl$within$mirror))
                        perm.wi <- nrow(ord)
                        if(j == 1) {
                            a <- 1
                            b <- Nperms / perm.wi
                        } else {
                            b <- b/perm.wi
                            a <- Nperms / (b*perm.wi)
                        }
                        res[[j]] <- matrix(rep(bar(ord+add[j], a),
                                               each = b),
                                           ncol = tab[j])
                    }
                    res <- do.call(cbind, res)
                    sp <- split(v, ctrl$strata)
                    res <- t(apply(res, 1,
                                   function(x, inds, v) {v[inds] <- inds[x]; v},
                                   unlist(sp), v))
                } else {
                    ## same number of observations per level of strata
                    ctrl.wi <- permControl(strata = NULL, within = ctrl$within)
                    nperms <- numPerms(pg, ctrl.wi)
                    ord <-
                        switch(type.wi,
                               free = allFree(pg),
                               series = allSeries(pg, nperms, ctrl$within$mirror),
                               grid = allGrid(pg, nperms, ctrl$within$nrow,
                               ctrl$within$ncol, ctrl$within$mirror,
                               ctrl$within$constant))
                    perm.wi <- nrow(ord)
                    add <- seq(from = 0, by = pg, length.out = ng)
                    res <- vector(mode = "list", length = ng)
                    a <- 1
                    b <- Nperms / perm.wi
                    for(i in seq_len(ng)) {
                        res[[i]] <- matrix(rep(bar(ord+add[i], a), each = b),
                                           ncol = pg)
                        a <- a*perm.wi
                        b <- b/perm.wi
                    }
                    res <- do.call(cbind, res)
                    sp <- split(v, ctrl$strata)
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
            .NotYetImplemented()
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
