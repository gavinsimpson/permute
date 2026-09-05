#' Complete enumeration of all possible permutations
#'
#' `allPerms` is a utility function to return the set of permutations for
#' a given R object and a specified permutation design.
#'
#' Function `allPerms` enumerates all possible permutations for the number
#' of observations and the selected permutation scheme. It has
#' [base::print()] and [base::summary()] methods. `allPerms`
#' returns a matrix containing all possible permutations, possibly containing
#' the observed ordering (if argument `observed` is `TRUE`). The rows
#' of this matrix are the various permutations and the columns reflect the
#' number of samples.
#'
#' With free permutation designs, and restricted permutation schemes with large
#' numbers of observations, there are a potentially huge number of possible
#' permutations of the samples. It would be inefficient, not to mention
#' incredibly time consuming, to enumerate them all. Storing all possible
#' permutations would also become problematic in such cases. To control this
#' and guard against trying to evaluate too large a number of permutations, if
#' the number of possible permutations is larger than
#' `getMaxperm(control)`, `allPerms` exits with an error.
#'
#' The `as.matrix` method sets the `control` and `seed`
#' attributes to `NULL` and removes the `"permutationMatrix"` class,
#' resulting in a standard matrix object.
#'
#' With `Plots(type = "partition")`, the rows enumerate distinct
#' assignments to the labelled groups in `Plots$strata`. If the group
#' sizes are \eqn{n_1, \ldots, n_K}, there are \eqn{n! / \prod_k n_k!} such
#' assignments before optionally removing the observed assignment.
#'
#' @aliases allPerms print.allPerms summary.allPerms print.summary.allPerms
#' @aliases as.matrix.allPerms as.allPerms
#' @param n the number of observations or an 'object' from which the number of
#' observations can be determined via `getNumObs`.
#' @param control a list of control values describing properties of the
#' permutation design, as returned by a call to [how()].
#' @param check logical; should `allPerms` check the design? The default
#' is to check, but this can be skipped, for example if a function checked the
#' design earlier.
#' @param object for `summary.allPerms`, an object of class
#' `"allPerms"`. For `as.allPerms` a matrix or something that can be
#' coerced to a matrix by [base::as.matrix()].
#' @param ... arguments to other methods.
#' @param x an object of class `"allPerms"`, as returned by
#' `allPerms`.
#' @returns For `allPerms`, an object of class `"allPerms"`: a matrix
#' whose rows are the set of all possible permutations for the supplies number
#' of observations and permutation scheme selected. The matrix has two
#' additional attributes `control` and `observed`. Attribute
#' `control` contains the argument `control` (possibly updated via
#' `check`). Attribute `observed` contains argument `observed`.
#' @section Warning: If permuting the strata themselves, a balanced design is
#' required (the same number of observations in each level of `strata`).
#' This does not apply to `Plots(type = "partition")`, which supports
#' unequal group sizes.
#' @author Gavin Simpson
#' @order 1
#' @examples
#'
#' ## allPerms can work with a vector
#' vec <- c(3,4,5)
#' allPerms(vec) ## free permutation
#'
#' ## enumerate all possible permutations for a more complicated
#' ## design
#' fac <- gl(2,6)
#' ctrl <- how(within = Within(type = "grid", mirror = FALSE,
#'                             constant = TRUE, nrow = 3, ncol = 2),
#'             plots = Plots(strata = fac))
#' Nobs <- length(fac)
#' numPerms(seq_len(Nobs), control = ctrl) ## 6
#' (tmp <- allPerms(Nobs, control = update(ctrl, observed = TRUE)))
#' (tmp2 <- allPerms(Nobs, control = ctrl))
#'
#' ## turn on mirroring
#' ##ctrl$within$mirror <- TRUE
#' ctrl <- update(ctrl, within = update(getWithin(ctrl), mirror = TRUE))
#' numPerms(seq_len(Nobs), control = ctrl)
#' (tmp3 <- allPerms(Nobs, control = update(ctrl, observed = TRUE)))
#' (tmp4 <- allPerms(Nobs, control = ctrl))
#'
#' ## prints out details of the permutation scheme as
#' ## well as the matrix of permutations
#' summary(tmp3)
#' summary(tmp4)
#'
`allPerms` <- function(n, control = how(), check = TRUE) {
    ## start
    v <- n
    ## expand n if a numeric or integer vector of length 1
    if((is.numeric(n) || is.integer(n)) && (length(n) == 1))
         v <- seq_len(n)
    ## number of observations in data
    n <- nobs(v)
    checkPartitionDesign(control, n)
    ## check permutation scheme and update control
    make <- getMake(control)
    if (check) {
        control2 <- control
        setMake(control2) <- FALSE
        pcheck <- check(v, control = control2, quietly = TRUE)
    }
    ## ctrl <- pcheck$control
    ## if we do copy the new updated control, we need to update to
    ## reset make
    ## ctrl <- update(ctrl, make = make)

    ## get max number of permutations
    nperms <- numPerms(v, control = control, check = check)

    ## sanity check - don't let this run away to infinity
    ## esp with type = "free"
    if(nperms > getMaxperm(control))
        stop("Number of possible permutations too large (> 'maxperm')")

    WI <- getWithin(control)
    strataP <- getStrata(control, which = "plots")
    typeW <- getType(control, which = "within")
    typeP <- getType(control, which = "plot")
    BLOCKS <- getBlocks(control)
    dimW <- getDim(control, which = "within")
    dimP <- getDim(control, which = "plots")
    mirrorW <- getMirror(control, which = "within")
    mirrorP <- getMirror(control, which = "plots")
    symmetricW <- getSymmetric(control, which = "within")
    symmetricP <- getSymmetric(control, which = "plots")
    constantW <- getConstant(control)

    ## give a BLOCKS if non supplied - i.e. one block
    if(is.null(BLOCKS))
        BLOCKS <- factor(rep(1, n))

    ## split v by blocks
    spl <- split(seq_len(n), BLOCKS)
    nb <- length(spl) # number of blocks

    ## result object
    out <- vector(mode = "list", length = nb)

    ## null-out Blocks in control
    control2 <- control
    setBlocks(control2) <- NULL

    ## loop over blocks and return allPerms on each block
    for (i in seq_along(spl)) {
        out[[i]] <-
            doAllPerms(spl[[i]], strataP, typeW, typeP, mirrorW,
                       mirrorP, symmetricW, symmetricP, constantW,
                       dimW, dimP, control2,
                       nperms = nperms)
    }

    ## bind all blocks together, repeating them as required
    out <- cbindAllPerms(out)

    ## bind all the blocks together
    ## out <- do.call(cbind, out) ## hmm are any of these the same shape?
    out[, unlist(spl)] <- out  ## is this being done at the doAllPerms level?

    if(!(observed <- getObserved(control))) {
        obs.v <- seq_len(n)
        obs.row <- apply(out, 1, function(x, obs.v) all(x == obs.v), obs.v)
        out <- out[!obs.row, , drop = FALSE]
        ## reduce the number of permutations to get rid of the
        ## observed ordering
        setNperm(control) <- getNperm(control) - 1
    }

    ## as a permutationMatrix we pick up nice print method
    class(out) <- c("allPerms", "permutationMatrix", "matrix")
    attr(out, "control") <- control
    attr(out, "observed") <- observed
    out
}


`doAllPerms` <- function(obs, strataP, typeW, typeP, mirrorW, mirrorP,
                         symmetricW, symmetricP, constantW, dimW, dimP,
                         control, nperms) {
    n <- length(obs)

    ## subset strataP to take only the obs indices and drop the unused
    ## levels
    if (!is.null(strataP)) {
        strataP <- droplevels(strataP[obs])
    }

    ## also need to update the $strata component of control
    ## FIXME: this really should have a toplevel function to set/update
    ## sub-components of control
    ## Pl <- getPlots(control)
    ## setStrata(Pl) <- strataP
    ## setPlots(control) <- Pl
    control$plots$strata <- strataP

    ## permuting within?
    if (typeW != "none") {
        if(is.null(strataP)) {
            ## no plot-level permutations
            ## have to redo numPerms here because we could be within a block
            res <- switch(
                typeW,
                free = allFree(n),
                series = allSeries(
                    n, numPerms(n, control, check = FALSE), mirrorW
                ),
                grid = allGrid(
                    n, numPerms(n, control, check = FALSE),
                    dimW[1], dimW[2],
                    mirrorW, constantW, symmetricW
                )
            )
            ## use res to index original observation indices in this group
            res[] <- obs[res]
        } else {
            ## permuting within plots
            tab <- table(strataP)
            pg <- unique(tab)
            ng <-  length(tab)
            if(constantW) {
                ## same permutation in each plot
                controlW <- how(within = getWithin(control))
                nperms <- numPerms(pg, controlW)
                ord <- switch(
                    typeW,
                    free = allFree(pg),
                    series = allSeries(pg, nperms, mirrorW),
                    grid = allGrid(
                        pg, nperms, dimW[1],
                        dimW[2], mirrorW, constantW, symmetricW)
                    )
                res <- vector(mode = "list", length = ng)
                ss <- seq(0, to = prod(pg, ng-1), by = pg)
                for (i in seq_len(ng)) {
                    res[[i]] <- ord + ss[i]
                }
                ## same permutation within plots, so just cbind rather than
                ## cbindAllPerms as we don't need all combns of rows
                res <- do.call(cbind, res)
                res[] <- obs[res] ## index into the observations in this block
            } else {
                ## different permutations within plots
                nperms <- numPerms(sum(tab), control, check = FALSE)

                if(length(pg) > 1) {
                    ## different number of observations per level of strata
                    if(typeW == "grid")
                        ## FIXME: this should not be needed once all checks are
                        ## in place in check()
                        stop("Unbalanced grid designs are not supported")
                    controlW <- how(within = getWithin(control))
                    res <- vector(mode = "list", length = ng)
                    add <- c(0, cumsum(tab)[1:(ng-1)])
                    for(j in seq_along(tab)) {
                        np <- numPerms(tab[j], controlW, check = FALSE)
                        ord <- switch(typeW,
                                      free = allFree(tab[j]),
                                      series = allSeries(tab[j], np, mirrorW))
                        res[[j]] <- ord + add[j]
                    }
                    res <- cbindAllPerms(res)
                    res[] <- obs[res]
                } else {
                    ## same number of observations per level of strata
                    controlW <- how(within = getWithin(control))
                    np <- numPerms(pg, controlW, check = FALSE)
                    ord <-
                        switch(typeW,
                               free = allFree(pg),
                               series = allSeries(pg, np, mirrorW),
                               grid = allGrid(pg, np, dimW[1],
                               dimW[2], mirrorW, constantW, symmetricW))
                    res <- vector(mode = "list", length = ng)
                    ss <- seq(0, to = prod(pg, ng-1), by = pg)
                    for(i in seq_len(ng)) {
                        res[[i]] <- ord + ss[i]
                    }
                    res <- cbindAllPerms(res)
                    res[] <- obs[res]
                }
            }
        }
    }
    ## Do we need to permute plots?
    if (!is.null(strataP) && !isTRUE(all.equal(typeP, "none"))) {
        ## permuting plots ONLY
        if(typeW == "none") {
            res <- allStrata(n, control = control)
            if (typeP == "partition") {
                res[] <- obs[res]
            }
            } else {
            ## FIXME - this need updating to work with the new code
            ## permuting blocks AND within blocks
            ## need a local CONTROL that just permutes blocks
            controlP <- how(plots = getPlots(control),
                            within = Within(type = "none", constant = constantW))
            ## FIXME - the above should really only need to update
            ## within as shown, not fiddle with Plots

            ## number of permutations for just the block level
            permP <- numPerms(n, control = controlP, check = FALSE)
            ## get all permutations for the block level
            shuffP <- allStrata(n, control = controlP)
            ## copy the set of permutations for within blocks
            ## permP times - results is a list
            resP <- rep(list(res), permP)
            resP <- lapply(seq_along(resP),
                            function(k, wi, bl) {
                                t(apply(wi[[k]], 1,
                                        function(x, bl, kk) {
                                            x[bl[kk,]]
                                        }, bl = bl, kk = k))
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
##ctrl <- how(type = "grid", mirror = FALSE, strata = fac,
##                    constant = TRUE, nrow = 3, ncol = 2)
## ctrl <- how(strata = fac,
##                     within = Within(type = "grid", mirror = FALSE,
##                     constant = TRUE, nrow = 3, ncol = 2),
##                     blocks = Blocks(type = "free"))
## Nobs <- length(fac)
## numPerms(seq_len(Nobs), control = ctrl)
## numPerms(Nobs, control = ctrl) ## works just as well
## (tmp <- allPerms(Nobs, control = ctrl, observed = TRUE))
## (tmp2 <- allPerms(Nobs, control = ctrl))
