## new version of shuffleSet() that allows for blocking


#' Generate a set of permutations from the specified design
#'
#' `shuffleSet` returns a set of `nset` permutations from the
#' specified design. The main purpose of the function is to circumvent the
#' overhead of repeatedly calling [shuffle()] to generate a set of
#' permutations.
#'
#' `shuffleSet` is designed to generate a set of `nset` permutation
#' indices over which a function can iterate as part of a permutation test. It
#' is only slightly more efficient than calling [shuffle()]
#' `nset` times, but it is far more practical than the simpler function
#' because a set of permutations can be worked on by applying a function to the
#' rows of the returned object. This simplifies the function applied, and
#' facilitates the use of parallel processing functions, thus enabling a larger
#' number of permutations to be evaluated in reasonable time.
#'
#' By default, `shuffleSet` will check the permutations design following a
#' few simple heuristics. See [check()] for details of these. Whether
#' some of the heuristics are activated or not can be controlled via
#' [how()], essentially via its argument `minperm`. In
#' particular, if there are fewer than `minperm` permutations,
#' `shuffleSet` will generate and return **all possible permutations**, which
#' may differ from the number requested via argument
#' `nset`.
#'
#' The `check` argument to `shuffleSet` controls whether checking is
#' performed in the permutation design. If you set `check = FALSE` then
#' exactly `nset` permutations will be returned. However, do be aware that
#' there is no guarantee that the set of permutations returned will be unique,
#' especially so for designs and data sets where there are few possible
#' permutations relative to the number requested.
#'
#' For `Plots(type = "partition")`, rows represent distinct random
#' assignments to groups of fixed size. Independently sampled rows may repeat,
#' but permutations that differ only by reordering observations carrying the
#' same original group label have the same canonical representation.
#'
#' The `as.matrix` method sets the `control` and `seed`
#' attributes to `NULL` and removes the `"permutationMatrix"` class,
#' resulting in a standard matrix object.
#'
#' @aliases shuffleSet as.matrix.permutationMatrix
#' @param n numeric; the number of observations in the sample set. May also be
#' any object that [stats::nobs()] knows about; see [nobs-methods].
#' @param nset numeric; the number of permutations to generate for the set. Can
#' be missing, the default, in which case `nset` is determined from
#' `control`.
#' @param control an object of class `"how"` describing a valid
#' permutation design.
#' @param check logical; should the design be checked for various problems via
#' function [check()]? The default is to check the design for the
#' stated number of observations and update `control` accordingly. See
#' Details.
#' @param quietly logical; should messages by suppressed?
#' @param x an object of class `"permutationMatrix"`, as returned by
#' `shuffleSet`.
#' @param ... arguments passed to other methods. For the `as.matrix`
#' method only.
#' @returns A matrix of permutations, where each row is a separate
#' permutation. As such, the returned matrix has `nset` rows and `n`
#' columns.
#' @author Gavin L. Simpson
#' @seealso See [shuffle()] for generating a single permutation, and
#' [how()] for setting up permutation designs.
#' @references `shuffleSet()` is modelled after the permutation schemes of
#' Canoco 3.1 (ter Braak, 1990); see also Besag & Clifford (1989).
#'
#' Besag, J. and Clifford, P. (1989) Generalized Monte Carlo significance
#' tests. *Biometrika* **76**; 633--642.
#'
#' ter Braak, C. J. F. (1990). *Update notes: CANOCO version 3.1*.
#' Wageningen: Agricultural Mathematics Group. (UR).
#' @keywords htest design
#' @order 1
#' @examples
#'
#' \dontshow{suppressWarnings(RNGversion("3.5.0"))}
#' set.seed(1)
#' ## simple random permutations, 5 permutations in set
#' shuffleSet(n = 10, nset = 5)
#'
#' ## series random permutations, 5 permutations in set
#' shuffleSet(10, 5, how(within = Within(type = "series")))
#'
#' ## series random permutations, 10 permutations in set,
#' ## with possible mirroring
#' CTRL <- how(within = Within(type = "series", mirror = TRUE))
#' shuffleSet(10, 10, CTRL)
#'
#' ## Permuting strata
#' ## 4 groups of 5 observations
#' CTRL <- how(within = Within(type = "none"),
#'             plots = Plots(strata = gl(4,5), type = "free"))
#' shuffleSet(20, 10, control = CTRL)
#'
#' ## 10 random permutations in presence of Plot-level strata
#' plotStrata <- Plots(strata = gl(4,5))
#' CTRL <- how(plots = plotStrata,
#'             within = Within(type = "free"))
#' numPerms(20, control = CTRL)
#' shuffleSet(20, 10, control = CTRL)
#' ## as above but same random permutation within Plot-level strata
#' CTRL <- how(plots = plotStrata,
#'             within = Within(type = "free", constant = TRUE))
#' numPerms(20, control = CTRL)
#' shuffleSet(20, 10, CTRL) ## check this.
#'
#' ## time series within each level of Plot strata
#' CTRL <- how(plots = plotStrata,
#'             within = Within(type = "series"))
#' shuffleSet(20, 10, CTRL)
#' ## as above, but  with same permutation for each Plot-level stratum
#' CTRL <- how(plots = plotStrata,
#'             within = Within(type = "series", constant = TRUE))
#' shuffleSet(20, 10, CTRL)
#'
`shuffleSet` <- function(n, nset, control = how(), check = TRUE,
                         quietly = FALSE) {
    ## Store the .Random.seed, if it exists, so we can attach this as
    ## an attribute to the permutation matrix returned in out
    SEED <- NULL
    if (exists(".Random.seed", envir = globalenv())) {
        SEED <- .Random.seed
    }

    ## handle missing nset - take from control if can
    if(missing(nset)) {
        np <- getNperm(control)
        if(is.null(np)) ## something wrong, default back to 1
            nset <- 1
        else
            nset <- np
    } else {
        setNperm(control) <- nset ## this fixes the control$call too!
    }

    ## handle a vector, matrix, or data frame input; derive n from it
    if (((is.numeric(n) || is.integer(n) || is.factor(n) || is.character(n)) &&
         length(n) > 1L) ||
        is.matrix(n) ||
        is.data.frame(n)) {
        n <- nobs(n)
    }
    checkPartitionDesign(control, n)
    sn <- seq_len(n) ## sequence of samples in order of input

    ## if checking permutation design, may end up with more perms
    ## than requested in nset, depending upon what user specified
    ## in `control`. The `check` argument can turn this step off
    ## so you always get `nset` permutations and, yes, you can shoot
    ## yourself in the foot with this, hence the default is to check!
    if (isTRUE(check)) {
        ## store original complete
        all.orig <- getComplete(control)
        ## need to check number of permutations won't blow up
        pcheck <- check(sn, control = control, quietly = quietly)
        ## control possibly now updated
        control <- getControl(pcheck)
        ## if complete != TRUE originally, it must have been triggered by
        ## small permutation size. If so, don't change nset. Only change if
        ## user asked for complete = TRUE in the first place!
        if (isTRUE(all.orig)) {
            nset <- getNperm(control)       # update nset as it might have changed
        }
    }

    if(is.null(AP <- getAllperms(control))) {
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
        ## undo the original splitting. Can't use unsplit() here as the
        ## elements of out are matrices
        out <- do.call(cbind, out)
        out[, unlist(spln)] <- out ## reorders according to spln
    } else {
        ## if we have all.perms now then we must have generated it
        ## during checking or user passed it with control
        ## Use that instead of a ranodm set
        out <- AP
    }

    ## Because all.perms might have been generated, we have the
    ## possibility that nrow(out) != nset. In that case, also no random
    ## numbers have been generated. Hence we can sample nset rows from
    ## out and return that. This has the nice side-effect of not
    ## generating any non-unique permutations. Suggested by Jari.
    if ((nr <- nrow(out)) > nset) {
        out <- out[sample.int(nr, nset), , drop = FALSE]
    }

    ## Attach random seed stored earlier to permutation matrix
    attr(out, "seed") <- SEED
    attr(out, "control") <- control
    attr(out, "observed") <- NULL ## nullify this as allPerms may have added it?

    ## class the matrix so we can have a print method etc, but inherit from
    ## the matrix S3 class
    class(out) <- c("permutationMatrix", "matrix")

    ## return
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
                           mirror = withinCTRL$mirror,
                           symmetric = isTRUE(withinCTRL$symmetric)))
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
                                   list(strata = Pstrata[ind],
                                        type = typeP,
                                        mirror = plotCTRL$mirror,
                                        flip = NULL, ## runif(1L) < 0.5 ??
                                        nrow = plotCTRL$nrow,
                                        ncol = plotCTRL$ncol,
                                        symmetric =
                                            isTRUE(plotCTRL$symmetric)))
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
                        flip <- gridFlip(mirror = withinCTRL$mirror,
                                         symmetric =
                                             isTRUE(withinCTRL$symmetric))
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
                                           flip = flip,
                                           symmetric =
                                               isTRUE(withinCTRL$symmetric)))
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
