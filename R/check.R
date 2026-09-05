#' Utility functions for permutation schemes
#'
#' `check` provides checking of permutation schemes for validity.
#'
#' `check` is a utility functions for working with the new permutation
#' schemes available in [shuffle()].
#'
#' `check` is used to check the current permutation schemes against the
#' object to which it will be applied. If `num_perms` is `TRUE`,
#' `check` also calculates the maximum number of possible permutations for
#' the number of observations in `object` and the permutation scheme
#' described by `control`. The returned object contains component
#' `control`, an object of class `"how"` suitably modified if
#' `check` identifies a problem.
#'
#' The main problem is requesting more permutations than is possible with the
#' number of observations and the permutation design. In such cases,
#' `nperm` is reduced to equal the number of possible permutations, and
#' complete enumeration of all permutations is turned on
#' (`control$complete` is set to `TRUE`).
#'
#' Alternatively, if the number of possible permutations is low, and less than
#' `control$minperm`, it is better to enumerate all possible permutations,
#' and as such complete enumeration of all permutations is turned on
#' (`control$complete` is set to `TRUE`). This guarantees that
#' permutations are all unique and there are no duplicates.
#'
#' @aliases check print.check print.summary.check summary.check
#' @param object an R object. See Details for a complete description,
#' especially for [numPerms()]. For `summary.check()` an
#' object of class `"check"`.
#' @param control a list of control values describing properties of the
#' permutation design, as returned by a call to [how()].
#' @param quietly logical; should messages by suppressed?
#' @param num_perms logical; should the number of permutations be computed
#' during checks?
#' @param ... arguments to other methods.
#' @returns For `check` a list containing the maximum number of
#' permutations possible and an object of class `"how"`.
#' @author Gavin L. Simpson
#' @order 1
#' @seealso [shuffle()] and [how()].
#' @keywords utilities design methods
#' @examples
#'
#' ## only run this example if vegan is available
#' if (suppressPackageStartupMessages(require("vegan"))) {
#'     ## use example data from ?pyrifos in package vegan
#'     example(pyrifos)
#'
#'     ## Demonstrate the maximum number of permutations for the pyrifos data
#'     ## under a series of permutation schemes
#'
#'     ## no restrictions - lots of perms
#'     CONTROL <- how(within = Within(type = "free"))
#'     (check1 <- check(pyrifos, CONTROL))
#'     ## summary(check1)
#'
#'     ## no strata but data are series with no mirroring, so 132 permutations
#'     CONTROL <- how(within = Within(type = "series", mirror = FALSE))
#'     check(pyrifos, CONTROL)
#'
#'     ## no strata but data are series with mirroring, so 264 permutations
#'     CONTROL <- how(within = Within(type = "series", mirror = TRUE))
#'     check(pyrifos, control = CONTROL)
#'
#'     ## unrestricted within strata
#'     check(pyrifos, control = how(plots = Plots(strata = ditch),
#'                    within = Within(type = "free")))
#'
#'     ## time series within strata, no mirroring
#'     check(pyrifos,
#'           control = how(plots = Plots(strata = ditch),
#'           within = Within(type = "series", mirror = FALSE)))
#'
#'     ## time series within strata, with mirroring
#'     check(pyrifos,
#'           control = how(plots = Plots(strata = ditch),
#'           within = Within(type = "series", mirror = TRUE)))
#'
#'     ## time series within strata, no mirroring, same permutation
#'     ## within strata
#'     check(pyrifos,
#'           control = how(plots = Plots(strata = ditch),
#'           within = Within(type = "series", constant = TRUE)))
#'
#'     ## time series within strata, with mirroring, same permutation
#'     ## within strata
#'     check(pyrifos,
#'           control = how(plots = Plots(strata = ditch),
#'           within = Within(type = "series", mirror = TRUE,
#'           constant = TRUE)))
#'     ## permute strata
#'     check(pyrifos, how(plots = Plots(strata = ditch, type = "free"),
#'                        within = Within(type = "none")))
#' }
#'
#' ## this should also also for arbitrary vectors
#' vec1 <- check(1:100)
#' vec2 <- check(1:100, how())
#' all.equal(vec1, vec2)
#' vec3 <- check(1:100, how(within = Within(type = "series")))
#' all.equal(100, vec3$n)
#' vec4 <- check(1:100, how(within = Within(type= "series", mirror = TRUE)))
#' all.equal(vec4$n, 200)
#'
#' ## enumerate all possible permutations
#' fac <- gl(2,6)
#' ctrl <- how(plots = Plots(strata = fac),
#'             within = Within(type = "grid", mirror = FALSE,
#'                             constant = TRUE, nrow = 3, ncol = 2))
#' check(1:12, ctrl)
#'
#' numPerms(1:12, control = ctrl)
#' (tmp <- allPerms(12, control = update(ctrl, observed = TRUE)))
#' (tmp2 <- allPerms(12, control = ctrl))
#'
#' ## turn on mirroring
#' ctrl <- update(ctrl, within = update(getWithin(ctrl), mirror = TRUE))
#' numPerms(1:12, control = ctrl)
#' (tmp3 <- allPerms(12, control = update(ctrl, observed = TRUE)))
#' (tmp4 <- allPerms(12, control = ctrl))
#' ## prints out details of the permutation scheme as
#' ## well as the matrix of permutations
#' summary(tmp)
#' summary(tmp2)
#'
#' ## different numbers of observations per level of strata
#' fac <- factor(rep(1:3, times = c(3,2,2)))
#' ## free permutations in levels of strata
#' numPerms(7, how(within = Within(type = "free"),
#'                 plots = Plots(strata = fac, type = "none")))
#' allPerms(7, how(within = Within(type = "free"),
#'                 plots = Plots(strata = fac)))
#' ## series permutations in levels of strata
#' ctrl <- how(within = Within(type = "series"), plots = Plots(strata = fac))
#' numPerms(7, control = ctrl)
#' allPerms(7, control = ctrl)
#'
`check` <- function(object, control = how(), quietly = FALSE, num_perms = TRUE)
{
    ## In principle we are mainly dealing with integers, but many
    ## functions do not return integers but double, and the numbers
    ## can be so large that they overflow integer and they really must be
    ## double. Therefore we define EPS as a nice value between two
    ## successive integers
    EPS <- 0.5
    ## if object is numeric or integer and of length 1,
    ## extend the object
    if(length(object) == 1 &&
       (is.integer(object) || is.numeric(object)))
        object <- seq_len(object)

    ## check the number of observations in object
    N <- nobs(object)

    ## sample permutation type
    typeW <- getType(control, which = "within")
    typeP <- getType(control, which = "plots")

    checkPartitionDesign(control, N)

    ## check we're actually permuting something
    if (identical(typeW, typeP) && isTRUE(all.equal(typeW, "none"))) {
        stop("Permutation 'type' is \"none\" for both 'plots' & 'within'.\nNothing to permute.")
    }

    ## strata at plot & block levels
    plots <- getStrata(control, which = "plots")
    blocks <- getStrata(control, which = "blocks")

    ## check length of Blocks is equal to N
    if(!is.null(blocks)) {
        if(!isTRUE(all.equal(length(blocks), N)))
            stop("Number of observations and length of Block 'strata' do not match.")
    }

    ## if strata, check N == length of strata but beware empty levels
    if(!is.null(plots)) {
        tab <- table(plots)
        if(!identical(as.integer(N), as.integer(sum(tab))))
            stop("Number of observations and length of Plot 'strata' do not match.")

        ## if "grid", check design balanced?
        if((bal <- length(unique(tab))) > 1 && typeW == "grid")
            stop("Unbalanced 'grid' designs are not supported.")

        ## if grid design, check nrow*ncol is multiple of N
        if(typeW == "grid" &&
           !identical(N %% prod(getDim(control, which = "within")), 0))
            stop("Within 'nrow' * 'ncol' not a multiple of number of observations.")

        ## if constant, check design balanced?
        if(getConstant(control) && bal > 1)
            stop("Unbalanced designs not allowed with 'constant = TRUE'.")

        ## if permuting strata, must be balanced, but only *within* levels of
        ## blocks
        if (!is.null(blocks)) {
            plt_blk <- lapply(
                split(plots, blocks),
                FUN = function(x) length(unique(table(droplevels(x))))
            )
            if(typeP %in% c("free", "series", "grid") &&
               any(unlist(plt_blk) > 1L)) {
                stop("Design must be balanced within blocks if permuting 'strata'.")
            }
        } else {
            if(typeP %in% c("free", "series", "grid") && bal > 1L) {
                stop("Design must be balanced if permuting 'strata'.")
            }
        }

        ## if permuting Plots as a grid check dimensions match levels of
        ## Plot-level strata
        if(isTRUE(all.equal(typeP, "grid"))) {
            levP <- nlevels(plots)
            dimP <- getDim(control, which = "plots")
            if(!isTRUE(all.equal(levP, prod(dimP)))) {
                stop("Plot 'nrow' * 'ncol' not a multiple of number of Plots.")
            }
        }
        }

    ## check allPerms is of correct form
    if(!is.null(control$all.perms) &&
       !inherits(control$all.perms, "allPerms"))
        stop("'control$all.perms' must be of class 'allPerms'.")

    ## get number of possible permutations if requested
    if (isTRUE(num_perms)) {
        num.pos <- numPerms(object, control)

        ## check if number requested permutations exceeds or equals max
        ## possible
        nperm <- getNperm(control)
        if(nperm + EPS > (num.pos - !getObserved(control))) {
            setComplete(control) <- TRUE
            setMaxperm(control) <- num.pos
            setNperm(control) <- num.pos - !getObserved(control)
            if(!quietly)
                message("'nperm' >= set of all permutations: complete enumeration.")
        }

        ## if number of possible perms < minperm turn on complete
        ## enumeration
        if((num.pos - !getObserved(control)) < getMinperm(control) + EPS) {
            setComplete(control) <- TRUE
            setMaxperm(control) <- num.pos
            if(!quietly)
                message("Set of permutations < 'minperm'. Generating entire set.")
        }
    } else {
        num.pos <- NA
    }

    ## if complete enumeration, generate all permutations
    if(getComplete(control) && getMake(control)) {
        ap <- allPerms(N, control = control, check = FALSE)
        setAllperms(control) <- ap
        setNperm(control) <- nrow(ap)
    }
    retval <- list(n = num.pos, control = control)
    class(retval) <- "check"
    retval
}
