`check` <- function(object, control = how(), quietly = FALSE)
{
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

    ## strata at plot & block levels
    plots <- getStrata(control, which = "plots")
    blocks <- getStrata(control, which = "blocks")

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

        ## if permuting strata, must be balanced
        if(typeP != "none" && bal > 1)
            stop("Design must be balanced if permuting 'strata'.")

        ## if permuting Plots as a grid check dimensions match levels of
        ## Plot-level strata
        if(isTRUE(all.equal(typeP, "grid"))) {
            levP <- levels(Plots)
            dimP <- getDim(control, which = "plots")
            if(!identical(levP, prod(dimP))) {
                stop("Plot 'nrow' * 'ncol' not a multiple of number of Plots.")
            }
        }
    }

    ## check length of Blocks is equal to N
    if(!is.null(blocks)) {
        if(!isTRUE(all.equal(length(blocks), N)))
            stop("Number of observations and length of Block 'strata' do not match.")
    }

    ## check allPerms is of correct form
    if(!is.null(control$all.perms) &&
       !inherits(control$all.perms, "allPerms"))
        stop("'control$all.perms' must be of class 'allPerms'.")

    ## get number of possible permutations
    num.pos <- numPerms(object, control)

    ## check if number requested permutations exceeds max possible
    if(getNperm(control) > num.pos) {
        setComplete(control) <- TRUE
        setNperm(control) <- num.pos
        setMaxperm(control) <- num.pos
        if(!quietly)
            message("'nperm' > set of all permutations; Resetting 'nperm'.")
    }

    ## if number of possible perms < minperm turn on complete enumeration
    if((num.pos < getMinperm(control))) {
        setComplete(control) <- TRUE
        setNperm(control) <- num.pos
        setMaxperm(control) <- num.pos
        if(!quietly)
            message("Set of permutations < 'minperm'. Generating entire set.")
    }

    ## if complete enumeration, generate all permutations
    if(getComplete(control) && getMake(control)) {
        ap <- allPerms(N, control = control, check = FALSE)
        setAllperms(control) <- ap
    }
    retval <- list(n = num.pos, control = control)
    class(retval) <- "check"
    retval
}
