`numPerms` <- function(object, control = permControl())
{
    ## constant holding types where something is permuted
    PTYPES <- c("free","grid","series","none")
    ## expand object if a numeric or integer vector of length 1
    if((is.numeric(object) || is.integer(object)) &&
       (length(object) == 1))
        object <- seq_len(object)
    ## number of observations in data
    nobs <- nobs(object)
    ## within perms object
    WITHIN <- control$within
    ## strata perms object
    BLOCKS <- control$blocks
    ## are strata present?
    STRATA <- !is.null(control$strata)
    ## check that when permuting strata or constant within strata,
    ## strata have same number of samples
    if(STRATA) {
        tab.strata <- table(control$strata)
        same.n <- length(unique(tab.strata))
        if((BLOCKS$type %in% PTYPES || isTRUE(WITHIN$constant)) &&
           same.n > 1)
            stop("All levels of strata must have same number of samples for chosen scheme")
        if(BLOCKS$type == "grid" && same.n > 1)
            stop("Unbalanced grid designs are not supported")
    }
    ## generate multiplier for restricted permutations
    if(WITHIN$type %in% c("series","grid")) {
        within.multi <- 2
        if(WITHIN$type == "grid" && WITHIN$ncol > 2) {
            within.multi <- 4
        } else {
            if(nobs == 2)
                within.multi <- 1
        }
    }
    if(BLOCKS$type %in% c("series","grid")) {
        blocks.multi <- 2
        if(BLOCKS$type == "grid" && BLOCKS$ncol > 2) {
            blocks.multi <- 4
        } else {
            if(nobs == 2)
                blocks.multi <- 1
        }
    }
    ## calculate number of possible permutations
    ## blocks
    num.blocks <- 1
    if(BLOCKS$type %in% PTYPES) {
        num.blocks <- if(BLOCKS$type == "free")
            exp(lfactorial(length(levels(control$strata))))
        else if(BLOCKS$type %in% c("series","grid")) {
            if(BLOCKS$mirror)
                blocks.multi * nobs
            else
                nobs
        } else {
            1
        }
    }
    ## within
    if(!(WITHIN$type %in% PTYPES))
        stop("Ambiguous permutation type in 'control$within$type'")

    num.within <- if(WITHIN$type == "none") {
        ## no within permutations
        ## recall this is what we multiply num.blocks
        ## by hence not 0
        1
    } else if(WITHIN$type == "free") {
        if(STRATA)
            prod(factorial(tab.strata))
        else
            exp(lfactorial(nobs))
    } else {
        ##} else if(WITHIN$type %in% c("series","grid")) {
        if(STRATA) {
            if(same.n > 1) {
                multi <- rep(2, length = length(tab.strata))
                multi[which(tab.strata == 2)] <- 1
                if(WITHIN$mirror) {
                    prod(multi * tab.strata)
                } else {
                    prod(tab.strata)
                }
            } else {
                if(WITHIN$mirror) {
                    if(WITHIN$constant)
                        within.multi * unique(tab.strata)
                    else
                        prod(within.multi * tab.strata)
                } else {
                    if(WITHIN$constant)
                        unique(tab.strata)
                    else
                        prod(tab.strata)
                }
            }
        } else {
            if(WITHIN$mirror)
                within.multi * nobs
            else
                nobs
        }
    }
    return(num.blocks * num.within)
}
