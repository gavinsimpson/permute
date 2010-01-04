`numPerms` <- function(object, control = permControl())
{
    ## constant holding types where something is permuted
    PTYPES <- c("free","grid","series")
    ## expand object if a numeric or integer vector of length 1
    if((is.numeric(object) || is.integer(object)) && (length(object) == 1))
         object <- seq_len(object)
    ## number of observations in data
    nobs <- getNumObs(object)
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
        if((BLOCKS$type %in% PTYPES || isTRUE(WITHIN$constant)) && same.n > 1)
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
        if(BLOCKS$type == "free")
            num.blocks <- exp(lfactorial(length(levels(control$strata))))
        else if(BLOCKS$type %in% c("series","grid")) {
            if(BLOCKS$mirror)
                num.blocks <- blocks.multi * nobs
            else
                num.blocks <- nobs
        }
    }
    ## within
    num.within <- 1
    if(WITHIN$type %in% PTYPES) {
        if(WITHIN$type == "free") {
            if(STRATA)
                num.within <- prod(factorial(tab.strata))
            else
                num.within <- exp(lfactorial(nobs))
        } else if(WITHIN$type %in% c("series","grid")) {
            if(STRATA) {
                if(same.n > 1) {
                    multi <- rep(2, length = length(tab.strata))
                    multi[which(tab.strata == 2)] <- 1
                    if(WITHIN$mirror) {
                        num.within <- prod(multi * tab.strata)
                    } else {
                        num.within <- prod(tab.strata)
                    }
                } else {
                    if(WITHIN$mirror) {
                        if(WITHIN$constant)
                            num.within <- within.multi * unique(tab.strata)
                        else
                            num.within <- prod(within.multi * tab.strata)
                    } else {
                        if(WITHIN$constant)
                            num.within <- unique(tab.strata)
                        else
                            num.within <- prod(tab.strata)
                    }
                }
            } else {
                if(WITHIN$mirror)
                    num.within <- within.multi * nobs
                else
                    num.within <- nobs
            }
        } else {
            stop("Ambiguous permutation type in 'control$type'")
        }
    }
    return(num.blocks * num.within)
}
