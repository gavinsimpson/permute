`allStrata` <- function(n, control)
{
    ## seq vector of observation indices
    v <- seq_len(n)
    ## number of groups
    lev <- length(levels(control$strata))
    ## compute nperms on number of levels
    nperms <- numPerms(lev, control)
    ## result object
    X <- matrix(nrow = nperms, ncol = length(control$strata))
    ## store the type
    type <- control$blocks$type
    perms <- if(type == "free") {
        allFree(lev)
    } else if(type == "series") {
        mirror <- control$blocks$mirror
        allSeries(lev, nperms = nperms, mirror = mirror)
    } else if(type == "grid") {
        nr <- control$blocks$nrow
        nc <- control$blocks$ncol
        mirror <- control$blocks$mirror
        constant <- control$blocks$constant
        allGrid(lev, nperms = nperms, nr = nr, nc = nc,
                mirror = mirror, constant = constant)
    } else {
        ## if in here, must have both types == "none"
        ## this is here just in case - need to check if this
        ## is possible given calling function...
        return(v)
    }
    sp <- split(v, control$strata)
    ## build permutations by concatenating components of sp
    ## for each row of level permutations
    for(i in seq_len(nrow(perms)))
        X[i,] <- unname(do.call(c, sp[perms[i,]]))
    return(X)
}
