#' @rdname allUtils
#' @order 4
`allStrata` <- function(n, control)
{
    ## seq vector of observation indices
    v <- seq_len(n)
    ## number of groups
    strata <- getStrata(control, which = "plots")

    ## Partition permutations rearrange the group labels rather than the
    ## groups as whole units.
    type <- getType(control, which = "plots")
    if (type == "partition") {
        return(doAllPartitions(strata))
    }

    lev <- length(levels(strata))
    ## store the type
    type <- getType(control, which = "plots")
    mirror <- getMirror(control, which = "plots")
    symmetric <- getSymmetric(control, which = "plots")
    nr <- getRow(control, which = "plots")
    nc <- getCol(control, which = "plots")
    ## compute nperms on number of levels - for this need Within()
    ## and type == typeP
    within <- if(type == "grid") {
        Within(type = type, mirror = mirror, nrow = nr, ncol = nc,
               symmetric = symmetric)
    } else {
        Within(type = type, mirror = mirror)
    }
    newControl <- how(within = within)
    nperms <- numPerms(lev, newControl)
    ## result object
    X <- matrix(nrow = nperms, ncol = length(strata))
    perms <- if(type == "free") {
        allFree(lev)
    } else if(type == "series") {
        allSeries(lev, nperms = nperms, mirror = mirror)
    } else if(type == "grid") {
        constant <- getConstant(control)
        allGrid(lev, nperms = nperms, nr = nr, nc = nc,
                mirror = mirror, constant = constant,
                symmetric = symmetric)
    } else {
        ## if in here, must have both types == "none"
        ## this is here just in case - need to check if this
        ## is possible given calling function...
        return(v)
    }
    sp <- split(v, strata)
    ## build permutations by permuting the split indices (as list)
    ## then undo the original splitting. This respects original indices
    ## of the samples, even where strata ar not contiguous
    for(i in seq_len(nrow(perms))) {
        X[i, ] <- unsplit(sp[perms[i, ]], strata)
    }
    X
}
