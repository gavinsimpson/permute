## Extractor functions for blocks, plots and within, plus strata

## Blocks
getBlocks <- function(object, ...) {
    UseMethod("getBlocks")
}

getBlocks.default <- function(object, ...) {
    stop("No default method for 'getBlocks()'")
}

getBlocks.permControl <- function(object, ...) {
    object$blocks
}

getBlocks.how <- function(object, ...) {
    object$blocks
}

## Plots
getPlots <- function(object, ...) {
    UseMethod("getPlots")
}

getPlots.default <- function(object, ...) {
    stop("No default method for 'getPlots()'")
}

getPlots.permControl <- function(object, ...) {
    object$plots
}

getPlots.how <- function(object, ...) {
    object$plots
}

## Within plots
getWithin <- function(object, ...) {
    UseMethod("getWithin")
}

getWithin.default <- function(object, ...) {
    stop("No default method for 'getWithin()'")
}

getWithin.permControl <- function(object, ...) {
    object$within
}

getWithin.how <- function(object, ...) {
    object$within
}

## Strata
getStrata <- function(object, ...) {
    UseMethod("getStrata")
}

getStrata.default <- function(object, ...) {
    stop("No default method for 'getStrata()'")
}

getStrata.permControl <- function(object,
                                  which = c("plots", "blocks"),
                                  drop = TRUE, ...) {
    which <- match.arg(which)
    if(isTRUE(all.equal(which, "plots")))
        strata <- object$plots$strata
    else if(isTRUE(all.equal(which, "blocks")))
        strata <- object$blocks
        stop("Ambiguous `which`")
    if(isTRUE(drop) && !is.null(strata))
        strata <- droplevels(strata)
    strata
}

getStrata.how <- function(object,
                                  which = c("plots","blocks"),
                                  drop = TRUE, ...) {
    which <- match.arg(which)
    if(isTRUE(all.equal(which, "plots")))
        strata <- object$plots$strata
    else if(isTRUE(all.equal(which, "blocks")))
        strata <- object$blocks #object$blocks$strata
    else
        stop("Ambiguous `which`")
    if(isTRUE(drop) && !is.null(strata))
        strata <- droplevels(strata)
    strata
}

## Get type of permutation
getType <- function(object, ...) {
    UseMethod("getType")
}

getType.default <- function(object, ...) {
    stop("No default method for 'getType()'")
}

getType.permControl <- function(object,
                                which = c("plots","within"), ...) {
    which <- match.arg(which)
  if(isTRUE(all.equal(which, "plots")))
      type <- getPlots(object)$type
  else if(isTRUE(all.equal(which, "within")))
      type <- getWithin(object)$type
  else
      stop("Ambiguous `which`")
  type
}

getType.how <- function(object,
                                which = c("plots","within"), ...) {
    which <- match.arg(which)
  if(isTRUE(all.equal(which, "plots")))
      type <- getPlots(object)$type
  else if(isTRUE(all.equal(which, "within")))
      type <- getWithin(object)$type
  else
      stop("Ambiguous `which`")
  type
}

## suppose we can also have setBlocks() etc...
## to update the control object in place....

## Get mirroring status
`getMirror` <- function(object, ...) {
    UseMethod("getMirror")
}

`getMirror.default` <- function(object, ...) {
    stop("No default method for 'getMirror()'")
}

`getMirror.permControl` <- function(object,
                                    which = c("plots","within"), ...) {
    which <- match.arg(which)
    if(isTRUE(all.equal(which, "plots")))
        mirror <- getPlots(object)$mirror
    else if(isTRUE(all.equal(which, "within")))
        mirror <- getWithin(object)$mirror
    else
        stop("Ambiguous `which`")
    mirror
}

`getMirror.how` <- function(object,
                                    which = c("plots","within"), ...) {
    which <- match.arg(which)
    if(isTRUE(all.equal(which, "plots")))
        mirror <- getPlots(object)$mirror
    else if(isTRUE(all.equal(which, "within")))
        mirror <- getWithin(object)$mirror
    else
        stop("Ambiguous `which`")
    mirror
}

## Get constant status - i.e. same permutation in each Plot
`getConstant` <- function(object, ...) {
    UseMethod("getConstant")
}

`getConstant.default` <- function(object, ...) {
    stop("No default method for 'getConstant()'")
}

`getConstant.permControl` <- function(object, ...) {
    getWithin(object)$constant
}

`getConstant.how` <- function(object, ...) {
    getWithin(object)$constant
}

## Get the number of rows and colums from grid designs
`getRow` <- function(object, ...) {
    UseMethod("getRow")
}

`getRow.default` <- function(object, ...) {
    NROW(object)
}

`getRow.permControl` <- function(object, which = c("plots","within"),
                                 ...) {
    which <- match.arg(which)
    if(isTRUE(all.equal(which, "plots")))
        nrow <- getPlots(object)$nrow
    else if(isTRUE(all.equal(which, "within")))
        nrow <- getWithin(object)$nrow
    else
        stop("Ambiguous `which`")
    nrow
}

`getRow.how` <- function(object, which = c("plots","within"),
                                 ...) {
    which <- match.arg(which)
    if(isTRUE(all.equal(which, "plots")))
        nrow <- getPlots(object)$nrow
    else if(isTRUE(all.equal(which, "within")))
        nrow <- getWithin(object)$nrow
    else
        stop("Ambiguous `which`")
    nrow
}

`getCol` <- function(object, ...) {
    UseMethod("getCol")
}

`getCol.default` <- function(object, ...) {
    NCOL(object)
}

`getCol.permControl` <- function(object, which = c("plots","within"),
                                 ...) {
    which <- match.arg(which)
    if(isTRUE(all.equal(which, "plots")))
        ncol <- getPlots(object)$ncol
    else if(isTRUE(all.equal(which, "within")))
        ncol <- getWithin(object)$ncol
    else
        stop("Ambiguous `which`")
    ncol
}

`getCol.how` <- function(object, which = c("plots","within"),
                                 ...) {
    which <- match.arg(which)
    if(isTRUE(all.equal(which, "plots")))
        ncol <- getPlots(object)$ncol
    else if(isTRUE(all.equal(which, "within")))
        ncol <- getWithin(object)$ncol
    else
        stop("Ambiguous `which`")
    ncol
}

`getDim` <- function(object, ...) {
    UseMethod("getDim")
}

`getDim.default` <- function(object, ...) {
    dim(object)
}

`getDim.permControl` <- function(object, which = c("plots","within"),
                                 ...) {
    which <- match.arg(which)
    if(isTRUE(all.equal(which, "plots"))) {
        PL <- getPlots(object)
        nc <- PL$ncol
        nr <- PL$nrow
    } else if(isTRUE(all.equal(which, "within"))) {
        WI <- getWithin(object)
        nc <- WI$ncol
        nr <- WI$nrow
    } else {
        stop("Ambiguous `which`")
    }
    c(nr, nc)
}

`getDim.how` <- function(object, which = c("plots","within"),
                                 ...) {
    which <- match.arg(which)
    if(isTRUE(all.equal(which, "plots"))) {
        PL <- getPlots(object)
        nc <- PL$ncol
        nr <- PL$nrow
    } else if(isTRUE(all.equal(which, "within"))) {
        WI <- getWithin(object)
        nc <- WI$ncol
        nr <- WI$nrow
    } else {
        stop("Ambiguous `which`")
    }
    c(nr, nc)
}

## return the requested number of permutations
`getNperm` <- function(object, ...) {
    UseMethod("getNperm")
}

`getNperm.default` <- function(object, ...) {
    stop("No default method for `getNperm`")
}

`getNperm.permControl` <- function(object, ...) {
    object$nperm
}

`getNperm.how` <- function(object, ...) {
    object$nperm
}

## Returns maximum permutation threshold
`getMaxperm` <- function(object, ...) {
    UseMethod("getMaxperm")
}

`getMaxperm.default` <- function(object, ...) {
    stop("No default method for `getMaxperm`")
}

`getMaxperm.permControl` <- function(object, ...) {
    object$maxperm
}

`getMaxperm.how` <- function(object, ...) {
    object$maxperm
}

## Returns minimum permutation threshold
`getMinperm` <- function(object, ...) {
    UseMethod("getMinperm")
}

`getMinperm.default` <- function(object, ...) {
    stop("No default method for `getMinperm`")
}

`getMinperm.permControl` <- function(object, ...) {
    object$minperm
}

`getMinperm.how` <- function(object, ...) {
    object$minperm
}

## Returns status of complete enumeration
`getComplete` <- function(object, ...) {
    UseMethod("getComplete")
}

`getComplete.default` <- function(object, ...) {
    stop("No default method for `getComplete`")
}

`getComplete.permControl` <- function(object, ...) {
    list(complete = object$complete,
         minperm = object$minperm)
}

`getComplete.how` <- function(object, ...) {
    list(complete = object$complete,
         minperm = object$minperm)
}
