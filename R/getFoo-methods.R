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

## Strata
getStrata <- function(object, ...) {
    UseMethod("getStrata")
}

getStrata.default <- function(object, ...) {
    stop("No default method for 'getStrata()'")
}

getStrata.permControl <- function(object,
                                  which = c("plots","blocks"), 
                                  drop = TRUE, ...) {
    which <- match.arg(which)
    if(isTRUE(all.equal(which, "plots")))
        strata <- object$plots$strata
    else if(isTRUE(all.equal(which, "blocks")))
        strata <- object$blocks$strata
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
## suppose we can also have setBlocks() etc...
## to update the control object in place....
