##' Extractor functions to access components of a permutation design
##'
##' Simple functions to allow abstracted access to components of a permutation
##' design, for example as returned by \code{\link{how}}. Whilst many of these
##' are very simple index opertations on a list, using these rather than
##' directly accessing that list allows the internal representation of the
##' permutation design to change without breaking code.
##'
##' These are extractor functions for working with permutation design objects
##' created by \code{\link{how}}. They should be used in preference to directly
##' subsetting the permutation design in case the internal structure of object
##' changes as \pkg{permute} is developed.
##'
##' @aliases get-methods
##'
##' @param object An R object to dispatch on.
##' @param which character; which level of restriction to extract information
##' for.
##' @param drop logical; should un-used factor levels be dropped?
##' @param \dots Arguments passed on to other methods.
##' @return These are simple extractor functions and return the contents of the
##' corresponding components of \code{object}.
##' @author Gavin Simpson
##' @seealso \code{\link{check}}, a utility function for checking permutation
##' scheme described by \code{\link{how}}.
##' @keywords methods utils
##' @examples
##'
##' ## extract components from a "how" object
##' hh <- how()
##' getWithin(hh)
##' getNperm(hh)
##'
##' @export
##'
##' @name get-methods
## Blocks
`getBlocks` <- function(object, ...) {
    UseMethod("getBlocks")
}

##' @rdname get-methods
##'
##' @export
`getBlocks.default` <- function(object, ...) {
    stop("No default method for 'getBlocks()'")
}

##' @rdname get-methods
##'
##' @export
`getBlocks.permControl` <- function(object, ...) {
    object$blocks
}

##' @rdname get-methods
##'
##' @export
`getBlocks.how` <- function(object, ...) {
    object$blocks
}

## Plots
##' @rdname get-methods
##'
##' @export
`getPlots` <- function(object, ...) {
    UseMethod("getPlots")
}

##' @rdname get-methods
##'
##' @export
`getPlots.default` <- function(object, ...) {
    stop("No default method for 'getPlots()'")
}

##' @rdname get-methods
##'
##' @export
`getPlots.permControl` <- function(object, ...) {
    object$plots
}

##' @rdname get-methods
##'
##' @export
`getPlots.how` <- function(object, ...) {
    object$plots
}

## Within plots
##' @rdname get-methods
##'
##' @export
`getWithin` <- function(object, ...) {
    UseMethod("getWithin")
}

##' @rdname get-methods
##'
##' @export
`getWithin.default` <- function(object, ...) {
    stop("No default method for 'getWithin()'")
}

##' @rdname get-methods
##'
##' @export
`getWithin.permControl` <- function(object, ...) {
    object$within
}

##' @rdname get-methods
##'
##' @export
`getWithin.how` <- function(object, ...) {
    object$within
}

## Strata
##' @rdname get-methods
##'
##' @export
`getStrata` <- function(object, ...) {
    UseMethod("getStrata")
}

##' @rdname get-methods
##'
##' @export
`getStrata.default` <- function(object, ...) {
    stop("No default method for 'getStrata()'")
}

##' @rdname get-methods
##'
##' @export
`getStrata.permControl` <- function(object,
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

##' @rdname get-methods
##'
##' @export
`getStrata.how` <- function(object,
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

##' @rdname get-methods
##'
##' @export
`getStrata.Plots` <- function(object, drop = TRUE, ... ) {
    strata <- object$strata
    if(isTRUE(drop) && !is.null(strata))
        strata <- droplevels(strata)
    strata
}

## Get type of permutation
##' @rdname get-methods
##'
##' @export
`getType` <- function(object, ...) {
    UseMethod("getType")
}

##' @rdname get-methods
##'
##' @export
`getType.default` <- function(object, ...) {
    stop("No default method for 'getType()'")
}

##' @rdname get-methods
##'
##' @export
`getType.permControl` <- function(object,
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

##' @rdname get-methods
##'
##' @export
`getType.how` <- function(object,
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

##' @rdname get-methods
##'
##' @export
`getType.Within` <- function(object, ...) {
    object$within$type
}

##' @rdname get-methods
##'
##' @export
`getType.Plots` <- function(object, ...) {
    object$plots$type
}

## suppose we can also have setBlocks() etc...
## to update the control object in place....

## Get mirroring status
##' @rdname get-methods
##'
##' @export
`getMirror` <- function(object, ...) {
    UseMethod("getMirror")
}

##' @rdname get-methods
##'
##' @export
`getMirror.default` <- function(object, ...) {
    stop("No default method for 'getMirror()'")
}

##' @rdname get-methods
##'
##' @export
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

##' @rdname get-methods
##'
##' @export
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

##' @rdname get-methods
##'
##' @export
`getMirror.Within` <- function(object, ...) {
    object$within$mirror
}

##' @rdname get-methods
##'
##' @export
`getMirror.Plots` <- function(object, ...) {
    object$plots$mirror
}

##' @rdname get-methods
##'
##' @export
## Get constant status - i.e. same permutation in each Plot
`getConstant` <- function(object, ...) {
    UseMethod("getConstant")
}

##' @rdname get-methods
##'
##' @export
`getConstant.default` <- function(object, ...) {
    stop("No default method for 'getConstant()'")
}

##' @rdname get-methods
##'
##' @export
`getConstant.permControl` <- function(object, ...) {
    getWithin(object)$constant
}

##' @rdname get-methods
##'
##' @export
`getConstant.how` <- function(object, ...) {
    getWithin(object)$constant
}

##' @rdname get-methods
##'
##' @export
`getConstant.Within` <- function(object, ...) {
    object$within$constant
}

##' @rdname get-methods
##'
##' @export
## Get the number of rows and colums from grid designs
`getRow` <- function(object, ...) {
    UseMethod("getRow")
}

##' @rdname get-methods
##'
##' @export
`getRow.default` <- function(object, ...) {
    NROW(object)
}

##' @rdname get-methods
##'
##' @export
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

##' @rdname get-methods
##'
##' @export
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

##' @rdname get-methods
##'
##' @export
`getRow.Within` <- function(object, ...) {
    object$within$nrow
}

##' @rdname get-methods
##'
##' @export
`getRow.Plots` <- function(object, ...) {
    object$plots$nrow
}

##' @rdname get-methods
##'
##' @export
`getCol` <- function(object, ...) {
    UseMethod("getCol")
}

##' @rdname get-methods
##'
##' @export
`getCol.default` <- function(object, ...) {
    NCOL(object)
}

##' @rdname get-methods
##'
##' @export
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

##' @rdname get-methods
##'
##' @export
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

##' @rdname get-methods
##'
##' @export
`getCol.Within` <- function(object, ...) {
    object$within$ncol
}

##' @rdname get-methods
##'
##' @export
`getCol.Plots` <- function(object, ...) {
    object$plots$ncol
}

##' @rdname get-methods
##'
##' @export
`getDim` <- function(object, ...) {
    UseMethod("getDim")
}

##' @rdname get-methods
##'
##' @export
`getDim.default` <- function(object, ...) {
    dim(object)
}

##' @rdname get-methods
##'
##' @export
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

##' @rdname get-methods
##'
##' @export
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

##' @rdname get-methods
##'
##' @export
`getDim.Within` <- function(object, ...) {
    c(object$nrow, object$ncol)
}

##' @rdname get-methods
##'
##' @export
`getDim.Plots` <- function(object, ...) {
    c(object$nrow, object$ncol)
}

## return the requested number of permutations
##' @rdname get-methods
##'
##' @export
`getNperm` <- function(object, ...) {
    UseMethod("getNperm")
}

##' @rdname get-methods
##'
##' @export
`getNperm.default` <- function(object, ...) {
    stop("No default method for `getNperm`")
}

##' @rdname get-methods
##'
##' @export
`getNperm.permControl` <- function(object, ...) {
    object$nperm
}

##' @rdname get-methods
##'
##' @export
`getNperm.how` <- function(object, ...) {
    object$nperm
}

## Returns maximum permutation threshold
##' @rdname get-methods
##'
##' @export
`getMaxperm` <- function(object, ...) {
    UseMethod("getMaxperm")
}

##' @rdname get-methods
##'
##' @export
`getMaxperm.default` <- function(object, ...) {
    stop("No default method for `getMaxperm`")
}

##' @rdname get-methods
##'
##' @export
`getMaxperm.permControl` <- function(object, ...) {
    object$maxperm
}

##' @rdname get-methods
##'
##' @export
`getMaxperm.how` <- function(object, ...) {
    object$maxperm
}

## Returns minimum permutation threshold
##' @rdname get-methods
##'
##' @export
`getMinperm` <- function(object, ...) {
    UseMethod("getMinperm")
}

##' @rdname get-methods
##'
##' @export
`getMinperm.default` <- function(object, ...) {
    stop("No default method for `getMinperm`")
}

##' @rdname get-methods
##'
##' @export
`getMinperm.permControl` <- function(object, ...) {
    object$minperm
}

##' @rdname get-methods
##'
##' @export
`getMinperm.how` <- function(object, ...) {
    object$minperm
}

## Returns status of complete enumeration
##' @rdname get-methods
##'
##' @export
`getComplete` <- function(object, ...) {
    UseMethod("getComplete")
}

##' @rdname get-methods
##'
##' @export
`getComplete.default` <- function(object, ...) {
    stop("No default method for `getComplete`")
}

##' @rdname get-methods
##'
##' @export
`getComplete.permControl` <- function(object, ...) {
    object$complete
}

##' @rdname get-methods
##'
##' @export
`getComplete.how` <- function(object, ...) {
    object$complete
}

## Returns whether all permutation should/should not be made
##' @rdname get-methods
##'
##' @export
`getMake` <- function(object, ...) {
    UseMethod("getMake")
}

##' @rdname get-methods
##'
##' @export
`getMake.default` <- function(object, ...) {
    stop("No default method for `getMake`")
}

##' @rdname get-methods
##'
##' @export
`getMake.how` <- function(object, ...) {
    object$make
}

## Returns whether the observed permutation should be in
## the set of permutations
##' @rdname get-methods
##'
##' @export
`getObserved` <- function(object, ...) {
    UseMethod("getObserved")
}

##' @rdname get-methods
##'
##' @export
`getObserved.default` <- function(object, ...) {
    stop("No default method for `getObserved`")
}

##' @rdname get-methods
##'
##' @export
`getObserved.how` <- function(object, ...) {
    object$observed
}

## Extractor for all.perms component
##' @rdname get-methods
##'
##' @export
`getAllperms` <- function(object, ...) {
    UseMethod("getAllperms")
}

##' @rdname get-methods
##'
##' @export
`getAllperms.how` <- function(object, ...) {
    object$all.perms
}

##' @rdname get-methods
##'
##' @export
`getAllperms.default` <- function(object, ...) {
    stop("No default method for `getAllperms`")
}
