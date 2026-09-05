#' Extractor functions to access components of a permutation design
#'
#' These functions provide abstracted access to components of permutation
#' designs such as those returned by [how()]. Using them instead of directly
#' indexing the underlying list allows that representation to evolve without
#' breaking user code.
#'
#' `getHow()` is an alias for `getControl()`; specific `getControl()` methods
#' are useful when debugging.
#'
#' @param object An R object on which to dispatch.
#' @param which Character; the level of restriction for which information is
#'   required.
#' @param drop Logical; should unused factor levels be dropped?
#' @param ... Arguments passed to other methods.
#'
#' @returns The contents of the corresponding component of `object`.
#' @author Gavin Simpson
#' @seealso [check()] checks a permutation design described by [how()].
#' @keywords methods utils
#' @name get-methods
#' @order 0
#' @aliases getBlocks.default getBlocks.permControl getWithin.default
#' @aliases getWithin.permControl getStrata.default getStrata.permControl
#' @aliases getType.default getType.permControl getMirror.default
#' @aliases getMirror.permControl getSymmetric.default getSymmetric.permControl
#' @aliases getConstant.default getConstant.permControl
#' @aliases getPlots.default getPlots.permControl getRow.default getRow.permControl
#' @aliases getCol.default getCol.permControl getDim.default getDim.permControl
#' @aliases getNperm.default getNperm.permControl getMaxperm.default
#' @aliases getMaxperm.permControl getMinperm.default getMinperm.permControl
#' @aliases getComplete.default getComplete.permControl getMake.default
#' @aliases getObserved.default getAllperms.default getControl.default
#' @examples
#' hh <- how()
#' getWithin(hh)
#' getNperm(hh)
NULL

## Extractor functions for blocks, plots and within, plus strata,
## etc ...

## Blocks
#' @rdname get-methods
#' @order 2
`getBlocks` <- function(object, ...) {
    UseMethod("getBlocks")
}

#' @export
#' @noRd
`getBlocks.default` <- function(object, ...) {
    stop("No default method for 'getBlocks()'")
}

#' @rdname get-methods
#' @export
#' @order 21
`getBlocks.how` <- function(object, ...) {
    object$blocks
}

## Plots
#' @rdname get-methods
#' @order 13
`getPlots` <- function(object, ...) {
    UseMethod("getPlots")
}

#' @export
#' @noRd
`getPlots.default` <- function(object, ...) {
    stop("No default method for 'getPlots()'")
}

#' @rdname get-methods
#' @export
#' @order 39
`getPlots.how` <- function(object, ...) {
    object$plots
}

## Within plots
#' @rdname get-methods
#' @order 17
`getWithin` <- function(object, ...) {
    UseMethod("getWithin")
}

#' @export
#' @noRd
`getWithin.default` <- function(object, ...) {
    stop("No default method for 'getWithin()'")
}

#' @rdname get-methods
#' @export
#' @order 48
`getWithin.how` <- function(object, ...) {
    object$within
}

## Strata
#' @rdname get-methods
#' @order 15
`getStrata` <- function(object, ...) {
    UseMethod("getStrata")
}

#' @export
#' @noRd
`getStrata.default` <- function(object, ...) {
    stop("No default method for 'getStrata()'")
}

#' @rdname get-methods
#' @export
#' @order 43
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

#' @rdname get-methods
#' @export
#' @order 44
`getStrata.Plots` <- function(object, drop = TRUE, ... ) {
    strata <- object$strata
    if(isTRUE(drop) && !is.null(strata))
        strata <- droplevels(strata)
    strata
}

## Get type of permutation
#' @rdname get-methods
#' @order 16
`getType` <- function(object, ...) {
    UseMethod("getType")
}

#' @export
#' @noRd
`getType.default` <- function(object, ...) {
    stop("No default method for 'getType()'")
}

#' @rdname get-methods
#' @export
#' @order 45
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

#' @rdname get-methods
#' @export
#' @order 47
`getType.Within` <- function(object, ...) {
    object$type
}

#' @rdname get-methods
#' @export
#' @order 46
`getType.Plots` <- function(object, ...) {
    object$type
}

## suppose we can also have setBlocks() etc...
## to update the control object in place....

## Get mirroring status
#' @rdname get-methods
#' @order 10
`getMirror` <- function(object, ...) {
    UseMethod("getMirror")
}

#' @export
#' @noRd
`getMirror.default` <- function(object, ...) {
    stop("No default method for 'getMirror()'")
}

#' @rdname get-methods
#' @export
#' @order 34
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

#' @rdname get-methods
#' @export
#' @order 36
`getMirror.Within` <- function(object, ...) {
    object$mirror
}

#' @rdname get-methods
#' @export
#' @order 35
`getMirror.Plots` <- function(object, ...) {
    object$mirror
}

## Get symmetric spatial autocovariance status
#' @rdname get-methods
#' @order 11
`getSymmetric` <- function(object, ...) {
    UseMethod("getSymmetric")
}

#' @export
#' @noRd
`getSymmetric.default` <- function(object, ...) {
    stop("No default method for 'getSymmetric()'")
}

#' @rdname get-methods
#' @export
#' @order 37
`getSymmetric.how` <- function(object,
                              which = c("plots", "within"), ...) {
    which <- match.arg(which)
    getSymmetric(if(which == "plots") getPlots(object) else getWithin(object))
}

#' @rdname get-methods
#' @export
#' @order 39
`getSymmetric.Within` <- function(object, ...) {
    isTRUE(object$symmetric)
}

#' @rdname get-methods
#' @export
#' @order 38
`getSymmetric.Plots` <- function(object, ...) {
    isTRUE(object$symmetric)
}

## Get constant status - i.e. same permutation in each Plot
#' @rdname get-methods
#' @order 4
`getConstant` <- function(object, ...) {
    UseMethod("getConstant")
}

#' @export
#' @noRd
`getConstant.default` <- function(object, ...) {
    stop("No default method for 'getConstant()'")
}

#' @rdname get-methods
#' @export
#' @order 26
`getConstant.how` <- function(object, ...) {
    getWithin(object)$constant
}

#' @rdname get-methods
#' @export
#' @order 27
`getConstant.Within` <- function(object, ...) {
    object$constant
}

## Get the number of rows and colums from grid designs
#' @rdname get-methods
#' @order 14
`getRow` <- function(object, ...) {
    UseMethod("getRow")
}

#' @export
#' @noRd
`getRow.default` <- function(object, ...) {
    NROW(object)
}

#' @rdname get-methods
#' @export
#' @order 40
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

#' @rdname get-methods
#' @export
#' @order 42
`getRow.Within` <- function(object, ...) {
    object$nrow
}

#' @rdname get-methods
#' @export
#' @order 41
`getRow.Plots` <- function(object, ...) {
    object$nrow
}

#' @rdname get-methods
#' @order 5
`getCol` <- function(object, ...) {
    UseMethod("getCol")
}

#' @export
#' @noRd
`getCol.default` <- function(object, ...) {
    NCOL(object)
}

#' @rdname get-methods
#' @export
#' @order 22
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

#' @rdname get-methods
#' @export
#' @order 24
`getCol.Within` <- function(object, ...) {
    object$ncol
}

#' @rdname get-methods
#' @export
#' @order 23
`getCol.Plots` <- function(object, ...) {
    object$ncol
}

#' @rdname get-methods
#' @order 6
`getDim` <- function(object, ...) {
    UseMethod("getDim")
}

#' @export
#' @noRd
`getDim.default` <- function(object, ...) {
    dim(object)
}

#' @rdname get-methods
#' @export
#' @order 28
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

#' @rdname get-methods
#' @export
#' @order 30
`getDim.Within` <- function(object, ...) {
    c(object$nrow, object$ncol)
}

#' @rdname get-methods
#' @export
#' @order 29
`getDim.Plots` <- function(object, ...) {
    c(object$nrow, object$ncol)
}

## return the requested number of permutations
#' @rdname get-methods
#' @order 11
`getNperm` <- function(object, ...) {
    UseMethod("getNperm")
}

#' @export
#' @noRd
`getNperm.default` <- function(object, ...) {
    stop("No default method for `getNperm`")
}

#' @rdname get-methods
#' @export
#' @order 37
`getNperm.how` <- function(object, ...) {
    object$nperm
}

## Returns maximum permutation threshold
#' @rdname get-methods
#' @order 8
`getMaxperm` <- function(object, ...) {
    UseMethod("getMaxperm")
}

#' @export
#' @noRd
`getMaxperm.default` <- function(object, ...) {
    stop("No default method for `getMaxperm`")
}

#' @rdname get-methods
#' @export
#' @order 32
`getMaxperm.how` <- function(object, ...) {
    object$maxperm
}

## Returns minimum permutation threshold
#' @rdname get-methods
#' @order 9
`getMinperm` <- function(object, ...) {
    UseMethod("getMinperm")
}

#' @export
#' @noRd
`getMinperm.default` <- function(object, ...) {
    stop("No default method for `getMinperm`")
}

#' @rdname get-methods
#' @export
#' @order 33
`getMinperm.how` <- function(object, ...) {
    object$minperm
}

## Returns status of complete enumeration
#' @rdname get-methods
#' @order 3
`getComplete` <- function(object, ...) {
    UseMethod("getComplete")
}

#' @export
#' @noRd
`getComplete.default` <- function(object, ...) {
    stop("No default method for `getComplete`")
}

#' @rdname get-methods
#' @export
#' @order 25
`getComplete.how` <- function(object, ...) {
    object$complete
}

## Returns whether all permutation should/should not be made
#' @rdname get-methods
#' @order 7
`getMake` <- function(object, ...) {
    UseMethod("getMake")
}

#' @export
#' @noRd
`getMake.default` <- function(object, ...) {
    stop("No default method for `getMake`")
}

#' @rdname get-methods
#' @export
#' @order 31
`getMake.how` <- function(object, ...) {
    object$make
}

## Returns whether the observed permutation should be in
## the set of permutations
#' @rdname get-methods
#' @order 12
`getObserved` <- function(object, ...) {
    UseMethod("getObserved")
}

#' @export
#' @noRd
`getObserved.default` <- function(object, ...) {
    stop("No default method for `getObserved`")
}

#' @rdname get-methods
#' @export
#' @order 38
`getObserved.how` <- function(object, ...) {
    object$observed
}

## Extractor for all.perms component
#' @rdname get-methods
#' @order 1
`getAllperms` <- function(object, ...) {
    UseMethod("getAllperms")
}

#' @rdname get-methods
#' @export
#' @order 20
`getAllperms.how` <- function(object, ...) {
    object$all.perms
}

#' @export
#' @noRd
`getAllperms.default` <- function(object, ...) {
    stop("No default method for `getAllperms`")
}

## Extractor for control/how objects
#' @rdname get-methods
#' @order 18
`getControl` <- function(object, ...) {
    UseMethod("getControl")
}

#' @export
#' @noRd
`getControl.default` <- function(object, ...) {
    nams <- names(object)
    if (!"control" %in% nams) {
        stop("Failed to find a 'control' component in 'object'.")
    }
    object[["control"]]
}

#' @rdname get-methods
#' @export
#' @order 49
`getControl.allPerms` <- function(object, ...) {
    attr(object, "control")
}

#' @rdname get-methods
#' @order 19
`getHow` <- function(object, ...) {
    UseMethod("getControl")
}
