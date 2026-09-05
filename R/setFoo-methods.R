#' Replacement functions to set components of a permutation design
#'
#' These functions provide abstracted replacement of components in a
#' permutation design such as one returned by [how()]. They also update the
#' matched calls stored in the design so that [stats::update()] continues to
#' work correctly.
#'
#' Use these functions instead of directly modifying the underlying list, so
#' code does not depend on permute's internal representation.
#'
#' @param object An R object on which to dispatch.
#' @param value The replacement value or object.
#'
#' @returns `object`, suitably modified.
#' @section Note:
#' `setStrata<-` has methods for objects of class `"how"` and `"Plots"`. The
#' former sets the `blocks` component of the [how()] object, while the latter
#' sets the `strata` component of the [Plots()] object.
#'
#' `setDim<-`, `setRow<-`, and `setCol<-` cannot be used on an object of class
#' `"how"`. Instead, extract the [Plots()] or [Within()] component with
#' [getPlots()] or [getWithin()], alter it, and replace it with `setPlots<-` or
#' `setWithin<-`.
#' @author Gavin Simpson
#' @seealso [check()] checks a design described by [how()]. See [get-methods]
#'   for the corresponding extractor functions.
#' @keywords methods utils
#' @name set-methods
#' @order 0
#' @aliases setBlocks<-.default setBlocks<-.how
#' @aliases setWithin<-.default setWithin<-.how setStrata<-.default setStrata<-.how
#' @aliases setStrata<-.Plots setType<-.default setType<-.how setType<-.Plots
#' @aliases setType<-.Within setMirror<-.default setMirror<-.how setMirror<-.Plots
#' @aliases setMirror<-.Within setSymmetric<-.default setSymmetric<-.how
#' @aliases setSymmetric<-.Plots setSymmetric<-.Within
#' @aliases setConstant<-.default setConstant<-.how
#' @aliases setConstant<-.Plots setConstant<-.Within setPlots<-.default setPlots<-.how
#' @aliases setRow<-.default setRow<-.how setRow<-.Plots setRow<-.Within
#' @aliases setCol<-.default setCol<-.how setCol<-.Plots setCol<-.Within
#' @aliases setDim<-.default setDim<-.how setDim<-.Plots setDim<-.Within
#' @aliases setNperm<-.default setNperm<-.how
#' @aliases setAllperms<-.default setAllperms<-.how
#' @aliases setMaxperm<-.default setMaxperm<-.how
#' @aliases setMinperm<-.default setMinperm<-.how
#' @aliases setComplete<-.default setComplete<-.how
#' @aliases setMake<-.default setMake<-.how setObserved<-.default setObserved<-.how
#' @examples
#' hh <- how()
#' getNperm(hh)
#' setNperm(hh) <- 999
#' getNperm(hh)
NULL

## Replacement functions for blocks, plots and within, plus strata,
## etc ...
#' @rdname set-methods
#' @order 5
`setNperm<-` <- function(object, value) {
    UseMethod("setNperm<-")
}

#' @export
#' @noRd
`setNperm<-.default` <- function(object, value) {
    stop("No default method for `setNperm`")
}

#' @export
#' @noRd
`setNperm<-.how` <- function(object, value) {
    object[["nperm"]] <- value
    object <- fixupCall(object, "nperm", value)
    object
}

#' @rdname set-methods
#' @order 7
`setMaxperm<-` <- function(object, value) {
    UseMethod("setMaxperm<-")
}

#' @export
#' @noRd
`setMaxperm<-.default` <- function(object, value) {
    stop("No default method for `setMaxperm`")
}

#' @export
#' @noRd
`setMaxperm<-.how` <- function(object, value) {
    object[["maxperm"]] <- value
    object <- fixupCall(object, "maxperm", value)
    object
}

#' @rdname set-methods
#' @order 8
`setMinperm<-` <- function(object, value) {
    UseMethod("setMinperm<-")
}

#' @export
#' @noRd
`setMinperm<-.default` <- function(object, value) {
    stop("No default method for `setMinperm`")
}

#' @export
#' @noRd
`setMinperm<-.how` <- function(object, value) {
    object[["minperm"]] <- value
    object <- fixupCall(object, "minperm", value)
    object
}

#' @rdname set-methods
#' @order 9
`setComplete<-` <- function(object, value) {
    UseMethod("setComplete<-")
}

#' @export
#' @noRd
`setComplete<-.default` <- function(object, value) {
    stop("No default method for `setComplete`")
}

#' @export
#' @noRd
`setComplete<-.how` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1)
    object[["complete"]] <- value
    object <- fixupCall(object, "complete", value)
    object
}

#' @rdname set-methods
#' @order 6
`setAllperms<-` <- function(object, value) {
    UseMethod("setAllperms<-")
}

#' @export
#' @noRd
`setAllperms<-.default` <- function(object, value) {
    stop("No default method for `setAllperms`")
}

#' @export
#' @noRd
`setAllperms<-.how` <- function(object, value) {
    if (!is.null(value)) {
        value <- as.allPerms(value, control = object)
    }
    object[["all.perms"]] <- value
    object <- fixupCall(object, "all.perms", value)
    object
}

#' @rdname set-methods
#' @order 10
`setMake<-` <- function(object, value) {
    UseMethod("setMake<-")
}

#' @export
#' @noRd
`setMake<-.default` <- function(object, value) {
    stop("No default method for `setMake`")
}

#' @export
#' @noRd
`setMake<-.how` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1)
    object[["make"]] <- value
    object <- fixupCall(object, "make", value)
    object
}

#' @rdname set-methods
#' @order 1
`setBlocks<-` <- function(object, value) {
    UseMethod("setBlocks<-")
}

#' @export
#' @noRd
`setBlocks<-.default` <- function(object, value) {
    stop("No default method for `setBlocks`")
}

#' @export
#' @noRd
`setBlocks<-.how` <- function(object, value) {
    if (inherits(value, "formula")) {
        stop("formulas must be supplied through ",
             "'how(blocks = ..., data = ...)'")
    }
    object[["blocks.name"]] <- deparse(substitute(value))
    if (!is.null(value))
        value <- as.factor(value)
    object["blocks"] <- list(value)
    object <- fixupCall(object, "blocks", value)
    object
}

#' @rdname set-methods
#' @order 11
`setObserved<-` <- function(object, value) {
    UseMethod("setObserved<-")
}

#' @export
#' @noRd
`setObserved<-.default` <- function(object, value) {
    stop("No default method for `setObserved`")
}

#' @export
#' @noRd
`setObserved<-.how` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1)
    object[["observed"]] <- value
    object <- fixupCall(object, "observed", value)
    object
}

## Plots ##############################################################
#' @rdname set-methods
#' @order 2
`setPlots<-` <- function(object, value) {
    UseMethod("setPlots<-")
}

#' @export
#' @noRd
`setPlots<-.default` <- function(object, value) {
    stop("No default method for `setPlots`")
}

#' @export
#' @noRd
`setPlots<-.how` <- function(object, value) {
    stopifnot(inherits(value, "Plots"))
    call <- getCall(value)
    if ("data" %in% names(call)) {
        ## A Plots object supplied to a replacement method is already resolved.
        ## Store its strata rather than a nested data argument, which how()
        ## deliberately rejects when reconstructing the design via update().
        call[["strata"]] <- getStrata(value, drop = FALSE)
        call <- dropCallArgument(call, "data")
    }
    object[["plots"]] <- value
    object <- fixupCall(object, "plots", call)
    object
}

## Within ##############################################################
#' @rdname set-methods
#' @order 3
`setWithin<-` <- function(object, value) {
    UseMethod("setWithin<-")
}

#' @export
#' @noRd
`setWithin<-.default` <- function(object, value) {
    stop("No default method for `setWithin`")
}

#' @export
#' @noRd
`setWithin<-.how` <- function(object, value) {
    stopifnot(inherits(value, "Within"))
    object[["within"]] <- value
    object <- fixupCall(object, "within", getCall(value))
    object
}

## Strata #############################################################
#' @rdname set-methods
#' @order 4
`setStrata<-` <- function(object, value) {
    UseMethod("setStrata<-")
}

#' @export
#' @noRd
`setStrata<-.default` <- function(object, value) {
    stop("No default method for `setStrata`")
}

#' @export
#' @noRd
`setStrata<-.how` <- function(object, value) {
    if (inherits(value, "formula")) {
        stop("formulas must be supplied through ",
             "'Plots(strata = ..., data = ...)'")
    }
    if (!is.null(value)) {
        value <- as.factor(value)
    }
    ## get Plots
    plots <- getPlots(object)
    setStrata(plots) <- value
    setPlots(object) <- plots
    object
}

#' @export
#' @noRd
`setStrata<-.Plots` <- function(object, value) {
    if (inherits(value, "formula")) {
        stop("formulas must be supplied through ",
             "'Plots(strata = ..., data = ...)'")
    }
    if (!is.null(value))
        value <- as.factor(value)
    object[["strata"]] <- value
    object <- fixupCall(object, "strata", value) # value was getCall(value))
    object
}

## Grid dimensions ####################################################
#' @rdname set-methods
#' @order 12
`setRow<-` <- function(object, value) {
    UseMethod("setRow<-")
}

#' @export
#' @noRd
`setRow<-.default` <- function(object, value) {
    stop("No default method for `setRow`")
}

#' @export
#' @noRd
`setRow<-.how` <- function(object, value) {
    stop("`setRow` can not be used directly on '\"how\"' objects.")
}

#' @export
#' @noRd
`setRow<-.Within` <- function(object, value) {
    value <- as.integer(value)
    object[["nrow"]] <- value
    object <- fixupCall(object, "nrow", value)
    object
}

#' @export
#' @noRd
`setRow<-.Plots` <- function(object, value) {
    value <- as.integer(value)
    object[["nrow"]] <- value
    object <- fixupCall(object, "nrow", value)
    object
}

#' @rdname set-methods
#' @order 13
`setCol<-` <- function(object, value) {
    UseMethod("setCol<-")
}

#' @export
#' @noRd
`setCol<-.default` <- function(object, value) {
    stop("No default method for `setCol`")
}

#' @export
#' @noRd
`setCol<-.how` <- function(object, value) {
    stop("`setCol` can not be used directly on '\"how\"' objects.")
}

#' @export
#' @noRd
`setCol<-.Within` <- function(object, value) {
    value <- as.integer(value)
    object[["ncol"]] <- value
    object <- fixupCall(object, "ncol", value)
    object
}

#' @export
#' @noRd
`setCol<-.Plots` <- function(object, value) {
    value <- as.integer(value)
    object[["ncol"]] <- value
    object <- fixupCall(object, "ncol", value)
    object
}

#' @rdname set-methods
#' @order 14
`setDim<-` <- function(object, value) {
    UseMethod("setDim<-")
}

#' @export
#' @noRd
`setDim<-.default` <- function(object, value) {
    stop("No default method for `setDim`")
}

#' @export
#' @noRd
`setDim<-.how` <- function(object, value) {
    stop("`setDim` can not be used directly on '\"how\"' objects.")
}

#' @export
#' @noRd
`setDim<-.Within` <- function(object, value) {
    value <- as.integer(value)
    stopifnot(all.equal(length(value), 2L))
    setRow(object) <- value[1]
    setCol(object) <- value[2]
    object
}

#' @export
#' @noRd
`setDim<-.Plots` <- function(object, value) {
    value <- as.integer(value)
    stopifnot(all.equal(length(value), 2L))
    setRow(object) <- value[1]
    setCol(object) <- value[2]
    object
}

## setType ############################################################
#' @rdname set-methods
#' @order 15
`setType<-` <- function(object, value) {
    UseMethod("setType<-")
}

#' @export
#' @noRd
`setType<-.default` <- function(object, value) {
    stop("No default method for `setType`")
}

#' @export
#' @noRd
`setType<-.how` <- function(object, value) {
    stop("`setType` can not be used directly on '\"how\"' objects.")
}

#' @export
#' @noRd
`setType<-.Within` <- function(object, value) {
    value <- as.character(value)
    if (!value %in% c("free","series","grid","none"))
        stop("Invalid permutation type")
    value <- rep(value, length.out = 1L)
    object[["type"]] <- value
    object <- fixupCall(object, "type", value)
    object
}

#' @export
#' @noRd
`setType<-.Plots` <- function(object, value) {
    value <- as.character(value)
    if (!value %in% c("free","series","grid","none","partition"))
        stop("Invalid permutation type")
    value <- rep(value, length.out = 1L)
    object[["type"]] <- value
    object <- fixupCall(object, "type", value)
    object
}

## setMirror ############################################################
#' @rdname set-methods
#' @order 16
`setMirror<-` <- function(object, value) {
    UseMethod("setMirror<-")
}

#' @export
#' @noRd
`setMirror<-.default` <- function(object, value) {
    stop("No default method for `setMirror`")
}

#' @export
#' @noRd
`setMirror<-.how` <- function(object, value) {
    stop("`setMirror` can not be used directly on '\"how\"' objects.")
}

#' @export
#' @noRd
`setMirror<-.Within` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1)
    object[["mirror"]] <- value
    object <- fixupCall(object, "mirror", value)
    object
}

#' @export
#' @noRd
`setMirror<-.Plots` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1)
    object[["mirror"]] <- value
    object <- fixupCall(object, "mirror", value)
    object
}

## setSymmetric ###########################################################
#' @rdname set-methods
#' @order 17
`setSymmetric<-` <- function(object, value) {
    UseMethod("setSymmetric<-")
}

#' @export
#' @noRd
`setSymmetric<-.default` <- function(object, value) {
    stop("No default method for `setSymmetric`")
}

#' @export
#' @noRd
`setSymmetric<-.how` <- function(object, value) {
    stop("`setSymmetric` can not be used directly on '\"how\"' objects.")
}

#' @export
#' @noRd
`setSymmetric<-.Within` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1L)
    object[["symmetric"]] <- value
    object <- fixupCall(object, "symmetric", value)
    object
}

#' @export
#' @noRd
`setSymmetric<-.Plots` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1L)
    object[["symmetric"]] <- value
    object <- fixupCall(object, "symmetric", value)
    object
}

## setConstant ############################################################
#' @rdname set-methods
#' @order 17
`setConstant<-` <- function(object, value) {
    UseMethod("setConstant<-")
}

#' @export
#' @noRd
`setConstant<-.default` <- function(object, value) {
    stop("No default method for `setConstant`")
}

#' @export
#' @noRd
`setConstant<-.how` <- function(object, value) {
    stop("`setConstant` can not be used directly on '\"how\"' objects.")
}

#' @export
#' @noRd
`setConstant<-.Within` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1)
    object[["constant"]] <- value
    object <- fixupCall(object, "constant", value)
    object
}

#' @export
#' @noRd
`setConstant<-.Plots` <- function(object, value) {
    stop("`setConstant` does not apply to '\"Plots\"' objects.")
}
