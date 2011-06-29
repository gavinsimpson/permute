## Extractor functions for blocks and within
getBlocks <- function(object, ...) {
    UseMethod("getBlocks")
}

getBlocks.default <- function(object, ...) {
    stop("No default method for 'getBlocks()'")
}

getBlocks.permControl <- function(object, ...) {
    object$blocks
}

getWithin <- function(object, ...) {
    UseMethod("getWithin")
}

getWithin.default <- function(object, ...) {
    stop("No default method for 'getWithin()'")
}

getWithin.permControl <- function(object, ...) {
    object$within
}

getStrata <- function(object, ...) {
    UseMethod("getStrata")
}

getStrata.default <- function(object, ...) {
    stop("No default method for 'getStrata()'")
}

getStrata.permControl <- function(object, ...) {
    object$strata
}

## suppose we can also have setBlocks() etc...
## to update the control object in place....
