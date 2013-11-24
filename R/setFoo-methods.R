`setNperm<-` <- function(object, value) {
    UseMethod("setNperm<-")
}

`setNperm<-.default` <- function(object, value) {
    stop("No default method for `setNperm`")
}

`setNperm<-.how` <- function(object, value) {
    object[["nperm"]] <- value
    object <- fixupCall(object, "nperm", value)
    object
}

`setNperm<-.permControl` <- function(object, value) {
    object[["nperm"]] <- value
    object <- fixupCall(object, "nperm", value)
    object
}

`setMaxperm<-` <- function(object, value) {
    UseMethod("setMaxperm<-")
}

`setMaxperm<-.default` <- function(object, value) {
    stop("No default method for `setMaxperm`")
}

`setMaxperm<-.how` <- function(object, value) {
    object[["maxperm"]] <- value
    object <- fixupCall(object, "maxperm", value)
    object
}

`setMaxperm<-.permControl` <- function(object, value) {
    object[["maxperm"]] <- value
    object <- fixupCall(object, "maxperm", value)
    object
}

`setMinperm<-` <- function(object, value) {
    UseMethod("setMinperm<-")
}

`setMinperm<-.default` <- function(object, value) {
    stop("No default method for `setMinperm`")
}

`setMinperm<-.how` <- function(object, value) {
    object[["minperm"]] <- value
    object <- fixupCall(object, "minperm", value)
    object
}

`setMinperm<-.permControl` <- function(object, value) {
    object[["minperm"]] <- value
    object <- fixupCall(object, "minperm", value)
    object
}

`setComplete<-` <- function(object, value) {
    UseMethod("setComplete<-")
}

`setComplete<-.default` <- function(object, value) {
    stop("No default method for `setComplete`")
}

`setComplete<-.how` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1)
    object[["complete"]] <- value
    object <- fixupCall(object, "complete", value)
    object
}

`setComplete<-.permControl` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1)
    object[["complete"]] <- value
    object <- fixupCall(object, "complete", value)
    object
}

`setAllperms<-` <- function(object, value) {
    UseMethod("setAllperms<-")
}

`setAllperms<-.default` <- function(object, value) {
    stop("No default method for `setAllperms`")
}

`setAllperms<-.how` <- function(object, value) {
    if (!is.null(value))
        value <- as.matrix(value)
    object[["all.perms"]] <- value
    object <- fixupCall(object, "all.perms", value)
    object
}

`setAllperms<-.permControl` <- function(object, value) {
    if (!is.null(value))
        value <- as.matrix(value)
    object[["all.perms"]] <- value
    object <- fixupCall(object, "all.perms", value)
    object
}

`setMake<-` <- function(object, value) {
    UseMethod("setMake<-")
}

`setMake<-.default` <- function(object, value) {
    stop("No default method for `setMake`")
}

`setMake<-.how` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1)
    object[["make"]] <- value
    object <- fixupCall(object, "make", value)
    object
}

`setMake<-.permControl` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1)
    object[["make"]] <- value
    object <- fixupCall(object, "make", value)
    object
}

`setBlocks<-` <- function(object, value) {
    UseMethod("setBlocks<-")
}

`setBlocks<-.default` <- function(object, value) {
    stop("No default method for `setBlocks`")
}

`setBlocks<-.how` <- function(object, value) {
    if (!is.null(value))
        value <- as.factor(value)
    object["blocks"] <- list(value)
    object[["blocks.name"]] <- deparse(substitute(value))
    object <- fixupCall(object, "blocks", value)
    object
}

`setBlocks<-.permControl` <- function(object, value) {
    if (!is.null(value))
        value <- as.factor(value)
    object["blocks"] <- list(value)
    object[["blocks.name"]] <- deparse(substitute(value))
    object <- fixupCall(object, "blocks", value)
    object
}

`setObserved<-` <- function(object, value) {
    UseMethod("setObserved<-")
}

`setObserved<-.default` <- function(object, value) {
    stop("No default method for `setObserved`")
}

`setObserved<-.how` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1)
    object[["observed"]] <- value
    object <- fixupCall(object, "observed", value)
    object
}

`setObserved<-.permControl` <- function(object, value) {
    if (!is.null(value))
        value <- rep(as.logical(value), length.out = 1)
    object[["observed"]] <- value
    object <- fixupCall(object, "observed", value)
    object
}
