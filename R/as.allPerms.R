#' @rdname allPerms
#' @order 4
`as.allPerms` <- function(object, control) {
    if (inherits(object, "plotPermutations")) {
        if (!missing(control)) attr(object, "control") <- control
        return(object)
    }
    object <- as.matrix(object)
    class(object) <- c("allPerms", "matrix")
    if (!missing(control)) {
        attr(object, "control") <- control
        attr(object, "observed") <- getObserved(control)
    } else {
        ## This needs double checking that it is needed!
        attr(object, "control") <- attr(object, "observed") <- NA
    }
    object
}
