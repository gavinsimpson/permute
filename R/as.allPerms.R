`as.allPerms` <- function(object, control) {
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
