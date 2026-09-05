## Non-exported function that can be used to fix up elements of a call
`fixupCall` <- function(object, element, value) {
    ## element must be character! Try to coerce
    element <- as.character(element)     ## Fails if not coercible
    .call <- getElement(object, "call")  ## get call from object
    .ll <- as.list(.call)                ## convert to a list to manipulate
    names(.ll) <- names(.call)           ## set names on the list
    ## if element not in names need to create it
    if (! element %in% names(.ll)) {
        new <- list(value)
        names(new) <- element
        .ll <- c(.ll, new)
    } else {
        ## otherwise just update it
        .ll[[element]] <- value              ## set element to value
    }
    .call <- as.call(.ll)                ## convert back to a call
    object$call <- .call                 ## push back into object
    object
}

## Amend the stored constructor call used by update methods. NULL values must
## remain in the call, so arguments are replaced individually rather than via
## modifyList().
`amendCall` <- function(object, extras) {
    object.call <- getCall(object)
    if (is.null(object.call))
        stop("need an object with call component")

    if (length(extras)) {
        existing <- names(extras) %in% names(object.call)
        for (argument in names(extras)[existing])
            object.call[[argument]] <- extras[[argument]]
        if (any(!existing))
            object.call <- as.call(c(as.list(object.call), extras[!existing]))
    }
    object.call
}
