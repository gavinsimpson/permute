##' @title Coerce an object of class \code{permutationMatrix} to a matrix
##' @param x an object of class \code{permutationMatrix}
##' @param ... arguments passed to other methods
##' @return \code{x} coerced to a matrix.
##' @author Gavin L. Simpson
##'
##' @export
`as.matrix.permutationMatrix` <- function(x, ...) {
    ## as.matrix.permutationMatrix - an S3 method to convert to the S3
    ## matrix class. Essentially this just strips attributes and updates
    ## the class to only "matrix"

    attr(x, "seed") <- NULL
    attr(x, "control") <- NULL
    class(x) <- "matrix"
    x
}
