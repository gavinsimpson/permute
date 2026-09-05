#' @rdname allPerms
#' @order 3
#' @export
`as.matrix.allPerms` <- function(x, ...) {
    attr(x, "control") <- NULL
    attr(x, "observed") <- NULL
    class(x) <- "matrix"
    x
}
