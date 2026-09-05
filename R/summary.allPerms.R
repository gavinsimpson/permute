#' @rdname allPerms
#' @order 2
#' @export
`summary.allPerms` <- function(object, ...) {
    class(object) <- "summary.allPerms"
    object
}
