#' @rdname check
#' @order 2
#' @export
`summary.check` <- function(object, ...)
{
    class(object) <- "summary.check"
    object
}
