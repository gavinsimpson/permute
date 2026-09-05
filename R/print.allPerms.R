#' @export
#' @noRd
`print.allPerms` <- function(x, ...) {
    dims <- dim(x)
    attributes(x) <- NULL
    dim(x) <- dims
    print(x)
    return(invisible(x))
}
