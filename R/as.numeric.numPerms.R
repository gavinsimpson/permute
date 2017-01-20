`as.numeric.numPerms` <- function(x, ...) {
    class(x) <- class(x)[-1]
    x
}
