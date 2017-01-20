`print.numPerms` <- function(x, ...) {
    x <- as.numeric(x)
    if (is.infinite(x)) {
        print(paste(">", .Machine$double.xmax))
    } else {
        print(x, ...)
    }
}
