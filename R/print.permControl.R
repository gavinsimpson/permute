`print.permControl` <- function(x, ...)
{
    .Deprecated("print.how", package = "permute")
    class(x) <- "how"
    print(x)
}
