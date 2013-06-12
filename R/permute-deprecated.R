`permControl` <- function(within = Within(),
                          plots = Plots(),
                          blocks = NULL,
                          nperm = 199, complete = FALSE,
                          maxperm = 9999, minperm = 99,
                          all.perms = NULL,
                          observed = FALSE)
{
    .Deprecated("how", package = "permute")
    out <- list(within = within, plots = plots, blocks = blocks,
                nperm = nperm, complete = complete,
                maxperm = maxperm, minperm = minperm,
                all.perms = all.perms, observed = observed,
                blocks.name = deparse(substitute(blocks)))
    class(out) <- "how"
    out
}

`print.permControl` <- function(x, ...)
{
    .Deprecated("print.how", package = "permute")
    class(x) <- "how"
    print(x)
}


`print.permCheck` <- function(x, ...)
{
    print(x$n)
}

`print.summary.permCheck` <- function(x, ...)
{
    cat(paste("Number of possible permutations:", x$n, "\n"))
    print(x$control)
    invisible(x)
}

`summary.permCheck` <- function(object, ...)
{
    class(object) <- "summary.permCheck"
    object
}
