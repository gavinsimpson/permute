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
