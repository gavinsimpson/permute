`permControl` <- function(strata = NULL, nperm = 199, complete = FALSE,
                          within = Within(),
                          blocks = Blocks(),
                          maxperm = 9999, minperm = 99,
                          all.perms = NULL,
                          observed = FALSE)
{
    out <- list(strata = strata, nperm = nperm, complete = complete,
                within = within, blocks = blocks,
                maxperm = maxperm, minperm = minperm,
                all.perms = all.perms, observed = observed,
                name.strata = deparse(substitute(strata)))
    class(out) <- "permControl"
    return(out)
}
