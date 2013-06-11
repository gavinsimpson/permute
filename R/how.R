`how` <- function(within = Within(),
                  plots = Plots(),
                  blocks = NULL,
                  nperm = 199,
                  complete = FALSE,
                  maxperm = 9999,
                  minperm = 99,
                  all.perms = NULL,
                  observed = FALSE)
{
    out <- list(within = within, plots = plots, blocks = blocks,
                nperm = nperm, complete = complete,
                maxperm = maxperm, minperm = minperm,
                all.perms = all.perms, observed = observed,
                name.strata = deparse(substitute(strata)))
    class(out) <- "how"
    out
}
