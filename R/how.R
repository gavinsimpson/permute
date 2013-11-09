`how` <- function(within = Within(),
                  plots = Plots(),
                  blocks = NULL,
                  nperm = 199,
                  complete = FALSE,
                  maxperm = 9999,
                  minperm = 99,
                  all.perms = NULL,
                  make = TRUE,
                  observed = FALSE) {
    out <- list(within = within, plots = plots, blocks = blocks,
                nperm = nperm, complete = complete,
                maxperm = maxperm, minperm = minperm,
                all.perms = all.perms, make = make,
                observed = observed,
                blocks.name = deparse(substitute(blocks)),
                call = match.call())
    class(out) <- "how"
    out
}
