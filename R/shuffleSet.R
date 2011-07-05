`shuffleSet` <- function(n, nset = 1, control = permControl()) {
    Set <- matrix(nrow = nset, ncol = n)
    ## If no strata then permute all samples using stated scheme
    WI <- getWithin(control)
    if(is.null(getStrata(control))) {
        Args <- switch(WI$type,
                       "free" = list(x = n, size = n),
                       "series" = list(x = seq_len(n), mirror = WI$mirror),
                       "grid" = list(nrow = WI$nrow, ncol = WI$ncol,
                       mirror = WI$mirror))
        FUN <- switch(WI$type,
                      "free" = shuffleFree,
                      "series" = shuffleSeries,
                      "grid" = shuffleGrid)
        if(WI$type == "none") {
            Set <- rep(seq_len(n), each = nset)
        } else {
            for(i in seq_len(nset)) {
                Set[i,] <- do.call(FUN, Args)
            }
        }
    } else {
        .NotYetImplemented()
    }
    Set
}
