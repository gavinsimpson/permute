#' @rdname allUtils
#' @order 3
`allGrid` <- function(n, nperms, nr, nc, mirror, symmetric = FALSE)
{
    stopifnot(n == nr * nc)
    orientations <- gridOrientations(nr, nc, mirror, symmetric)
    expected <- n * nrow(orientations)
    if(!isTRUE(all.equal(nperms, expected)))
        stop("'nperms' does not match the number of grid permutations")
    X <- matrix(nrow = expected, ncol = n)
    idx <- 1L
    for(k in seq_len(nrow(orientations))) {
        for(i in seq_len(nr)) {
            for(j in seq_len(nc)) {
                ir <- seq(i, length = nr) %% nr
                ic <- seq(j, length = nc) %% nc
                if(orientations[k, "row"])
                    ir <- rev(ir)
                if(orientations[k, "col"])
                    ic <- rev(ic)
                X[idx, ] <- rep(ic, each = nr) * nr +
                    rep(ir, length.out = nr * nc) + 1L
                idx <- idx + 1L
            }
        }
    }
    X
}
