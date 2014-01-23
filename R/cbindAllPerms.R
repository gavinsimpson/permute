##' @title Replicate and cbind all block-level permutations
##' @param x a list whose compontents are the set of all permutations
##' at the block level
##' @return a matrix
##' @author Gavin L. Simpson
`cbindAllPerms` <- function(x) {
    nb <- length(x) ## number of blocks

    ## prepares nb seqence vectors 1:`obs in block` for expand.grid
    rowind <- do.call(expand.grid, lapply(x, function(i) seq_len(nrow(i))))

    ## contains row indices for each block, but 1st block varies fastest
    ## and allPerms() traditionally had nth block varying fastest, so
    ## reverse order of columns. drop ensures this work if only 1 block.
    rowind <- rowind[, seq.int(nb, 1), drop = FALSE]

    ## index elements of x using the row indices - gives a list to cbind
    ## next. sapply() over-simplifies to wrong dimensions so not used
    out <- lapply(seq_len(nb), function(i, m, ind) m[[i]][ind[, i] ,],
                  m = x, ind = rowind)
    do.call(cbind, out) ## returns
}
