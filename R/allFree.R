#' Utility functions for complete enumeration of all possible permutations
#'
#' Utility functions to return the set of all permutations under different
#' designs. For most practical applications, such as combining designs that
#' permute blocks and/or observations within blocks, [allPerms()] is required.
#'
#' These utility functions are not designed for casual use. See [allPerms()]
#' for further details.
#'
#' @param n The number of observations.
#' @param v Numeric vector of indices. The default is `seq_len(n)`.
#' @param nperms Numeric; number of possible permutations.
#' @param mirror Logical; should mirroring of permutations be allowed?
#' @param symmetric Logical; for grid permutations, should simultaneous
#'   mirroring in both spatial directions be disallowed?
#' @param nr,nc Integer; number of rows and columns of grid designs.
#' @param control A list describing the permutation design, as returned by
#'   [how()].
#'
#' @returns A matrix of all possible permutations of `n` observations or of
#'   `v`, given the provided options.
#' @author Gavin Simpson
#' @name allUtils
#' @aliases NULL
#' @order 0
NULL

#' @rdname allUtils
#' @order 1
## Modified version of allFree() provided by Doug Bates
## via personal email on 19 Jan 2012
`allFree` <- function(n, v = seq_len(n)) {
    if(n == 1L) return(array(v, c(1L, 1L)))
    do.call(rbind,
            lapply(seq_len(n),
                   function(i) cbind(v[i], allFree(n - 1L, v[-i]))))
}
