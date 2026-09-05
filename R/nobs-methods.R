#' Number of observations in a given object
#'
#' [stats::nobs()] is a generic function that returns the number of
#' observations from a model. permute provides methods for several other types
#' of R object.
#'
#' These methods return the number of observations in numeric, integer,
#' character, or factor vectors, matrices, and data frames.
#'
#' @param object A data frame or matrix, or a numeric, integer, character, or
#'   factor vector.
#' @param ... Arguments passed to other methods.
#'
#' @returns The numeric number of observations in `object`.
#' @author Gavin Simpson
#' @name nobs-methods
#' @rdname nobs
#' @order 0
#' @examples
#' \dontshow{suppressWarnings(RNGversion("3.5.0"))}
#' set.seed(1)
#' ## numeric vector
#' len <- sample(1:10, 1)
#' v <- as.numeric(sample(1:100, len))
#' len
#' obs <- nobs(v)
#' isTRUE(all.equal(len, obs))
#'
#' ## integer
#' len <- sample(1L:10L, 1)
#' obs <- nobs(len)
#' isTRUE(all.equal(len, obs))
NULL

## Add nobs() methods for the supported vector and tabular types.
#' @rdname nobs
#' @order 1
#' @export
`nobs.numeric` <- function(object, ...) {
    length(object)
}

#' @rdname nobs
#' @order 2
#' @export
`nobs.integer` <- function(object, ...) {
    nobs.numeric(object, ...)
}

#' @rdname nobs
#' @order 3
#' @export
`nobs.matrix` <- function(object, ...) {
    NROW(object)
}

#' @rdname nobs
#' @order 4
#' @export
`nobs.data.frame` <- function(object, ...) {
    NROW(object)
}

#' @rdname nobs
#' @order 6
#' @export
`nobs.factor` <- function(object, ...) {
    length(object)
}

#' @rdname nobs
#' @order 5
#' @export
`nobs.character` <- function(object, ...) {
    length(object)
}
