#' Permute a vector of data
#'
#' `permutations()` generates permutations from a specified design and applies
#' them to the values in a supplied vector. Each row of the returned object is
#' one permutation of `x`.
#'
#' By default, the permutation design is checked in the same way as for
#' [shuffleSet()]. Consequently, the number of rows returned can differ from
#' `nset` when the design requests complete enumeration or when there are few
#' possible permutations. Set `check = FALSE` to always request exactly
#' `nset` permutations, which need not be unique.
#'
#' @param x A non-empty, one-dimensional atomic vector to permute. Factors are
#'   supported and are returned using their character labels.
#' @param nset Numeric; the number of permutations to generate. If missing,
#'   the number is taken from `control`.
#' @param control An object of class `"how"` describing a valid permutation
#'   design.
#' @param check Logical; should the permutation design be checked by [check()]?
#' @param quietly Logical; should messages from checking the design be
#'   suppressed?
#' @param ... Arguments passed to other methods.
#'
#' @returns An object of class `"permutations"`, inheriting from `"matrix"`.
#'   Rows are permutations and columns correspond to the elements of `x`. The
#'   object has `control` and `seed` attributes containing the checked design
#'   and the random-number seed at the start of permutation generation.
#'
#'   `as.matrix()` returns an ordinary matrix with the permutation metadata
#'   removed.
#' @seealso [shuffleSet()] for generating permutation indices and [how()] for
#'   specifying permutation designs.
#' @export
#' @examples
#' \dontshow{suppressWarnings(RNGversion("3.5.0"))}
#' set.seed(1)
#' permutations(letters[1:5], nset = 3, check = FALSE)
#'
#' ## Restricted permutations of data from a time series
#' control <- how(within = Within(type = "series"))
#' permutations(c(10, 20, 30, 40, 50), nset = 3,
#'              control = control, check = FALSE)
`permutations` <- function(x, nset, control = how(), check = TRUE,
                           quietly = FALSE) {
    if (!is.atomic(x) || length(x) == 0L || !is.null(dim(x))) {
        stop("'x' must be a non-empty, one-dimensional atomic vector.")
    }
    if (is.object(x) && !is.factor(x)) {
        stop("'x' must be an unclassed atomic vector or a factor.")
    }

    if (missing(nset)) {
        indices <- shuffleSet(length(x), control = control, check = check,
                              quietly = quietly)
    } else {
        indices <- shuffleSet(length(x), nset = nset, control = control,
                              check = check, quietly = quietly)
    }

    if (is.factor(x)) {
        x <- as.character(x)
    }
    out <- matrix(x[indices], nrow = nrow(indices), ncol = ncol(indices))
    attr(out, "seed") <- attr(indices, "seed")
    attr(out, "control") <- attr(indices, "control")
    class(out) <- c("permutations", "matrix")
    out
}

#' @rdname permutations
#' @export
`print.permutations` <- function(x, ...) {
    print.permutationMatrix(x, ...)
    invisible(x)
}

#' @rdname permutations
#' @export
`as.matrix.permutations` <- function(x, ...) {
    attr(x, "seed") <- NULL
    attr(x, "control") <- NULL
    class(x) <- "matrix"
    x
}
