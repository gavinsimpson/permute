#' @rdname shuffle
#' @order 2
`permute` <- function(i, n, control, perms = NULL) {
    if (!is.null(perms)) {
        n <- if (!missing(n)) permutationNobs(n) else NULL
        validatePermutationMatrix(perms, n = n, rows = integer())
        i <- validatePermutationRow(i, nrow(perms))
        validatePermutationMatrix(perms, n = n, rows = i)
        return(perms[i, , drop = TRUE])
    }

    complete <- getComplete(control)
    ap <- getAllperms(control)
    perm <- if (complete && !is.null(ap)) {
        ap[i, ]                 # select ith permutation
    } else {
        if (complete) {
            warning("'$all.perms' is NULL, yet '$complete = TRUE'.\nReturning a random permutation.")
        }
        shuffle(n, control)
    }
    perm
}

#' @rdname shuffle
#' @order 3
`permutator` <- function(n, nset, control = how(), check = TRUE,
                         quietly = FALSE, perms = NULL) {
    supplied <- !missing(perms) && !is.null(perms)

    if (supplied) {
        generated <- !missing(n) || !missing(nset) || !missing(control) ||
            !missing(check) || !missing(quietly)
        if (generated) {
            stop("Supply either 'perms' or permutation-generation arguments, not both.")
        }
    } else {
        if (missing(n)) {
            stop("'n' must be supplied when 'perms' is not supplied.")
        }
        perms <- if (missing(nset)) {
            shuffleSet(n, control = control, check = check, quietly = quietly)
        } else {
            shuffleSet(n, nset = nset, control = control, check = check,
                       quietly = quietly)
        }
    }

    validatePermutationMatrix(perms)
    nr <- nrow(perms)
    nc <- ncol(perms)
    i <- 0L

    iterator <- function() {
        if (i >= nr) {
            return(NULL)
        }
        i <<- i + 1L
        perms[i, , drop = TRUE]
    }
    attr(iterator, "nperm") <- nr
    attr(iterator, "n") <- nc
    iterator
}

`validatePermutationRow` <- function(i, nr) {
    if (length(i) != 1L || is.na(i) || !is.numeric(i) ||
        !is.finite(i) || i != floor(i)) {
        stop("'i' must be a single, non-missing integer.")
    }
    if (i < 1 || i > nr) {
        stop("'i' must select a row of 'perms'.")
    }
    as.integer(i)
}

`permutationNobs` <- function(n) {
    if (!((is.numeric(n) || is.integer(n)) && length(n) == 1L)) {
        n <- nobs(n)
    }
    if (length(n) != 1L || is.na(n) || !is.numeric(n) ||
        !is.finite(n) || n != floor(n) || n < 1) {
        stop("'n' must specify a positive integer number of observations.")
    }
    as.integer(n)
}

`validatePermutationMatrix` <- function(perms, n = NULL, rows = NULL) {
    if (inherits(perms, "permutations")) {
        stop("'perms' must contain permutation indices, not permuted values.")
    }
    if (!is.matrix(perms) || !is.numeric(perms)) {
        stop("'perms' must be a numeric matrix of permutation indices.")
    }
    nc <- ncol(perms)
    if (nc < 1L) {
        stop("'perms' must have at least one column.")
    }
    if (!is.null(n) && n != nc) {
        stop("'n' does not match the number of columns in 'perms'.")
    }

    if (is.null(rows)) {
        rows <- seq_len(nrow(perms))
    }
    for (i in rows) {
        perm <- perms[i, , drop = TRUE]
        invalid <- anyNA(perm) || any(!is.finite(perm)) ||
            any(perm != floor(perm)) || any(perm < 1 | perm > nc) ||
            anyDuplicated(perm)
        if (invalid) {
            stop(paste0("Each row of 'perms' must contain every index from 1 ",
                        "to ncol(perms) exactly once."))
        }
    }
    invisible(perms)
}
