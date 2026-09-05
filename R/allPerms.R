#' Complete enumeration of all possible permutations
#'
#' `allPerms` is a utility function to return the set of permutations for
#' a given R object and a specified permutation design.
#'
#' Function `allPerms` enumerates all possible permutations for the number
#' of observations and the selected permutation scheme. It has
#' [base::print()] and [base::summary()] methods. `allPerms`
#' returns a matrix containing all possible permutations, possibly containing
#' the observed ordering (if argument `observed` is `TRUE`). The rows
#' of this matrix are the various permutations and the columns reflect the
#' number of samples.
#'
#' With free permutation designs, and restricted permutation schemes with large
#' numbers of observations, there are a potentially huge number of possible
#' permutations of the samples. It would be inefficient, not to mention
#' incredibly time consuming, to enumerate them all. Storing all possible
#' permutations would also become problematic in such cases. To control this
#' and guard against trying to evaluate too large a number of permutations, if
#' the number of possible permutations is larger than
#' `getMaxperm(control)`, `allPerms` exits with an error.
#'
#' The `as.matrix` method sets the `control` and `seed`
#' attributes to `NULL` and removes the `"permutationMatrix"` class,
#' resulting in a standard matrix object.
#'
#' With `Plots(type = "partition")`, the rows enumerate distinct
#' assignments to the labelled groups in `Plots$strata`. If the group
#' sizes are \eqn{n_1, \ldots, n_K}, there are \eqn{n! / \prod_k n_k!} such
#' assignments before optionally removing the observed assignment.
#'
#' @aliases allPerms print.allPerms summary.allPerms print.summary.allPerms
#' @aliases as.matrix.allPerms as.allPerms
#' @param n the number of observations or an 'object' from which the number of
#' observations can be determined via `getNumObs`.
#' @param control a list of control values describing properties of the
#' permutation design, as returned by a call to [how()].
#' @param check logical; should `allPerms` check the design? The default
#' is to check, but this can be skipped, for example if a function checked the
#' design earlier.
#' @param object for `summary.allPerms`, an object of class
#' `"allPerms"`. For `as.allPerms` a matrix or something that can be
#' coerced to a matrix by [base::as.matrix()].
#' @param ... arguments to other methods.
#' @param x an object of class `"allPerms"`, as returned by
#' `allPerms`.
#' @returns For `allPerms`, an object of class `"allPerms"`: a matrix
#' whose rows are the set of all possible permutations for the supplies number
#' of observations and permutation scheme selected. The matrix has two
#' additional attributes `control` and `observed`. Attribute
#' `control` contains the argument `control` (possibly updated via
#' `check`). Attribute `observed` contains argument `observed`.
#' @section Warning: If permuting the strata themselves, a balanced design is
#' required (the same number of observations in each level of `strata`).
#' This does not apply to `Plots(type = "partition")`, which supports
#' unequal group sizes.
#' @author Gavin Simpson
#' @order 1
#' @examples
#'
#' ## allPerms can work with a vector
#' vec <- c(3,4,5)
#' allPerms(vec) ## free permutation
#'
#' ## enumerate all possible permutations for a more complicated
#' ## design
#' fac <- gl(2,6)
#' ctrl <- how(within = Within(type = "grid", mirror = FALSE,
#'                             constant = TRUE, nrow = 3, ncol = 2),
#'             plots = Plots(strata = fac))
#' Nobs <- length(fac)
#' numPerms(seq_len(Nobs), control = ctrl) ## 6
#' (tmp <- allPerms(Nobs, control = update(ctrl, observed = TRUE)))
#' (tmp2 <- allPerms(Nobs, control = ctrl))
#'
#' ## turn on mirroring
#' ##ctrl$within$mirror <- TRUE
#' ctrl <- update(ctrl, within = update(getWithin(ctrl), mirror = TRUE))
#' numPerms(seq_len(Nobs), control = ctrl)
#' (tmp3 <- allPerms(Nobs, control = update(ctrl, observed = TRUE)))
#' (tmp4 <- allPerms(Nobs, control = ctrl))
#'
#' ## prints out details of the permutation scheme as
#' ## well as the matrix of permutations
#' summary(tmp3)
#' summary(tmp4)
#'
`allPerms` <- function(n, control = how(), check = TRUE) {
    ## start
    v <- n
    ## expand n if a numeric or integer vector of length 1
    if((is.numeric(n) || is.integer(n)) && (length(n) == 1))
         v <- seq_len(n)
    ## number of observations in data
    n <- nobs(v)
    checkPartitionDesign(control, n)

    ## get max number of permutations
    nperms <- numPerms(v, control = control, check = check)

    ## sanity check - don't let this run away to infinity
    ## esp with type = "free"
    if(nperms > getMaxperm(control))
        stop("Number of possible permutations too large (> 'maxperm')")

    strataP <- getStrata(control, which = "plots")
    typeW <- getType(control, which = "within")
    typeP <- getType(control, which = "plots")
    BLOCKS <- getBlocks(control)
    dimW <- getDim(control, which = "within")
    mirrorW <- getMirror(control, which = "within")
    symmetricW <- getSymmetric(control, which = "within")
    constantW <- getConstant(control)

    ## give a BLOCKS if non supplied - i.e. one block
    if(is.null(BLOCKS))
        BLOCKS <- factor(rep(1, n))

    ## split v by blocks
    spl <- split(seq_len(n), BLOCKS)
    nb <- length(spl) # number of blocks

    ## result object
    out <- vector(mode = "list", length = nb)

    ## null-out Blocks in control
    control2 <- control
    setBlocks(control2) <- NULL

    ## loop over blocks and return allPerms on each block
    for (i in seq_along(spl)) {
        out[[i]] <-
            doAllPerms(spl[[i]], strataP, typeW, typeP, mirrorW,
                       symmetricW, constantW, dimW, control2)
    }

    ## bind all blocks together, repeating them as required
    out <- cbindAllPerms(out)

    ## Restore the original observation order after combining blocks.
    out[, unlist(spl)] <- out

    if(!(observed <- getObserved(control))) {
        obs.v <- seq_len(n)
        obs.row <- apply(out, 1, function(x, obs.v) all(x == obs.v), obs.v)
        out <- out[!obs.row, , drop = FALSE]
    }
    setNperm(control) <- nrow(out)

    ## as a permutationMatrix we pick up nice print method
    class(out) <- c("allPerms", "permutationMatrix", "matrix")
    attr(out, "control") <- control
    attr(out, "observed") <- observed
    out
}


`doAllPerms` <- function(obs, strataP, typeW, typeP, mirrorW, symmetricW,
                         constantW, dimW, control) {
    n <- length(obs)

    ## Subset plot strata to the observations in this block.
    if (!is.null(strataP)) {
        strataP <- droplevels(strataP[obs])
    }

    ## Keep the block-local strata in the private control copy.
    control$plots$strata <- strataP

    if (typeW != "none") {
        res <- if (is.null(strataP)) {
            allWithin(obs, typeW, mirrorW, symmetricW, dimW, control)
        } else {
            allWithinPlots(obs, strataP, typeW, mirrorW, symmetricW,
                           constantW, dimW, control)
        }
    }

    if (!is.null(strataP) && typeP != "none") {
        res <- combineAllPlots(res, obs, typeW, typeP, constantW, control)
    }

    ## Enumeration helpers can return doubles; indices are always integers.
    storage.mode(res) <- "integer"
    res
}

## Enumerate within-level permutations when no plot strata are present.
`allWithin` <- function(obs, type, mirror, symmetric, dimensions, control) {
    n <- length(obs)
    out <- switch(
        type,
        free = allFree(n),
        series = allSeries(
            n, numPerms(n, control, check = FALSE), mirror
        ),
        grid = allGrid(
            n, numPerms(n, control, check = FALSE),
            dimensions[1L], dimensions[2L], mirror, symmetric
        )
    )
    out[] <- obs[out]
    out
}

## Enumerate within-level permutations separately for each plot stratum.
`allWithinPlots` <- function(obs, strata, type, mirror, symmetric, constant,
                             dimensions, control) {
    sizes <- table(strata)
    unique.sizes <- unique(sizes)
    nplots <- length(sizes)
    within.control <- how(within = getWithin(control))

    if (constant) {
        nperms <- numPerms(unique.sizes, within.control)
        ordering <- switch(
            type,
            free = allFree(unique.sizes),
            series = allSeries(unique.sizes, nperms, mirror),
            grid = allGrid(unique.sizes, nperms, dimensions[1L],
                           dimensions[2L], mirror, symmetric)
        )
        offsets <- seq(0, to = prod(unique.sizes, nplots - 1L),
                       by = unique.sizes)
        out <- lapply(offsets, function(offset) ordering + offset)
        out <- do.call(cbind, out)
    } else if (length(unique.sizes) > 1L) {
        if (type == "grid")
            stop("Unbalanced grid designs are not supported")

        offsets <- c(0, cumsum(sizes)[seq_len(nplots - 1L)])
        out <- vector(mode = "list", length = nplots)
        for (i in seq_along(sizes)) {
            nperms <- numPerms(sizes[i], within.control, check = FALSE)
            ordering <- switch(
                type,
                free = allFree(sizes[i]),
                series = allSeries(sizes[i], nperms, mirror)
            )
            out[[i]] <- ordering + offsets[i]
        }
        out <- cbindAllPerms(out)
    } else {
        nperms <- numPerms(unique.sizes, within.control, check = FALSE)
        ordering <- switch(
            type,
            free = allFree(unique.sizes),
            series = allSeries(unique.sizes, nperms, mirror),
            grid = allGrid(unique.sizes, nperms, dimensions[1L],
                           dimensions[2L], mirror, symmetric)
        )
        offsets <- seq(0, to = prod(unique.sizes, nplots - 1L),
                       by = unique.sizes)
        out <- lapply(offsets, function(offset) ordering + offset)
        out <- cbindAllPerms(out)
    }

    out[] <- obs[out]
    out
}

## Combine within-plot enumerations with every plot-level permutation.
`combineAllPlots` <- function(within.perms, obs, within.type, plot.type,
                              constant, control) {
    n <- length(obs)
    if (within.type == "none") {
        out <- allStrata(n, control = control)
        if (plot.type == "partition")
            out[] <- obs[out]
        return(out)
    }

    plot.control <- how(
        plots = getPlots(control),
        within = Within(type = "none", constant = constant)
    )
    nplot.perms <- numPerms(n, control = plot.control, check = FALSE)
    plot.perms <- allStrata(n, control = plot.control)
    out <- rep(list(within.perms), nplot.perms)
    out <- lapply(
        seq_along(out),
        function(i, permutations, plots) {
            t(apply(permutations[[i]], 1L, function(x) x[plots[i, ]]))
        },
        permutations = out,
        plots = plot.perms
    )
    do.call(rbind, out)
}
