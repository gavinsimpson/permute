variablesToFrame <- function(variables, data = NULL, label = "variables") {
    if (is.null(variables)) return(NULL)
    if (inherits(variables, "formula")) {
        terms <- stats::terms(variables, data = data)
        if (attr(terms, "response") != 0L || !length(attr(terms, "term.labels"))) {
            stop("'variables' must be a one-sided formula selecting variables.")
        }
        variables <- stats::model.frame(terms, data = data,
                                        na.action = stats::na.pass)
    } else if (is.atomic(variables) && is.null(dim(variables))) {
        referenced <- all.vars(parse(text = label))
        name <- referenced[length(referenced)]
        if (!length(name)) name <- "variables"
        variables <- stats::setNames(data.frame(variables), name)
    }
    if (!is.data.frame(variables) || !ncol(variables) ||
        anyDuplicated(names(variables)) || any(!nzchar(names(variables)))) {
        stop("'variables' must be a named data frame, vector, or one-sided formula.")
    }
    valid <- vapply(variables, function(x) {
        is.atomic(x) && is.null(dim(x)) && (!is.object(x) || is.factor(x))
    }, logical(1))
    if (!all(valid)) stop("each variable must be an atomic vector or factor.")
    as.data.frame(variables)
}

isVariableDesign <- function(control) !is.null(getPlots(control)$variables)

variableLayout <- function(control, n) {
    plots <- getPlots(control)
    if (!identical(getType(plots), "free")) {
        stop("'variables' requires Plots(type = \"free\").")
    }
    if (!identical(getType(control, which = "within"), "none") || getConstant(control)) {
        stop("'variables' requires Within(type = \"none\", constant = FALSE).")
    }
    strata <- getStrata(plots)
    variables <- plots$variables
    if (is.null(strata) || length(strata) != n || anyNA(strata)) {
        stop("variable 'strata' must contain one non-missing plot ID per data row.")
    }
    if (nrow(variables) != n || anyNA(variables)) {
        stop("'variables' must contain one non-missing value per data row in each column.")
    }
    if (n < 1L) stop("a variable design needs at least one data row.")
    first <- which(!duplicated(strata))
    membership <- match(strata, strata[first])
    same <- vapply(variables, function(x) all(x == x[first][membership]), logical(1))
    if (!all(same)) stop("'variables' must be constant within each plot.")
    profiles <- variables[first, , drop = FALSE]
    codes <- lapply(profiles, function(x) factor(match(x, unique(x))))
    profile.id <- do.call(interaction, c(codes, list(drop = TRUE)))
    blocks <- getBlocks(control)
    plot.blocks <- NULL
    if (!is.null(blocks)) {
        if (length(blocks) != n || anyNA(blocks)) {
            stop("'blocks' must contain one non-missing value per data row.")
        }
        if (any(as.character(blocks) != as.character(blocks[first][membership]))) {
            stop("each plot must belong to exactly one block.")
        }
        plot.blocks <- droplevels(blocks[first])
    }
    reduced <- how(
        plots = Plots(strata = factor(profile.id), type = "partition"),
        blocks = plot.blocks, nperm = getNperm(control),
        complete = getComplete(control), maxperm = getMaxperm(control),
        minperm = getMinperm(control), make = getMake(control),
        observed = getObserved(control)
    )
    cached <- getAllperms(control)
    if (!is.null(cached)) {
        if (!inherits(cached, "plotPermutations")) {
            stop("a variable design requires a plotPermutations cache.")
        }
        previous <- attr(cached, "control")
        if (!identical(getPlots(previous)$strata, strata) ||
            !identical(getPlots(previous)$variables, variables) ||
            !identical(getBlocks(previous), blocks) ||
            !identical(getObserved(previous), getObserved(control))) {
            stop("cached permutations do not match this design; clear all.perms.")
        }
        setAllperms(reduced) <- cached$plot.indices
    }
    list(first = first, membership = membership, control = reduced)
}

expandPlotPermutations <- function(indices, layout, control) {
    if (is.matrix(indices)) {
        expanded <- matrix(layout$first[indices[, layout$membership, drop = FALSE]],
                           nrow = nrow(indices), ncol = length(layout$membership))
        setNperm(control) <- nrow(indices)
    } else {
        expanded <- layout$first[indices[layout$membership]]
    }
    out <- list(indices = expanded, plot.indices = indices)
    attr(out, "control") <- control
    attr(out, "seed") <- attr(indices, "seed")
    observed <- isTRUE(attr(indices, "observed"))
    reduced <- attr(indices, "control")
    if (is.matrix(indices) && !is.null(reduced) &&
        !is.null(getAllperms(reduced)) && getObserved(reduced) &&
        nrow(indices) == numPerms(length(layout$first), layout$control)) {
        observed <- TRUE
    }
    attr(out, "observed") <- observed
    class(out) <- "plotPermutations"
    out
}

subsetPlotPermutation <- function(x, i, n = NULL) {
    if (!is.matrix(x$indices)) stop("expected a set of plot permutations.")
    i <- validatePermutationRow(i, nrow(x))
    if (!is.null(n) && n != ncol(x)) stop("'n' does not match the number of data rows.")
    x$indices <- x$indices[i, ]
    x$plot.indices <- x$plot.indices[i, ]
    x
}

#' Extract expanded permutations of plot-level variables
#'
#' `Plots(strata = plot_id, type = "free", variables = ~ A + B)` defines
#' joint permutations of variables that are constant within plots. Both the
#' plot IDs and the variables are supplied at the original data-row level.
#' `variables` accepts a named data frame, an atomic vector, or a one-sided
#' formula evaluated in `data`. Factor levels and numeric precision are retained.
#'
#' The design is reduced to one complete row of variable values per plot.
#' Existing set partition generators permute these rows among plots and omit
#' duplicate arrangements of identical rows. Higher-level blocks restrict
#' permutations among plots within each block. Every plot must be in one block.
#' Balanced and unbalanced plots use the same implementation.
#'
#' `shuffle()`, `shuffleSet()`, `allPerms()`, `permute()`, and `permutator()`
#' return `plotPermutations` objects. `numPerms()` includes the observed
#' arrangement. Enumeration excludes it by default, following
#' `how(observed = FALSE)`. `Within(type = "none")` is required.
#'
#' `permutationIndices()` extracts expanded source data-row indices. Each
#' source plot is represented by its first row. Indices can repeat or omit rows
#' even for balanced plots and even for the observed arrangement. These indices
#' apply jointly to all selected variables, or to the corresponding constraint
#' matrix rows in a consumer that supports constraint permutations.
#'
#' `permutedVariables(x, i)` returns a data frame for row `i` of a permutation
#' set, preserving the original column types. Without `i`, multiple variables
#' return one data frame per draw, or a single data frame for a single draw.
#' For one variable, the convenience result is a vector or matrix, with factors
#' represented by character labels.
#'
#' These objects require explicit extraction. A response permutation matrix
#' consumer cannot interpret the repeated source indices. The accompanying
#' vegan implementation accepts the design through `model = "constraints"`
#' in `anova()` and `adonis2()`. It permutes stored constraint matrix rows and
#' recomputes their centering, weighting and QR decomposition for each draw.
#'
#' Validity requires a permutation distribution that matches the experimental
#' randomization, or a justified exchangeability assumption. Multiple selected
#' variables move jointly. Separate tests of main effects or interactions need
#' appropriate null-specific procedures. Conditioning on nuisance variables
#' alone does not justify free permutations of the constraints.
#'
#' @param x A `plotPermutations` object returned by a variable design.
#' @param i Optional row of a permutation set to extract as a data frame.
#' @param ... Arguments passed to the default print method.
#' @returns `permutationIndices()` returns an index vector for a single draw,
#'   or a matrix with one row per draw and one column per original data row.
#'   `permutedVariables()` returns values as described in Details.
#' @name plotPermutations
#' @examples
#' plot_id <- rep(LETTERS[1:4], c(2, 5, 3, 4))
#' treatment <- rep(c("T", "C", "T", "C"), c(2, 5, 3, 4))
#' h <- how(plots = Plots(plot_id, type = "free", variables = treatment),
#'          observed = TRUE)
#' numPerms(length(plot_id), h) # choose(4, 2) = 6
#' a <- allPerms(length(plot_id), h)
#' permutedVariables(a)
#' sort(rowSums(permutedVariables(a) == "T")) # 5 6 7 7 8 9
#' set.seed(1)
#' treatment[permutationIndices(shuffle(length(plot_id), h))]
NULL

#' @rdname plotPermutations
#' @export
`permutationIndices` <- function(x) {
    if (!inherits(x, "plotPermutations")) stop("'x' must be plotPermutations.")
    x$indices
}

#' @rdname plotPermutations
#' @export
`permutedVariables` <- function(x, i = NULL) {
    indices <- permutationIndices(x)
    variables <- getPlots(attr(x, "control"))$variables
    if (!is.null(i)) {
        i <- validatePermutationRow(i, nrow(indices))
        return(variables[indices[i, ], , drop = FALSE])
    }
    if (!is.matrix(indices)) {
        if (ncol(variables) == 1L) {
            values <- variables[[1L]]
            if (is.factor(values)) values <- as.character(values)
            return(values[indices])
        }
        return(variables[indices, , drop = FALSE])
    }
    if (ncol(variables) == 1L) {
        values <- variables[[1L]]
        if (is.factor(values)) values <- as.character(values)
        return(matrix(values[indices], nrow = nrow(indices), ncol = ncol(indices)))
    }
    lapply(seq_len(nrow(indices)), function(i) variables[indices[i, ], , drop = FALSE])
}

#' @export
#' @noRd
`dim.plotPermutations` <- function(x) dim(x$indices)

#' @rdname plotPermutations
#' @export
`print.plotPermutations` <- function(x, ...) {
    cat("Plot variables expanded to", length(getPlots(attr(x, "control"))$strata),
        "data rows\n")
    print(permutedVariables(x), ...)
    invisible(x)
}

#' @export
#' @noRd
`as.matrix.plotPermutations` <- function(x, ...) {
    stop("Plot variable permutations require constraint recomputation; use permutationIndices() or permutedVariables() explicitly.")
}

#' @export
#' @noRd
`Math.plotPermutations` <- function(x, ...) {
    stop("Plot variable permutations require constraint recomputation. Use permutedVariables() in a refit loop.")
}
