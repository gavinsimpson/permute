## Convert a one-sided grouping formula to a single factor.
`formulaToFactor` <- function(formula, data, argument) {
    if (!is.data.frame(data)) {
        stop("'data' must be a data frame when '", argument,
             "' is a formula")
    }

    trms <- stats::terms(formula, data = data)
    if (attr(trms, "response") != 0L) {
        stop("'", argument, "' must be a one-sided formula")
    }

    factors <- attr(trms, "factors")
    if (is.null(factors) || length(factors) == 0L || ncol(factors) == 0L) {
        stop("'", argument, "' formula must contain grouping terms")
    }

    active <- rowSums(factors != 0L) > 0L
    if (!any(active)) {
        stop("'", argument, "' formula must contain grouping terms")
    }

    frame <- stats::model.frame(trms, data = data,
                                na.action = stats::na.pass,
                                drop.unused.levels = TRUE)
    variables <- rownames(factors)[active]
    take <- match(variables, names(frame))
    if (anyNA(take)) {
        stop("unable to evaluate all grouping terms in '", argument, "'")
    }
    groups <- frame[take]

    nr <- nrow(frame)
    valid <- vapply(groups, function(x) {
        is.atomic(x) && is.null(dim(x)) && length(x) == nr
    }, logical(1))
    if (!all(valid)) {
        stop("each term in '", argument,
             "' must produce one value per observation")
    }

    groups[] <- lapply(groups, as.factor)
    if (length(groups) == 1L) {
        groups[[1L]]
    } else {
        do.call(interaction, c(groups, list(drop = TRUE)))
    }
}

## Return a compact label for the right-hand side of a grouping formula.
`formulaRhsLabel` <- function(formula) {
    paste(deparse(formula[[2L]]), collapse = " ")
}

## Is an unevaluated expression a direct call to Plots()?
`isPlotsCall` <- function(expr) {
    if (!is.call(expr)) {
        return(FALSE)
    }

    fun <- expr[[1L]]
    if (identical(fun, quote(Plots))) {
        return(TRUE)
    }

    namespace.call <- is.call(fun) && length(fun) == 3L &&
        (identical(fun[[1L]], quote(`::`)) ||
         identical(fun[[1L]], quote(`:::`)))
    namespace.call && identical(fun[[3L]], quote(Plots))
}

## Remove an argument from a matched call while retaining its other attributes.
`dropCallArgument` <- function(call, argument) {
    keep <- names(call) != argument
    as.call(as.list(call)[keep])
}
