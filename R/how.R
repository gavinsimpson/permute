##' How to define a permutation design?
##'
##' Utility functions to describe unrestricted and restricted permutation
##' designs for time series, line transects, spatial grids and blocking
##' factors.
##'
##' \code{shuffle} can generate permutations for a wide range of restricted
##' permutation schemes. A small selection of the available combinations of
##' options is provided in the Examples section below.
##'
##' Argument \code{mirror} determines whether grid or series permutations can
##' be mirrored. Consider the sequence 1,2,3,4. The relationship between
##' consecutive observations is preserved if we reverse the sequence to
##' 4,3,2,1. If there is no inherent direction in your experimental design,
##' mirrored permutations can be considered part of the Null model, and as such
##' increase the number of possible permutations. The default is to not use
##' mirroring so you must explicitly turn this on using \code{mirror = TRUE} in
##' \code{how}.
##'
##' To permute plots rather than the observations within plots (the levels of
##' \code{strata}), use \code{Within(type = "none")} and \code{Plots(type =
##' foo)}, where \code{foo} is how you want the plots to be permuted. However,
##' note that the number of observations within each plot \strong{must} be
##' equal!
##'
##' For some experiments, such as BACI designs, one might wish to use the same
##' permutation within each plot. This is controlled by argument
##' \code{constant}. If \code{constant = TRUE} then the same permutation will
##' be generated for each level of \code{strata}. The default is \code{constant
##' = FALSE}.
##'
##' @aliases how
##'
##' @param within,plots,blocks Permutation designs for samples within the
##' levels of \code{plots} (\code{within}), permutation of \code{plots}
##' themselves, or for the definition of blocking structures which further
##' restrict permutations (\code{blocks}). \code{within} and \code{plots} each
##' require a named list as produced by \code{Within} and \code{Plots}
##' respectively. \code{blocks} takes a factor (or an object coercible to a
##' factor via \code{as.factor}), the levels of which define the blocking
##' structure.
##' @param nperm numeric; the number of permutations.
##' @param complete logical; should complete enumeration of all permutations be
##' performed?
##' @param type character; the type of permutations required. One of
##' \code{"free"}, \code{"series"}, \code{"grid"} or \code{"none"}. See
##' Details.
##' @param maxperm numeric; the maximum number of permutations to perform.
##' Currently unused.
##' @param minperm numeric; the lower limit to the number of possible
##' permutations at which complete enumeration is performed. See argument
##' \code{complete} and Details, below.
##' @param all.perms an object of class \code{allPerms}, the result of a call
##' to \code{\link{allPerms}}.
##' @param make logical; should \code{check} generate all possible
##' permutations? Useful if want to check permutation design but not produce
##' the matrix of all permutations, or to circumvent the heuristics governing
##' when complete enumeration is activated.
##' @param observed logical; should the observed permutation be returned as
##' part of the set of all permutations? Default is \code{FALSE} to facilitate
##' usage in higher level functions.
##' @param constant logical; should the same permutation be used within each
##' level of strata? If \code{FALSE} a separate, possibly restricted,
##' permutation is produced for each level of \code{strata}.
##' @param mirror logical; should mirroring of sequences be allowed?
##' @param ncol,nrow numeric; the number of columns and rows of samples in the
##' spatial grid respectively.
##' @param strata A factor, or an object that can be coerced to a factor via
##' \code{as.factor}, specifying the strata for permutation.
##' @return For \code{how} a list with components for each of the possible
##' arguments.
##' @author Gavin Simpson
##' @seealso \code{\link{shuffle}} and \code{\link{shuffleSet}} for permuting
##' from a design, and \code{\link{check}}, a utility function for checking
##' permutation design described by \code{how}.
##' @references \code{shuffle()} is modelled after the permutation schemes of
##' Canoco 3.1 (ter Braak, 1990); see also Besag & Clifford (1989).
##'
##' Besag, J. and Clifford, P. (1989) Generalized Monte Carlo significance
##' tests. \emph{Biometrika} \strong{76}; 633--642.
##'
##' ter Braak, C. J. F. (1990). \emph{Update notes: CANOCO version 3.1}.
##' Wageningen: Agricultural Mathematics Group. (UR).
##' @keywords utils
##' @examples
##'
##' ## Set up factors for the Plots and Blocks
##' plts <- gl(4, 10) ## 4 Plots of 10 samples each
##' blks <- gl(2, 20) ## 2 Blocks of 20 samples each
##'
##' ## permutation design
##' h1 <- how(within = Within(type = "series", mirror = TRUE),
##'           plots = Plots(strata = plts, type = "series"),
##'           blocks = blks)
##'
##' ## The design can be updated...
##' ## ... remove the blocking:
##' update(h1, blocks = NULL)
##'
##' ## ... or switch the type of shuffling at a level:
##' #update(h1, plots = update(getPlots(h1), type = "none"))
##' plots2 <- update(getPlots(h1), type = "none")
##' update(h1, plots = plots2)
##'
##' @export how
##' @name how
`how` <- function(within = Within(),
                  plots = Plots(),
                  blocks = NULL,
                  nperm = 199,
                  complete = FALSE,
                  maxperm = 9999,
                  minperm = 99,
                  all.perms = NULL,
                  make = TRUE,
                  observed = FALSE) {

    blocks.name <- deparse(substitute(blocks))
    ## blocks should also be a factor - coerce
    if(!is.null(blocks))
        blocks <- as.factor(blocks)

    ## process the call to make it standalone
    .call <- match.call()
    if (length(.call) > 1L) {
        .ll <- as.list(.call[-1])
        args <- names(.call)[-1]
        ## evaluate arguments other than within and plots
        ## those handled in their respective functions
        for (i in args[!args %in% c("within","plots")]) {
            if(!is.null(.ll[[i]])) {
                .ll[[i]] <- eval(.ll[[i]], parent.frame())
            }
        }
    }

    out <- list(within = within, plots = plots, blocks = blocks,
                nperm = nperm, complete = complete,
                maxperm = maxperm, minperm = minperm,
                all.perms = all.perms, make = make,
                observed = observed,
                blocks.name = blocks.name)

    ## process within and plots separately
    if (length(.call) > 1L && "within" %in% args) {
        .ll[["within"]] <- getCall(within)
    }
    if (length(.call) > 1L && "plots" %in% args) {
        .ll[["plots"]] <- getCall(plots)
    }

    ## finsh off
    if (length(.call) > 1L) {
        .ll <- c(as.list(.call[[1]]), .ll)
        names(.ll) <- names(.call)
        .call <- as.call(.ll)
    }

    out$call <- .call

    class(out) <- "how"
    out
}

##' @rdname how
##'
##' @export
`Plots` <- function(strata = NULL, type = c("none","free","series","grid"),
                    mirror = FALSE, ncol = NULL, nrow = NULL) {

    plots.name <- deparse(substitute(strata))
    ## strata should also be a factor - coerce
    if(!is.null(strata))
        strata <- as.factor(strata)

    type <- match.arg(type)

    ## process the call to make it standalone
    .call <- match.call()
    if (length(.call) > 1L) {
        .ll <- as.list(.call[-1])
        for (i in seq_along(.ll))
            .ll[[i]] <- eval(.ll[[i]], parent.frame())
        .ll <- c(as.list(.call[[1]]), .ll)
        names(.ll) <- names(.call)
        .call <- as.call(.ll)
    }

    out <- list(strata = strata, type = type, mirror = mirror,
                ncol = ncol, nrow = nrow,
                plots.name = plots.name, call = .call)
    class(out) <- "Plots"
    out
}

##' @rdname how
##'
##' @export
`Within` <- function(type = c("free","series","grid","none"),
                     constant = FALSE, mirror = FALSE,
                     ncol = NULL, nrow = NULL) {
    type <- match.arg(type)

    ## process the call to make it standalone
    .call <- match.call()
    if (length(.call) > 1L) {
        .ll <- as.list(.call[-1])
        for (i in seq_along(.ll))
            .ll[[i]] <- eval(.ll[[i]], parent.frame())
        .ll <- c(as.list(.call[[1]]), .ll)
        names(.ll) <- names(.call)
        .call <- as.call(.ll)
    }

    out <- list(type = type, constant = constant, mirror = mirror,
                ncol = ncol, nrow = nrow, call = .call)
    class(out) <- "Within"
    out
}

##' @rdname how
##'
##' @export
`Blocks` <- function(strata = NULL) {
    ## Temporarily drop this here

    out <- list(strata = strata)
    ## keep as list for now
    ##class(out) <- "Blocks"
    out
}

### Extractor, replacement functions for blocks

### Extractor methods
`blocks` <- function(object, ...) {
    UseMethod("blocks")
}

`blocks.default` <- function(object, ...) {
    stop("No default method for 'blocks()'")
}

`blocks.permControl` <- function(object, ...) {
    object$blocks
}

`blocks.how` <- function(object, ...) {
    object$blocks
}

### Replacement methods
`blocks<-` <- function(object, value) {
    UseMethod("setBlocks<-")
}

`blocks<-.default` <- function(object, value) {
    stop("No default method for `setBlocks`")
}

`blocks<-.how` <- function(object, value) {
    object[["blocks.name"]] <- deparse(substitute(value))
    if (!is.null(value))
        value <- as.factor(value)
    object["blocks"] <- list(value)
    object <- fixupCall(object, "blocks", value)
    object
}

`blocks<-.permControl` <- function(object, value) {
    if (!is.null(value))
        value <- as.factor(value)
    object["blocks"] <- list(value)
    object[["blocks.name"]] <- deparse(substitute(value))
    object <- fixupCall(object, "blocks", value)
    object
}
