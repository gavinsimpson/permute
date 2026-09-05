#' How to define a permutation design?
#'
#' Utility functions to describe unrestricted and restricted permutation
#' designs for time series, line transects, spatial grids and blocking factors.
#'
#' `shuffle` can generate permutations for a wide range of restricted
#' permutation schemes. A small selection of the available combinations of
#' options is provided in the Examples section below.
#'
#' Argument type controls how samples are actually permuted; `"free"`
#' indicates randomization, `"series"` indicates permutation via cyclic
#' shifts (suitable for evenly-spaced line transect or time series data),
#' `"grid"` indicates permutation via toroidal shifts (suitable for
#' samples on a regular grid), and `"none"` indicates no permutation of
#' samples. See the package vignette (`browseVignettes("permute")`) for
#' additional information on each of these types of permutation.
#'
#' `Plots(type = "partition")` randomly assigns observations to the
#' labelled groups supplied in `strata`, retaining the observed number
#' assigned to each group. Permutations that differ only by reordering
#' observations carrying the same group label are omitted. If `within` is
#' not supplied, `how` uses `Within(type = "none")` for this design.
#' Supplying any other within-plot type is an error.
#'
#' Argument `mirror` determines whether grid or series permutations can be
#' mirrored. Consider the sequence 1,2,3,4. The relationship between
#' consecutive observations is preserved if we reverse the sequence to 4,3,2,1.
#' If there is no inherent direction in your experimental design, mirrored
#' permutations can be considered part of the Null model, and as such increase
#' the number of possible permutations. The default is to not use mirroring so
#' you must explicitly turn this on using `mirror = TRUE` in `how`.
#'
#' To permute plots rather than the observations within plots (the levels of
#' `strata`), use `Within(type = "none")` and `Plots(type = foo)`, where
#' `foo` is how you want the plots to be permuted. However,
#' note that the number of observations within each plot **must** be
#' equal!
#'
#' For some experiments, such as BACI designs, one might wish to use the same
#' permutation within each plot. This is controlled by argument
#' `constant`. If `constant = TRUE` then the same permutation will be
#' generated for each level of `strata`. The default is `constant = FALSE`.
#'
#' @aliases how print.how Blocks Within Plots
#' @param within,plots,blocks Permutation designs for samples within the levels
#' of `plots` (`within`), permutation of `plots` themselves, or
#' for the definition of blocking structures which further restrict
#' permutations (`blocks`). `within` and `plots` each require a
#' named list as produced by `Within` and `Plots` respectively.
#' `blocks` takes a factor (or an object coercible to a factor via
#' `as.factor`), the levels of which define the blocking structure.
#' @param nperm numeric; the number of permutations.
#' @param complete logical; should complete enumeration of all permutations be
#' performed?
#' @param type character; the type of permutations required. One of
#' `"free"`, `"series"`, `"grid"`, `"none"`, or, for
#' `Plots`, `"partition"`. See Details.
#' @param maxperm numeric; the maximum number of permutations to perform.
#' Currently unused.
#' @param minperm numeric; the lower limit to the number of possible
#' permutations at which complete enumeration is performed. When `nperm`
#' is lower than `minperm`, sampling is performed from the set of complete
#' permutations to avoid duplicate permutations. See argument `complete`
#' and Details, below.
#' @param all.perms an object of class `allPerms`, the result of a call to
#' [allPerms()].
#' @param make logical; should `check` generate all possible permutations?
#' Useful if want to check permutation design but not produce the matrix of all
#' permutations, or to circumvent the heuristics governing when complete
#' enumeration is activated.
#' @param observed logical; should the observed permutation be returned as part
#' of the set of all permutations? Default is `FALSE` to facilitate usage
#' in higher level functions.
#' @param constant logical; should the same permutation be used within each
#' level of strata? If `FALSE` a separate, possibly restricted,
#' permutation is produced for each level of `strata`.
#' @param mirror logical; should mirroring of sequences be allowed?
#' @param ncol,nrow numeric; the number of columns and rows of samples in the
#' spatial grid respectively.
#' @param strata A factor, or an object that can be coerced to a factor via
#' `as.factor`, specifying the strata for permutation.
#' @returns For `how` a list with components for each of the possible
#' arguments.
#' @author Gavin Simpson
#' @seealso [shuffle()] and [shuffleSet()] for permuting
#' from a design, and [check()], a utility function for checking
#' permutation design described by `how`.
#' @references `shuffle()` is modelled after the permutation schemes of
#' Canoco 3.1 (ter Braak, 1990); see also Besag & Clifford (1989).
#'
#' Besag, J. and Clifford, P. (1989) Generalized Monte Carlo significance
#' tests. *Biometrika* **76**; 633--642.
#'
#' ter Braak, C. J. F. (1990). *Update notes: CANOCO version 3.1*.
#' Wageningen: Agricultural Mathematics Group. (UR).
#' @keywords utils
#' @importFrom stats getCall nobs runif update
#' @order 1
#' @examples
#'
#' ## Set up factors for the Plots and Blocks
#' plts <- gl(4, 10) ## 4 Plots of 10 samples each
#' blks <- gl(2, 20) ## 2 Blocks of 20 samples each
#'
#' ## permutation design
#' h1 <- how(within = Within(type = "series", mirror = TRUE),
#'           plots = Plots(strata = plts, type = "series"),
#'           blocks = blks)
#'
#' ## The design can be updated...
#' ## ... remove the blocking:
#' update(h1, blocks = NULL)
#'
#' ## ... or switch the type of shuffling at a level:
#' #update(h1, plots = update(getPlots(h1), type = "none"))
#' plots2 <- update(getPlots(h1), type = "none")
#' update(h1, plots = plots2)
#'
#' ## Random assignments to groups of fixed size
#' groups <- factor(c("a", "a", "a", "b", "b"))
#' (h2 <- how(plots = Plots(strata = groups, type = "partition")))
#' shuffle(length(groups), control = h2)
#'
`how` <- function(within = Within(),
                  plots = Plots(),
                  blocks = NULL,
                  nperm = 199,
                  complete = FALSE,
                  maxperm = 9999,
                  minperm = 5040,
                  all.perms = NULL,
                  make = TRUE,
                  observed = FALSE) {

    ## A partition permutation already permutes the observations by
    ## assigning them to groups. If within was not explicitly supplied,
    ## do not apply a second permutation within those groups.
    if (missing(within) && identical(getType(plots), "partition")) {
        within <- Within(type = "none")
    }

    blocks.name <- deparse(substitute(blocks))
    ## blocks should also be a factor - coerce
    if(!is.null(blocks)) {
        blocks <- as.factor(blocks)
    }

    ## process the call to make it standalone
    .call <- match.call()
    if (length(.call) > 1L) {
        .ll <- as.list(.call[-1])
        args <- names(.call)[-1]
        ## evaluate arguments other than within and plots
        ## those handled in their respective functions
        for (i in args[!args %in% c("within", "plots")]) {
            if(!is.null(.ll[[i]])) {
                .ll[[i]] <- eval(.ll[[i]], parent.frame())
            }
        }
    }

    out <- list(
      within = within,
      plots = plots,
      blocks = blocks,
      nperm = nperm,
      complete = complete,
      maxperm = maxperm,
      minperm = minperm,
      all.perms = all.perms,
      make = make,
      observed = observed,
      blocks.name = blocks.name
    )

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
