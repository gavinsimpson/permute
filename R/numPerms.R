#' Number of possible permutations for a given object
#'
#' `numPerms` calculates the maximum number of permutations possible under
#' the current permutation scheme.
#'
#' Function `numPerms` returns the number of permutations for the passed
#' `object` and the selected permutation scheme. `object` can be one
#' of a data frame, matrix, an object for which a scores method exists, or a
#' numeric or integer vector. In the case of a numeric or integer vector, a
#' vector of length 1 can be used and it will be expanded to a vector of length
#' `object` (i.e., `1:object`) before computing the number of
#' permutations. As such, `object` can be the number of observations not
#' just the object containing the observations.
#'
#' For `Plots(type = "partition")`, if the group sizes are \eqn{n_1,
#' \ldots, n_K}, the number of distinct assignments is \eqn{n! / \prod_k n_k!}.
#' With blocks, this quantity is calculated within each block and the results
#' are multiplied.
#'
#' @param object any object handled by [stats::nobs()].
#' @param control a list of control values describing properties of the
#' permutation design, as returned by a call to [how()].
#' @param check logical; should `control` be checked for problems?
#' @returns The (numeric) number of possible permutations of observations in
#' `object`.
#' @note In general, mirroring `"series"` designs doubles the number of
#' permutations and mirroring `"grid"` designs can quadruple it (within
#' levels of strata if present). For grids with `symmetric = TRUE`, at most
#' three orientations are included because simultaneous row and column
#' mirroring is disallowed. Reflections of grid axes containing one or two
#' cells are equivalent to toroidal shifts and do not add distinct
#' permutations.
#'
#' Mirroring does not double the number of series permutations when the series
#' contains only two observations.
#'
#' For example, with 2 observations there are 2 permutations for
#' `"series"` designs:
#'
#' 1. 1-2, and
#' 2. 2-1.
#'
#' If these two permutations were mirrored, we would have:
#'
#' 1. 2-1, and
#' 2. 1-2.
#'
#' It is immediately clear that this is the same set of permutations without
#' mirroring (if one reorders the rows).
#' @author Gavin Simpson
#' @seealso [shuffle()] and [how()]. Additional
#' [stats::nobs()] methods are provided; see [nobs-methods].
#' @examples
#'
#' ## permutation design --- see ?how
#' ctrl <- how() ## defaults to freely exchangeable
#'
#' ## vector input
#' v <- 1:10
#' (obs <- nobs(v))
#' numPerms(v, control = ctrl)
#'
#' ## integer input
#' len <- length(v)
#' (obs <- nobs(len))
#' numPerms(len, control = ctrl)
#'
#' ## new design, objects are a time series
#' ctrl <- how(within = Within(type = "series"))
#' numPerms(v, control = ctrl)
#' ## number of permutations possible drastically reduced...
#' ## ...turn on mirroring
#' ctrl <- how(within = Within(type = "series", mirror = TRUE))
#' numPerms(v, control = ctrl)
#'
#' ## Try blocking --- 2 groups of 5
#' bl <- numPerms(v, control = how(blocks = gl(2,5)))
#' bl
#'
#' ## should be same as
#' pl <- numPerms(v, control = how(plots = Plots(strata = gl(2,5))))
#' pl
#' stopifnot(all.equal(bl, pl))
#'
#' ## Distinct assignments to groups of sizes 3 and 2
#' groups <- factor(c("a", "a", "a", "b", "b"))
#' ctrl <- how(plots = Plots(groups, type = "partition"))
#' numPerms(length(groups), control = ctrl) ## 10
#'
`numPerms` <- function(object, control = how(), check = TRUE) {
  ## constant holding types where something is permuted
  TYPES <- c("free","grid","series","none")

  ## expand object if a numeric or integer vector of length 1
  if((is.numeric(object) || is.integer(object)) &&
     (length(object) == 1))
    object <- seq_len(object)
  ## number of observations in data
  n <- nobs(object)
  checkPartitionDesign(control, n)

  # run check here unless instructed not to
  if (isTRUE(check)) {
    chk <- check(
      object = object,
      control = control,
      quietly = TRUE,
      num_perms = FALSE
    )
    control <- chk$control
  }

  ## any strata to permute within / blocking?
  BLOCKS <- getStrata(control, which = "blocks")
  PSTRATA <- getStrata(control, which = "plots")
  typeP <- getType(control, which = "plots")
  typeW <- getType(control, which = "within")

  ## mirroring?
  mirrorP <- getMirror(control, which = "plots")
  mirrorW <- getMirror(control, which = "within")
  symmetricP <- getSymmetric(control, which = "plots")
  symmetricW <- getSymmetric(control, which = "within")

  ## constant - i.e. same perm within each plot?
  constantW <- getConstant(control)

  ## grid dimensions
  dimW <- getDim(control, which = "within")
  dimP <- getDim(control, which = "plots")

  if (!is.null(PSTRATA))
    tab <- table(PSTRATA)

  ## the various designs allowed imply multipliers to number of samples
  ## for the restricted permutations

  mult.p <- mult.wi <- 1

  ## within types
  if(typeW == "series") {
    mult.wi <- 2
    if(n == 2L)
      mult.wi <- 1
  } else if(typeW == "grid") {
    mult.wi <- gridOrientationMultiplier(dimW[1L], dimW[2L],
                                         mirror = TRUE,
                                         symmetric = symmetricW)
  }
  ## plot-level types
  if(typeP == "series") {
    mult.p <- 2
    if(length(tab) == 2L)
      mult.p <- 1
  } else if(typeP == "grid") {
    mult.p <- gridOrientationMultiplier(dimP[1L], dimP[2L],
                                        mirror = TRUE,
                                        symmetric = symmetricP)
  }

  ## within
  ## another check - shouldn't this be moved? FIXME
  if(!typeW %in% TYPES) {
    stop("Ambiguous permutation type in 'control$within$type'")
  }

  ## calculate the number of possible permutations

  ## Compute number of permutations for each block
  if(is.null(BLOCKS))
      BLOCKS <- factor(rep(1, n))

  ## split an index vector
  indv <- seq_len(n)
  spl <- split(indv, BLOCKS)

  ## loop over the components of spl & apply doNumPerms
  np <- sapply(spl, doNumPerms, mult.p, mult.wi, typeP, typeW, PSTRATA,
               mirrorP, mirrorW, constantW)

  ## multiply up n perms per block
  round(prod(np), 0)
}

`doNumPerms` <- function(obs, mult.p, mult.wi, typeP, typeW, PSTRATA,
                         mirrorP, mirrorW, constantW) {
    n <- nobs(obs) ## obs is index vector for object, split by blocks

    if(!is.null(PSTRATA)) {
        ## take only the PSTRATA needed for this block, drop unused levels
        PSTRATA <- droplevels(PSTRATA[obs])

        ## need only those strata for the current block. As obs is the index
        ## vector, split by block, this now gives nobs per plot strata
        tab <- table(PSTRATA)
        same.n <- length(unitab <- unique(tab))
    }

    ## plots
    num.p <- if(typeP == "partition") {
        numPartitions(PSTRATA)
    } else if(typeP == "free") {
        exp(lfactorial(length(levels(PSTRATA))))
    } else if(typeP %in% c("series", "grid")) {
        if(isTRUE(mirrorP)) {
            mult.p * length(tab)
        } else {
            length(tab)
        }
    } else {
        1
    }

    num.wi <- if(typeW == "none") {
        ## no within permutations. note we multiply num.p by this
        ## values so it is 1 not 0!!
        1
    } else if(typeW == "free") {
        if(!is.null(PSTRATA)) {
            if(constantW) {
                factorial(tab[1])
            } else {
                prod(factorial(tab))
            }
        } else {
            exp(lfactorial(n))
        }
    } else {
        if(!is.null(PSTRATA)) {
            if(same.n > 1) {
                multi <- rep(2, length = length(tab))
                multi[which(tab == 2)] <- 1
                if(mirrorW) {
                    prod(multi * tab)
                } else {
                    prod(tab)
                }
            } else {
                if(mirrorW) {
                    if(constantW) {
                        mult.wi * unitab[1]
                    } else {
                        prod(mult.wi * tab)
                    }
                } else {
                    if(constantW) {
                        unitab[1] ## FIXME: unitab[1]?? (unique(tab)[1])
                    } else {
                        prod(tab)
                    }
                }
            }
        } else {
            if(mirrorW)
                mult.wi * n
            else
                n
        }
    }

    ## return, trying to avoid floating point issues
    zapsmall(num.p) * zapsmall(num.wi)
}
