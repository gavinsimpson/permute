##' Number of possible permutations for a given object
##' 
##' \code{numPerms} calculates the maximum number of permutations possible
##' under the current permutation scheme.
##' 
##' Function \code{numPerms} returns the number of permutations for the passed
##' \code{object} and the selected permutation scheme. \code{object} can be one
##' of a data frame, matrix, an object for which a scores method exists, or a
##' numeric or integer vector. In the case of a numeric or integer vector, a
##' vector of length 1 can be used and it will be expanded to a vector of
##' length \code{object} (i.e., \code{1:object}) before computing the number of
##' permutations. As such, \code{object} can be the number of observations not
##' just the object containing the observations.
##' 
##' @usage numPerms(object, control = how())
##' @param object any object handled by \code{\link{nobs}}.
##' @param control a list of control values describing properties of the
##' permutation design, as returned by a call to \code{\link{how}}.
##' @return The (numeric) number of possible permutations of observations in
##' \code{object}.
##' @note In general, mirroring \code{"series"} or \code{"grid"} designs
##' doubles or quadruples, respectively, the number of permutations without
##' mirroring (within levels of strata if present). This is \strong{not} true
##' in two special cases: \enumerate{ \item In \code{"grid"} designs where the
##' number of columns is equal to 2, and \item In \code{"series"} designs where
##' the number of observations in a series is equal to 2.  }
##' 
##' For example, with 2 observations there are 2 permutations for
##' \code{"series"} designs: \enumerate{ \item 1-2, and \item 2-1.  } If these
##' two permutations were mirrored, we would have: \enumerate{ \item 2-1, and
##' \item 1-2.  } It is immediately clear that this is the same set of
##' permutations without mirroring (if one reorders the rows). A similar
##' situation arises in \code{"grid"} designs where the number of
##' \strong{columns} per \emph{grid} is equal to 2. Note that the number of
##' rows per \emph{grid} is not an issue here.
##' @author Gavin Simpson
##' @seealso \code{\link{shuffle}} and \code{\link{how}}. Additional
##' \code{\link{nobs}} methods are provide, see \code{\link{nobs-methods}}.
##' @examples
##' 
##' ## permutation design --- see ?how
##' ctrl <- how() ## defaults to freely exchangeable
##' 
##' ## vector input
##' v <- 1:10
##' (obs <- nobs(v))
##' numPerms(v, control = ctrl)
##' 
##' ## integer input
##' len <- length(v)
##' (obs <- nobs(len))
##' numPerms(len, control = ctrl)
##' 
##' ## new design, objects are a time series
##' ctrl <- how(within = Within(type = "series"))
##' numPerms(v, control = ctrl)
##' ## number of permutations possible drastically reduced...
##' ## ...turn on mirroring
##' ctrl <- how(within = Within(type = "series", mirror = TRUE))
##' numPerms(v, control = ctrl)
##' 
##' ## Try blocking --- 2 groups of 5
##' bl <- numPerms(v, control = how(blocks = gl(2,5)))
##' bl
##' 
##' ## should be same as
##' pl <- numPerms(v, control = how(plots = Plots(strata = gl(2,5))))
##' pl
##' stopifnot(all.equal(bl, pl))
##' 
##' @export numPerms
`numPerms` <- function(object, control = how()) {
  ## constant holding types where something is permuted
  TYPES <- c("free","grid","series","none")

  ## expand object if a numeric or integer vector of length 1
  if((is.numeric(object) || is.integer(object)) &&
     (length(object) == 1))
    object <- seq_len(object)
  ## number of observations in data
  n <- nobs(object)

  ## get the permutation levels from control
  WI <- getWithin(control)
  PL <- getPlots(control)
  BL <- getBlocks(control)

  ## any strata to permute within / blocking?
  BLOCKS <- getStrata(control, which = "blocks")
  PSTRATA <- getStrata(control, which = "plots")
  typeP <- getType(control, which = "plots")
  typeW <- getType(control, which = "within")

  ## mirroring?
  mirrorP <- getMirror(control, which = "plots")
  mirrorW <- getMirror(control, which = "within")

  ## constant - i.e. same perm within each plot?
  constantW <- getConstant(control)

  ## grid dimensions
  colW <- getCol(control, which = "within")
  colP <- getRow(control, which = "plots")

  ## Some checks; i) Plot strata must be of same size when permuting strata
  ##                 or having the same constant permutation within strata
  ##             ii) In grid designs, grids must be of the same size for all
  ##                 strata
  ##
  ## FIXME - this probably should be in check()!
  if(!is.null(PSTRATA)) {
    tab <- table(PSTRATA)
    same.n <- length(unique(tab))
    if((typeP != "none" || isTRUE(constantW)) && same.n > 1) {
      stop("All levels of strata must have same number of samples for chosen scheme")
    }
    if(typeP == "grid" && same.n > 1) {
      stop("Unbalanced grid designs are not supported")
    }
  }

  ## the various designs allowed imply multipliers to number of samples
  ## for the restricted permutations

  mult.p <- mult.wi <- 1

  ## within types
  if(typeW %in% c("series","grid")) {
    mult.wi <- 2
    if(isTRUE(all.equal(typeW, "grid")) && !is.null(colW) && colW > 2) {
      mult.wi <- 4
    } else {
      if(isTRUE(all.equal(n, 2)))
        mult.wi <- 1
    }
  }
  ## plot-level types
  if(typeP %in% c("series","grid")) {
    mult.p <- 2
    if(isTRUE(all.equal(typeP, "grid")) && !is.null(colP) && colP > 2) {
      mult.p <- 4
    } else {
      if(isTRUE(all.equal(n, 2)))
        mult.p <- 1
    }
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
  prod(np)
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
    num.p <- if(isTRUE(all.equal(typeP, "free"))) {
        exp(lfactorial(length(levels(PSTRATA))))
    } else if(typeP %in% c("series", "grid")) {
        if(isTRUE(mirrorP)) {
            mult.p * n
        } else {
            n
        }
    } else {
        1
    }

    num.wi <- if(isTRUE(all.equal(typeW, "none"))) {
        ## no within permutations. note we multiply num.p by this
        ## values so it is 1 not 0!!
        1
    } else if(isTRUE(all.equal(typeW, "free"))) {
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
                    if(constantW)
                        mult.wi * unitab
                    else
                        prod(mult.wi * tab)
                } else {
                    if(constantW)
                        unitab ## FIXME: unitab[1]?? (unique(tab)[1])
                    else
                        prod(tab)
                }
            }
        } else {
            if(mirrorW)
                mult.wi * n
            else
                n
        }
    }

    ## return
    num.p * num.wi
}
