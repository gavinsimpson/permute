`how` <- function(within = Within(),
    plots = Plots(),
    blocks = NULL,
    nperm = 199,
    complete = FALSE,
    maxperm = 9999,
    minperm = 5040,
    all.perms = NULL,
    make = TRUE,
    observed = FALSE,
    data = NULL
  ) {
  # handle blocks - this can be a formula or something we can convert to a
  # character
  is_fml <- inherits(blocks, "formula")
  ## what not to eval later
  no_eval <- c("within", "plots")
  if (is_fml) {
    if (is.null(data)) {
      stop("'data' must be supplied if 'blocks' is a formula")
    }
    # copy the call
    mf <- match.call(expand.dots = FALSE)
    take <- match(c("blocks", "data"), names(mf), 0L)
    mf <- mf[c(1L, take)]
    names(mf)[2] <- "formula"
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    blocks.name <- all.vars(blocks)
    blocks <- mf[[blocks.name]]
    # add blocks to list of things we don't evaluate
    no_eval <- c(no_eval, c("blocks"))
  } else {
    blocks.name <- deparse(substitute(blocks))
    ## blocks should also be a factor - coerce
    if (!is.null(blocks)) {
      blocks <- as.factor(blocks)
    }
  }
  ## process the call to make it standalone
  .call <- match.call()
  if (length(.call) > 1L) {
    .ll <- as.list(.call[-1])
    args <- names(.call)[-1]
    ## evaluate arguments other than within and plots
    ## those handled in their respective functions or above
    for (i in args[!args %in% no_eval]) {
      if (!is.null(.ll[[i]])) {
        .ll[[i]] <- eval(.ll[[i]], parent.frame())
      }
    }
  }

  out <- list(within = within, plots = plots, blocks = blocks,
    nperm = nperm, complete = complete,
    maxperm = maxperm, minperm = minperm,
    all.perms = all.perms, make = make,
    observed = observed,
    blocks.name = blocks.name,
    data = data)

  ## process within and plots separately
  if (length(.call) > 1L && "within" %in% args) {
    .ll[["within"]] <- getCall(within)
  }
  if (length(.call) > 1L && "plots" %in% args) {
    .ll[["plots"]] <- getCall(plots)
  }
  # and also blocks if it was a formula
  #if (is_fml) {
  #  .ll["blocks"] <- blocks
  #}

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
