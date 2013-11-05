`Plots` <- function(strata = NULL, type = c("none","free","series","grid"),
                    mirror = FALSE, ncol = NULL, nrow = NULL) {
    type <- match.arg(type)
    out <- list(strata = strata, type = type, mirror = mirror,
                ncol = ncol, nrow = nrow,
                plots.name = deparse(substitute(strata)), call = match.call())
    ## keep as list for now
    ##class(out) <- "Plots"
    out
}
