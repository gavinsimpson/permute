`Plots` <- function(strata = NULL, type = c("free","series","grid","none"),
                    mirror = FALSE, ncol = NULL, nrow = NULL) {
    type <- match.arg(type)
    out <- list(strata = strata, type = type, mirror = mirror,
                ncol = ncol, nrow = nrow)
    ## keep as list for now
    ##class(out) <- "Plots"
    out
}
