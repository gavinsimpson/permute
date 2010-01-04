`Within` <- function(type = c("free","series","grid","none"),
                     constant = FALSE, mirror = FALSE,
                     ncol = NULL, nrow = NULL)
{
    if(missing(type))
        type <- "free"
    else
        type <- match.arg(type)
    out <- list(type = type, constant = constant, mirror = mirror,
                ncol = ncol, nrow = nrow)
    ## keep as default list for now
    ##class(out) <- "Within"
    return(out)
}
