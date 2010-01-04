`Blocks` <- function(type = c("free","series","grid","none"),
                     mirror = FALSE, ncol = NULL, nrow = NULL)
{
    if(missing(type))
        type <- "none"
    else
        type <- match.arg(type)
    out <- list(type = type, mirror = mirror,
                ncol = ncol, nrow = nrow)
    ## keep as list for now
    ##class(out) <- "Blocks"
    return(out)
}
