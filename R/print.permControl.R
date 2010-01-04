`print.permControl` <- function(x, ...)
{
    ## only for objects of correct class
    stopifnot(class(x) == "permControl")
    ## set-up the messages we wish to print
    if (!is.null(x$strata)) {
        if(x$blocks$type == "none") {
            msg.perm.strata <- "Strata unpermuted\n"
        } else {
            if(x$blocks$type == "grid") {
                msg.grid.strata <- paste("Strata are a spatial grid of dimension",
                                         x$blocks$nrow, "*",
                                         x$blocks$ncol, "\n")
            }
            msg.perm.strata <- paste("Permutation type:", x$blocks$type, "\n")
            msg.mirror.strata <- paste("Mirrored permutations for Strata?:",
                                       ifelse(x$blocks$mirror, "Yes", "No"), "\n")
        }
        msg.strata <- paste("Permutations are stratified within:", x$name.strata, "\n")
    } else {
        msg.strata <- "Permutations are unstratified\n"
    }
    msg.perm.sample <- paste("Permutation type:", x$within$type, "\n")
    if(x$within$type == "grid")
        msg.grid.sample <- paste("Data are spatial grid(s) of dimension",
                                 x$within$nrow, "*", x$within$ncol, "\n")
    msg.nperm <- paste("No. of permutations:", x$nperm,
                       ifelse(x$complete, "(complete enumeration)", ""),
                       "\n")
    msg.mirror.sample <- paste("Mirrored permutations for Samples?:",
                               ifelse(x$within$mirror, "Yes", "No"), "\n")
    msg.constant <- paste("Use same permutation within strata?:",
                          ifelse(x$within$constant, "Yes", "No"), "\n")
    ## print out the messages
    cat("\n")
    cat(msg.nperm)
    cat("\n**** STRATA ****\n")
    if(exists("msg.strata"))
        cat(msg.strata)
    if(exists("msg.perm.strata"))
        cat(msg.perm.strata)
    if(exists("msg.mirror.strata"))
        cat(msg.mirror.strata)
    if(exists("msg.grid.strata"))
        cat(msg.grid.strata)
    cat("\n**** SAMPLES ****\n")
    cat(msg.perm.sample)
    if(exists("msg.grid.sample"))
        cat(msg.grid.sample)
    cat(msg.mirror.sample)
    if(exists("msg.perm.strata"))
        cat(msg.constant)
    cat("\n")
}
