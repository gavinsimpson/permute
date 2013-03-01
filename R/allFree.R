## `allFree` <- function(n, v = 1:n)
## {
##     if( n == 1 ) {
##         matrix(v, 1, 1)
##     } else {
##         X <- NULL
##         for(i in 1:n)
##             X <- rbind(X, cbind(v[i], Recall(n-1, v[-i])))
##         X
##     }
## }

## Modified version of allFree() provided by Doug Bates
## via personal email on 19 Jan 2012
`allFree` <- function(n, v = seq_len(n)) {
    if(n == 1L) return(array(v, c(1L, 1L)))
    do.call(rbind,
            lapply(seq_len(n),
                   function(i) cbind(v[i], allFree(n - 1L, v[-i]))))
}
