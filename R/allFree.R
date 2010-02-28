`allFree` <- function(n, v = 1:n)
{
    if( n == 1 ) {
        matrix(v, 1, 1)
    } else {
        X <- NULL
        for(i in 1:n)
            X <- rbind(X, cbind(v[i], Recall(n-1, v[-i])))
        X
    }
}
