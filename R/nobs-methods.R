## add some nobs() methods - need to be documented
`nobs.numeric` <- function(object, ...) {
    length(object)
}

`nobs.integer` <- function(object, ...) {
    nobs.numeric(object, ...)
}

`nobs.matrix` <- function(object, ...) {
    NROW(object)
}

`nobs.data.frame` <- function(object, ...) {
    NROW(object)
}

