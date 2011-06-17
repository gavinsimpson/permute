`getNumObs` <- function(object, ...) UseMethod("getNumObs")

`getNumObs.default` <- function(object, ...)
{
    NROW(scores(object, display = "sites"))
}

`getNumObs.numeric` <- function(object, ...)
{
    length(object)
}

`getNumObs.integer` <- function(object, ...)
{
    getNumObs.numeric(object)
}

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

`nobs.cca` <- function(object, ...) {
    NROW(scores(object, display = "sites"))
}
