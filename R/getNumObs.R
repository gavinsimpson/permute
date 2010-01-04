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
