## deprecate check
`permCheck` <- function(object, control = how()) {
    .Deprecated(new = "check", "permute")
    check(object = object, control = control)
}
