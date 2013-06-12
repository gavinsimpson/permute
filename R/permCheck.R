## deprecate check
`permCheck` <- function(object, control = how(),
                        make.all = TRUE) {
    .Deprecated(new = "check", "permute")
    check(object = object, control = control, make.all = make.all)
}
