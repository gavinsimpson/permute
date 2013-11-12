## Simple print method for objects of class "permutationMatrix"
##  - at the moment, don't print the attributes

`print.permutationMatrix` <- function(x, ...) {
    x <- as.matrix(x)
    print(x)
}
