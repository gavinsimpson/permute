# Extractor functions to access components of a permutation design

These functions provide abstracted access to components of permutation
designs such as those returned by
[`how()`](https://gavinsimpson.github.io/permute/reference/how.md).
Using them instead of directly indexing the underlying list allows that
representation to evolve without breaking user code.

## Usage

``` r
getAllperms(object, ...)

getBlocks(object, ...)

getComplete(object, ...)

getConstant(object, ...)

getCol(object, ...)

getDim(object, ...)

getMake(object, ...)

getMaxperm(object, ...)

getMinperm(object, ...)

getMirror(object, ...)

getSymmetric(object, ...)

getNperm(object, ...)

getObserved(object, ...)

getPlots(object, ...)

getRow(object, ...)

getStrata(object, ...)

getType(object, ...)

getWithin(object, ...)

getControl(object, ...)

getHow(object, ...)

# S3 method for class 'how'
getAllperms(object, ...)

# S3 method for class 'how'
getBlocks(object, ...)

# S3 method for class 'how'
getCol(object, which = c("plots", "within"), ...)

# S3 method for class 'Plots'
getCol(object, ...)

# S3 method for class 'Within'
getCol(object, ...)

# S3 method for class 'how'
getComplete(object, ...)

# S3 method for class 'how'
getConstant(object, ...)

# S3 method for class 'Within'
getConstant(object, ...)

# S3 method for class 'how'
getDim(object, which = c("plots", "within"), ...)

# S3 method for class 'Plots'
getDim(object, ...)

# S3 method for class 'Within'
getDim(object, ...)

# S3 method for class 'how'
getMake(object, ...)

# S3 method for class 'how'
getMaxperm(object, ...)

# S3 method for class 'how'
getMinperm(object, ...)

# S3 method for class 'how'
getMirror(object, which = c("plots", "within"), ...)

# S3 method for class 'Plots'
getMirror(object, ...)

# S3 method for class 'Within'
getMirror(object, ...)

# S3 method for class 'how'
getSymmetric(object, which = c("plots", "within"), ...)

# S3 method for class 'how'
getNperm(object, ...)

# S3 method for class 'Plots'
getSymmetric(object, ...)

# S3 method for class 'how'
getObserved(object, ...)

# S3 method for class 'how'
getPlots(object, ...)

# S3 method for class 'Within'
getSymmetric(object, ...)

# S3 method for class 'how'
getRow(object, which = c("plots", "within"), ...)

# S3 method for class 'Plots'
getRow(object, ...)

# S3 method for class 'Within'
getRow(object, ...)

# S3 method for class 'how'
getStrata(object, which = c("plots", "blocks"), drop = TRUE, ...)

# S3 method for class 'Plots'
getStrata(object, drop = TRUE, ...)

# S3 method for class 'how'
getType(object, which = c("plots", "within"), ...)

# S3 method for class 'Plots'
getType(object, ...)

# S3 method for class 'Within'
getType(object, ...)

# S3 method for class 'how'
getWithin(object, ...)

# S3 method for class 'allPerms'
getControl(object, ...)
```

## Arguments

- object:

  An R object on which to dispatch.

- ...:

  Arguments passed to other methods.

- which:

  Character; the level of restriction for which information is required.

- drop:

  Logical; should unused factor levels be dropped?

## Value

The contents of the corresponding component of `object`.

## Details

`getHow()` is an alias for `getControl()`; specific `getControl()`
methods are useful when debugging.

## See also

[`check()`](https://gavinsimpson.github.io/permute/reference/check.md)
checks a permutation design described by
[`how()`](https://gavinsimpson.github.io/permute/reference/how.md).

## Author

Gavin Simpson

## Examples

``` r
hh <- how()
getWithin(hh)
#> $type
#> [1] "free"
#> 
#> $constant
#> [1] FALSE
#> 
#> $mirror
#> [1] FALSE
#> 
#> $symmetric
#> [1] FALSE
#> 
#> $ncol
#> NULL
#> 
#> $nrow
#> NULL
#> 
#> $call
#> Within()
#> 
#> attr(,"class")
#> [1] "Within"
getNperm(hh)
#> [1] 199
```
