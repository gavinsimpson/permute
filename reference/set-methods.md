# Replacement functions to set components of a permutation design

These functions provide abstracted replacement of components in a
permutation design such as one returned by
[`how()`](https://gavinsimpson.github.io/permute/reference/how.md). They
also update the matched calls stored in the design so that
[`stats::update()`](https://rdrr.io/r/stats/update.html) continues to
work correctly.

## Usage

``` r
setBlocks(object) <- value

setPlots(object) <- value

setWithin(object) <- value

setStrata(object) <- value

setNperm(object) <- value

setAllperms(object) <- value

setMaxperm(object) <- value

setMinperm(object) <- value

setComplete(object) <- value

setMake(object) <- value

setObserved(object) <- value

setRow(object) <- value

setCol(object) <- value

setDim(object) <- value

setType(object) <- value

setMirror(object) <- value

setSymmetric(object) <- value

setConstant(object) <- value
```

## Arguments

- object:

  An R object on which to dispatch.

- value:

  The replacement value or object.

## Value

`object`, suitably modified.

## Details

Use these functions instead of directly modifying the underlying list,
so code does not depend on permute's internal representation.

## Note

`setStrata<-` has methods for objects of class `"how"` and `"Plots"`.
The former sets the `blocks` component of the
[`how()`](https://gavinsimpson.github.io/permute/reference/how.md)
object, while the latter sets the `strata` component of the
[`Plots()`](https://gavinsimpson.github.io/permute/reference/how.md)
object.

`setDim<-`, `setRow<-`, and `setCol<-` cannot be used on an object of
class `"how"`. Instead, extract the
[`Plots()`](https://gavinsimpson.github.io/permute/reference/how.md) or
[`Within()`](https://gavinsimpson.github.io/permute/reference/how.md)
component with
[`getPlots()`](https://gavinsimpson.github.io/permute/reference/get-methods.md)
or
[`getWithin()`](https://gavinsimpson.github.io/permute/reference/get-methods.md),
alter it, and replace it with `setPlots<-` or `setWithin<-`.

## See also

[`check()`](https://gavinsimpson.github.io/permute/reference/check.md)
checks a design described by
[`how()`](https://gavinsimpson.github.io/permute/reference/how.md). See
[get-methods](https://gavinsimpson.github.io/permute/reference/get-methods.md)
for the corresponding extractor functions.

## Author

Gavin Simpson

## Examples

``` r
hh <- how()
getNperm(hh)
#> [1] 199
setNperm(hh) <- 999
getNperm(hh)
#> [1] 999
```
