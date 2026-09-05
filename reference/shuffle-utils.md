# Utility functions for unrestricted and restricted permutations

Unrestricted and restricted permutations for time series, line
transects, spatial grids, and blocking factors.

## Usage

``` r
shuffleFree(x, size)

shuffleSeries(x, mirror = FALSE, start = NULL, flip = NULL)

shuffleGrid(
  nrow,
  ncol,
  mirror = FALSE,
  start.row = NULL,
  start.col = NULL,
  flip = NULL,
  symmetric = FALSE
)

shuffleStrata(
  strata,
  type,
  mirror = FALSE,
  start = NULL,
  flip = NULL,
  nrow,
  ncol,
  start.row = NULL,
  start.col = NULL,
  symmetric = FALSE
)
```

## Arguments

- x:

  A vector of indices to permute.

- size:

  The number of indices required.

- mirror:

  Logical; should mirroring of sequences be allowed?

- start:

  Integer; the starting point for time-series permutations. If missing,
  a random starting point is determined.

- flip:

  Logical of length one for `shuffleSeries()` or length two for
  `shuffleGrid()`; force mirroring of the permutation. For
  `shuffleGrid()`, the first element flips rows and the second flips
  columns.

- nrow, ncol:

  Numeric; the number of rows and columns in the grid.

- start.row, start.col:

  Numeric; the starting row and column for the shifted grid permutation.
  If not supplied, they are selected randomly.

- symmetric:

  Logical; for grid permutations, should simultaneous mirroring in both
  spatial directions be disallowed?

- strata:

  A factor containing the blocks to permute.

- type:

  Character; the permutation type used to shuffle `strata`. One of
  `"free"`, `"grid"`, `"series"`, or `"partition"`.

## Value

An integer vector of permuted indices.

## Details

These are developer-level functions for generating permuted indices from
restricted and unrestricted designs.

`shuffleFree()` is a lightweight wrapper around the code underlying
[`base::sample()`](https://rdrr.io/r/base/sample.html). It calls
`base::sample.int(x, size, replace = FALSE)` without additional checks.
Sampling is without replacement and without regard to prior
probabilities. `size` can be one to draw a single index; in general use
it is set equal to `length(x)`.

With `type = "partition"`, `shuffleStrata()` returns one canonical index
permutation for a random arrangement of the labels in `strata`; indices
carrying the same original label retain their relative order.

## See also

[`check()`](https://gavinsimpson.github.io/permute/reference/check.md)
checks a permutation design,
[`how()`](https://gavinsimpson.github.io/permute/reference/how.md)
describes a design, and
[`shuffle()`](https://gavinsimpson.github.io/permute/reference/shuffle.md)
is the user-oriented wrapper around these functions.

## Author

Gavin Simpson

## Examples

``` r
set.seed(3)

## draw 1 value at random from the set 1:10
shuffleFree(1:10, 1)
#> [1] 2

## permute the series 1:10
x <- 1:10
shuffleSeries(x)                ## with random starting point
#>  [1] 10  1  2  3  4  5  6  7  8  9
shuffleSeries(x, start = 5L)    ## known starting point
#>  [1]  6  7  8  9 10  1  2  3  4  5
shuffleSeries(x, flip = TRUE)   ## random start, forced mirror
#>  [1]  5  6  7  8  9 10  1  2  3  4
shuffleSeries(x, mirror = TRUE) ## random start, possibly mirror
#>  [1]  5  6  7  8  9 10  1  2  3  4

## permute a grid of size 3x3
shuffleGrid(3, 3)                      ## random starting row/col
#> [1] 6 4 5 9 7 8 3 1 2
shuffleGrid(3, 3, start.row = 2,
            start.col = 3)             ## with known row/col
#> [1] 3 1 2 6 4 5 9 7 8
shuffleGrid(3, 3, flip = rep(TRUE, 2)) ## random start, forced mirror
#> [1] 8 9 7 2 3 1 5 6 4
shuffleGrid(3, 3, mirror = TRUE,
            symmetric = TRUE)          ## never mirror both directions
#> [1] 9 7 8 3 1 2 6 4 5
```
