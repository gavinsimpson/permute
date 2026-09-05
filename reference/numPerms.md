# Number of possible permutations for a given object

`numPerms` calculates the maximum number of permutations possible under
the current permutation scheme.

## Usage

``` r
numPerms(object, control = how(), check = TRUE)
```

## Arguments

- object:

  any object handled by
  [`stats::nobs()`](https://rdrr.io/r/stats/nobs.html).

- control:

  a list of control values describing properties of the permutation
  design, as returned by a call to
  [`how()`](https://gavinsimpson.github.io/permute/reference/how.md).

- check:

  logical; should `control` be checked for problems?

## Value

The (numeric) number of possible permutations of observations in
`object`.

## Details

Function `numPerms` returns the number of permutations for the passed
`object` and the selected permutation scheme. `object` can be one of a
data frame, matrix, an object for which a scores method exists, or a
numeric or integer vector. In the case of a numeric or integer vector, a
vector of length 1 can be used and it will be expanded to a vector of
length `object` (i.e., `1:object`) before computing the number of
permutations. As such, `object` can be the number of observations not
just the object containing the observations.

For `Plots(type = "partition")`, if the group sizes are \\n_1, \ldots,
n_K\\, the number of distinct assignments is \\n! / \prod_k n_k!\\. With
blocks, this quantity is calculated within each block and the results
are multiplied.

## Note

In general, mirroring `"series"` designs doubles the number of
permutations and mirroring `"grid"` designs can quadruple it (within
levels of strata if present). For grids with `symmetric = TRUE`, at most
three orientations are included because simultaneous row and column
mirroring is disallowed. Reflections of grid axes containing one or two
cells are equivalent to toroidal shifts and do not add distinct
permutations.

Mirroring does not double the number of series permutations when the
series contains only two observations.

For example, with 2 observations there are 2 permutations for `"series"`
designs:

1.  1-2, and

2.  2-1.

If these two permutations were mirrored, we would have:

1.  2-1, and

2.  1-2.

It is immediately clear that this is the same set of permutations
without mirroring (if one reorders the rows).

## See also

[`shuffle()`](https://gavinsimpson.github.io/permute/reference/shuffle.md)
and [`how()`](https://gavinsimpson.github.io/permute/reference/how.md).
Additional [`stats::nobs()`](https://rdrr.io/r/stats/nobs.html) methods
are provided; see
[nobs-methods](https://gavinsimpson.github.io/permute/reference/nobs.md).

## Author

Gavin Simpson

## Examples

``` r

## permutation design --- see ?how
ctrl <- how() ## defaults to freely exchangeable

## vector input
v <- 1:10
(obs <- nobs(v))
#> [1] 10
numPerms(v, control = ctrl)
#> [1] 3628800

## integer input
len <- length(v)
(obs <- nobs(len))
#> [1] 1
numPerms(len, control = ctrl)
#> [1] 3628800

## new design, objects are a time series
ctrl <- how(within = Within(type = "series"))
numPerms(v, control = ctrl)
#> [1] 10
## number of permutations possible drastically reduced...
## ...turn on mirroring
ctrl <- how(within = Within(type = "series", mirror = TRUE))
numPerms(v, control = ctrl)
#> [1] 20

## Try blocking --- 2 groups of 5
bl <- numPerms(v, control = how(blocks = gl(2,5)))
bl
#> [1] 14400

## should be same as
pl <- numPerms(v, control = how(plots = Plots(strata = gl(2,5))))
pl
#> [1] 14400
stopifnot(all.equal(bl, pl))

## Distinct assignments to groups of sizes 3 and 2
groups <- factor(c("a", "a", "a", "b", "b"))
ctrl <- how(plots = Plots(groups, type = "partition"))
numPerms(length(groups), control = ctrl) ## 10
#> [1] 10
```
