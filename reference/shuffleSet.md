# Generate a set of permutations from the specified design

`shuffleSet` returns a set of `nset` permutations from the specified
design. The main purpose of the function is to circumvent the overhead
of repeatedly calling
[`shuffle()`](https://gavinsimpson.github.io/permute/reference/shuffle.md)
to generate a set of permutations.

## Usage

``` r
shuffleSet(n, nset, control = how(), check = TRUE, quietly = FALSE)

# S3 method for class 'permutationMatrix'
as.matrix(x, ...)
```

## Arguments

- n:

  numeric; the number of observations in the sample set. May also be any
  object that [`stats::nobs()`](https://rdrr.io/r/stats/nobs.html) knows
  about; see
  [nobs-methods](https://gavinsimpson.github.io/permute/reference/nobs.md).

- nset:

  numeric; the number of permutations to generate for the set. Can be
  missing, the default, in which case `nset` is determined from
  `control`.

- control:

  an object of class `"how"` describing a valid permutation design.

- check:

  logical; should the design be checked for various problems via
  function
  [`check()`](https://gavinsimpson.github.io/permute/reference/check.md)?
  The default is to check the design for the stated number of
  observations and update `control` accordingly. See Details.

- quietly:

  logical; should messages by suppressed?

- x:

  an object of class `"permutationMatrix"`, as returned by `shuffleSet`.

- ...:

  arguments passed to other methods. For the `as.matrix` method only.

## Value

A matrix of permutations, where each row is a separate permutation. As
such, the returned matrix has `nset` rows and `n` columns.

## Details

`shuffleSet` is designed to generate a set of `nset` permutation indices
over which a function can iterate as part of a permutation test. It is
only slightly more efficient than calling
[`shuffle()`](https://gavinsimpson.github.io/permute/reference/shuffle.md)
`nset` times, but it is far more practical than the simpler function
because a set of permutations can be worked on by applying a function to
the rows of the returned object. This simplifies the function applied,
and facilitates the use of parallel processing functions, thus enabling
a larger number of permutations to be evaluated in reasonable time.

By default, `shuffleSet` will check the permutations design following a
few simple heuristics. See
[`check()`](https://gavinsimpson.github.io/permute/reference/check.md)
for details of these. Whether some of the heuristics are activated or
not can be controlled via
[`how()`](https://gavinsimpson.github.io/permute/reference/how.md),
essentially via its argument `minperm`. In particular, if there are
fewer than `minperm` permutations, `shuffleSet` will generate and return
**all possible permutations**, which may differ from the number
requested via argument `nset`.

The `check` argument to `shuffleSet` controls whether checking is
performed in the permutation design. If you set `check = FALSE` then
exactly `nset` permutations will be returned. However, do be aware that
there is no guarantee that the set of permutations returned will be
unique, especially so for designs and data sets where there are few
possible permutations relative to the number requested.

For `Plots(type = "partition")`, rows represent distinct random
assignments to groups of fixed size. Independently sampled rows may
repeat, but permutations that differ only by reordering observations
carrying the same original group label have the same canonical
representation.

The `as.matrix` method sets the `control` and `seed` attributes to
`NULL` and removes the `"permutationMatrix"` class, resulting in a
standard matrix object.

## References

`shuffleSet()` is modelled after the permutation schemes of Canoco 3.1
(ter Braak, 1990); see also Besag & Clifford (1989).

Besag, J. and Clifford, P. (1989) Generalized Monte Carlo significance
tests. *Biometrika* **76**; 633–642.

ter Braak, C. J. F. (1990). *Update notes: CANOCO version 3.1*.
Wageningen: Agricultural Mathematics Group. (UR).

## See also

See
[`shuffle()`](https://gavinsimpson.github.io/permute/reference/shuffle.md)
for generating a single permutation, and
[`how()`](https://gavinsimpson.github.io/permute/reference/how.md) for
setting up permutation designs.

## Author

Gavin L. Simpson

## Examples

``` r

set.seed(1)
## simple random permutations, 5 permutations in set
shuffleSet(n = 10, nset = 5)
#> No. of Permutations: 5
#> No. of Samples: 10 (Randomised)
#> 
#>     1 2 3  4  5  6 7 8  9 10
#> p1  3 4 5  7  2  8 9 6 10  1
#> p2  3 2 6 10  5  7 8 4  1  9
#> p3 10 2 6  1  9  8 7 5  3  4
#> p4  5 6 4  2 10  8 9 1  7  3
#> p5  9 6 7  4  8 10 1 2  3  5

## series random permutations, 5 permutations in set
shuffleSet(10, 5, how(within = Within(type = "series")))
#> Set of permutations < 'minperm'. Generating entire set.
#> No. of Permutations: 5
#> No. of Samples: 10 (Sequence)
#> 
#>    1 2  3 4  5  6 7  8  9 10
#> p1 6 7  8 9 10  1 2  3  4  5
#> p2 8 9 10 1  2  3 4  5  6  7
#> p3 5 6  7 8  9 10 1  2  3  4
#> p4 3 4  5 6  7  8 9 10  1  2
#> p5 2 3  4 5  6  7 8  9 10  1

## series random permutations, 10 permutations in set,
## with possible mirroring
CTRL <- how(within = Within(type = "series", mirror = TRUE))
shuffleSet(10, 10, CTRL)
#> Set of permutations < 'minperm'. Generating entire set.
#> No. of Permutations: 10
#> No. of Samples: 10 (Sequence; mirrored)
#> 
#>      1 2  3  4  5  6  7  8 9 10
#> p1   3 4  5  6  7  8  9 10 1  2
#> p2   7 8  9 10  1  2  3  4 5  6
#> p3  10 1  2  3  4  5  6  7 8  9
#> p4   2 1 10  9  8  7  6  5 4  3
#> p5   8 9 10  1  2  3  4  5 6  7
#> p6   4 3  2  1 10  9  8  7 6  5
#> p7   5 6  7  8  9 10  1  2 3  4
#> p8   9 8  7  6  5  4  3  2 1 10
#> p9   5 4  3  2  1 10  9  8 7  6
#> p10  6 5  4  3  2  1 10  9 8  7

## Permuting strata
## 4 groups of 5 observations
CTRL <- how(within = Within(type = "none"),
            plots = Plots(strata = gl(4,5), type = "free"))
shuffleSet(20, 10, control = CTRL)
#> Set of permutations < 'minperm'. Generating entire set.
#> No. of Permutations: 10
#> No. of Samples: 20 (Nested in: plots; )
#> Restricted by Plots: gl(4, 5) (4 plots; Randomised)
#> 
#>      1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
#> p1   6  7  8  9 10  1  2  3  4  5 11 12 13 14 15 16 17 18 19 20
#> p2   6  7  8  9 10 16 17 18 19 20 11 12 13 14 15  1  2  3  4  5
#> p3  11 12 13 14 15 16 17 18 19 20  6  7  8  9 10  1  2  3  4  5
#> p4   1  2  3  4  5 11 12 13 14 15  6  7  8  9 10 16 17 18 19 20
#> p5  16 17 18 19 20  6  7  8  9 10 11 12 13 14 15  1  2  3  4  5
#> p6   6  7  8  9 10  1  2  3  4  5 16 17 18 19 20 11 12 13 14 15
#> p7  11 12 13 14 15  6  7  8  9 10 16 17 18 19 20  1  2  3  4  5
#> p8  16 17 18 19 20 11 12 13 14 15  6  7  8  9 10  1  2  3  4  5
#> p9  11 12 13 14 15 16 17 18 19 20  1  2  3  4  5  6  7  8  9 10
#> p10 16 17 18 19 20  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15

## 10 random permutations in presence of Plot-level strata
plotStrata <- Plots(strata = gl(4,5))
CTRL <- how(plots = plotStrata,
            within = Within(type = "free"))
numPerms(20, control = CTRL)
#> [1] 207360000
shuffleSet(20, 10, control = CTRL)
#> No. of Permutations: 10
#> No. of Samples: 20 (Nested in: plots; Randomised)
#> Restricted by Plots: gl(4, 5) (4 plots)
#> 
#>     1 2 3 4 5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
#> p1  5 4 2 3 1  8 10  7  6  9 12 13 11 14 15 17 16 20 18 19
#> p2  4 2 5 3 1  9  7  6 10  8 12 11 15 13 14 20 18 17 16 19
#> p3  1 3 5 4 2 10  7  9  6  8 13 15 11 14 12 18 16 19 17 20
#> p4  3 5 2 4 1  9  8  6 10  7 13 11 15 14 12 19 18 16 20 17
#> p5  1 3 5 4 2  7  9 10  8  6 13 11 12 15 14 19 20 18 16 17
#> p6  5 3 4 2 1  7  6  8 10  9 11 14 13 12 15 19 17 16 20 18
#> p7  3 1 5 4 2  7  6 10  9  8 13 15 11 14 12 20 16 18 17 19
#> p8  2 3 4 5 1  7  6  9 10  8 13 12 11 15 14 17 16 20 18 19
#> p9  4 3 2 1 5  7  8  9  6 10 12 15 11 13 14 19 20 17 18 16
#> p10 1 5 2 4 3  8  7  6  9 10 11 15 13 12 14 16 20 17 19 18
## as above but same random permutation within Plot-level strata
CTRL <- how(plots = plotStrata,
            within = Within(type = "free", constant = TRUE))
numPerms(20, control = CTRL)
#> [1] 120
shuffleSet(20, 10, CTRL) ## check this.
#> Set of permutations < 'minperm'. Generating entire set.
#> No. of Permutations: 10
#> No. of Samples: 20 (Nested in: plots; Randomised; same permutation in
#> each plot)
#> Restricted by Plots: gl(4, 5) (4 plots)
#> 
#>     1 2 3 4 5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
#> p1  1 5 4 3 2  6 10  9  8  7 11 15 14 13 12 16 20 19 18 17
#> p2  2 3 1 5 4  7  8  6 10  9 12 13 11 15 14 17 18 16 20 19
#> p3  1 5 4 2 3  6 10  9  7  8 11 15 14 12 13 16 20 19 17 18
#> p4  3 2 4 1 5  8  7  9  6 10 13 12 14 11 15 18 17 19 16 20
#> p5  4 3 5 2 1  9  8 10  7  6 14 13 15 12 11 19 18 20 17 16
#> p6  1 2 5 3 4  6  7 10  8  9 11 12 15 13 14 16 17 20 18 19
#> p7  3 4 1 2 5  8  9  6  7 10 13 14 11 12 15 18 19 16 17 20
#> p8  5 1 3 4 2 10  6  8  9  7 15 11 13 14 12 20 16 18 19 17
#> p9  2 5 1 3 4  7 10  6  8  9 12 15 11 13 14 17 20 16 18 19
#> p10 1 3 2 4 5  6  8  7  9 10 11 13 12 14 15 16 18 17 19 20

## time series within each level of Plot strata
CTRL <- how(plots = plotStrata,
            within = Within(type = "series"))
shuffleSet(20, 10, CTRL)
#> Set of permutations < 'minperm'. Generating entire set.
#> No. of Permutations: 10
#> No. of Samples: 20 (Nested in: plots; Sequence)
#> Restricted by Plots: gl(4, 5) (4 plots)
#> 
#>     1 2 3 4 5  6  7 8  9 10 11 12 13 14 15 16 17 18 19 20
#> p1  2 3 4 5 1 10  6 7  8  9 14 15 11 12 13 18 19 20 16 17
#> p2  3 4 5 1 2 10  6 7  8  9 12 13 14 15 11 17 18 19 20 16
#> p3  2 3 4 5 1 10  6 7  8  9 11 12 13 14 15 18 19 20 16 17
#> p4  2 3 4 5 1 10  6 7  8  9 13 14 15 11 12 19 20 16 17 18
#> p5  3 4 5 1 2  7  8 9 10  6 14 15 11 12 13 19 20 16 17 18
#> p6  3 4 5 1 2  7  8 9 10  6 15 11 12 13 14 17 18 19 20 16
#> p7  2 3 4 5 1 10  6 7  8  9 13 14 15 11 12 18 19 20 16 17
#> p8  1 2 3 4 5  6  7 8  9 10 13 14 15 11 12 17 18 19 20 16
#> p9  3 4 5 1 2 10  6 7  8  9 12 13 14 15 11 18 19 20 16 17
#> p10 4 5 1 2 3  9 10 6  7  8 14 15 11 12 13 18 19 20 16 17
## as above, but  with same permutation for each Plot-level stratum
CTRL <- how(plots = plotStrata,
            within = Within(type = "series", constant = TRUE))
shuffleSet(20, 10, CTRL)
#> 'nperm' >= set of all permutations: complete enumeration.
#> Set of permutations < 'minperm'. Generating entire set.
#> No. of Permutations: 4
#> No. of Samples: 20 (Nested in: plots; Sequence; same permutation in
#> each plot)
#> Restricted by Plots: gl(4, 5) (4 plots)
#> 
#>    1 2 3 4 5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
#> p1 2 3 4 5 1  7  8  9 10  6 12 13 14 15 11 17 18 19 20 16
#> p2 3 4 5 1 2  8  9 10  6  7 13 14 15 11 12 18 19 20 16 17
#> p3 4 5 1 2 3  9 10  6  7  8 14 15 11 12 13 19 20 16 17 18
#> p4 5 1 2 3 4 10  6  7  8  9 15 11 12 13 14 20 16 17 18 19
```
