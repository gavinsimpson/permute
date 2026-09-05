# Utility functions for permutation schemes

`check` provides checking of permutation schemes for validity.

## Usage

``` r
check(object, control = how(), quietly = FALSE, num_perms = TRUE)

# S3 method for class 'check'
summary(object, ...)
```

## Arguments

- object:

  an R object. See Details for a complete description, especially for
  [`numPerms()`](https://gavinsimpson.github.io/permute/reference/numPerms.md).
  For `summary.check()` an object of class `"check"`.

- control:

  a list of control values describing properties of the permutation
  design, as returned by a call to
  [`how()`](https://gavinsimpson.github.io/permute/reference/how.md).

- quietly:

  logical; should messages by suppressed?

- num_perms:

  logical; should the number of permutations be computed during checks?

- ...:

  arguments to other methods.

## Value

For `check` a list containing the maximum number of permutations
possible and an object of class `"how"`.

## Details

`check` is a utility functions for working with the new permutation
schemes available in
[`shuffle()`](https://gavinsimpson.github.io/permute/reference/shuffle.md).

`check` is used to check the current permutation schemes against the
object to which it will be applied. If `num_perms` is `TRUE`, `check`
also calculates the maximum number of possible permutations for the
number of observations in `object` and the permutation scheme described
by `control`. The returned object contains component `control`, an
object of class `"how"` suitably modified if `check` identifies a
problem.

The main problem is requesting more permutations than is possible with
the number of observations and the permutation design. In such cases,
`nperm` is reduced to equal the number of possible permutations, and
complete enumeration of all permutations is turned on
(`control$complete` is set to `TRUE`).

Alternatively, if the number of possible permutations is low, and less
than `control$minperm`, it is better to enumerate all possible
permutations, and as such complete enumeration of all permutations is
turned on (`control$complete` is set to `TRUE`). This guarantees that
permutations are all unique and there are no duplicates.

## See also

[`shuffle()`](https://gavinsimpson.github.io/permute/reference/shuffle.md)
and [`how()`](https://gavinsimpson.github.io/permute/reference/how.md).

## Author

Gavin L. Simpson

## Examples

``` r

## only run this example if vegan is available
if (suppressPackageStartupMessages(require("vegan"))) {
    ## use example data from ?pyrifos in package vegan
    example(pyrifos)

    ## Demonstrate the maximum number of permutations for the pyrifos data
    ## under a series of permutation schemes

    ## no restrictions - lots of perms
    CONTROL <- how(within = Within(type = "free"))
    (check1 <- check(pyrifos, CONTROL))
    ## summary(check1)

    ## no strata but data are series with no mirroring, so 132 permutations
    CONTROL <- how(within = Within(type = "series", mirror = FALSE))
    check(pyrifos, CONTROL)

    ## no strata but data are series with mirroring, so 264 permutations
    CONTROL <- how(within = Within(type = "series", mirror = TRUE))
    check(pyrifos, control = CONTROL)

    ## unrestricted within strata
    check(pyrifos, control = how(plots = Plots(strata = ditch),
                   within = Within(type = "free")))

    ## time series within strata, no mirroring
    check(pyrifos,
          control = how(plots = Plots(strata = ditch),
          within = Within(type = "series", mirror = FALSE)))

    ## time series within strata, with mirroring
    check(pyrifos,
          control = how(plots = Plots(strata = ditch),
          within = Within(type = "series", mirror = TRUE)))

    ## time series within strata, no mirroring, same permutation
    ## within strata
    check(pyrifos,
          control = how(plots = Plots(strata = ditch),
          within = Within(type = "series", constant = TRUE)))

    ## time series within strata, with mirroring, same permutation
    ## within strata
    check(pyrifos,
          control = how(plots = Plots(strata = ditch),
          within = Within(type = "series", mirror = TRUE,
          constant = TRUE)))
    ## permute strata
    check(pyrifos, how(plots = Plots(strata = ditch, type = "free"),
                       within = Within(type = "none")))
}
#> 
#> pyrifs> data(pyrifos)
#> 
#> pyrifs> ditch <- gl(12, 1, length=132)
#> 
#> pyrifs> week <- gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24))
#> 
#> pyrifs> dose <- factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11))
#> 'nperm' >= set of all permutations: complete enumeration.
#> Set of permutations < 'minperm'. Generating entire set.
#> Set of permutations < 'minperm'. Generating entire set.
#> 'nperm' >= set of all permutations: complete enumeration.
#> Set of permutations < 'minperm'. Generating entire set.
#> 'nperm' >= set of all permutations: complete enumeration.
#> Set of permutations < 'minperm'. Generating entire set.
#> [1] 479001600

## this should also also for arbitrary vectors
vec1 <- check(1:100)
vec2 <- check(1:100, how())
all.equal(vec1, vec2)
#> [1] TRUE
vec3 <- check(1:100, how(within = Within(type = "series")))
#> 'nperm' >= set of all permutations: complete enumeration.
#> Set of permutations < 'minperm'. Generating entire set.
all.equal(100, vec3$n)
#> [1] TRUE
vec4 <- check(1:100, how(within = Within(type= "series", mirror = TRUE)))
#> 'nperm' >= set of all permutations: complete enumeration.
#> Set of permutations < 'minperm'. Generating entire set.
all.equal(vec4$n, 200)
#> [1] TRUE

## enumerate all possible permutations
fac <- gl(2,6)
ctrl <- how(plots = Plots(strata = fac),
            within = Within(type = "grid", mirror = FALSE,
                            constant = TRUE, nrow = 3, ncol = 2))
check(1:12, ctrl)
#> 'nperm' >= set of all permutations: complete enumeration.
#> Set of permutations < 'minperm'. Generating entire set.
#> [1] 6

numPerms(1:12, control = ctrl)
#> [1] 6
(tmp <- allPerms(12, control = update(ctrl, observed = TRUE)))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#> [1,]    5    6    4    2    3    1   11   12   10     8     9     7
#> [2,]    2    3    1    5    6    4    8    9    7    11    12    10
#> [3,]    6    4    5    3    1    2   12   10   11     9     7     8
#> [4,]    3    1    2    6    4    5    9    7    8    12    10    11
#> [5,]    4    5    6    1    2    3   10   11   12     7     8     9
#> [6,]    1    2    3    4    5    6    7    8    9    10    11    12
(tmp2 <- allPerms(12, control = ctrl))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#> [1,]    5    6    4    2    3    1   11   12   10     8     9     7
#> [2,]    2    3    1    5    6    4    8    9    7    11    12    10
#> [3,]    6    4    5    3    1    2   12   10   11     9     7     8
#> [4,]    3    1    2    6    4    5    9    7    8    12    10    11
#> [5,]    4    5    6    1    2    3   10   11   12     7     8     9

## turn on mirroring
ctrl <- update(ctrl, within = update(getWithin(ctrl), mirror = TRUE))
numPerms(1:12, control = ctrl)
#> [1] 12
(tmp3 <- allPerms(12, control = update(ctrl, observed = TRUE)))
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#>  [1,]    5    6    4    2    3    1   11   12   10     8     9     7
#>  [2,]    2    3    1    5    6    4    8    9    7    11    12    10
#>  [3,]    6    4    5    3    1    2   12   10   11     9     7     8
#>  [4,]    3    1    2    6    4    5    9    7    8    12    10    11
#>  [5,]    4    5    6    1    2    3   10   11   12     7     8     9
#>  [6,]    1    2    3    4    5    6    7    8    9    10    11    12
#>  [7,]    4    6    5    1    3    2   10   12   11     7     9     8
#>  [8,]    1    3    2    4    6    5    7    9    8    10    12    11
#>  [9,]    5    4    6    2    1    3   11   10   12     8     7     9
#> [10,]    2    1    3    5    4    6    8    7    9    11    10    12
#> [11,]    6    5    4    3    2    1   12   11   10     9     8     7
#> [12,]    3    2    1    6    5    4    9    8    7    12    11    10
(tmp4 <- allPerms(12, control = ctrl))
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#>  [1,]    5    6    4    2    3    1   11   12   10     8     9     7
#>  [2,]    2    3    1    5    6    4    8    9    7    11    12    10
#>  [3,]    6    4    5    3    1    2   12   10   11     9     7     8
#>  [4,]    3    1    2    6    4    5    9    7    8    12    10    11
#>  [5,]    4    5    6    1    2    3   10   11   12     7     8     9
#>  [6,]    4    6    5    1    3    2   10   12   11     7     9     8
#>  [7,]    1    3    2    4    6    5    7    9    8    10    12    11
#>  [8,]    5    4    6    2    1    3   11   10   12     8     7     9
#>  [9,]    2    1    3    5    4    6    8    7    9    11    10    12
#> [10,]    6    5    4    3    2    1   12   11   10     9     8     7
#> [11,]    3    2    1    6    5    4    9    8    7    12    11    10
## prints out details of the permutation scheme as
## well as the matrix of permutations
summary(tmp)
#> 
#>  Complete enumeration of permutations
#> 
#> Permutation Design:
#> 
#> Blocks:
#>   Defined by: none
#> 
#> Plots:
#>   Plots: fac
#>   Permutation type: none
#>   Mirrored?: No
#> 
#> Within Plots:
#>   Permutation type: grid
#>   Mirrored?: No
#>   Different permutation within each Plot?: No
#>   Symmetric?: No
#>   Grid dimensions: 3 rows 2 cols
#> 
#> Permutation details:
#>   Number of permutations: 6
#>   Max. number of permutations allowed: 9999
#>   Evaluate all permutations?: No.  Activation limit: 5040
#> 
#> All permutations:
#> Contains observed ordering?: Yes 
#> 
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#> [1,]    5    6    4    2    3    1   11   12   10     8     9     7
#> [2,]    2    3    1    5    6    4    8    9    7    11    12    10
#> [3,]    6    4    5    3    1    2   12   10   11     9     7     8
#> [4,]    3    1    2    6    4    5    9    7    8    12    10    11
#> [5,]    4    5    6    1    2    3   10   11   12     7     8     9
#> [6,]    1    2    3    4    5    6    7    8    9    10    11    12
summary(tmp2)
#> 
#>  Complete enumeration of permutations
#> 
#> Permutation Design:
#> 
#> Blocks:
#>   Defined by: none
#> 
#> Plots:
#>   Plots: fac
#>   Permutation type: none
#>   Mirrored?: No
#> 
#> Within Plots:
#>   Permutation type: grid
#>   Mirrored?: No
#>   Different permutation within each Plot?: No
#>   Symmetric?: No
#>   Grid dimensions: 3 rows 2 cols
#> 
#> Permutation details:
#>   Number of permutations: 5
#>   Max. number of permutations allowed: 9999
#>   Evaluate all permutations?: No.  Activation limit: 5040
#> 
#> All permutations:
#> Contains observed ordering?: No 
#> 
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#> [1,]    5    6    4    2    3    1   11   12   10     8     9     7
#> [2,]    2    3    1    5    6    4    8    9    7    11    12    10
#> [3,]    6    4    5    3    1    2   12   10   11     9     7     8
#> [4,]    3    1    2    6    4    5    9    7    8    12    10    11
#> [5,]    4    5    6    1    2    3   10   11   12     7     8     9

## different numbers of observations per level of strata
fac <- factor(rep(1:3, times = c(3,2,2)))
## free permutations in levels of strata
numPerms(7, how(within = Within(type = "free"),
                plots = Plots(strata = fac, type = "none")))
#> [1] 24
allPerms(7, how(within = Within(type = "free"),
                plots = Plots(strata = fac)))
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#>  [1,]    1    2    3    4    5    7    6
#>  [2,]    1    2    3    5    4    6    7
#>  [3,]    1    2    3    5    4    7    6
#>  [4,]    1    3    2    4    5    6    7
#>  [5,]    1    3    2    4    5    7    6
#>  [6,]    1    3    2    5    4    6    7
#>  [7,]    1    3    2    5    4    7    6
#>  [8,]    2    1    3    4    5    6    7
#>  [9,]    2    1    3    4    5    7    6
#> [10,]    2    1    3    5    4    6    7
#> [11,]    2    1    3    5    4    7    6
#> [12,]    2    3    1    4    5    6    7
#> [13,]    2    3    1    4    5    7    6
#> [14,]    2    3    1    5    4    6    7
#> [15,]    2    3    1    5    4    7    6
#> [16,]    3    1    2    4    5    6    7
#> [17,]    3    1    2    4    5    7    6
#> [18,]    3    1    2    5    4    6    7
#> [19,]    3    1    2    5    4    7    6
#> [20,]    3    2    1    4    5    6    7
#> [21,]    3    2    1    4    5    7    6
#> [22,]    3    2    1    5    4    6    7
#> [23,]    3    2    1    5    4    7    6
## series permutations in levels of strata
ctrl <- how(within = Within(type = "series"), plots = Plots(strata = fac))
numPerms(7, control = ctrl)
#> [1] 12
allPerms(7, control = ctrl)
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#>  [1,]    2    3    1    5    4    7    6
#>  [2,]    2    3    1    5    4    6    7
#>  [3,]    2    3    1    4    5    7    6
#>  [4,]    2    3    1    4    5    6    7
#>  [5,]    3    1    2    5    4    7    6
#>  [6,]    3    1    2    5    4    6    7
#>  [7,]    3    1    2    4    5    7    6
#>  [8,]    3    1    2    4    5    6    7
#>  [9,]    1    2    3    5    4    7    6
#> [10,]    1    2    3    5    4    6    7
#> [11,]    1    2    3    4    5    7    6
```
