# Complete enumeration of all possible permutations

`allPerms` is a utility function to return the set of permutations for a
given R object and a specified permutation design.

## Usage

``` r
allPerms(n, control = how(), check = TRUE)

# S3 method for class 'allPerms'
summary(object, ...)

# S3 method for class 'allPerms'
as.matrix(x, ...)

as.allPerms(object, control)
```

## Arguments

- n:

  the number of observations or an 'object' from which the number of
  observations can be determined via `getNumObs`.

- control:

  a list of control values describing properties of the permutation
  design, as returned by a call to
  [`how()`](https://gavinsimpson.github.io/permute/reference/how.md).

- check:

  logical; should `allPerms` check the design? The default is to check,
  but this can be skipped, for example if a function checked the design
  earlier.

- object:

  for `summary.allPerms`, an object of class `"allPerms"`. For
  `as.allPerms` a matrix or something that can be coerced to a matrix by
  [`base::as.matrix()`](https://rdrr.io/r/base/matrix.html).

- ...:

  arguments to other methods.

- x:

  an object of class `"allPerms"`, as returned by `allPerms`.

## Value

For `allPerms`, an object of class `"allPerms"`: a matrix whose rows are
the set of all possible permutations for the supplies number of
observations and permutation scheme selected. The matrix has two
additional attributes `control` and `observed`. Attribute `control`
contains the argument `control` (possibly updated via `check`).
Attribute `observed` contains argument `observed`.

## Details

Function `allPerms` enumerates all possible permutations for the number
of observations and the selected permutation scheme. It has
[`base::print()`](https://rdrr.io/r/base/print.html) and
[`base::summary()`](https://rdrr.io/r/base/summary.html) methods.
`allPerms` returns a matrix containing all possible permutations,
possibly containing the observed ordering (if argument `observed` is
`TRUE`). The rows of this matrix are the various permutations and the
columns reflect the number of samples.

With free permutation designs, and restricted permutation schemes with
large numbers of observations, there are a potentially huge number of
possible permutations of the samples. It would be inefficient, not to
mention incredibly time consuming, to enumerate them all. Storing all
possible permutations would also become problematic in such cases. To
control this and guard against trying to evaluate too large a number of
permutations, if the number of possible permutations is larger than
`getMaxperm(control)`, `allPerms` exits with an error.

The `as.matrix` method sets the `control` and `seed` attributes to
`NULL` and removes the `"permutationMatrix"` class, resulting in a
standard matrix object.

With `Plots(type = "partition")`, the rows enumerate distinct
assignments to the labelled groups in `Plots$strata`. If the group sizes
are \\n_1, \ldots, n_K\\, there are \\n! / \prod_k n_k!\\ such
assignments before optionally removing the observed assignment.

## Warning

If permuting the strata themselves, a balanced design is required (the
same number of observations in each level of `strata`). This does not
apply to `Plots(type = "partition")`, which supports unequal group
sizes.

## Author

Gavin Simpson

## Examples

``` r

## allPerms can work with a vector
vec <- c(3,4,5)
allPerms(vec) ## free permutation
#>      [,1] [,2] [,3]
#> [1,]    1    3    2
#> [2,]    2    1    3
#> [3,]    2    3    1
#> [4,]    3    1    2
#> [5,]    3    2    1

## enumerate all possible permutations for a more complicated
## design
fac <- gl(2,6)
ctrl <- how(within = Within(type = "grid", mirror = FALSE,
                            constant = TRUE, nrow = 3, ncol = 2),
            plots = Plots(strata = fac))
Nobs <- length(fac)
numPerms(seq_len(Nobs), control = ctrl) ## 6
#> [1] 6
(tmp <- allPerms(Nobs, control = update(ctrl, observed = TRUE)))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#> [1,]    5    6    4    2    3    1   11   12   10     8     9     7
#> [2,]    2    3    1    5    6    4    8    9    7    11    12    10
#> [3,]    6    4    5    3    1    2   12   10   11     9     7     8
#> [4,]    3    1    2    6    4    5    9    7    8    12    10    11
#> [5,]    4    5    6    1    2    3   10   11   12     7     8     9
#> [6,]    1    2    3    4    5    6    7    8    9    10    11    12
(tmp2 <- allPerms(Nobs, control = ctrl))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#> [1,]    5    6    4    2    3    1   11   12   10     8     9     7
#> [2,]    2    3    1    5    6    4    8    9    7    11    12    10
#> [3,]    6    4    5    3    1    2   12   10   11     9     7     8
#> [4,]    3    1    2    6    4    5    9    7    8    12    10    11
#> [5,]    4    5    6    1    2    3   10   11   12     7     8     9

## turn on mirroring
##ctrl$within$mirror <- TRUE
ctrl <- update(ctrl, within = update(getWithin(ctrl), mirror = TRUE))
numPerms(seq_len(Nobs), control = ctrl)
#> [1] 12
(tmp3 <- allPerms(Nobs, control = update(ctrl, observed = TRUE)))
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
(tmp4 <- allPerms(Nobs, control = ctrl))
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
summary(tmp3)
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
#>   Mirrored?: Yes
#>   Different permutation within each Plot?: No
#>   Symmetric?: No
#>   Grid dimensions: 3 rows 2 cols
#> 
#> Permutation details:
#>   Number of permutations: 12
#>   Max. number of permutations allowed: 9999
#>   Evaluate all permutations?: No.  Activation limit: 5040
#> 
#> All permutations:
#> Contains observed ordering?: Yes 
#> 
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
summary(tmp4)
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
#>   Mirrored?: Yes
#>   Different permutation within each Plot?: No
#>   Symmetric?: No
#>   Grid dimensions: 3 rows 2 cols
#> 
#> Permutation details:
#>   Number of permutations: 11
#>   Max. number of permutations allowed: 9999
#>   Evaluate all permutations?: No.  Activation limit: 5040
#> 
#> All permutations:
#> Contains observed ordering?: No 
#> 
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
```
