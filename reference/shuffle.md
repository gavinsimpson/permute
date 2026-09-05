# Unrestricted and restricted permutations

Unrestricted and restricted permutation designs for time series, line
transects, spatial grids and blocking factors.

## Usage

``` r
shuffle(n, control = how())

permute(i, n, control, perms = NULL)

permutator(
  n,
  nset,
  control = how(),
  check = TRUE,
  quietly = FALSE,
  perms = NULL
)
```

## Arguments

- n:

  numeric; the length of the returned vector of permuted values. Usually
  the number of observations under consideration. May also be any object
  that [`stats::nobs()`](https://rdrr.io/r/stats/nobs.html) knows about;
  see
  [nobs-methods](https://gavinsimpson.github.io/permute/reference/nobs.md).

- control:

  a list of control values describing properties of the permutation
  design, as returned by a call to `how`.

- i:

  integer; row of `control$all.perms` to return.

- perms:

  a matrix of permutation indices, such as an object returned by
  [`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md)
  or
  [`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md).
  Each row must contain the indices from 1 to the number of columns
  exactly once.

- nset:

  numeric; the number of permutations to generate. If missing, the
  number is taken from `control`.

- check:

  logical; should the permutation design be checked by
  [`check()`](https://gavinsimpson.github.io/permute/reference/check.md)?

- quietly:

  logical; should messages from checking the design be suppressed?

## Value

For `shuffle` a vector of length `n` containing a permutation of the
observations 1, ..., n using the permutation scheme described by
argument `control`.

For `permute`, the `i`th row of `perms`, the `i`th permutation from the
set of all permutations, or a random permutation from the design.

For `permutator`, a zero-argument function with `nperm` and `n`
attributes giving the number of permutations and observations. The
function returns successive permutation-index vectors, then `NULL` after
exhaustion.

## Details

`shuffle` can generate permutations for a wide range of restricted
permutation schemes. A small selection of the available combinations of
options is provided in the Examples section below.

With `Plots(type = "partition")`, `shuffle` returns one canonical index
permutation for a random assignment to the groups in `Plots$strata`.
Group sizes and the relative order of observations originally carrying
the same group label are retained.

`permute` is a higher level utility function for use in a loop within a
function implementing a permutation test. It returns a requested row
from `perms` when a permutation matrix is supplied. Otherwise, it
returns either a random permutation from the current design or the
requested permutation from `control$all.perms` when complete enumeration
is enabled.

`permutator` returns a function that iterates over a fixed set of
permutations. The set can be supplied through `perms`, or generated when
`permutator` is called by forwarding `n`, `nset`, `control`, `check`,
and `quietly` to
[`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md).
Supply either `perms` or the generation arguments, but not both. Each
call to the returned function yields the next row, and returns `NULL`
once the set is exhausted.

## References

`shuffle()` is modelled after the permutation schemes of Canoco 3.1 (ter
Braak, 1990); see also Besag & Clifford (1989).

Besag, J. and Clifford, P. (1989) Generalized Monte Carlo significance
tests. *Biometrika* **76**; 633–642.

ter Braak, C. J. F. (1990). *Update notes: CANOCO version 3.1*.
Wageningen: Agricultural Mathematics Group. (UR).

## See also

[`check()`](https://gavinsimpson.github.io/permute/reference/check.md),
a utility function for checking permutation scheme described by
[`how()`](https://gavinsimpson.github.io/permute/reference/how.md).

## Author

Gavin Simpson

## Examples

``` r

set.seed(1234)

## unrestricted permutations
shuffle(20)
#>  [1]  3 12 11 18 14 10  1  4  8  6  7  5 20 15  2  9 17 16 19 13

## observations represent a time series of line transect
CTRL <- how(within = Within(type = "series"))
shuffle(20, control = CTRL)
#>  [1]  8  9 10 11 12 13 14 15 16 17 18 19 20  1  2  3  4  5  6  7

## observations represent a time series of line transect
## but with mirroring allowed
CTRL <- how(within = Within(type = "series", mirror = TRUE))
shuffle(20, control = CTRL)
#>  [1]  7  6  5  4  3  2  1 20 19 18 17 16 15 14 13 12 11 10  9  8

## observations represent a spatial grid, 5rx4c
nr <- 5
nc <- 4
CTRL <- how(within = Within(type = "grid", ncol = nc, nrow = nr))
perms <- shuffle(20, control = CTRL)
## view the permutation as a grid
matrix(matrix(1:20, nrow = nr, ncol = nc)[perms],
       ncol = nc, nrow = nr)
#>      [,1] [,2] [,3] [,4]
#> [1,]    7   12   17    2
#> [2,]    8   13   18    3
#> [3,]    9   14   19    4
#> [4,]   10   15   20    5
#> [5,]    6   11   16    1

## random permutations in presence of strata
plots <- Plots(strata = gl(4, 5))
CTRL <- how(plots = plots, within = Within(type = "free"))
shuffle(20, CTRL)
#>  [1]  5  3  4  2  1  8  7  6  9 10 14 11 15 12 13 18 20 16 17 19
## as above but same random permutation within strata
CTRL <- how(plots = plots, within = Within(type = "free",
            constant = TRUE))
shuffle(20, CTRL)
#>  [1]  3  5  2  1  4  8 10  7  6  9 13 15 12 11 14 18 20 17 16 19

## time series within each level of block
CTRL <- how(plots = plots, within = Within(type = "series"))
shuffle(20, CTRL)
#>  [1]  2  3  4  5  1  8  9 10  6  7 15 11 12 13 14 19 20 16 17 18
## as above, but  with same permutation for each level
CTRL <- how(plots = plots, within = Within(type = "series",
            constant = TRUE))
shuffle(20, CTRL)
#>  [1]  2  3  4  5  1  7  8  9 10  6 12 13 14 15 11 17 18 19 20 16

## spatial grids within each level of block, 4 x (5r x 5c)
nr <- 5
nc <- 5
nb <- 4 ## number of blocks
plots <- Plots(gl(nb, 25))
CTRL <- how(plots = plots,
            within = Within(type = "grid", ncol = nc, nrow = nr))
shuffle(100, CTRL)
#>   [1]  24  25  21  22  23   4   5   1   2   3   9  10   6   7   8  14  15  11
#>  [19]  12  13  19  20  16  17  18  27  28  29  30  26  32  33  34  35  31  37
#>  [37]  38  39  40  36  42  43  44  45  41  47  48  49  50  46  56  57  58  59
#>  [55]  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  51  52
#>  [73]  53  54  55  83  84  85  81  82  88  89  90  86  87  93  94  95  91  92
#>  [91]  98  99 100  96  97  78  79  80  76  77
## as above, but with same permutation for each level
CTRL <- how(plots = plots,
            within = Within(type = "grid", ncol = nc, nrow = nr,
                            constant = TRUE))
shuffle(100, CTRL)
#>   [1]  23  24  25  21  22   3   4   5   1   2   8   9  10   6   7  13  14  15
#>  [19]  11  12  18  19  20  16  17  48  49  50  46  47  28  29  30  26  27  33
#>  [37]  34  35  31  32  38  39  40  36  37  43  44  45  41  42  73  74  75  71
#>  [55]  72  53  54  55  51  52  58  59  60  56  57  63  64  65  61  62  68  69
#>  [73]  70  66  67  98  99 100  96  97  78  79  80  76  77  83  84  85  81  82
#>  [91]  88  89  90  86  87  93  94  95  91  92

## permuting levels of plots instead of observations
CTRL <- how(plots = Plots(gl(4, 5), type = "free"),
            within = Within(type = "none"))
shuffle(20, CTRL)
#>  [1] 11 12 13 14 15  1  2  3  4  5  6  7  8  9 10 16 17 18 19 20
## permuting levels of plots instead of observations
## but plots represent a time series
CTRL <- how(plots = Plots(gl(4, 5), type = "series"),
            within = Within(type = "none"))
shuffle(20, CTRL)
#>  [1]  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20  1  2  3  4  5

## permuting levels of plots but plots represent a time series
## free permutation within plots
CTRL <- how(plots = Plots(gl(4, 5), type = "series"),
            within = Within(type = "free"))
shuffle(20, CTRL)
#>  [1] 10  7  6  9  8 15 12 11 13 14 16 19 20 18 17  3  2  1  5  4

## permuting within blocks
grp <- gl(2, 10) # 2 groups of 10 samples each
CTRL <- how(blocks = grp)
shuffle(length(grp), control = CTRL)
#>  [1]  6  3  1  9  5  8  7 10  4  2 14 12 19 20 11 17 16 13 15 18

## Reuse a generated permutation set
perm_set <- shuffleSet(6, nset = 3, check = FALSE)
permute(2, perms = perm_set)
#> [1] 6 5 4 2 1 3
nextPerm <- permutator(perms = perm_set)
nextPerm()
#> [1] 6 5 2 1 4 3
nextPerm()
#> [1] 6 5 4 2 1 3

## Simple function using permute() to assess significance
## of a t.test
pt.test <- function(x, group, control) {
    ## function to calculate t
    t.statistic <- function(x, y) {
        m <- length(x)
        n <- length(y)
        ## means and variances, but for speed
        xbar <- mean(x)
        ybar <- mean(y)
        xvar <- var(x)
        yvar <- var(y)
        pooled <- sqrt(((m-1)*xvar + (n-1)*yvar) / (m+n-2))
        (xbar - ybar) / (pooled * sqrt(1/m + 1/n))
    }
    ## number of observations
    Nobs <- nobs(x)
    ## group names
    lev <- names(table(group))
    ## generate and retain the permutations once
    nextPerm <- permutator(Nobs, control = control)
    nperm <- attr(nextPerm, "nperm")
    ## vector to hold results, +1 because of observed t
    t.permu <- numeric(length = nperm + 1L)
    ## calculate observed t
    t.permu[1] <- t.statistic(x[group == lev[1]], x[group == lev[2]])
    ## generate randomisation distribution of t
    for(i in seq_len(nperm)) {
        ## return a permutation
        want <- nextPerm()
        ## calculate permuted t
        t.permu[i+1] <- t.statistic(x[want][group == lev[1]],
                                    x[want][group == lev[2]])
    }
    ## pval from permutation test
    pval <- sum(abs(t.permu) >= abs(t.permu[1])) / (nperm + 1L)
    ## return value
    return(list(t.stat = t.permu[1], pval = pval))
}

## generate some data with slightly different means
set.seed(1234)
gr1 <- rnorm(20, mean = 9)
gr2 <- rnorm(20, mean = 10)
dat <- c(gr1, gr2)
## grouping variable
grp <- gl(2, 20, labels = paste("Group", 1:2))
## create the permutation design
control <- how(nperm = 999, within = Within(type = "free"))
## perform permutation t test
perm.val <- pt.test(dat, grp, control)
perm.val
#> $t.stat
#> [1] -2.342064
#> 
#> $pval
#> [1] 0.024
#> 

## compare perm.val with the p-value from t.test()
t.test(dat ~ grp, var.equal = TRUE)
#> 
#>  Two Sample t-test
#> 
#> data:  dat by grp
#> t = -2.3421, df = 38, p-value = 0.02452
#> alternative hypothesis: true difference in means between group Group 1 and group Group 2 is not equal to 0
#> 95 percent confidence interval:
#>  -1.25582408 -0.09136416
#> sample estimates:
#> mean in group Group 1 mean in group Group 2 
#>              8.749336              9.422930 
#> 
```
