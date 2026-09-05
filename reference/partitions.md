# Generate random assignments to groups of fixed size

Generate all assignments, one random assignment, or a set of random
assignments of observations to labelled groups whose sizes are fixed by
a grouping factor.

## Usage

``` r
allPartitions(strata, control = how(), check = TRUE)

shufflePartition(strata, control = how())

shufflePartitionSet(
  strata,
  nset,
  control = how(),
  check = TRUE,
  quietly = FALSE
)
```

## Arguments

- strata:

  A factor, or an object coercible to a factor, containing the group
  membership of every observation.

- control:

  An object of class `"how"`. Any blocking factor and the
  permutation-count controls are retained; its `plots` and `within`
  components are replaced by the partition design.

- check:

  Logical; should the permutation design be checked?

- nset:

  The number of random assignments to generate. If missing, it is
  obtained from `control`.

- quietly:

  Logical; should messages about complete enumeration be suppressed?

## Value

`shufflePartition()` returns an integer vector of length
`length(strata)`. `allPartitions()` and `shufflePartitionSet()` return
permutation matrices with one assignment per row.

## Details

The number assigned to each group is fixed by `table(strata)`. If the
group sizes are \\n_1, \ldots, n_K\\, the number of distinct assignments
is \$\$n! / \prod\_{k=1}^K n_k!.\$\$

Each assignment is returned as a permutation of observation indices, so
it can be used wherever output from
[`shuffle()`](https://gavinsimpson.github.io/permute/reference/shuffle.md),
[`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md),
or
[`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md)
is accepted. The relative order of observations originally belonging to
the same group is retained. This selects one canonical index permutation
for each distinct arrangement of the group labels and omits permutations
that differ only by reordering observations carrying the same label.

These functions are convenience wrappers for a design constructed with
`Plots(strata = strata, type = "partition")` and
`Within(type = "none")`.

## See also

[`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md),
[`shuffle()`](https://gavinsimpson.github.io/permute/reference/shuffle.md),
[`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md),
and [`how()`](https://gavinsimpson.github.io/permute/reference/how.md).

## Examples

``` r
groups <- factor(c("a", "a", "a", "b", "b"))

## One of 5! / (3! 2!) = 10 assignments
set.seed(1)
(p <- shufflePartition(groups))
#> [1] 1 2 4 3 5
groups[p]
#> [1] a a b a b
#> Levels: a b

## A set of random assignments
shufflePartitionSet(groups, nset = 5, check = FALSE)
#> No. of Permutations: 5
#> No. of Samples: 5 (Nested in: plots; Random assignment)
#> Restricted by Plots: strata (2 plots; Random assignment to groups)
#> 
#>    1 2 3 4 5
#> p1 4 1 2 3 5
#> p2 1 2 3 4 5
#> p3 4 5 1 2 3
#> p4 1 2 3 4 5
#> p5 1 2 3 4 5

## Complete enumeration, excluding the observed assignment by default
allPartitions(groups)
#>       [,1] [,2] [,3] [,4] [,5]
#>  [1,]    1    2    4    3    5
#>  [2,]    1    2    4    5    3
#>  [3,]    1    4    2    3    5
#>  [4,]    1    4    2    5    3
#>  [5,]    1    4    5    2    3
#>  [6,]    4    1    2    3    5
#>  [7,]    4    1    2    5    3
#>  [8,]    4    1    5    2    3
#>  [9,]    4    5    1    2    3
```
