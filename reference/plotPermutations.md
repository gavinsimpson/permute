# Extract expanded permutations of plot-level variables

`Plots(strata = plot_id, type = "free", variables = ~ A + B)` defines
joint permutations of variables that are constant within plots. Both the
plot IDs and the variables are supplied at the original data-row level.
`variables` accepts a named data frame, an atomic vector, or a one-sided
formula evaluated in `data`. Factor levels and numeric precision are
retained.

## Usage

``` r
permutationIndices(x)

permutedVariables(x, i = NULL)

# S3 method for class 'plotPermutations'
print(x, ...)
```

## Arguments

- x:

  A `plotPermutations` object returned by a variable design.

- i:

  Optional row of a permutation set to extract as a data frame.

- ...:

  Arguments passed to the default print method.

## Value

`permutationIndices()` returns an index vector for a single draw, or a
matrix with one row per draw and one column per original data row.
`permutedVariables()` returns values as described in Details.

## Details

The design is reduced to one complete row of variable values per plot.
Existing set partition generators permute these rows among plots and
omit duplicate arrangements of identical rows. Higher-level blocks
restrict permutations among plots within each block. Every plot must be
in one block. Balanced and unbalanced plots use the same implementation.

[`shuffle()`](https://gavinsimpson.github.io/permute/reference/shuffle.md),
[`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md),
[`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md),
[`permute()`](https://gavinsimpson.github.io/permute/reference/shuffle.md),
and
[`permutator()`](https://gavinsimpson.github.io/permute/reference/shuffle.md)
return `plotPermutations` objects.
[`numPerms()`](https://gavinsimpson.github.io/permute/reference/numPerms.md)
includes the observed arrangement. Enumeration excludes it by default,
following `how(observed = FALSE)`. `Within(type = "none")` is required.

`permutationIndices()` extracts expanded source data-row indices. Each
source plot is represented by its first row. Indices can repeat or omit
rows even for balanced plots and even for the observed arrangement.
These indices apply jointly to all selected variables, or to the
corresponding constraint matrix rows in a consumer that supports
constraint permutations.

`permutedVariables(x, i)` returns a data frame for row `i` of a
permutation set, preserving the original column types. Without `i`,
multiple variables return one data frame per draw, or a single data
frame for a single draw. For one variable, the convenience result is a
vector or matrix, with factors represented by character labels.

These objects require explicit extraction. A response permutation matrix
consumer cannot interpret the repeated source indices. A compatible
statistical method must apply them to the predictor variables or
constraint matrix and recompute its fit for each draw, keeping the
response fixed.

Validity requires a permutation distribution that matches the
experimental randomisation, or a justified exchangeability assumption.
Multiple selected variables move jointly. Separate tests of main effects
or interactions need appropriate null-specific procedures. Conditioning
on nuisance variables alone does not justify free permutations of the
constraints.

## Examples

``` r
plot_id <- rep(LETTERS[1:4], c(2, 5, 3, 4))
treatment <- rep(c("T", "C", "T", "C"), c(2, 5, 3, 4))
h <- how(plots = Plots(plot_id, type = "free", variables = treatment),
         observed = TRUE)
numPerms(length(plot_id), h) # choose(4, 2) = 6
#> [1] 6
a <- allPerms(length(plot_id), h)
permutedVariables(a)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14]
#> [1,] "T"  "T"  "T"  "T"  "T"  "T"  "T"  "C"  "C"  "C"   "C"   "C"   "C"   "C"  
#> [2,] "T"  "T"  "C"  "C"  "C"  "C"  "C"  "T"  "T"  "T"   "C"   "C"   "C"   "C"  
#> [3,] "T"  "T"  "C"  "C"  "C"  "C"  "C"  "C"  "C"  "C"   "T"   "T"   "T"   "T"  
#> [4,] "C"  "C"  "T"  "T"  "T"  "T"  "T"  "T"  "T"  "T"   "C"   "C"   "C"   "C"  
#> [5,] "C"  "C"  "T"  "T"  "T"  "T"  "T"  "C"  "C"  "C"   "T"   "T"   "T"   "T"  
#> [6,] "C"  "C"  "C"  "C"  "C"  "C"  "C"  "T"  "T"  "T"   "T"   "T"   "T"   "T"  
sort(rowSums(permutedVariables(a) == "T")) # 5 6 7 7 8 9
#> [1] 5 6 7 7 8 9
set.seed(1)
treatment[permutationIndices(shuffle(length(plot_id), h))]
#>  [1] "C" "C" "C" "C" "C" "C" "C" "T" "T" "T" "T" "T" "T" "T"
```
