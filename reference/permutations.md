# Permute a vector of data

`permutations()` generates permutations from a specified design and
applies them to the values in a supplied vector. Each row of the
returned object is one permutation of `x`.

## Usage

``` r
permutations(x, nset, control = how(), check = TRUE, quietly = FALSE)

# S3 method for class 'permutations'
print(x, ...)

# S3 method for class 'permutations'
as.matrix(x, ...)
```

## Arguments

- x:

  A non-empty, one-dimensional atomic vector to permute. Factors are
  supported and are returned using their character labels.

- nset:

  Numeric; the number of permutations to generate. If missing, the
  number is taken from `control`.

- control:

  An object of class `"how"` describing a valid permutation design.

- check:

  Logical; should the permutation design be checked by
  [`check()`](https://gavinsimpson.github.io/permute/reference/check.md)?

- quietly:

  Logical; should messages from checking the design be suppressed?

- ...:

  Arguments passed to other methods.

## Value

An object of class `"permutations"`, inheriting from `"matrix"`. Rows
are permutations and columns correspond to the elements of `x`. The
object has `control` and `seed` attributes containing the checked design
and the random-number seed at the start of permutation generation.

[`as.matrix()`](https://rdrr.io/r/base/matrix.html) returns an ordinary
matrix with the permutation metadata removed.

## Details

By default, the permutation design is checked in the same way as for
[`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md).
Consequently, the number of rows returned can differ from `nset` when
the design requests complete enumeration or when there are few possible
permutations. Set `check = FALSE` to always request exactly `nset`
permutations, which need not be unique.

## See also

[`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md)
for generating permutation indices and
[`how()`](https://gavinsimpson.github.io/permute/reference/how.md) for
specifying permutation designs.

## Examples

``` r
set.seed(1)
permutations(letters[1:5], nset = 3, check = FALSE)
#> No. of Permutations: 3
#> No. of Samples: 5 (Randomised)
#> 
#>    1   2   3   4   5  
#> p1 "b" "e" "d" "c" "a"
#> p2 "e" "d" "b" "c" "a"
#> p3 "b" "a" "c" "d" "e"

## Restricted permutations of data from a time series
control <- how(within = Within(type = "series"))
permutations(c(10, 20, 30, 40, 50), nset = 3,
             control = control, check = FALSE)
#> No. of Permutations: 3
#> No. of Samples: 5 (Sequence)
#> 
#>     1  2  3  4  5
#> p1 40 50 10 20 30
#> p2 50 10 20 30 40
#> p3 10 20 30 40 50
```
