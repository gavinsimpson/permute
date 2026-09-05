# Utility functions for complete enumeration of all possible permutations

Utility functions to return the set of all permutations under different
designs. For most practical applications, such as combining designs that
permute blocks and/or observations within blocks,
[`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md)
is required.

## Usage

``` r
allFree(n, v = seq_len(n))

allSeries(n, nperms, mirror = FALSE)

allGrid(n, nperms, nr, nc, mirror, symmetric = FALSE)

allStrata(n, control)
```

## Arguments

- n:

  The number of observations.

- v:

  Numeric vector of indices. The default is `seq_len(n)`.

- nperms:

  Numeric; number of possible permutations.

- mirror:

  Logical; should mirroring of permutations be allowed?

- nr, nc:

  Integer; number of rows and columns of grid designs.

- symmetric:

  Logical; for grid permutations, should simultaneous mirroring in both
  spatial directions be disallowed?

- control:

  A list describing the permutation design, as returned by
  [`how()`](https://gavinsimpson.github.io/permute/reference/how.md).

## Value

A matrix of all possible permutations of `n` observations or of `v`,
given the provided options.

## Details

These utility functions are not designed for casual use. See
[`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md)
for further details.

## Author

Gavin Simpson
