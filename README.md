# Restricted permutations with R

[![CRAN version](https://www.r-pkg.org/badges/version/permute)](https://cran.r-project.org/package=permute)
[![](https://cranlogs.r-pkg.org/badges/grand-total/permute)](https://cran.r-project.org/package=permute)
[![R-CMD-check](https://github.com/gavinsimpson/permute/workflows/R-CMD-check/badge.svg)](https://github.com/gavinsimpson/permute/actions)
[![codecov](https://codecov.io/gh/gavinsimpson/permute/graph/badge.svg?token=2FYEfBBSJ7)](https://app.codecov.io/gh/gavinsimpson/permute)

## What is permute?

**permute** generates permutations from a range of restricted permutation
designs.

**permute** provides an R implementation of permutation schemes developed by
Cajo ter Braak and made available in CANOCO, beginning with version 3.1
(ter Braak, 1990). These schemes draw upon ideas from Besag & Clifford (1989)
and were refined in later versions of CANOCO. This intellectual lineage is the
foundation for the selection of restricted designs in **permute**.

Several types of permutation are available in **permute**:

- Free permutation of objects.
- Time-series or line-transect designs that preserve temporal or spatial
  ordering.
- Spatial-grid designs that preserve ordering in both coordinate directions.
- Permutation of plots or groups of samples.
- Blocking factors that restrict permutations to within blocks. Designs can
  be nested within blocks to represent hierarchical and split-plot designs.

Unlike general combinatorics packages, **permute** describes which
rearrangements are admissible under a statistical exchangeability design. It
generates permutation indices independently of the test statistic, allowing
the same design to be reused by packages such as
[**vegan**](https://cran.r-project.org/package=vegan) and
[**permuco**](https://cran.r-project.org/package=permuco).

## Installation

Install the released version from CRAN:

```r
install.packages("permute")
```

The development version can be installed from GitHub:

```r
# install.packages("pak")
pak::pak("gavinsimpson/permute")
```

## A small example

Define a cyclic permutation design for a time series and generate five
permutations:

```r
library("permute")

set.seed(42)
control <- how(within = Within(type = "series"))
shuffleSet(10, nset = 5, control = control)
```

For blocked, spatial-grid, and split-plot designs, see the
[restricted permutations vignette](https://gavinsimpson.github.io/permute/articles/permutations.html)
and the [function reference](https://gavinsimpson.github.io/permute/reference/).

## Getting help and contributing

Questions and bug reports are welcome in the
[GitHub issue tracker](https://github.com/gavinsimpson/permute/issues).
Please read [CONTRIBUTING.md](CONTRIBUTING.md) before proposing a change. All
participants are expected to follow the [code of conduct](CODE_OF_CONDUCT.md).

## Citation

Please cite **permute** when it contributes to published work. The current
citation can be displayed from R with:

```r
citation("permute")
```

## References

Besag, J. and Clifford, P. (1989) Generalized Monte Carlo significance tests. *Biometrika* **76**; 633&ndash;642.

ter Braak, C. J. F. (1990). *Update notes: CANOCO version 3.1*. Wageningen: Agricultural Mathematics Group. (UR).
