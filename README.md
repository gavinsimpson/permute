## Restricted permutations with R

#### Released version
[![CRAN version](http://www.r-pkg.org/badges/version/permute)](http://cran.rstudio.com/web/packages/permute/index.html) [![](http://cranlogs.r-pkg.org/badges/grand-total/permute)](http://cran.rstudio.com/web/packages/permute/index.html)

#### Build status
[![Build Status](https://travis-ci.org/gavinsimpson/permute.svg?branch=master)](https://travis-ci.org/gavinsimpson/permute)  [![Build status](https://ci.appveyor.com/api/projects/status/ytql5bm7rphweeoh/branch/master?svg=true)](https://ci.appveyor.com/project/gavinsimpson/permute/branch/master)  [![codecov.io](https://codecov.io/github/gavinsimpson/permute/coverage.svg?branch=master)](https://codecov.io/github/gavinsimpson/permute?branch=master)

## What is permute?

**permute** generates permutations from a range of restricted 
permutation designs.

Permute provides an R implementation of the permutation schemes 
developed by Cajo ter Braak and made available in the Canoco software, 
version 3.1 (ter Braak, 1990). These permutation schemes draw upon 
ideas from an earlier paper by Besag & Clifford (1989).

Several types of permutation are available in **permute**:

 * Free permutation of objects
 * Time series or line transect designs, where the temporal or spatial ordering is preserved.
 * Spatial grid designs, where the spatial ordering is preserved in both coordinate directions
 * Permutation of plots or groups of samples.
 * Blocking factors which restrict permutations to within blocks. The preceding designs can be nested within blocks, allowing analysis of hierachical designs (e.g. split plot designs)

### References

Besag, J. and Clifford, P. (1989) Generalized Monte Carlo significance 
tests. *Biometrika* **76**; 633&ndash;642.

ter Braak, C. J. F. (1990). *Update notes: CANOCO version 3.1*. 
Wageningen: Agricultural Mathematics Group. (UR).
