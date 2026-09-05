# How to define a permutation design?

Utility functions to describe unrestricted and restricted permutation
designs for time series, line transects, spatial grids and blocking
factors.

## Usage

``` r
how(
  within = Within(),
  plots = Plots(),
  blocks = NULL,
  nperm = 199,
  complete = FALSE,
  maxperm = 9999,
  minperm = 5040,
  all.perms = NULL,
  make = TRUE,
  observed = FALSE,
  data = NULL
)

Within(
  type = c("free", "series", "grid", "none"),
  constant = FALSE,
  mirror = FALSE,
  ncol = NULL,
  nrow = NULL,
  symmetric = FALSE
)

Plots(
  strata = NULL,
  type = c("none", "free", "series", "grid", "partition"),
  mirror = FALSE,
  ncol = NULL,
  nrow = NULL,
  symmetric = FALSE,
  data = NULL
)
```

## Arguments

- within, plots, blocks:

  Permutation designs for samples within the levels of `plots`
  (`within`), permutation of `plots` themselves, or for the definition
  of blocking structures which further restrict permutations (`blocks`).
  `within` and `plots` each require a named list as produced by `Within`
  and `Plots` respectively. `blocks` takes a factor (or an object
  coercible to a factor via `as.factor`), or a one-sided formula, the
  levels of which define the blocking structure.

- nperm:

  numeric; the number of permutations.

- complete:

  logical; should complete enumeration of all permutations be performed?

- maxperm:

  numeric; the maximum number of permutations that
  [`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md)
  may enumerate.

- minperm:

  numeric; the lower limit to the number of possible permutations at
  which complete enumeration is performed. When `nperm` is lower than
  `minperm`, sampling is performed from the set of complete permutations
  to avoid duplicate permutations. See argument `complete` and Details,
  below.

- all.perms:

  an object of class `allPerms`, the result of a call to
  [`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md).

- make:

  logical; should `check` generate all possible permutations? Useful if
  want to check permutation design but not produce the matrix of all
  permutations, or to circumvent the heuristics governing when complete
  enumeration is activated.

- observed:

  logical; should the observed permutation be returned as part of the
  set of all permutations? Default is `FALSE` to facilitate usage in
  higher level functions.

- data:

  A data frame in which to evaluate formula values supplied to `blocks`
  or `strata`. When `Plots()` is called inside `how()`, supply `data` to
  `how()` rather than to `Plots()`. A standalone call to `Plots()` can
  take its own `data` argument.

- type:

  character; the type of permutations required. One of `"free"`,
  `"series"`, `"grid"`, `"none"`, or, for `Plots`, `"partition"`. See
  Details.

- constant:

  logical; should the same permutation be used within each level of
  strata? If `FALSE` a separate, possibly restricted, permutation is
  produced for each level of `strata`.

- mirror:

  logical; should mirroring of sequences be allowed?

- ncol, nrow:

  numeric; the number of columns and rows of samples in the spatial grid
  respectively.

- symmetric:

  logical; for grid permutations, should simultaneous mirroring in both
  spatial directions be disallowed?

- strata:

  A factor, an object that can be coerced to a factor via `as.factor`,
  or a one-sided formula specifying the strata for permutation. Multiple
  variables and nested terms in a formula are combined into a single
  factor representing their interaction.

## Value

For `how` a list with components for each of the possible arguments.

## Details

`shuffle` can generate permutations for a wide range of restricted
permutation schemes. A small selection of the available combinations of
options is provided in the Examples section below.

Argument type controls how samples are actually permuted; `"free"`
indicates randomization, `"series"` indicates permutation via cyclic
shifts (suitable for evenly-spaced line transect or time series data),
`"grid"` indicates permutation via toroidal shifts (suitable for samples
on a regular grid), and `"none"` indicates no permutation of samples.
See the package vignette (`browseVignettes("permute")`) for additional
information on each of these types of permutation.

`Plots(type = "partition")` randomly assigns observations to the
labelled groups supplied in `strata`, retaining the observed number
assigned to each group. Permutations that differ only by reordering
observations carrying the same group label are omitted. If `within` is
not supplied, `how` uses `Within(type = "none")` for this design.
Supplying any other within-plot type is an error.

Argument `mirror` determines whether grid or series permutations can be
mirrored. Consider the sequence 1,2,3,4. The relationship between
consecutive observations is preserved if we reverse the sequence to
4,3,2,1. If there is no inherent direction in your experimental design,
mirrored permutations can be considered part of the Null model, and as
such increase the number of possible permutations. The default is to not
use mirroring so you must explicitly turn this on using `mirror = TRUE`
in `how`.

For spatial grids, `symmetric = TRUE` preserves symmetric spatial
autocovariance by preventing a permutation from being mirrored in both
the row and column directions at the same time. It has no effect unless
`type = "grid"` and `mirror = TRUE`.

To permute plots rather than the observations within plots (the levels
of `strata`), use `Within(type = "none")` and `Plots(type = foo)`, where
`foo` is how you want the plots to be permuted. However, note that the
number of observations within each plot **must** be equal!

For some experiments, such as BACI designs, one might wish to use the
same permutation within each plot. This is controlled by argument
`constant`. If `constant = TRUE` then the same permutation will be
generated for each level of `strata`. The default is `constant = FALSE`.

One-sided formulas can be used to define `blocks` and plot-level
`strata`. A formula containing a single variable, such as `~ site`,
produces a factor from that variable. Variables participating in
multiple model terms are combined into a single interaction factor.
Consequently, `~ site + plot`, `~ site:plot`, and the nested forms
`~ site / plot` and `~ plot %in% site` all describe groups formed from
`site` and `plot`. Standard formula expansion and subtraction are
honoured, so, for example, `~ . - unused` uses all variables in `data`
except `unused`.

Formulas are evaluated with standard model-frame semantics: variables
are looked up first in `data` and then in the formula environment.
Missing values are retained to preserve alignment with the observations,
and unused factor levels and unobserved combinations are dropped. The
formulas must be one-sided and contain at least one grouping term.

## References

[`shuffle()`](https://gavinsimpson.github.io/permute/reference/shuffle.md)
is modelled after the permutation schemes of Canoco 3.1 (ter Braak,
1990); see also Besag & Clifford (1989).

Besag, J. and Clifford, P. (1989) Generalized Monte Carlo significance
tests. *Biometrika* **76**; 633–642.

ter Braak, C. J. F. (1990). *Update notes: CANOCO version 3.1*.
Wageningen: Agricultural Mathematics Group. (UR).

## See also

[`shuffle()`](https://gavinsimpson.github.io/permute/reference/shuffle.md)
and
[`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md)
for permuting from a design, and
[`check()`](https://gavinsimpson.github.io/permute/reference/check.md),
a utility function for checking permutation design described by `how`.

## Author

Gavin Simpson

## Examples

``` r

## Set up factors for the Plots and Blocks
plts <- gl(4, 10) ## 4 Plots of 10 samples each
blks <- gl(2, 20) ## 2 Blocks of 20 samples each

## permutation design
h1 <- how(within = Within(type = "series", mirror = TRUE),
          plots = Plots(strata = plts, type = "series"),
          blocks = blks)

## The design can be updated...
## ... remove the blocking:
update(h1, blocks = NULL)
#> 
#> Permutation Design:
#> 
#> Blocks:
#>   Defined by: none
#> 
#> Plots:
#>   Plots: plts
#>   Permutation type: series
#>   Mirrored?: No
#> 
#> Within Plots:
#>   Permutation type: series
#>   Mirrored?: Yes
#>   Different permutation within each Plot?: Yes
#> 
#> Permutation details:
#>   Number of permutations: 199
#>   Max. number of permutations allowed: 9999
#>   Evaluate all permutations?: No.  Activation limit: 5040

## ... or switch the type of shuffling at a level:
#update(h1, plots = update(getPlots(h1), type = "none"))
plots2 <- update(getPlots(h1), type = "none")
update(h1, plots = plots2)
#> 
#> Permutation Design:
#> 
#> Blocks:
#>   Blocks: blks
#> 
#> Plots:
#>   Plots: plts
#>   Permutation type: none
#>   Mirrored?: No
#> 
#> Within Plots:
#>   Permutation type: series
#>   Mirrored?: Yes
#>   Different permutation within each Plot?: Yes
#> 
#> Permutation details:
#>   Number of permutations: 199
#>   Max. number of permutations allowed: 9999
#>   Evaluate all permutations?: No.  Activation limit: 5040

## Random assignments to groups of fixed size
groups <- factor(c("a", "a", "a", "b", "b"))
(h2 <- how(plots = Plots(strata = groups, type = "partition")))
#> 
#> Permutation Design:
#> 
#> Blocks:
#>   Defined by: none
#> 
#> Plots:
#>   Plots: groups
#>   Permutation type: partition
#>   Group sizes: 3, 2
#>   Within-group order retained
#> 
#> Within Plots:
#>   Permutation type: none
#> 
#> Permutation details:
#>   Number of permutations: 199
#>   Max. number of permutations allowed: 9999
#>   Evaluate all permutations?: No.  Activation limit: 5040
shuffle(length(groups), control = h2)
#> [1] 4 1 2 3 5

## Formula interfaces use a single data frame
dat <- data.frame(
    block = gl(2, 6),
    site = gl(3, 2, 12),
    plot = gl(2, 1, 12)
)
how(plots = Plots(strata = ~ site / plot), blocks = ~ block, data = dat)
#> 
#> Permutation Design:
#> 
#> Blocks:
#>   Blocks: block
#> 
#> Plots:
#>   Plots: site/plot
#>   Permutation type: none
#>   Mirrored?: No
#> 
#> Within Plots:
#>   Permutation type: free
#> 
#> Permutation details:
#>   Number of permutations: 199
#>   Max. number of permutations allowed: 9999
#>   Evaluate all permutations?: No.  Activation limit: 5040
getStrata(Plots(strata = ~ site / plot, data = dat))
#>  [1] 1.1 1.2 2.1 2.2 3.1 3.2 1.1 1.2 2.1 2.2 3.1 3.2
#> Levels: 1.1 2.1 3.1 1.2 2.2 3.2
```
