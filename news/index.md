# Changelog

## permute (development version)

### New features

- [`Plots()`](https://gavinsimpson.github.io/permute/reference/how.md)
  plot strata and
  [`how()`](https://gavinsimpson.github.io/permute/reference/how.md)
  blocks can now be specified using one-sided formulas evaluated in a
  shared `data` frame. Multiple variables, interactions, and nested
  terms are combined into a single grouping factor. Resolves
  [\#38](https://github.com/gavinsimpson/permute/issues/38).

- Grid permutation designs gain a `symmetric` option that prevents
  mirroring in both spatial directions at the same time. Resolves
  [\#1](https://github.com/gavinsimpson/permute/issues/1).

- [`permute()`](https://gavinsimpson.github.io/permute/reference/shuffle.md)
  can now reuse a supplied matrix of permutation indices, and the new
  [`permutator()`](https://gavinsimpson.github.io/permute/reference/shuffle.md)
  function provides sequential access to either a supplied or newly
  generated permutation set. Resolves
  [\#34](https://github.com/gavinsimpson/permute/issues/34).

- New function
  [`permutations()`](https://gavinsimpson.github.io/permute/reference/permutations.md)
  applies a permutation design to the values in an atomic vector and
  returns a matrix-like object with one permutation per row. This
  addresses [\#35](https://github.com/gavinsimpson/permute/issues/35)
  and the original request in
  [\#33](https://github.com/gavinsimpson/permute/issues/33).

- [`Plots()`](https://gavinsimpson.github.io/permute/reference/how.md)
  gains type `"partition"` for permuting group membership while keeping
  the number of observations in each group fixed.

- When permuting plots, balance is now only required *within* blocks.
  Requested by [@dbaranger](https://github.com/dbaranger) in
  [\#27](https://github.com/gavinsimpson/permute/issues/27)

- [`numPerms()`](https://gavinsimpson.github.io/permute/reference/numPerms.md)
  gains argument `check` that works analogously to the same argument of
  [`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md)
  to request or suppress checking of the permutation design when
  computing the number of permutations. Users should not normally need
  to touch this default of `TRUE`.

- [`check()`](https://gavinsimpson.github.io/permute/reference/check.md)
  gains argument `num_perms` to request or suppress the computing of the
  number of possible permutations for the current design. The default is
  `TRUE`, meaning the number of permutations is computed.

- *permute* now uses the *roxygen* system for documentation and building
  `.Rd` manual files.

### Bug fixes

- [`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md)
  now records the number of returned permutations in the attached
  control object after applying the `observed` setting.

- [`Plots()`](https://gavinsimpson.github.io/permute/reference/how.md)
  and
  [`Within()`](https://gavinsimpson.github.io/permute/reference/how.md)
  now preserve explicit `NULL` arguments when storing their matched
  calls, allowing later positional arguments and
  [`update()`](https://rdrr.io/r/stats/update.html) to work correctly.

- Formula objects supplied to `how(blocks = ...)` through local
  variables are now retained in the matched call, allowing the design to
  be updated after the variable has left scope.

- Replacing the plots in a `how` object with a formula-built `Plots`
  object no longer leaves a nested `data` argument that prevents
  subsequent updates.

- Complete enumeration of grid permutations no longer treats two-column
  grids as flattened series, which generated permutations that were not
  valid two-dimensional toroidal shifts. Reflections of grid axes
  containing one or two cells are also no longer counted as distinct
  when they duplicate an ordinary toroidal shift.
  [`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md)
  and
  [`numPerms()`](https://gavinsimpson.github.io/permute/reference/numPerms.md)
  now return consistent, distinct grid permutations in these cases.

- [`check()`](https://gavinsimpson.github.io/permute/reference/check.md)
  would fail all permutation designs where the (whole) plots were
  permuted with toroidal grid shifts.
  [\#42](https://github.com/gavinsimpson/permute/issues/42) Reported and
  fixed by [@sims1253](https://github.com/sims1253)

### Maintenance

- Removed the obsolete `Blocks()` compatibility helper and its
  documentation. This is the only public API removal in this release.

- Simplified complete permutation enumeration and control accessors by
  removing unreachable compatibility code, unused arguments, and
  duplicate validation while preserving permutation order and seeded RNG
  behaviour.

## permute 0.9-10

CRAN release: 2026-02-06

### User visible changes

- *permute* now depends on R versions \>= 3.6.0

### Bug fixes

- [`numPerms()`](https://gavinsimpson.github.io/permute/reference/numPerms.md)
  tries harder to avoid floating point issues when computing the number
  of permutations for the current design.
  [\#41](https://github.com/gavinsimpson/permute/issues/41)

## permute 0.9-8

CRAN release: 2025-06-25

- Updated reference output for examples following release of *vegan*
  2.7.1.

## permute 0.9-7

CRAN release: 2022-01-27

### Bug fixes

- The documented behaviour of
  [`shuffleFree()`](https://gavinsimpson.github.io/permute/reference/shuffle-utils.md)
  allowed `shuffleFree(x = 1:10)`, with `x` passed immediately to
  [`sample.int()`](https://rdrr.io/r/base/sample.html), which is
  incorrect and raises an error in the development version of R (at the
  time of writing). Reported by Brian Ripley.

## permute 0.9-6

### Bug fixes

- [`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md)
  was calling
  [`allSeries()`](https://gavinsimpson.github.io/permute/reference/allUtils.md)
  with the incorrect number of permutations, leading to problems with
  [`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md)
  also. [\#28 reported by Cajo ter
  Braak](https://github.com/gavinsimpson/permute/issues/28)

## permute 0.9-5

CRAN release: 2019-03-12

Changes to accommodate the new RNG in R \>= 3.6.0

## permute 0.9-4

CRAN release: 2016-09-09

The example in
[`?check`](https://gavinsimpson.github.io/permute/reference/check.md)
was made to suppress package startup messages from vegan.

## permute 0.9-3

This release fixed some non-canonical-form CRAN URLs.

## permute 0.9-2

This release was purely to avoid issues with CRAN as a new release of
vegan had been released and the example reference material hadn’t been
updated to match.

## permute 0.9-1

#### General

A minor bug fix release to address a single problem.

#### Bug Fixes

- [`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md)
  wasn’t returning a matrix if `nset = 1` *and* `allPerms` was invoked
  because of a low set of possible permutations. [GitHub
  Issue](https://github.com/gavinsimpson/permute/issues/19)
  [\#19](https://github.com/gavinsimpson/permute/issues/19)

## permute 0.9-0

CRAN release: 2016-01-24

#### General

This is small update to **permute**, focused mainly on ensuring the many
combinations of restrictions on permutations allowed by the package
work. An extensive test suite has been written which covers ~87% of the
package’s codebase at the time of release.

#### New features

- Permutation matrices produced by
  [`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md)
  are now printed in a more compact form.

- Better heuristics in
  [`check()`](https://gavinsimpson.github.io/permute/reference/check.md)
  allow for more reliable permutations (i.e. fewer duplicate
  permutations) when the set is small. This has increased the `minperms`
  setting. Consequently we generate all possible permutations up to
  ~500,000 more often as we now randomly sample from the entirely
  generated set rather than randomly generate permutations. This
  provides a small performance hit in some rare cases.

- [`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md)
  has a new argument `quietly = FALSE` which is passed on to
  [`check()`](https://gavinsimpson.github.io/permute/reference/check.md).

- A number of bugs were fixed. See the Changelog and the Bug reports on
  github for details.

#### Defunct

- `permControl()` and `permuplot()` are defunct and have been removed
  from the package.

## permute 0.8-0

CRAN release: 2013-12-01

#### General

- Version 0.8-0 represents a major update of **permute**, with some
  backwards-incompatible changes to the main functions. The main
  addition is the availability of block-level restrictions on the
  permutations, which are required for whole- and split-plot designs.

#### New features

- [`how()`](https://gavinsimpson.github.io/permute/reference/how.md), a
  new function to create permutation designs. This replaces the
  deprecated function `permControl`.

- **permute** gains the addition of true blocking structures with which
  to restrict the permutations. Blocks sit as the outermost layer of the
  permutations, and can contain plots which in turn contain samples. In
  contrasts to plots, blocks are never permuted and samples are never
  shuffled between blocks. Permutation only ever happens within blocks.

  To facilitate this, plot-level strata are now specified via
  [`Plots()`](https://gavinsimpson.github.io/permute/reference/how.md)
  instead of via the old `strata` argument of
  [`how()`](https://gavinsimpson.github.io/permute/reference/how.md).
  Blocks are specified via a new argument `blocks`, which takes a factor
  variable.

- A new suite of extractor and replacement functions is provided with
  which to interact with permutation designs created by
  `how(). Extractor functions have names`getFoo()`, where`Foo()`is a component of the design. Replacement functions have names`setFoo\`.
  The replacement function are especially for use by package authors
  wishing to alter permutation within their functions. The extractor
  functions are recommended for all users wishing to extract features of
  the permutation design.

- As a convenience for users, the function will now work with objects of
  classes `"how"`, `"Plots"` or `"Within"` to allow quick updating of
  features of the permutation design. This approach is intended for
  interactive use at the top-level and not within functions, where the
  new `setFoo` replacement functions should be used.

- [`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md)
  is enhanced in this version. Firstly, the function now returns a
  classed object which has a
  [`print()`](https://rdrr.io/r/base/print.html) method to allow for
  compact printing of the design elements used to generate the set of
  permutations. Second,
  [`shuffleSet()`](https://gavinsimpson.github.io/permute/reference/shuffleSet.md)
  will sample `nset` permutations from the entire set of permutations
  should a small number of possible permutations trigger generation of
  the entire set. This avoids the generation of a set of non-unique
  permutations. Finally the random seed that generated the set is stored
  as an attribute.

- [`allPerms()`](https://gavinsimpson.github.io/permute/reference/allPerms.md)
  no longer assumes that samples are in block and/or plot ordering.

- The package vignette is much expanded in this version with new
  sections on using **permute** within functions that will be of
  interest to package authors wishing to use **permute** in their
  packages.

#### Deprecated

- `permControl()` is deprecated in favour of
  [`how()`](https://gavinsimpson.github.io/permute/reference/how.md).

- `permuplot()` is broken and effectively defunct given the changes to
  the way permutation are defined and the addition of blocks.
  `permuplot()` is no longer exported from the package namespace.
