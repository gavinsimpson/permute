# permute (development version)

## User visible changes

* *permute* now depends on R versions >= 3.6.0

## Bug fixes

* `numPerms()` tries harder to avoid floating point issues when computing the
  number of permutations for the current design.