## Update

This is an update from permute 0.10-0 to 0.10-1. It adds joint permutations
of plot-level variables with unequal sample replication, and fixes whole-plot
enumeration and evaluated NULL arguments. The package works with current
CRAN dependencies and does not require an unreleased version of vegan.

## R CMD check results

0 errors | 0 warnings | 0 notes

* Local macOS Tahoe 26.7.3 (arm64), R 4.6.2, using
  `R CMD check --as-cran` on `permute_0.10-1.tar.gz`.
* Current CRAN vegan 2.7-8 was installed in an isolated library for the check.
* All 1,093 tests passed, with no skipped tests. Examples, incoming checks,
  vignette rebuilding, and the PDF and HTML manuals were checked.

## Reverse dependency checks

CRAN vegan 2.7-8 passed full local `R CMD check` with both released permute
0.10-0 and candidate permute 0.10-1: 0 errors, 0 warnings and 0 notes in
both checks. There were no new problems.

All 32 CRAN reverse dependencies completed checks with released permute
0.10-0 and development permute 0.10-0-1 at commit 3614506. The workflow's
comparison found no changes between versions; this was independently
confirmed using the downloaded artifacts and
`tools::check_packages_in_dir_changes()`:
[reverse dependency checks](https://github.com/gavinsimpson/permute/actions/runs/44522558910).

For both versions, 23 reverse dependencies were OK, one had a warning and
eight had notes. These were unchanged baseline issues:

* BiodiversityR: an installation warning because the headless runner has
  no DISPLAY, so Tk is unavailable.
* LOCOM2, bipartite, npcure, permuco, rankdist and vegan: native-code
  inspection notes because `nm` reported no symbols.
* Storm: a trailing period in its DESCRIPTION Title field.
* monoClust: unescaped braces in Rd documentation.
* vegan: ordering differences in saved test output. Its full local checks
  above were clean with both versions of permute.

## GitHub validation

The package-check matrix passed on all five configurations: Ubuntu
R-devel, R-release and R-oldrel-1, plus macOS and Windows R-release.
Ubuntu R-release reported Status: OK and all 1,093 tests passed. The
initial Linux attempts failed during Ubuntu package downloads before
checking permute; all three passed on retry:
[package checks](https://github.com/gavinsimpson/permute/actions/runs/44522483057).
