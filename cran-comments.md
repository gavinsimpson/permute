Comments to CRAN maintainers for submission
===========================================

## Version 0.9-9000

This release adds formula-based permutation strata, symmetric grid designs,
reusable permutation indices, sequential access to permutation sets, vector
permutations, and partition designs. It also fixes permutation enumeration and
constructor/update behaviour. See NEWS.md for the full list of changes.

## Test environments

* Local: R 4.6.1, aarch64-apple-darwin23, macOS Tahoe 26.6.1.
* GitHub Actions: macOS and Windows (R release), and Ubuntu (R devel, release,
  and oldrel-1). Checks for this version are triggered by the release-preparation
  push; results are pending.
* win-builder and mac-builder: pending; the maintainer will initiate these checks.

## R CMD check information

The source package was built with `R CMD build --compact-vignettes=gs` and
checked locally with `R CMD check --as-cran`.

Result: 0 errors, 0 warnings, 1 NOTE.

The NOTE from the CRAN incoming-feasibility check is:

```
Version contains large components (0.9-9000)
```

This is expected for the requested version number and is reported by the
incoming-feasibility checks enabled by `--as-cran`. Tests, examples, vignette
rebuilding, and the PDF and HTML manuals were checked successfully. No
`R CMD check` stages were skipped.

## Previous reverse dependency checks

The most recent locally audited GitHub Actions run checked 32 reverse
dependencies, comparing CRAN permute 0.9-10 with development permute 0.9-10-7:

https://github.com/gavinsimpson/permute/actions/runs/33990884341

One regression was found in douconca 1.2.5.1: six whole-object reference
comparisons fail because the nested permutation control metadata now includes
`symmetric`. The statistical result payloads are unchanged. This was reported
upstream, and a compatibility patch was proposed:

https://github.com/Biometris/douconca/issues/2
https://github.com/Biometris/douconca/pull/3

All other non-OK results in that run also occurred with CRAN permute and were
unrelated to these changes. Reverse dependency checks have not been rerun for
this version bump.
