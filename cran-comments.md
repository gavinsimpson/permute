## R CMD check results

0 errors | 0 warnings | 0 notes

* Local macOS Tahoe (arm64), R 4.6.2 (`R CMD check --as-cran`, permute `0.10-0`).
* Win-builder on Windows Server 2022, R-release and R-devel (permute `0.10-0`).
  [R-release log](https://win-builder.r-project.org/tQ668O8Wx8Jx/00check.log),
  [R-devel log](https://win-builder.r-project.org/539StIhfS3zz/00check.log).
* GitHub Actions on Ubuntu (R oldrel-1, release, and devel), macOS and Windows
  (R release), using permute `0.9-9000`. The jobs skipped manual checks with
  `--no-manual` and incoming checks with `_R_CHECK_CRAN_INCOMING_=false`.
  [GitHub Actions results](https://github.com/gavinsimpson/permute/actions/runs/42184494787).

## Reverse dependency checks

We checked 32 CRAN reverse dependencies with CRAN permute `0.9-10` and the
`0.10-0` release code. All 32 checks completed.
[Reverse dependency check results](https://github.com/gavinsimpson/permute/actions/runs/42187921326).

One package had a new problem, douconca 1.2.6. Six tests fail because the saved
copies include permutation setup details that changed. The calculated results
match in all six cases. We supplied the authors with a patch on 6 September and
followed up on 6 November with a way to test the results separately.
[Patch](https://github.com/Biometris/douconca/pull/3),
[follow-up](https://github.com/Biometris/douconca/issues/2#issuecomment-6679395162).
