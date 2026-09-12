# Package readiness review

Assessment date: 12 September 2026

This review compares **permute** with the current
[rOpenSci package development guide](https://devguide.ropensci.org/), its
[package-building guidance](https://devguide.ropensci.org/pkg_building.html),
and the checks collected by
[`goodpractice`](https://docs.ropensci.org/goodpractice/). It also notes items
that are visible in the
[JOSS review checklist](https://joss.readthedocs.io/en/latest/review_checklist.html).
The guide is used as a benchmark; this document does not claim that **permute**
has undergone rOpenSci software peer review.

## Executive assessment

**permute** is technically strong and already satisfies the main software
quality expectations for a JOSS submission. It is mature, openly developed,
licensed, documented, tested, and checked across operating systems. Its hard
dependency footprint is unusually small. The main pre-submission weaknesses
were project-governance and onboarding material rather than the statistical
implementation: there was no contributor guide, code of conduct, citation
metadata file, or concise installation and usage path in the README. Those
items have been added on the `joss-paper` branch.

The audit does not recommend a broad refactor before JOSS submission. The
remaining items are maintenance improvements or portability refinements and
can be handled independently of the paper.

## Standards matrix

| Area | Status | Evidence and critique | Recommendation |
|---|---|---|---|
| Purpose and scope | Pass | `DESCRIPTION`, README, package help, and the principal vignette consistently describe restricted permutation generation. The scope is focused and does not duplicate model fitting or test statistics. | Preserve this narrow positioning in the JOSS paper. |
| Open development | Pass | Public Git history extends from 2010 to the present; the default branch is `main`, and issues are public. | Link the repository and issue tracker prominently. |
| Licensing and copyright | Pass | `License: GPL-2`, a `LICENSE` file, and `inst/COPYRIGHTS` identify licensing and inherited copyright. GPL-2 is OSI approved. | Express the machine-readable licence as `GPL-2.0-only` in `CITATION.cff`. |
| Authorship metadata | Pass | `Authors@R` records author, copyright holder, contributor, creator, and Gavin Simpson's ORCID. | Keep `DESCRIPTION` as the authority for software roles; paper authorship is separate. |
| Dependencies | Pass | The only hard import is the recommended `stats` package. Optional integration, tests, and documentation are in `Suggests`. | No change. Avoid adding a runtime dependency for paper infrastructure. |
| Installation | Fixed | The package is available from CRAN, but the previous README did not show installation. | The README now gives CRAN and GitHub installation commands. |
| Quick usage | Fixed | A detailed vignette existed, but the previous README did not provide a minimal path from installation to a result. | The README now demonstrates `how()`, `Within()`, and `shuffleSet()`. |
| Positioning and related software | Fixed | Package documentation described CANOCO and `vegan`, but did not concisely distinguish statistical exchangeability designs from test-oriented and combinatorics packages. | The README and paper now make that distinction and explicitly credit ter Braak and CANOCO. |
| Contribution pathway | Fixed | No root contributor guidance previously existed, although issues and pull requests were enabled. | `CONTRIBUTING.md` now covers issues, development setup, tests, documentation, and pull requests. |
| Code of conduct | Fixed | No code of conduct previously existed. This is a visible JOSS community-guideline requirement. | Contributor Covenant 2.1 is now included with an enforcement contact. |
| Citation metadata | Fixed | The package had no `CITATION.cff`; `citation("permute")` therefore fell back to package metadata. | A CFF file now describes release 0.9-10. Add the JOSS DOI and preferred article citation after acceptance. |
| API documentation | Pass | Exported interfaces have generated Rd documentation, value descriptions, and examples. Roxygen Markdown is enabled and package-level documentation is present. | Family cross-references could improve navigation but are not a release or JOSS blocker. |
| Long-form documentation | Pass | `vignettes/permutations.Rmd` is a substantial tutorial covering motivation, design construction, hierarchy, use in functions, and computation. A structured pkgdown site is deployed. | Keep the JOSS example short and direct readers to the vignette for exhaustive coverage. |
| Automated tests | Pass | Tests cover generation, enumeration, checking, formulas, partitions, accessors, coercion, print contracts, edge cases, and seeded behaviour. Measured line coverage is approximately 99.1%, well above rOpenSci's 75% guidance. | Maintain behavioural and error-path coverage as the API evolves. |
| Continuous integration | Pass | R CMD check runs on Linux, macOS, and Windows for devel, release, and oldrel R. Separate workflows run coverage, reverse-dependency checks, and pkgdown. | Include the JOSS branch in the relevant paper workflow; existing package CI remains authoritative for package checks. |
| Cross-platform operation | Pass with note | Package tests and examples pass locally. The main vignette includes a socket-cluster demonstration that cannot run in sandboxes where socket creation is prohibited. This is an environment restriction, not evidence of a core permutation failure. | In later maintenance, guard the parallel example, use a serial fallback, or avoid executing the socket portion during constrained checks. |
| Input validation and errors | Pass | Design checking rejects incompatible dimensions, invalid strata, impossible plot structures, and unsupported supplied permutations. Tests exercise these failures. | No pre-submission change. |
| Reproducibility | Pass | Tests explicitly cover seeded RNG compatibility; permutation matrices retain control metadata and can be reused. | Highlight this in the paper and retain seeded regression tests. |
| Package build hygiene | Pass after paper exclusion | `.Rbuildignore` already excludes development and community files but did not exclude the new manuscript directory. | `^paper$` has been added. |
| Test framework currency | Partial | `testthat >= 3.1.5` is suggested, but no `Config/testthat/edition` field is declared and `tests/testthat.R` uses the traditional bootstrap. The suite is nevertheless comprehensive and functional. | Consider an edition-3 migration separately; do not combine it with paper preparation. |
| Static style checks | Optional gap | No `.lintr` policy or lint workflow is present. rOpenSci encourages consistent style but does not require a specific formatter. | Adopt linting only after agreeing project-wide conventions; avoid a paper-driven mechanical rewrite. |
| Security policy | Not applicable / optional | The package is local statistical software with no network service, authentication, or privileged data path. Vulnerabilities can be reported privately using the contributor-guide contact. | A separate `SECURITY.md` is optional rather than a JOSS blocker. |

## Verification performed

- The Git working tree was clean before the paper branch was created.
- A temporary-source R CMD build and check completed package installation,
  namespace loading, examples, code checks, documentation checks, and tests.
  Documentation warnings caused by deliberately building without vignettes
  are not package defects. Executing the vignette code reached the socket
  restriction described above.
- `covr::package_coverage()` reported approximately 99.13% overall coverage.
- The repository already contained cross-platform R CMD check, coverage,
  reverse-dependency, and pkgdown workflows.

## Priorities after submission setup

1. Resolve the acknowledgements and funding TODO and have both authors verify
   the manuscript's historical and technical claims.
2. Confirm the JOSS PDF workflow and all existing package checks on GitHub.
3. Treat the parallel-vignette guard as the highest-value remaining package
   portability improvement.
4. Consider testthat edition, documentation cross-linking, and lint policy only
   as separate maintenance work.
