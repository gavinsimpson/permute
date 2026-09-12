# Contributing to permute

Thank you for considering a contribution to **permute**. Bug reports,
documentation improvements, examples, tests, and focused code changes are all
welcome.

## Questions and bug reports

Use the [GitHub issue tracker](https://github.com/gavinsimpson/permute/issues)
for questions, feature proposals, and reproducible bug reports. Before opening
a new issue, please search existing issues. A useful bug report includes the
smallest reproducible example, the observed and expected behaviour, and the
output of `sessionInfo()`.

Please do not use the issue tracker to disclose a sensitive security problem;
contact the maintainer at `ucfagls@gmail.com` instead.

## Development setup

Fork and clone the repository, then install the development dependencies from
R. The package intentionally has few runtime dependencies; packages listed in
`Suggests` are needed for the full test, vignette, and documentation checks.

```r
install.packages(c(
  "devtools", "knitr", "rmarkdown", "sessioninfo", "testthat", "vegan"
))
devtools::install_deps(dependencies = TRUE)
```

Create a branch for the change. Keep commits focused and do not include
generated files unrelated to the contribution.

## Tests and checks

Add or update tests for behavioural changes. Before submitting a pull request,
run:

```r
devtools::test()
devtools::check()
```

If exported documentation changes, regenerate it with:

```r
devtools::document()
```

If a vignette changes, confirm that it builds without relying on local files,
credentials, or network access. Pull requests are checked on current and older
R releases on Linux, macOS, and Windows.

## Pull requests

Describe why the change is needed and summarize its user-visible effect. Link
related issues, include tests where appropriate, and update documentation or
`NEWS.md` for user-facing changes. By participating, you agree to follow the
[code of conduct](CODE_OF_CONDUCT.md).
