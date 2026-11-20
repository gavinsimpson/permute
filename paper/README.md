# Building the JOSS paper

`paper.qmd` is the editable manuscript source. JOSS consumes the generated
`paper.md`; both files are committed so that the submission branch always
contains the format expected by the journal.

## Render Markdown locally

Install `knitr` and `rmarkdown` in the R installation that Quarto uses.
From the repository root, install the checked-out package and render the
manuscript:

```sh
R CMD INSTALL .
quarto render paper/paper.qmd
git diff -- paper/paper.md
```

The Quarto configuration emits Pandoc Markdown with a YAML metadata block,
leaves citations in `[@key]` form for JOSS/Inara, and preserves source
wrapping. Commit changes to `paper.qmd` and the regenerated `paper.md`
together.

## Build the JOSS PDF

The GitHub Actions workflow passes the committed `paper/paper.md` directly
to the Open Journals draft action and uploads `paper/paper.pdf` as a workflow
artifact. To run the same Inara build locally with Docker, follow the
[Open Journals draft action instructions](https://github.com/openjournals/openjournals-draft-action).

Before submission, replace the acknowledgements funding TODO, review all
claims and references, and confirm that the manuscript remains within the
JOSS word limit.
