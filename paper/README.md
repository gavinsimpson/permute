# Building the JOSS paper

`paper.qmd` is the editable manuscript source. JOSS consumes the generated
`paper.md`; both files are committed so that the submission branch always
contains the format expected by the journal.

## Render with Quarto

From the repository root, run:

```sh
quarto render paper/paper.qmd
git diff --exit-code -- paper/paper.md
```

The Quarto configuration emits Pandoc Markdown with a YAML metadata block,
leaves citations in `[@key]` form for JOSS/Inara, and preserves source
wrapping. Commit changes to `paper.qmd` and the regenerated `paper.md`
together.

## Build the JOSS PDF locally

The GitHub Actions workflow uses the Open Journals draft action and publishes
`paper/paper.pdf` as a workflow artifact. If Docker is available, the same
Inara-based build can be run locally following the current instructions in
the [Open Journals draft action](https://github.com/openjournals/openjournals-draft-action).

Before submission, replace the acknowledgements funding TODO, review all
claims and references, and confirm that the manuscript remains within the
JOSS word limit.
