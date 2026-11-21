---
name: build-and-check
description: Regenerate docs and run R CMD check, build, or install for EasyCalculator. Use after any change to R/ code or roxygen comments.
---

# Build and check

Run from the repo root (use `Rscript -e`):

1. `Rscript -e 'devtools::document()'` — regenerates `man/` and `NAMESPACE`. Review the diff; these are generated, never hand-edit.
2. `Rscript -e 'devtools::check(manual = FALSE)'` — report ERRORs/WARNINGs/NOTEs verbatim. Known likely findings: placeholder License, undeclared Imports (plyr/stats), `.Rd` source filename mismatch.
3. Optional: `Rscript -e 'devtools::build(path = "misc/release")'` (the scaffold script's `release/` path doesn't exist here), `devtools::install()`.

Don't run `_make_package.R` as a whole. Don't build/install without being asked.
