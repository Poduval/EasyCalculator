---
name: r-package-development
description: R package development with devtools, testthat, and roxygen2. Use when the user is working on an R package, running tests, writing documentation, or building package infrastructure.
metadata:
  author: Simon P. Couch (@simonpcouch)
  version: "1.2"
---

# R package development

> Vendored from [posit-dev/skills](https://github.com/posit-dev/skills) (MIT, see `../POSIT_SKILLS_LICENSE`). **Local overrides for EasyCalculator:** `CLAUDE.md` wins on conflicts. In particular: use `plyr` (not dplyr); the changelog is `inst/NEWS` (no `NEWS.md`); the pkgdown site is configured in `_pkgdown.yml` (output `pkgdown-site/`, deployed by `.github/workflows/pkgdown.yaml`) so `pkgdown::check_pkgdown()` applies; pkgdown only reads `NEWS.md`, so the `inst/NEWS` changelog gets no News page; `air format` is not set up, so skip it; keep existing code style rather than forcing `|>`; and `man/` + `NAMESPACE` are roxygen-generated, never hand-edited.

## Key commands

```
# Run code in the package
Rscript -e "devtools::load_all(); code"

# Run all tests
Rscript -e "devtools::test()"

# Run all tests for files starting with {name}
Rscript -e "devtools::test(filter = '^{name}')"

# Run all tests for R/{name}.R
Rscript -e "devtools::test_active_file('R/{name}.R')"

# Run a single test "blah" for R/{name}.R
Rscript -e "devtools::test_active_file('R/{name}.R', desc = 'blah')"

# Redocument the package
Rscript -e "devtools::document()"

# Check pkgdown documentation
Rscript -e "pkgdown::check_pkgdown()"

# Check the package with R CMD check
Rscript -e "devtools::check()"

# Format code
air format .
```

## Coding

* Always run `air format .` after generating code.
* Use the base pipe operator (`|>`) not the magrittr pipe (`%>%`).
* Use `\() ...` for single-line anonymous functions. For all other cases, use `function() {...}`.

## Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
- All new code should have an accompanying test.
- If there are existing tests, place new tests next to similar existing tests.
- Strive to keep tests minimal with few comments.
- Avoid `expect_true()` and `expect_false()` in favour of a specific expectation which will give a better failure message.
- When testing errors and warnings, don't use `expect_error()` or `expect_warning()`. Instead, use `expect_snapshot(error = TRUE)` for errors and `expect_snapshot()` for warnings because these allow the user to review the full text of the output.

## Documentation

- Every user-facing function should be exported and have roxygen2 documentation.
- Wrap roxygen comments at 80 characters.
- Internal functions should not have roxygen documentation.
- Whenever you add a new (non-internal) documentation topic, also add the topic to `_pkgdown.yml`.
- Always re-document the package after changing a roxygen2 comment.
- Use `pkgdown::check_pkgdown()` to check that all topics are included in the reference index.

## `inst/NEWS` (changelog)

- Every user-facing change should be given a bullet in `inst/NEWS` (this project has no `NEWS.md`; do not create one). Do not add bullets for small documentation changes or internal refactorings.
- Each bullet should briefly describe the change to the end user.
- If the change is related to a function, put the name of the function early in the bullet.
- If the bullet is related to a GitHub issue or pull request, reference it by number in parentheses before the final period: `(#123).`.
- Order bullets alphabetically by function name. Put all bullets that don't mention function names at the beginning.
