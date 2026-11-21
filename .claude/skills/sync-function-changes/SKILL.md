---
name: sync-function-changes
description: After adding, changing, renaming, or removing any R function or dataset in EasyCalculator, propagate the change to roxygen/man, NAMESPACE, tests, the pkgdown site (gh-pages), README, NEWS, and the docs/agent files. Use whenever R/ changes.
---

# Sync a function change everywhere

Run this checklist whenever a function, S3 method, or `data.*` table in `R/` is added, changed, renamed, or removed. Work through every item; say "n/a" explicitly for those that don't apply.

## 1. Decide visibility first (see `manage-exports-and-site`)
Only `tax_payable()` (and future calculators) is public. Everything else is internal.
- Public: roxygen `@export`, listed in `_pkgdown.yml`.
- Internal helper / `data.*` table / S3 method: **no `@export`** (S3 methods keep `@export` only for registration), add `@keywords internal`, **not** in `_pkgdown.yml`.

## 2. Code and roxygen (`R/`)
- Update the roxygen block: `@param`, `@return`, `@description`, `@examples`, `@keywords`. Examples must use `library(EasyCalculator)`, not `taxcalculator`.
- New NSE globals go in `globalVariables`.
- `air format .`

## 3. Generated files
- `devtools::document()` → review the diff of `NAMESPACE` and `man/*.Rd`. Never hand-edit.
- Check `NAMESPACE` exports: `grep '^export' NAMESPACE` should list only intended public functions.

## 4. Tests (`tests/testthat/`)
- Add/update/delete tests for the changed behaviour (hand-computed values; see `r-testthat`, `verify-tax-calc`). A renamed/removed function must have its test file renamed/removed.
- `devtools::test()`.

## 5. pkgdown site (gh-pages)
- Edit `_pkgdown.yml` `reference:` — add public topics; remove renamed/removed/internal ones.
- `Rscript -e 'pkgdown::check_pkgdown()'` must report "No problems found" (it flags public topics missing from the index).
- Optional local preview: `pkgdown::build_site()` → `pkgdown-site/reference/index.html` should list only public functions. Don't commit `pkgdown-site/`.
- The site deploys from CI (`.github/workflows/pkgdown.yaml`) on push to main; nothing to push to gh-pages by hand.

## 6. Prose and index files
Search for the old/new name everywhere outside `misc/`, `.git`, `pkgdown-site`:
`grep -rIn "<name>" . --exclude-dir=.git --exclude-dir=misc --exclude-dir=pkgdown-site --exclude-dir=.Rproj.user`
Update each hit as needed:
- `README.md` (usage examples)
- `inst/NEWS` (user-visible change; don't bump the version without asking)
- `CLAUDE.md` and `AGENTS.md` (architecture / pipeline / traps)
- `docs/FILE_INDEX.md` (function lists, NAMESPACE description, any added/removed file)
- `docs/INDIAN_TAX_BACKGROUND.md` (if tax rules changed)
- other skills that mention the function (`add-financial-year`, `add-calculator`, `verify-tax-calc`)

## 7. Final gate
`air format . --check; devtools::document(); devtools::test(); devtools::check(manual = FALSE); pkgdown::check_pkgdown()` — report results verbatim, then use `commit-changes`.
