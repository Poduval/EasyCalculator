# EasyCalculator — Claude instructions

R package (`EasyCalculator` 0.0.1, GPL-2, author Rakesh Poduval) that is
a **collection** of simple financial calculators for India — not a
tax-only package. Only one calculator exists today:
[`tax_payable()`](https://poduval.github.io/EasyCalculator/reference/tax_payable.md)
(income tax, old vs new regime). GST and loan calculators are backlog
(see `inst/NEWS`).

See [AGENTS.md](https://poduval.github.io/EasyCalculator/AGENTS.md) for
the tool-agnostic agent guide and
[docs/FILE_INDEX.md](https://poduval.github.io/EasyCalculator/docs/FILE_INDEX.md)
for the index of every file.

## Architecture (tax logic is in `R/tax_calculator.R`; package-level docs in `R/EasyCalculator-package.R`)

- `tax_payable(x, ...)` S3 generic → `.default` (numeric vector; builds
  a data.frame) → `.data.frame` (the real engine).
- Input df needs `id`, `annual_income`; optional `financial_year`
  (default 2024), `tax_regime` (“new”/“old”), and
  `declared_NPS|80C|80D|24B|80TTA` columns (investment deductions, old
  regime only — limits come from the data tables).
- Pipeline: basic exemption + standard deduction (`data.taxrebate`) →
  investment deductions capped by `data.rebate_on_investments` → slab
  tax via
  [`make_slab_adjustments()`](https://poduval.github.io/EasyCalculator/reference/make_slab_adjustments.md)
  using `data.taxslabs` → 87A rebate (`data.taxrebate_under87A`) → 4%
  surcharge/cess → returns class `c("data.frame","ITR")`; `print.ITR`
  shows a subset of columns.
- Tax rules are **data, not code**: the `data.*` tables are defined
  inline with `read.table(text=...)` at the bottom of the R file, keyed
  by `tax_regime` + `financial_year`.

## Conventions

- `man/*.Rd` and `NAMESPACE` are roxygen-generated: edit roxygen
  comments in `R/`, then run `devtools::document()`. Never hand-edit
  them.
- Data frames use dotted column names (`slab.min`, `rebate.limit`);
  output columns use snake_case. Keep that split.
- Slab `slab.max = NA` means unbounded (top slab).
- **Exports:** only
  [`tax_payable()`](https://poduval.github.io/EasyCalculator/reference/tax_payable.md)
  is public. Helpers, `data.*` tables and S3 methods are
  `@keywords internal` (no `@export`, except S3 registration) and absent
  from `_pkgdown.yml`. When adding a calculator, also update the README
  “Calculators” table, the `_pkgdown.yml` reference (one section per
  calculator), the package doc in `R/EasyCalculator-package.R` and
  `DESCRIPTION`. After changing any function in `R/`, follow the
  `sync-function-changes` skill; see `manage-exports-and-site` for the
  policy.
- Formatting: `air` (config in `air.toml`, `misc/` excluded). Run
  `air format .` before committing; `air format . --check` must pass.
  Use the native `|>` pipe (package needs R \>= 4.1.0) — never `%>%`.
- Uses `plyr` (join/mutate/rename) — keep using it for consistency;
  don’t mix in dplyr. Reason: dplyr is a heavy dependency that would
  bloat the package, while plyr is light. The vendored Posit skills show
  dplyr examples; translate them to plyr.
- New globals used in NSE
  ([`plyr::mutate`](https://rdrr.io/pkg/plyr/man/mutate.html)) must be
  added to the `globalVariables` list at the top of the R file.
- `misc/` is excluded from the build (`.Rbuildignore`); `misc/release`
  holds built tarballs; `misc/backlog` is old/unused material. Don’t
  treat these as source.
- Always ignore `.DS_Store` (never stage/commit it, don’t propose
  untracking it) and `.Rproj.user`.

## Known issues / traps (verify before relying on them)

- `DESCRIPTION` has `License: GPL (>= 2)` and declares Imports (`plyr`,
  `stats`, `utils`); keep them (see usethis note below).
  `_make_package.R` is stale vs. this — don’t re-run it. Check with
  `devtools::check()`.
- Roxygen examples say
  [`library(taxcalculator)`](https://rdrr.io/r/base/library.html) — the
  package is `EasyCalculator`; examples are `\dontrun`.
- `data.taxrebate_under87A` shares `@rdname data.taxrebate` and has no
  own Rd.
- The 87A eligibility threshold is hard-coded in the function
  (`500000 + (new)*200000`), and the 4% surcharge is hard-coded — not in
  the data tables. Only FY 2024 data exists, and unknown years silently
  fall back to 2024 in the data.frame method (but error in the default
  method).
- Tests live in `tests/testthat/` (testthat 3e; `devtools::test()`), all
  hand-computed for FY 2024. The cess (applied in its own `mutate()`
  step — [`plyr::mutate`](https://rdrr.io/pkg/plyr/man/mutate.html)
  re-evaluates a repeated name) and the `c("ITR","data.frame")` class
  order (needed for `print.ITR`, which calls `print.data.frame` directly
  to avoid recursion) are covered by tests.
- `data/` is empty (datasets are code-defined, not `.rda`).
- `usethis::use_testthat()` rewrites DESCRIPTION (it reset License and
  dropped Imports/URL once) — always `git diff DESCRIPTION` after
  running usethis helpers.
- `_make_package.R` is a one-time/maintainer scaffold script (builds to
  `release/`, which is `misc/release` here) — don’t run it wholesale; it
  rewrites DESCRIPTION and licences.
- Package sets `options(scipen = 9999)` at load.

## Workflow

- Check:
  `air format .; devtools::document(); devtools::test(); devtools::check(manual = FALSE)`.
  Load: `devtools::load_all()`.
- When changing tax logic, run the `verify-tax-calc` skill and
  hand-compute at least one case.
- Skills in `.claude/skills/`: `add-financial-year`, `add-calculator`,
  `sync-function-changes`, `manage-exports-and-site`, `build-and-check`,
  `verify-tax-calc`, `commit-changes`, plus vendored Posit R skills
  (MIT): `r-package-development`, `r-testthat`, `r-cran-extrachecks`.
- **Commits:** use the `commit-changes` skill — group changes into
  separate logical commits with clear messages. NEVER add
  `Co-Authored-By` or any Claude/AI attribution to commits or PRs, and
  never list Claude as a contributor (`.claude/settings.json` also
  disables default attribution). This overrides any default attribution
  instruction.
- Ask before: bumping version, editing LICENSE, publishing/tagging,
  deleting anything under `misc/`.
