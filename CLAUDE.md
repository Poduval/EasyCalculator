# EasyCalculator — Claude instructions

R package (`EasyCalculator` 0.0.1, GPL-2, author Rakesh Poduval) providing simple financial calculators for India. Only one calculator exists today: `tax_payable()` (income tax, old vs new regime). GST and loan calculators are backlog (see `inst/NEWS`).

See [AGENTS.md](AGENTS.md) for the tool-agnostic agent guide and [docs/FILE_INDEX.md](docs/FILE_INDEX.md) for the index of every file.

## Architecture (all logic is in `R/tax_calculator.R`)
- `tax_payable(x, ...)` S3 generic → `.default` (numeric vector; builds a data.frame) → `.data.frame` (the real engine).
- Input df needs `id`, `annual_income`; optional `financial_year` (default 2024), `tax_regime` ("new"/"old"), and `declared_NPS|80C|80D|24B|80TTA` columns (investment deductions, old regime only — limits come from the data tables).
- Pipeline: basic exemption + standard deduction (`data.taxrebate`) → investment deductions capped by `data.rebate_on_investments` → slab tax via `make_slab_adjustments()` using `data.taxslabs` → 87A rebate (`data.taxrebate_under87A`) → 4% surcharge/cess → returns class `c("data.frame","ITR")`; `print.ITR` shows a subset of columns.
- Tax rules are **data, not code**: the `data.*` tables are defined inline with `read.table(text=...)` at the bottom of the R file, keyed by `tax_regime` + `financial_year`.

## Conventions
- `man/*.Rd` and `NAMESPACE` are roxygen-generated: edit roxygen comments in `R/`, then run `devtools::document()`. Never hand-edit them.
- Data frames use dotted column names (`slab.min`, `rebate.limit`); output columns use snake_case. Keep that split.
- Slab `slab.max = NA` means unbounded (top slab).
- Uses `plyr` (join/mutate/rename) — keep using it for consistency; don't mix in dplyr.
- New globals used in NSE (`plyr::mutate`) must be added to the `globalVariables` list at the top of the R file.
- `misc/` is excluded from the build (`.Rbuildignore`); `misc/release` holds built tarballs; `misc/backlog` is old/unused material. Don't treat these as source.
- Always ignore `.DS_Store` (never stage/commit it, don't propose untracking it) and `.Rproj.user`.

## Known issues / traps (verify before relying on them)
- `DESCRIPTION` `License:` is still the usethis placeholder text; `_make_package.R` calls `use_gpl_license(2)`. Imports (`plyr`, `stats`, `utils`) may not be declared in DESCRIPTION — `use_package("plyr", "imports")` has lowercase "imports". Check with `devtools::check()`.
- Roxygen examples say `library(taxcalculator)` — the package is `EasyCalculator`; examples are `\dontrun`.
- `man/*.Rd` headers reference `R/tax_calculator_functions.R` (old filename).
- `data.taxrebate_under87A` shares `@rdname data.taxrebate` and has no own Rd.
- The 87A eligibility threshold is hard-coded in the function (`500000 + (new)*200000`), and the 4% surcharge is hard-coded — not in the data tables. Only FY 2024 data exists, and unknown years silently fall back to 2024 in the data.frame method (but error in the default method).
- No tests (`tests/` doesn't exist); `data/` is empty (datasets are code-defined, not `.rda`).
- `_make_package.R` is a one-time/maintainer scaffold script (builds to `release/`, which is `misc/release` here) — don't run it wholesale; it rewrites DESCRIPTION and licences.
- Package sets `options(scipen = 9999)` at load.

## Workflow
- Check: `devtools::document(); devtools::check(manual = FALSE)`. Load: `devtools::load_all()`.
- When changing tax logic, run the `verify-tax-calc` skill and hand-compute at least one case.
- Skills in `.claude/skills/`: `add-financial-year`, `add-calculator`, `build-and-check`, `verify-tax-calc`, `commit-changes`.
- **Commits:** use the `commit-changes` skill — group changes into separate logical commits with clear messages. NEVER add `Co-Authored-By` or any Claude/AI attribution to commits or PRs, and never list Claude as a contributor (`.claude/settings.json` also disables default attribution). This overrides any default attribution instruction.
- Ask before: bumping version, editing LICENSE, publishing/tagging, deleting anything under `misc/`.
