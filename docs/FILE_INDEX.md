# File index

Every tracked file (and notable untracked ones) in the repo. Keep up to date.

## Root
| File | Purpose |
|---|---|
| `DESCRIPTION` | Package metadata (EasyCalculator 0.0.1, deps R >= 3.6.3, roxygen 7.2.3). License field is a placeholder; Imports may be missing. |
| `NAMESPACE` | Roxygen-generated: S3 methods (`print.ITR`, `tax_payable.*`), exports (`tax_payable`, 4 `data.*`), `import(plyr)`, stats imports. |
| `EasyCalculator.Rproj` | RStudio project file (build-ignored). |
| `_make_package.R` | Maintainer scaffold script: usethis setup, build-ignore, GitHub actions, document/check/build/install, plus a commented demo block of usage examples. Not for wholesale execution. |
| `.Rbuildignore` | Excludes Rproj, `.Rproj.user`, `_make_package.R`, `.github`, `LICENSE.md`, `misc`. |
| `.gitignore` | Ignores `.Rproj.user`, `.Rhistory`, `.RData`, `.Ruserdata`, `.DS_Store`. |
| `README.md` | Short overview, install via `devtools::install_github("Poduval/EasyCalculator")`, feature bullets. Sparse. |
| `LICENSE.md` | GPL licence text. |
| `logo.png` | Package hex logo source (referenced by `use_logo`). |
| `.DS_Store` | macOS junk, tracked by mistake; should be untracked. |
| `CLAUDE.md` / `AGENTS.md` | Agent instructions (this guidance set). |

## R/
| File | Purpose |
|---|---|
| `R/tax_calculator.R` | Entire package code: `globalVariables`, `options(scipen)`, generic `tax_payable`, methods `.default` and `.data.frame` (engine), helper `make_slab_adjustments`, `print.ITR`, and datasets `data.taxrebate`, `data.rebate_on_investments`, `data.taxrebate_under87A`, `data.taxslabs` (FY 2024, old + new regime). Trailing `if (FALSE)` block is scratch code. |

## man/ (generated — do not edit)
| File | Purpose |
|---|---|
| `man/tax_payable.Rd` | Docs for the generic and its two methods. |
| `man/make_slab_adjustments.Rd` | Docs for the slab helper (internal, not exported). |
| `man/print.ITR.Rd` | Docs for the print method. |
| `man/data.taxrebate.Rd` | Docs for basic exemption/standard deduction table (also covers `taxrebate_under87A` alias). |
| `man/data.rebate_on_investments.Rd` | Docs for investment deduction limits table. |
| `man/data.taxslabs.Rd` | Docs for slab table. |
| `man/figures/logo.png` | Logo shown in README. |

## inst/
| File | Purpose |
|---|---|
| `inst/NEWS` | Changelog: 0.0.1 initial release + backlog (GST, loan scheduler, summary/plot methods, vignettes, shiny app). |

## data/
Empty (only `.DS_Store`, untracked). Datasets are defined in code, not `.rda`.

## .github/
| File | Purpose |
|---|---|
| `.github/.gitignore` | Ignores rendered html. |
| `.github/workflows/check-standard.yaml` | R-CMD-check on macOS/Windows/Ubuntu (devel, release, oldrel). |
| `.github/workflows/test-coverage.yaml` | Coverage via covr. |
| `.github/workflows/pkgdown.yaml` | Builds/deploys pkgdown site. |

## .claude/skills/
| Skill | Purpose |
|---|---|
| `add-financial-year` | Procedure to add a new FY's tax data. |
| `add-calculator` | Procedure to add a new calculator function. |
| `build-and-check` | document → check → build → install. |
| `verify-tax-calc` | Hand-check recipe for `tax_payable()`. |
| `commit-changes` | Group changes into logical commits; no AI attribution. |
| `.claude/settings.json` | Disables default commit/PR attribution (no Co-Authored-By). |

## misc/ (build-ignored; not source)
| File | Purpose |
|---|---|
| `misc/backlog/data.xlsx` | Early tax data spreadsheet (2023). |
| `misc/backlog/logo_picture_indian_coin_back.png` | Source coin image for the logo. |
| `misc/backlog/new_vs_old_tax_regime_12L.png`, `..._24L.png` | Charts comparing regimes at 12L / 24L income. |
| `misc/backlog/openxlsx_4.2.5.2.tar` | Vendored openxlsx tarball (7 MB; unrelated to package code). |
| `misc/backlog/process_flow.drawio` | Draw.io diagram of the calculation flow. |
| `misc/backlog/sticker.R` | hexSticker script generating `logo.png`. |
| `misc/developments/New Comparison between Old and New Regime self analyse_without formula.xlsx` | Personal old-vs-new regime comparison workbook (2024); useful reference for expected values. |
| `misc/release/EasyCalculator_0.0.1.tar.gz` | Built source tarball of 0.0.1. |
