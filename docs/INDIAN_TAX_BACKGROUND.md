# Indian income tax — the background behind EasyCalculator

`tax_payable()` is not a guess at how Indian income tax works. Every rule it applies is a published rule of the Income Tax Act, and each one is stored as a data table (not buried in code) so it can be checked against the sources below. This page lists the rules, where they come from, and what the package deliberately leaves out.

## 1. Two regimes, one choice

Resident individuals compute tax under one of two regimes:

- **New regime** (Section 115BAC): lower rates, almost no deductions. It has been the **default** since AY 2024-25; a taxpayer must opt out to use the old regime.
- **Old regime**: higher rates, but investment and expense deductions (80C, 80D, NPS, home-loan interest…) reduce taxable income.

This is why `tax_payable()` takes a `tax_regime` argument and why investment deductions are applied to the old regime only.

Read: [Section 115BAC explained (ClearTax)](https://cleartax.in/s/section-115bac-features-new-tax-regime-benefits) · [Old vs new regime (ClearTax)](https://cleartax.in/s/old-tax-regime-vs-new-tax-regime)

## 2. How tax is computed (the pipeline in the code)

| Step | Rule | Where in the package |
|---|---|---|
| 1 | Subtract basic exemption and **standard deduction** (salaried, ₹50,000) | `data.taxrebate` |
| 2 | Old regime only: subtract declared deductions, each **capped** at its statutory limit | `data.rebate_on_investments` |
| 3 | Apply **progressive slab rates** — each rate applies only to the income inside its band | `data.taxslabs`, `make_slab_adjustments()` |
| 4 | Subtract the **Section 87A rebate** if taxable income is under the threshold | `data.taxrebate_under87A` |
| 5 | Add **4% Health & Education Cess** on the remaining tax | `tax_surcharge` |

The order matters: the Income Tax Department's own computation format shows tax on total income, then the 87A rebate, then cess — and if tax is zero after the rebate, so is the cess.

Official: [Computation of tax for individuals](https://www.incometaxindia.gov.in/w/computation-of-tax-for-individual-1) · [Income tax rates](https://www.incometaxindia.gov.in/w/tax-rates%E2%80%8B) · [How to compute total tax liability](https://www.incometaxindia.gov.in/w/how-to-compute-the-total-tax-liability-)

## 3. The numbers the package uses (data year `2024`)

These match the rules introduced by **Budget 2023** (FY 2023-24 / AY 2024-25):

| | New regime | Old regime |
|---|---|---|
| Basic exemption | ₹3,00,000 | ₹2,50,000 (₹2,00,000 + ₹50,000 standard deduction in the tables) |
| Slabs | 0 / 5 / 10 / 15 / 20 / 30% at ₹3L, 6L, 9L, 12L, 15L | 0 / 5 / 20 / 30% at ₹2.5L, 5L, 10L |
| 87A rebate | up to ₹25,000 if taxable income ≤ ₹7,00,000 | up to ₹12,500 if taxable income ≤ ₹5,00,000 |
| Deduction limits | — | 80C ₹1.5L · NPS 80CCD(1B) ₹50,000 · 24(b) ₹2L (self-occupied) |

Budget 2023 raised the new-regime basic exemption from ₹2.5L to ₹3L, lifted the 87A threshold from ₹5L to ₹7L, and extended the ₹50,000 standard deduction to the new regime.

Read: [Slabs FY 2023-24 (Taxmann)](https://www.taxmann.com/post/blog/income-tax-slab-rates/) · [Changes from April 2023 (ClearTax)](https://cleartax.in/s/income-tax-changes-from-april-2024) · [Slabs FY 2023-24 (IndiaFilings)](https://www.indiafilings.com/learn/income-tax-slab-rates-for-f-y-2023-24-a-y-2024-25)

## 4. Honest limits of the current version

Showing what is *not* modelled is part of being trustworthy. Known gaps:

- **Rules have moved on.** Budget 2024 and Budget 2025 changed slabs, the standard deduction and the 87A rebate (for AY 2026-27 the official site lists a ₹60,000 rebate up to ₹12L under the new regime). Only one data year exists; adding a year is a data-only change (see the `add-financial-year` skill).
- **Year label.** The data is labelled `2024` but follows the FY 2023-24 / AY 2024-25 rules above — worth relabelling when more years are added.
- **Simplified deductions.** 80D is stored as a single ₹1.5L cap; the real limit depends on age and on whether parents are covered. Check the [Section 80D rules](https://cleartax.in/s/income-tax-savings) before relying on it.
- **Not modelled:** surcharge on high incomes, marginal relief just above the 87A threshold, capital-gains rates, HRA/LTA and other exemptions, non-residents, and senior-citizen slabs.

## 5. Where to verify anything here

Treat the [Income Tax Department](https://www.incometaxindia.gov.in/) and the Finance Act of the relevant year as the source of truth; the links above are secondary explainers. Every figure in the package lives in the `data.*` tables at the bottom of [R/tax_calculator.R](../R/tax_calculator.R), so a reviewer can compare them line by line.
