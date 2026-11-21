---
name: add-financial-year
description: Add tax slabs, exemptions, 87A rebate and investment limits for a new financial year to EasyCalculator. Use when asked to support a new FY (e.g. 2025) or update rates after a Budget.
---

# Add a financial year

1. Ask the user for (or confirm) the official figures per regime: slabs, basic exemption, standard deduction, 87A limit **and income threshold**, investment limits (old regime), cess/surcharge. Don't guess; cite the source.
2. In `R/tax_calculator.R`, append rows for the new `financial_year` to `data.taxslabs`, `data.taxrebate`, `data.taxrebate_under87A`, `data.rebate_on_investments`. Reuse `rebate.id`/`slab.id` numbering per (regime, year); last slab has `slab.max = NA`.
3. The 87A threshold (`500000 + (tax_regime == "new") * 200000`) and 4% cess are hard-coded in `tax_payable.data.frame`. If the new FY differs, move them into data columns rather than adding year branches.
4. The default `financial_year = 2024` in `tax_payable.default` and the `.data.frame` fallback: update only if the user wants a new default.
5. Run `build-and-check`, then `verify-tax-calc` for both regimes with hand-computed cases at slab boundaries.
6. Add an entry in `inst/NEWS`; don't bump the version without asking.
