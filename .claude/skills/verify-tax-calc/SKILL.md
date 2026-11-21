---
name: verify-tax-calc
description: Sanity-check tax_payable() output against hand calculations after changing tax logic or data tables.
---

# Verify tax calculations

1. `Rscript -e 'devtools::load_all(); print(tax_payable(c(300000,700000,1000000,1500000,2500000), tax_regime="new"), partial=FALSE)'` and the same with `"old"`.
2. Hand-compute each case: taxable = income − (basic exemption + standard deduction) − capped investments (old only) → sum slab tax → subtract 87A if eligible → ×1.04. Check slab boundaries (slab.max values) and the 87A cliff.
3. Test the bulk path with `declared_80C`/`declared_NPS` columns (see demo in `_make_package.R`), and confirm the caps from `data.rebate_on_investments` apply.
4. Cross-check against `misc/developments/New Comparison ... .xlsx` where applicable.
5. Report any mismatch with both numbers; mind the known quirks (taxable_amount for 87A uses post-deduction amount; FY fallback to 2024 in data.frame method).
6. Encode verified cases as testthat tests in `tests/testthat/test-tax_payable.R`.
