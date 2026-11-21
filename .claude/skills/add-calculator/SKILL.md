---
name: add-calculator
description: Add a new financial calculator (e.g. gst_calculator, loan_schedular from the NEWS backlog) to the EasyCalculator package following its conventions.
---

# Add a calculator

1. Create `R/<name>.R` (one file per calculator; keep `tax_calculator.R` for tax).
2. Follow the existing pattern: S3 generic + `.default` (vector) + `.data.frame` (bulk with an `id` column), returning a classed data.frame with a `print` method.
3. Put rates/rules in exported `data.*` tables, not in code. Dotted column names for tables, snake_case for outputs.
4. Document with roxygen (`@export`, `@examples` using `library(EasyCalculator)`); add any NSE column names to `globalVariables`.
5. Add tests in `tests/testthat/`, with hand-computed expected values.
6. Run `build-and-check`; update `README.md`, `inst/NEWS`, `docs/FILE_INDEX.md`, and remove the item from the NEWS backlog.
