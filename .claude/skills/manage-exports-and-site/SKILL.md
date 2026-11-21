---
name: manage-exports-and-site
description: Keep EasyCalculator's public API minimal — only user-facing calculators exported and shown on the pkgdown site; helpers, data tables and S3 methods stay internal. Use when adding/reviewing roxygen tags, NAMESPACE, or _pkgdown.yml.
---

# Exports and the pkgdown reference index

## Policy
| Kind | `@export` | `@keywords internal` | In `_pkgdown.yml` |
|---|---|---|---|
| Calculator (`tax_payable`, future `gst_*`, `loan_*`) | yes | no | yes |
| Helper (`make_slab_adjustments`) | no | yes | no |
| Data table (`data.taxslabs`, `data.taxrebate`, `data.taxrebate_under87A`, `data.rebate_on_investments`) | no | yes | no |
| S3 method (`print.ITR`, `tax_payable.default`) | yes (registration only) | yes (except methods documented under the generic's `@rdname`) | no |

`@keywords internal` makes pkgdown omit the topic from the index, and `check_pkgdown()` won't demand it be listed. Internal objects are still available to package code, and to tests via `devtools::test()`; users reach them with `EasyCalculator:::name`.

## Audit (run after any roxygen change)
1. `devtools::document()`
2. `grep '^export' NAMESPACE` → only public calculators.
3. `grep -L "keywords internal" man/*.Rd` → only public topics (plus `tax_payable.Rd`) should be listed.
4. `Rscript -e 'pkgdown::check_pkgdown()'` → "No problems found".
5. If a new calculator was added, put it in `_pkgdown.yml` under a sensible section.

Do **not** add internal topics to `_pkgdown.yml` "Internals" sections; hide them instead. Do not hand-edit `NAMESPACE`/`man/`.
