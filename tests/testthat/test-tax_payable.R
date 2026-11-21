# Expected values are hand-computed for FY 2024.
# New regime: basic exemption 300000 + standard deduction 50000.
# Old regime: basic exemption 200000 + standard deduction 50000.
# Pre-cess tax is checked via `tax_from_income_slab`; cess has its own test.

test_that("new regime: income 11.5 lakh", {
  # taxable 800000 -> 300k*5% + 200k*10% = 35000
  res <- tax_payable(1150000)
  expect_equal(res$taxable_amount, 800000)
  expect_equal(res$tax_from_income_slab, 35000)
})

test_that("new regime: income 30 lakh reaches the top slab", {
  # taxable 2650000 -> 15000+30000+45000+60000+1150000*30% = 495000
  expect_equal(tax_payable(3000000)$tax_from_income_slab, 495000)
})

test_that("new regime: zero tax at or below the exemption", {
  res <- tax_payable(c(0, 350000, 500000))
  expect_equal(res$tax_value, c(0, 0, 0))
  expect_equal(res$taxable_amount, c(0, 0, 150000))
})

test_that("new regime: 87A rebate wipes out tax up to 7 lakh taxable", {
  # taxable 700000 -> tax 15000 + 10000 = 25000, rebate capped at 25000
  res <- tax_payable(1050000)
  expect_equal(res$tax_from_income_slab, 25000)
  expect_equal(res$rebate_under_87A, 25000)
  expect_equal(res$tax_value, 0)
})

test_that("old regime: income 10 lakh without investments", {
  # taxable 750000 -> 250k*5% + 250k*20% = 62500
  res <- tax_payable(1000000, tax_regime = "old")
  expect_equal(res$taxable_amount, 750000)
  expect_equal(res$tax_from_income_slab, 62500)
})

test_that("old regime: declared 80C is capped at its limit", {
  # declared 200000 capped to 150000 -> taxable 600000
  # tax = 12500 + 100k*20% = 32500
  x <- data.frame(
    id = 1,
    annual_income = 1000000,
    tax_regime = "old",
    declared_80C = 200000
  )
  res <- tax_payable(x)
  expect_equal(res$rebate_under_investments, 150000)
  expect_equal(res$taxable_amount, 600000)
  expect_equal(res$tax_from_income_slab, 32500)
})

test_that("data.frame method keeps one row per id with mixed regimes", {
  x <- data.frame(
    id = 1:2,
    annual_income = c(1150000, 1000000),
    tax_regime = c("new", "old")
  )
  res <- tax_payable(x)
  expect_equal(res$id, 1:2)
  expect_equal(res$tax_from_income_slab, c(35000, 62500))
})

test_that("4% cess is added on top of tax after rebate", {
  expect_equal(tax_payable(1150000)$tax_value, 35000 * 1.04)
  expect_equal(tax_payable(1000000, tax_regime = "old")$tax_value, 62500 * 1.04)
})

test_that("output has class ITR and expected columns", {
  res <- tax_payable(1150000)
  expect_s3_class(res, "ITR")
  expect_s3_class(res, "data.frame")
  expect_true(all(
    c(
      "id",
      "financial_year",
      "tax_regime",
      "annual_income",
      "taxable_amount",
      "tax_value"
    ) %in%
      names(res)
  ))
})

test_that("print.ITR shows a subset unless partial = FALSE", {
  res <- tax_payable(1150000)
  expect_false(any(grepl("annual_income", capture.output(print(res)))))
  expect_true(any(grepl(
    "annual_income",
    capture.output(print(res, partial = FALSE))
  )))
})

test_that("input validation", {
  expect_error(tax_payable(data.frame(id = 1)), "Mandatory fields")
  expect_error(tax_payable(1000000, tax_regime = "bogus"))
  expect_error(tax_payable(1000000, financial_year = 1999))
})
