test_that("income below the slab minimum attracts no tax", {
  expect_equal(make_slab_adjustments(100000, 300000, 600000, 0.05), 0)
})

test_that("income inside a slab is taxed only on the part above slab minimum", {
  expect_equal(make_slab_adjustments(400000, 300000, 600000, 0.05), 5000)
})

test_that("income above a slab is capped at the slab width", {
  expect_equal(make_slab_adjustments(1000000, 300000, 600000, 0.05), 15000)
})

test_that("unbounded top slab (NA max) has no cap", {
  expect_equal(make_slab_adjustments(2000000, 1500000, NA, 0.3), 150000)
})

test_that("function is vectorised", {
  expect_equal(
    make_slab_adjustments(c(0, 400000, 1000000), 300000, 600000, 0.05),
    c(0, 5000, 15000)
  )
})
