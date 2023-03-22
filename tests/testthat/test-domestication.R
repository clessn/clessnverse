test_that("normalization works", {
  expect_equal(normalize_min_max(c(4, 0, 2, 0)), c(1, 0, 0.5, 0))
})

test_that("reducing outliers works", {
  expect_equal(reduce_outliers(c(5, 4, 6, -24, 5, 10, 2, 0, 2, 3, 32)), c(5, 4, 6, -3.25, 5, 10, 2, 0, 2, 3, 10.75))
})
