test_that("normalization works", {
  expect_equal(normalize_min_max(c(4, 0, 2, 0)), c(1, 0, 0.5, 0))
})
