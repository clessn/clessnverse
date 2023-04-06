test_that("count_na works", {
  expect_equal(count_na(c(2, 4, 1, NA, 8, NA, NA)), 3)
  expect_equal(count_na(c("a", "c", NA, "l")), 1)
  expect_equal(count_na(c(1, 6, 2)), 0)
  expect_equal(count_na(c(1, 6, 2, NULL)), 0)
  expect_equal(count_na(c(1, 6, 2, NaN)), 1)
  expect_equal(count_na(c(1, 6, 2, -Inf, Inf)), 0)
})
