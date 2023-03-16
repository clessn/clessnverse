data <- tibble::tibble(a = c(2, 0, 0, 0), b = c(0, 0, 4, 0))

data_output <- tibble::tibble(a = c(1, 0, 0, 0), b = c(0, 0, 1, 0))

test_that("normalization works", {
  expect_equal(dplyr::mutate(data, dplyr::across(c(a, b), normalize_min_max)), data_output)
})
