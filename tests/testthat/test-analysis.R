library(dplyr)
library(magrittr)
library(purrr)
library(rlang)
library(testthat)

#### 2. Sampling ####
#### ~2.1 Creating multiple samples with probabilities biased by category ####
context("Biased samples")

test_that("`size` argument works", {
  expect_equal(object = length(sample_biased(data = CO2, x = Treatment, size = 1,
                                             probs = c(1, 1))$Treatment),
               expected = 1)
  expect_equal(object = length(sample_biased(data = CO2, x = Treatment, size = 4,
                                             probs = c(1, 1))$Treatment),
               expected = 4)
  expect_equal(object = length(sample_biased(data = CO2, x = Treatment, size = 4.9,
                                             probs = c(1, 1))$Treatment),
               expected = 4)
  expect_equal(object = length(sample_biased(data = CO2, x = Treatment, size = "4",
                                             probs = c(1, 1))$Treatment),
               expected = 4)
  expect_error(object = sample_biased(data = CO2, x = Treatment, size = -2,
                                      probs = c(1, 1))$Treatment)
  expect_error(object = sample_biased(data = CO2, x = Treatment,
                                      size = length(CO2$Treatment) + 1,
                                      probs = c(1, 1))$Treatment)
  expect_warning(object = sample_biased(data = CO2, x = Treatment,
                                        size = length(CO2$Treatment),
                                        probs = c(1, 1))$Treatment)
})

test_that("`iterations` argument works", {
  expect_equal(object = length(sample_biased(data = CO2, x = Treatment, size = 4,
                                             iterations = 4,
                                             probs = c(1, 1))$Treatment),
               expected = (4 * 4))
  expect_equal(object = length(sample_biased(data = CO2, x = Treatment, size = 4,
                                             iterations = 5,
                                             probs = c(1, 1))$Treatment),
               expected = (5 * 4))
  expect_equal(object = length(sample_biased(data = CO2, x = Treatment, size = 4,
                                             iterations = 5.8,
                                             probs = c(1, 1))$Treatment),
               expected = (5 * 4))
  expect_equal(object = length(sample_biased(data = CO2, x = Treatment, size = 4,
                                             iterations = "4",
                                             probs = c(1, 1))$Treatment),
               expected = (4 * 4))
  expect_error(object = sample_biased(data = CO2, x = Treatment, size = 4,
                                      iterations = -1, probs = c(1, 1))$Treatment)
})

test_that("`probs` argument works", {
  expect_equal(object = length(sample_biased(data = CO2, x = Treatment, size = 4,
                                             probs = c(100, 0.5))$Treatment),
               expected = 4)
  expect_equal(object = length(sample_biased(data = CO2, x = Treatment, size = 4,
                                             probs = c(0.1, 0.04))$Treatment),
               expected = 4)
  expect_equal(object = length(sample_biased(data = CO2, x = Treatment, size = 4,
                                             probs = c(0.1, "0.5"))$Treatment),
               expected = 4)
  expect_error(object = sample_biased(data = CO2, x = Treatment, size = 4,
                                      probs = c(0.1, -4))$Treatment)
  expect_error(object = sample_biased(data = CO2, x = Treatment, size = 4,
                                      probs = c(0.1, 0.04, 0.5))$Treatment)
})

test_that("`replace` argument works", {
  expect_error(object = length(sample_biased(data = CO2, x = Treatment,
                                             size = length(CO2$Treatment) + 1,
                                             replace = FALSE,
                                             probs = c(1, 1))$Treatment))
  expect_equal(object = length(sample_biased(data = CO2, x = Treatment,
                                             size = length(CO2$Treatment) + 1,
                                             replace = TRUE,
                                             probs = c(1, 1))$Treatment),
               expected = length(CO2$Treatment) + 1)
})


#### 3. Weighting ####
#### ~3.1 Calc proportions of categories for one variable ####
context("Calculate Proportions")
test_that("`variable` argument works", {
  expect_equal(object = unlist(unname(c(
    round(calculate_props(data = CO2, variable = Plant)[1, 3], 4)
  ))),
  expected = 0.0833)
  expect_equal(object = unlist(unname(c(
    round(calculate_props(data = CO2, variable = Type)[1, 3], 1)
  ))),
  expected = 0.5)
  expect_equal(object = unlist(unname(c(
    round(calculate_props(data = CO2, variable = uptake)[1, 3], 4)
  ))),
  expected = 0.0119)
  expect_equal(object = nrow(calculate_props(data = CO2, variable = uptake)[, 1]),
               expected = length(as.numeric(names(
                 table(CO2$uptake)
               ))))
  expect_equal(object = nrow(calculate_props(data = CO2, variable = Type)),
               expected = 2)
  expect_error(object = calculate_props(data = CO2, variable = c(Type, Plant)))
})

