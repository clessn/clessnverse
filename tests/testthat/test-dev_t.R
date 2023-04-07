test_that("removing accents works", {
  expect_equal(rm_accents(c("áćéíńóśúÁĆÉÍŃÓŚÚýÝźŹ")), "aceinosuACEINOSUyYzZ")  # Acute
})
