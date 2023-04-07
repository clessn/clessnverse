test_that("removing accents works", {
  expect_equal(rm_accents(c("áćéíńóśúÁĆÉÍŃÓŚÚýÝźŹ")), "aceinosuACEINOSUyYzZ")  # Acute
  expect_equal(rm_accents(c("àèìòùÀÈÌÒÙ")), "aeiouAEIOU")  # grave
  expect_equal(rm_accents(c("âêîôûÂÊÎÔÛ")), "aeiouAEIOU")  # circunflex
  expect_equal(rm_accents(c("ãõÃÕñÑ")), "aoAOnN")  # tilde
  expect_equal(rm_accents(c("äëïöőüÄËÏÖŐÜÿ")), "aeioouAEIOOUy")  # umlaut
  expect_equal(rm_accents(c("ąĄçÇęĘņŅțȚșȘşŞßßţŢ")), "aAcCenNEtTsSsSSStT")  # cedil
  expect_equal(rm_accents(c("ăĂďĎěĚĕĔňŇšŠčČřŘžŽĕĔ")), "aAdDeEeEnNsScCrRzZeE")  # flex
  expect_equal(rm_accents(c("ėĖżŻ")), "eEzZ")  # dotted
  expect_equal(rm_accents(c("åÅ")), "aA")  # round
  expect_equal(rm_accents(c("āĀēĒīĪūŪ")), "aAeEiIuU")  # bar
  expect_equal(rm_accents(c("æÆ")), "aA")  # interlaced
  expect_equal(rm_accents(c("łŁøØđĐ")), "lLoOdD")  # cross
})
