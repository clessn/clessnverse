test_that("removing accents works", {

  # Removing accents without parameters

  expect_equal(rm_accents("áćéíńóśúÁĆÉÍŃÓŚÚýÝźŹ"), "aceinosuACEINOSUyYzZ")  # Acute
  expect_equal(rm_accents("àèìòùÀÈÌÒÙ"), "aeiouAEIOU")  # grave
  expect_equal(rm_accents("âêîôûÂÊÎÔÛ"), "aeiouAEIOU")  # circunflex
  expect_equal(rm_accents("ãõÃÕñÑ"), "aoAOnN")  # tilde
  expect_equal(rm_accents("äëïöőüÄËÏÖŐÜÿ"), "aeioouAEIOOUy")  # umlaut
  expect_equal(rm_accents("ąĄçÇęĘņŅțȚșȘşŞßßţŢ"), "aAcCenNEtTsSsSSStT")  # cedil
  expect_equal(rm_accents("ăĂďĎěĚĕĔňŇšŠčČřŘžŽĕĔ"), "aAdDeEeEnNsScCrRzZeE")  # flex
  expect_equal(rm_accents("ėĖżŻ"), "eEzZ")  # dotted
  expect_equal(rm_accents("åÅ"), "aA")  # round
  expect_equal(rm_accents("āĀēĒīĪūŪ"), "aAeEiIuU")  # bar
  expect_equal(rm_accents("æÆ"), "aA")  # interlaced
  expect_equal(rm_accents("łŁøØđĐ"), "lLoOdD")  # cross

  # Unicode
  expect_equal(rm_accents("\u00e6\u00c6"), "aA")  # interlaced

  # Remove one accent type

  str <- "áèîõüçę"

  expect_equal(rm_accents(str, pattern = "´"), "aèîõüçę")
  expect_equal(rm_accents(str, pattern = "`"), "áeîõüçę")
  expect_equal(rm_accents(str, pattern = "^"), "áèiõüçę")
  expect_equal(rm_accents(str, pattern = "~"), "áèîoüçę")
  expect_equal(rm_accents(str, pattern = "¨"), "áèîõuçę")
  expect_equal(rm_accents(str, pattern = "ç"), "áèîõüce")
  expect_equal(rm_accents(str, pattern = ""), "áèîõüçę")
})
