
test_that("sas_scan extracts words by position", {
  expect_equal(sas_scan("John Smith", 1), "John")
  expect_equal(sas_scan("John Smith", 2), "Smith")
  expect_equal(sas_scan("John Smith", -1), "Smith")
  expect_equal(sas_scan("John Smith", -2), "John")
})

test_that("sas_scan handles custom delimiters", {
  expect_equal(sas_scan("A-B-C", 2, "-"), "B")
  expect_equal(sas_scan("A,B,C", 3, ","), "C")
  expect_equal(sas_scan("A--B", 2, "-"), "B")
})

test_that("sas_scan treats delimiters as characters", {
  expect_equal(sas_scan("A.B.C", 2, "."), "B")
  expect_equal(sas_scan("A|B|C", 2, "|"), "B")
  expect_equal(sas_scan("A+B+C", 2, "+"), "B")
})

test_that("sas_scan handles negative positions", {
  expect_equal(sas_scan("A-B-C", -1, "-"), "C")
  expect_equal(sas_scan("A-B-C", -2, "-"), "B")
  expect_equal(sas_scan("A-B-C", -3, "-"), "A")
})

test_that("sas_scan returns NA for missing words", {
  expect_equal(sas_scan("A-B", 3, "-"), NA_character_)
  expect_equal(sas_scan("A-B", -3, "-"), NA_character_)
  expect_equal(sas_scan("", 1), NA_character_)
  expect_equal(sas_scan(NA_character_, 1), NA_character_)
})

test_that("sas_scan handles vectorized inputs", {
  expect_equal(
    sas_scan(c("John Smith", "Jane Doe"), 2),
    c("Smith", "Doe")
  )
})

test_that("sas_scan handles empty delimiters", {
  expect_equal(sas_scan("ABC", 1, ""), "ABC")
  expect_equal(sas_scan("ABC", 2, ""), NA_character_)
})

test_that("sas_scan rejects invalid arguments", {
  expect_error(sas_scan(123, 1), "`x`")
  expect_error(sas_scan("ABC", 0), "`n`")
  expect_error(sas_scan("ABC", 1.5), "`n`")
  expect_error(sas_scan("ABC", NA_real_), "`n`")
  expect_error(sas_scan("ABC", 1, NA_character_), "`delimiters`")
})
