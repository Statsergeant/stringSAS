test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("sas_substr extracts by position and length", {
  expect_equal(
    sas_substr("STUDY001", 1, 5),
    "STUDY"
  )

  expect_equal(
    sas_substr("STUDY001", 6, 3),
    "001"
  )
})

test_that("omitted length extracts to the end", {
  expect_equal(
    sas_substr("STUDY001", 6),
    "001"
  )
})

test_that("sas_substr handles positions beyond the string", {
  expect_equal(
    sas_substr("ABC", 10, 2),
    ""
  )
})

test_that("sas_substr handles zero length", {
  expect_equal(
    sas_substr("ABC", 1, 0),
    ""
  )
})

test_that("sas_substr handles vectors", {
  expect_equal(
    sas_substr(c("ABC123", "DEF456"), 1, 3),
    c("ABC", "DEF")
  )
})

test_that("sas_substr preserves missing values", {
  expect_equal(
    sas_substr(c("ABC", NA_character_), 1, 2),
    c("AB", NA_character_)
  )
})

test_that("sas_substr validates arguments", {
  expect_error(sas_substr(123, 1, 2), "`x`")
  expect_error(sas_substr("ABC", NA_real_, 2), "`start`")
  expect_error(sas_substr("ABC", 1.5, 2), "`start`")
  expect_error(sas_substr("ABC", 1, -1), "`length`")
})
