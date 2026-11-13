
test_that("sas_tranwrd replaces substrings", {
  expect_equal(
    sas_tranwrd("A-B-C", "-", "_"),
    "A_B_C"
  )

  expect_equal(
    sas_tranwrd("Hello SAS", "SAS", "R"),
    "Hello R"
  )
})

test_that("sas_tranwrd replaces all occurrences", {
  expect_equal(
    sas_tranwrd("ABCABC", "ABC", "X"),
    "XX"
  )
})

test_that("sas_tranwrd performs literal matching", {
  expect_equal(
    sas_tranwrd("A.B.C", ".", "-"),
    "A-B-C"
  )

  expect_equal(
    sas_tranwrd("A+B+C", "+", "-"),
    "A-B-C"
  )
})

test_that("sas_tranwrd is case-sensitive", {
  expect_equal(
    sas_tranwrd("SAS sas", "sas", "R"),
    "SAS R"
  )
})

test_that("sas_tranwrd supports vectors", {
  expect_equal(
    sas_tranwrd(c("A-B", "C-D"), "-", "_"),
    c("A_B", "C_D")
  )
})

test_that("sas_tranwrd preserves missing values", {
  expect_equal(
    sas_tranwrd(c("A-B", NA_character_), "-", "_"),
    c("A_B", NA_character_)
  )
})

test_that("sas_tranwrd validates arguments", {
  expect_error(sas_tranwrd(123, "A", "B"), "`x`")
  expect_error(sas_tranwrd("ABC", "", "X"), "`target`")
  expect_error(
    sas_tranwrd("ABC", "A", NA_character_),
    "`replacement`"
  )
})
