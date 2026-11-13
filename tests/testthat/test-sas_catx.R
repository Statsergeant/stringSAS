
test_that("sas_catx concatenates strings", {
  expect_equal(
    sas_catx("-", "STUDY01", "001", "0001"),
    "STUDY01-001-0001"
  )
})

test_that("sas_catx strips surrounding spaces", {
  expect_equal(
    sas_catx("-", " ABC ", " DEF "),
    "ABC-DEF"
  )
})

test_that("sas_catx skips blank strings", {
  expect_equal(
    sas_catx("-", "ABC", "", "DEF"),
    "ABC-DEF"
  )
})

test_that("sas_catx skips missing values", {
  expect_equal(
    sas_catx("-", "ABC", NA_character_, "DEF"),
    "ABC-DEF"
  )
})

test_that("sas_catx supports vectors", {
  expect_equal(
    sas_catx("-", c("A", "B"), c("1", "2")),
    c("A-1", "B-2")
  )
})

test_that("sas_catx recycles scalar arguments", {
  expect_equal(
    sas_catx("-", c("A", "B"), "001"),
    c("A-001", "B-001")
  )
})

test_that("sas_catx handles all missing inputs", {
  expect_equal(
    sas_catx("-", NA_character_, ""),
    ""
  )
})

test_that("sas_catx rejects incompatible lengths", {
  expect_error(
    sas_catx("-", c("A", "B"), c("1", "2", "3")),
    "common length"
  )
})

test_that("sas_catx validates arguments", {
  expect_error(sas_catx(1, "A", "B"), "`delimiter`")
  expect_error(sas_catx("-", "A", 123), "character vectors")
})
