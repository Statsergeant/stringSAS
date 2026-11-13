
test_that("default removes ordinary spaces", {
  expect_equal(sas_compress("A B C"), "ABC")
  expect_equal(sas_compress("ABC"), "ABC")
})

test_that("literal characters are removed", {
  expect_equal(sas_compress("A-B-C", "-"), "ABC")
  expect_equal(sas_compress("A-B_C", "-_"), "ABC")
})

test_that("digit modifier works",{
  expect_equal(sas_compress("AB123", modifier = "d"), "AB")
  expect_equal(sas_compress("AB123", modifier = "kd"), "123")
  expect_equal(sas_compress("A 1 B 2", modifier = "kd"), "12")
})

test_that("alphabetic modifier works", {
  expect_equal(sas_compress("AB123", modifier = "a"), "123")
  expect_equal(sas_compress("AB123", modifier = "ka"), "AB")
})

test_that("whitespace modifier works", {
  expect_equal(sas_compress("A B\tC\n", modifier = "s"), "ABC")
  expect_equal(sas_compress("A B\tC", modifier = "ks"), " \t")
})

test_that("modifier combinations work", {
  expect_equal(
    sas_compress("AB123- CD", modifier = "kad"),
    "AB123CD"
  )
  expect_equal(
    sas_compress("AB123-CD", chars = "-", modifier = "kd"),
    "123-"
  )
})

test_that("empty and missing inputs are preserved", {
  expect_equal(sas_compress(""), "")
  expect_equal(sas_compress(NA_character_), NA_character_)
  expect_equal(sas_compress(character()), character())
})

test_that("vectorized inputs work", {
  expect_equal(
    sas_compress(c("A1", "B2", NA_character_),
                 modifier = "kd"),
    c("1", "2", NA_character_)
  )
})

test_that("invalid arguments fail clearly", {
  expect_error(sas_compress(123), "`x`")
  expect_error(sas_compress("ABC", chars = NA_character_),
               "`chars`")
  expect_error(sas_compress("ABC", modifier = "z"),
               "Unsupported modifier")
  expect_error(sas_compress("ABC", modifier = NA_character_),
               "`modifier`")
})
