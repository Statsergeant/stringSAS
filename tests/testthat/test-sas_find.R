
test_that("sas_find returns correct positions", {
  expect_equal(sas_find("Hello World", "World"), 7L)
  expect_equal(sas_find("ABCABC", "ABC"), 1L)
})

test_that("sas_find returns zero for no match", {
  expect_equal(sas_find("Hello World", "Python"), 0L)
  expect_equal(sas_find("ABC", "Z"), 0L)
})

test_that("sas_find supports case-insensitive search", {
  expect_equal(
    sas_find("Hello World", "world", "i"),
    7L
  )

  expect_equal(
    sas_find("Hello World", "world"),
    0L
  )
})

test_that("sas_find supports starting positions", {
  expect_equal(
    sas_find("ABCABC", "ABC", startpos = 4),
    4L
  )

  expect_equal(
    sas_find("ABCABC", "ABC", startpos = 5),
    0L
  )
})

test_that("sas_find handles vectors", {
  expect_equal(
    sas_find(c("ABC", "DEF", "BAB"), "B"),
    c(2L, 0L, 1L)
  )
})

test_that("sas_find preserves missing values", {
  expect_equal(
    sas_find(c("ABC", NA_character_), "A"),
    c(1L, NA_integer_)
  )
})

test_that("sas_find validates arguments", {
  expect_error(sas_find(123, "A"), "`x`")
  expect_error(sas_find("ABC", NA_character_), "`substring`")
  expect_error(sas_find("ABC", "A", "z"), "`modifier`")
  expect_error(
    sas_find("ABC", "A", startpos = 0),
    "`startpos`"
  )
})

test_that("case-insensitive FIND works without warnings", {
  expect_no_warning(
    expect_equal(
      sas_find("STUDY01-001", "study", "i"),
      1L
    )
  )

  expect_equal(
    sas_find("Hello World", "world", "i"),
    7L
  )

  expect_equal(
    sas_find("ABC.ABC", ".", "i"),
    4L
  )

  expect_equal(
    sas_find("A+B+C", "+", "i"),
    2L
  )
})
