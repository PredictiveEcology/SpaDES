test_that("inRange produces logical", {
  # inputs for x
  expect_equal(inRange(0.5, 0, 1),   TRUE)
  expect_equal(inRange(-0.5, 0, 1),  FALSE)
  expect_equal(inRange(NA_real_),    NA)
  expect_equal(inRange(NA_integer_), NA)
  expect_equal(inRange(NULL),        NULL)
  expect_that(inRange(),             testthat::throws_error())

  # inputs for a & b
  expect_that(inRange(0.5, 1, 0),            testthat::throws_error())
  expect_that(inRange(-0.5, NA_integer_, 1), testthat::throws_error())
  expect_that(inRange(-0.5, NA_real_, 1),    testthat::throws_error())
  expect_that(inRange(-0.5, 0, NA_integer_), testthat::throws_error())
  expect_that(inRange(-0.5, 0, NA_real_),    testthat::throws_error())
  expect_that(inRange(-0.5, NULL, 1),        testthat::throws_error())
  expect_that(inRange(-0.5, 0, NULL),        testthat::throws_error())
})
