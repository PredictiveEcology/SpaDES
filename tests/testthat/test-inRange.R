test_that("numerical-comparisons: inRange handles various inputs", {
  # inputs for x
  expect_equal(inRange(0.5, 0, 1),   TRUE)
  expect_equal(inRange(-0.5, 0, 1),  FALSE)
  expect_equal(inRange(NA_real_),    NA)
  expect_equal(inRange(NA_integer_), NA)
  expect_equal(inRange(NULL),        NULL)
  expect_error(inRange())
  expect_error(inRange("non-numeric"), "x must be numeric.")

  # inputs for a & b
  expect_error(inRange( 0.5, 1, 0))
  expect_error(inRange(-0.5, NA_integer_, 1))
  expect_error(inRange(-0.5, NA_real_, 1))
  expect_error(inRange(-0.5, 0, NA_integer_))
  expect_error(inRange(-0.5, 0, NA_real_))
  expect_error(inRange(-0.5, NULL, 1))
  expect_error(inRange(-0.5, 0, NULL))
})
