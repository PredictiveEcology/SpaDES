test_that("inRange produces logical", {
  # inputs for x
  expect_that(inRange(0.5, 0, 1),   equals(TRUE))
  expect_that(inRange(-0.5, 0, 1),  equals(FALSE))
  expect_that(inRange(NA_real_),    equals(NA))
  expect_that(inRange(NA_integer_), equals(NA))
  expect_that(inRange(NULL),        equals(NULL))
  expect_that(inRange(), throws_error())
  
  # inputs for a & b
  expect_that(inRange(0.5, 1, 0),            throws_error())
  expect_that(inRange(-0.5, NA_integer_, 1), throws_error())
  expect_that(inRange(-0.5, NA_real_, 1),    throws_error())
  expect_that(inRange(-0.5, 0, NA_integer_), throws_error())
  expect_that(inRange(-0.5, 0, NA_real_), throws_error())
  expect_that(inRange(-0.5, NULL, 1),        throws_error())
  expect_that(inRange(-0.5, 0, NULL),        throws_error())
})
