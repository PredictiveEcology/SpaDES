test_that("spread produces legal RasterLayer", {
  # inputs for x
  a = raster(extent(0,100,0,100),res=1)
  b = raster(extent(a), res = 1 ,vals=runif(ncell(a), 0, 1))

  # check it makes a RasterLayer
  expect_that(spread(a, loci=ncell(a)/2, runif(1,0.15,0.25)), is_a("RasterLayer"))

  #check wide range of spreadProbs
  for(i in 1:20) {
    expect_that(spread(a, loci=ncell(a)/2, runif(1,0, 1)), is_a("RasterLayer"))
  }

  #check spreadProbs outside of legal returns an "spreadProb is not a probability"
  expect_that(spread(a, loci=ncell(a)/2, 1.1), throws_error("spreadProb is not a probability"))
  expect_that(spread(a, loci=ncell(a)/2, -0.1), throws_error("spreadProb is not a probability"))

#   expect_that(spread(a, loci=ncell(a)/2, b), is_a("RasterLayer"))
#
#   expect_that(inRange(-0.5, 0, 1),  equals(FALSE))
#   expect_that(inRange(NA_real_),    equals(NA))
#   expect_that(inRange(NA_integer_), equals(NA))
#   expect_that(inRange(NULL),        equals(NULL))
#   expect_that(inRange(), throws_error())
#
#   # inputs for a & b
#   expect_that(inRange(0.5, 1, 0),            throws_error())
#   expect_that(inRange(-0.5, NA_integer_, 1), throws_error())
#   expect_that(inRange(-0.5, NA_real_, 1),    throws_error())
#   expect_that(inRange(-0.5, 0, NA_integer_), throws_error())
#   expect_that(inRange(-0.5, 0, NA_real_), throws_error())
#   expect_that(inRange(-0.5, NULL, 1),        throws_error())
#   expect_that(inRange(-0.5, 0, NULL),        throws_error())
})
