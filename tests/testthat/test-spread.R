test_that("spread produces legal RasterLayer", {
  set.seed(123)

  # inputs for x
  a = raster(extent(0,100,0,100), res=1)
  b = raster(extent(a), res=1, vals=stats::runif(ncell(a),0,1))

  # check it makes a RasterLayer
  expect_that(spread(a, loci=ncell(a)/2, stats::runif(1,0.15,0.25)), is_a("RasterLayer"))

  #check wide range of spreadProbs
  for(i in 1:20) {
    expect_that(spread(a, loci=ncell(a)/2, stats::runif(1,0,1)), is_a("RasterLayer"))
  }

  # check spreadProbs outside of legal returns an "spreadProb is not a probability"
  expect_that(spread(a, loci=ncell(a)/2, 1.1), throws_error("spreadProb is not a probability"))
  expect_that(spread(a, loci=ncell(a)/2, -0.1), throws_error("spreadProb is not a probability"))

  # checks if maxSize is working properly
  # One process spreading
  expect_equal(ncell(a), tabulate(spread(a, spreadProb=1, mapID=TRUE)[]))

  # several processes spreading
  expect_equal(rep_len(3300,3),
               tabulate(spread(a, loci=c(100, 3500, 8000), spreadProb = 1,
                               mapID = TRUE, maxSize = rep_len(3300,3))[]))

})
