test_that("seedDispRcv passes tests", {

  # check for needing a square pixel raster
  a = raster(extent(0,100,0,100),res=c(1,2))
  a[]=1
  expect_that(seedDispRcv(a), throws_error("seedSrc resolution must be same in x and y dimension"))

  # check that seedDispRcv does not throw an error
  a <- raster::raster(extent(0,1e4,0,1e4),res=100)
  hab <- SpaDES::gaussMap(a,speedup=1) # if raster is large (>1e6 pixels), use speedup>1
  names(hab)="hab"

  seedSrc <- hab>4
  setColors(seedSrc,1) <- c("white","black")

  set.seed(1300)
  seedRcv <- as.integer(sample(1:ncell(hab), 10))
  expect_that(seeds <- seedDispRcv(seedSrc, seedRcv=seedRcv),
              testthat::not(throws_error()))

  # check that seedDispRcv returns a numeric
  expect_that(seeds, is_a("numeric"))

  #check wide range of spreadProbs
#   for(i in 1:20) {
#     expect_that(spread(a, loci=ncell(a)/2, runif(1,0, 1)), is_a("RasterLayer"))
#   }
#
#   #check spreadProbs outside of legal returns an "spreadProb is not a probability"
#   expect_that(spread(a, loci=ncell(a)/2, 1.1), throws_error("spreadProb is not a probability"))
#   expect_that(spread(a, loci=ncell(a)/2, -0.1), throws_error("spreadProb is not a probability"))
#
#   # Checks if maxSize is working properly
#     # One process spreading
#     expect_equal(ncell(a), tabulate(spread(a, spreadProb = 1, mapID = TRUE)[]))
#
#     # Several processes spreading
#     expect_equal(rep_len(3300,3), tabulate(spread(a, loci=c(100, 3500, 8000),
#                                               spreadProb = 1, mapID = TRUE,
#                                               maxSize = rep_len(3300,3))[]))

})
