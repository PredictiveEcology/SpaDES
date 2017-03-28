test_that("spreadDT produces legal RasterLayer", {
  library(raster); on.exit(detach("package:raster"), add = TRUE)

  # inputs for x
  a <- raster(extent(0, 10 , 0,10), res = 1)

  # check it makes a RasterLayer
  set.seed(123)

  for(i in 1:20){
    sams <- sample(ncell(a),2)
    out <- spreadDT(a, start = sams, 0.225, asRaster = FALSE)
    expect_true(length(unique(out$initialPixels))==2)
    expect_true(all(out$active==0))
  }

  maxSizes <- 2:3
  for(i in 1:20) {
    seed <- sample(1e6,1)
    set.seed(seed)
    print(seed)
    sams <- sample(ncell(a),2)
    out <- spreadDT(a, start = sams, 0.225, size = maxSizes, asRaster = FALSE)
    expect_true(all(out[,.N,by="initialPixels"]$N <= maxSizes))
  }

  exactSizes <- c(5, 3)
  for(i in 1:20) {
    seed <- sample(1e6,1)
    set.seed(seed)
    print(seed)
    sams <- sample(ncell(a),2)
    out <- spreadDT(a, start = sams, 0.225, size = exactSizes, exactSize = TRUE,
                    asRaster = FALSE)
    expect_true(all(out[,.N,by="initialPixels"]$N == exactSizes))
  }

  exactSizes <- c(154,51, 134) # too big for landscape, can't achieve it --
  #  will hit max numRetries, and will try jumping
  for(i in 1:20) {
    seed <- sample(1e6,1)
    set.seed(seed)
    print(seed)
    sams <- sample(ncell(a),3)
    out <- spreadDT(a, start = sams, 0.225, size = exactSizes, exactSize = TRUE,
                    asRaster = FALSE)
    expect_true(all(out[,.N,by="initialPixels"]$N < exactSizes))
    expect_true(all(out$numRetries == 11)) # current max
  }

  for(i in 1:20) {
    seed <- sample(1e6,1)
    set.seed(seed)
    print(seed)
    sams <- sample(ncell(a),length(sams))
    expect_error(spreadDT(a, start = sams, 0.225, circle = TRUE,
                    asRaster = FALSE, plot.it = TRUE))
    out <- spreadDT(a, start = sams, 1, circle = TRUE, asRaster = FALSE)
    expect_true(is.numeric(out$distance))
    expect_true(NROW(out)==ncell(a))
  }

  # test circle
  out <- spreadDT(a, start = sams, 1, circle = TRUE,
                  asRaster = FALSE, returnDistances = TRUE)
  expect_true(NROW(out)==ncell(a))
  expect_true(all(out$state=="inactive"))
  expect_true(all(out$distance<=(sqrt(2)*ncol(a))))

  out <- spreadDT(a, start = sams, 1, circle = TRUE, allowOverlap = TRUE,
                  asRaster = FALSE, returnDistances = TRUE)
  expect_true(NROW(out)==ncell(a)*length(sams))
  expect_true(all(out$state=="inactive"))
  expect_true(all(out$distance<=(sqrt(2)*ncol(a))))

  # Scales with number of starts, not size of raster
  set.seed(21)
  b <- raster(extent(0, 33000 , 0,33000), res = 1)
  sams <- sample(ncell(b),2)
  st1 <- system.time(out <- spreadDT(b, start = sams, 0.225,
                                     allowOverlap = TRUE, asRaster=FALSE))
  expect_lt(st1[1],1)


  # test neighProbs
  out <- spreadDT(a, spreadProb=1, neighProbs = c(0.7,0.3), size = 14, asRaster=FALSE)
  expect_true(uniqueN(out)==14)

  #check wide range of spreadProbs
  for (i in 1:20) {
    expect_that(spreadDT(a, spreadProb=stats::runif(1, 0, 1)), is_a("RasterLayer"))
  }

  skip("benchmarking spreadDT")
  a <- raster(extent(0, 1000, 0,1000), res = 1)
  set.seed(123); sams <- sample(ncell(a),30)
  set.seed(123); profvis::profvis({out <- spreadDT(a, start = sams, 0.235, asRaster=FALSE)})
  set.seed(123); profvis::profvis({out <- spreadDT(a, start = sams, 0.235, asRaster=FALSE,
                                                   allowOverlap = TRUE)})

  set.seed(123); microbenchmark(times = 1, {out1 <- spreadDT(a, start = sams, 0.235, asRaster=FALSE)},
                                b={out2 <- spread(a, loci = sams, 0.235, id=TRUE)},
                                c={out2 <- spread(a, loci = sams, 0.235, id=TRUE, lowMemory = TRUE)})
  set.seed(123); profvis::profvis({out <- spreadDT(a, start = sams, 0.235, asRaster=FALSE,
                                                   allowOverlap = TRUE)})


})
