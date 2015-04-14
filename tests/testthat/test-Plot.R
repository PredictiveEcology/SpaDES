test_that("Plot - check for errors", {
  f <- dir(file.path(find.package("SpaDES", quiet=FALSE), "maps"),
           full.names=TRUE, pattern="tif")

  # If any rearrangements are required, Plot searches for objects in Global Env
  # So all tests must run a clearPlot or a new=TRUE to be cleared to
  # prevent rearrangements
  clearPlot()
  expect_error(Plot(asdfd))
  fileList <- data.frame(files=f,
                         functions=rep("rasterToMemory", length(f)),
                         .stackName=rep("landscape87654", length(f)),
                         packages=rep("SpaDES", length(f)),
                         stringsAsFactors=FALSE)

  # Load files to memory (using rasterToMemory) and stack them (because .stackName is provided above)
  loadFiles(fileList=fileList)
  clearPlot()
  expect_that(Plot(landscape87654), testthat::not(throws_error()))

  clearPlot()
  caribou87654 <- sp::SpatialPoints(coords=cbind(x=runif(1e2, -50, 50), y=runif(1e2, -50, 50)))
  expect_that(Plot(caribou87654), testthat::not(throws_error()))
  #   rm("caribou87654", envir=.GlobalEnv)
  #
  #   # can add a plot to the plotting window
  clearPlot()
  expect_that(Plot(landscape87654), testthat::not(throws_error()))
  #   expect_that(Plot(caribou87654, new=FALSE), testthat::not(throws_error()))

  # Can add two maps with same name, if one is in a stack; they are given
  #  unique names based on object name
  #  assignGlobal(x = "DEM87654", landscape87654$DEM)
  clearPlot()
  expect_that(Plot(landscape87654, caribou87654, DEM87654), testthat::not(throws_error()))

  # can mix stacks, rasters, SpatialPoint*
  #  assignGlobal(x = "habitatQuality87654", landscape87654$habitatQuality)
  clearPlot()

  expect_that(Plot(landscape87654, habitatQuality87654, caribou87654), testthat::not(throws_error()))

  # can mix stacks, rasters, SpatialPoint*, and SpatialPolygons*
  clearPlot()
  expect_that(Plot(landscape87654, caribou87654), testthat::not(throws_error()))

  #expect_that(Plot(habitatQuality2, new=FALSE), not(throws_error()))
  Sr1 <- sp::Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2))*20-50)
  Sr2 <- sp::Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2))*20-50)
  Srs1 <- sp::Polygons(list(Sr1), "s1")
  Srs2 <- sp::Polygons(list(Sr2), "s2")
  SpP87654 <- sp::SpatialPolygons(list(Srs1, Srs2), 1:2)
  clearPlot()
  expect_that(Plot(SpP87654), testthat::not(throws_error()))
  clearPlot()
  expect_that(Plot(landscape87654, caribou87654), testthat::not(throws_error()))

  # test addTo
  expect_that(Plot(SpP87654, addTo="landscape87654$forestCover", gp=gpar(lwd=2)), testthat::not(throws_error()))

  # Test various arguments
  clearPlot()
  expect_that(Plot(caribou87654, new=TRUE, gpAxis=gpar(cex=0.4), size=1), testthat::not(throws_error()))
  clearPlot()
  expect_that(Plot(DEM87654, gpText=gpar(cex=0.4)), testthat::not(throws_error()))

  # test colors
  clearPlot()
  expect_that(Plot(DEM87654, cols=c("blue", "red")), testthat::not(throws_error()))

  # test visualSqueeze
  expect_that(Plot(DEM87654, visualSqueeze=0.2, new=TRUE), testthat::not(throws_error()))

  # test speedup
  expect_that(Plot(landscape87654, caribou87654, DEM87654, speedup=10, new=TRUE), testthat::not(throws_error()))

  # test ggplot2 and hist -- don't work unless invoke global environment
  clearPlot()
  #hist87654 <- hist(rnorm(1e3), plot=FALSE)
  #assignGlobal("hist87654", hist87654)
  #Plot(hist87654)


})


