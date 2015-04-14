test_that("Plot - check for errors", {
  startFileList <- dir()

  f <- dir(file.path(find.package("SpaDES", quiet=FALSE), "maps"),
         full.names=TRUE, pattern="tif")

  ras <- raster::raster(xmn=0, xmx=40, ymn=0, ymx=40, vals=1, res=1)
  DEM87654 <- SpaDES::gaussMap(ras, var = 2, speedup=1)
  assignGlobal("DEM87654", DEM87654)
  habitatQuality87654 <- gaussMap(ras, var = 2, speedup=1)
  assignGlobal("habitatQuality87654", habitatQuality87654)
  landscape87654 <- stack(DEM87654, habitatQuality87654)
  assignGlobal("landscape87654", landscape87654)
  caribou87654 <- sp::SpatialPoints(coords=cbind(x=runif(1e2, -50, 50), y=runif(1e2, -50, 50)))
  assignGlobal("caribou87654", caribou87654)

  # If any rearrangements are required, Plot searches for objects in Global Env
  # So all tests must run a clearPlot or a new=TRUE to be cleared to
  # prevent rearrangements
  clearPlot()
  expect_error(Plot(asdfd))
  clearPlot()
  expect_that(Plot(landscape87654), testthat::not(throws_error()))

  clearPlot()
  expect_that(Plot(caribou87654), testthat::not(throws_error()))

  #   # can add a plot to the plotting window
  clearPlot()
  expect_that(Plot(landscape87654), testthat::not(throws_error()))
  #   expect_that(Plot(caribou87654, new=FALSE), testthat::not(throws_error()))

  # Can add two maps with same name, if one is in a stack; they are given
  #  unique names based on object name
  clearPlot()
  expect_that(Plot(landscape87654, caribou87654, DEM87654), testthat::not(throws_error()))

  # can mix stacks, rasters, SpatialPoint*
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
  assignGlobal("SpP87654", SpP87654)
  clearPlot()
  expect_that(Plot(SpP87654), testthat::not(throws_error()))
  clearPlot()
  expect_that(Plot(landscape87654, caribou87654), testthat::not(throws_error()))

  # test addTo
  #expect_that(Plot(SpP87654, addTo="landscape87654$habitatQuality87654", gp=gpar(lwd=2)), testthat::not(throws_error()))

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
  hist87654 <- hist(rnorm(1e3), plot=FALSE)
  assignGlobal("hist87654", hist87654)
  expect_that(Plot(hist87654, new=TRUE), testthat::not(throws_error()))

  # test ggplot2 and hist -- don't work unless invoke global environment
  clearPlot()
  ggplot87654 <- ggplot2::qplot(rnorm(1e3))
  assignGlobal("ggplot87654", ggplot87654)
  expect_that(Plot(ggplot87654, new=TRUE), testthat::not(throws_error()))

  # test rearrangements
  expect_that(Plot(caribou87654, new=TRUE), testthat::not(throws_error()))
  expect_that(Plot(DEM87654), testthat::not(throws_error()))
  expect_that(Plot(habitatQuality87654), testthat::not(throws_error()))

  dev.off()
  endFileList <- dir()
  file.remove(endFileList[!(endFileList %in% startFileList)])
})


