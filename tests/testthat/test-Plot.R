test_that("Plot - check for errors", {
  library(raster)

  library(rgdal)
  library(magrittr)
  library(igraph)
  fileList <-
     data.frame(files =
       dir(file.path(
                     find.package("SpaDES",
                                  lib.loc=getOption("devtools.path"),
                                  quiet=FALSE),
                    "maps"),
          full.names=TRUE, pattern= "tif"),
       functions="rasterToMemory",
       .stackName="landscape",
       packages="SpaDES",
       stringsAsFactors=FALSE)

  # Load files to memory (using rasterToMemory) and stack them (because .stackName is provided above)
  loadFiles(fileList=fileList)

  expect_error(Plot(asdfd))
  expect_that(Plot(landscape), not(throws_error()))
  expect_that(Plot(caribou, addTo="landscape$forestAge", size=4, axes=FALSE), not(throws_error()))

  # can add a plot to the plotting window
  expect_that(Plot(caribou, new=FALSE), not(throws_error()))

  # Can add two maps with same name, if one is in a stack; they are given
  #  unique names based on object name
  expect_that(Plot(landscape, caribou, DEM), not(throws_error()))

  # can mix stacks, rasters, SpatialPoint*
  expect_that(Plot(landscape, habitatQuality2, caribou), not(throws_error()))

  # can mix stacks, rasters, SpatialPoint*, and SpatialPolygons*
  expect_that(Plot(landscape, caribou), not(throws_error()))

  expect_that(Plot(habitatQuality2, new=FALSE), not(throws_error()))
  Sr1 = Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2))*20-50)
  Sr2 = Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2))*20-50)
  Srs1 = Polygons(list(Sr1), "s1")
  Srs2 = Polygons(list(Sr2), "s2")
  SpP = SpatialPolygons(list(Srs1, Srs2), 1:2)
  expect_that(Plot(SpP), not(throws_error()))
  expect_that(Plot(SpP, addTo="landscape$forestCover", gp=gpar(lwd=2)), not(throws_error()))
})
