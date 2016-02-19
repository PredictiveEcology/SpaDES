test_that("Plot is not error-free", {
  library(sp)
  library(raster)

  tmpdir <- file.path(tempdir(), "test_Plot")
  dir.create(tmpdir, recursive = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  })

  ras <- raster::raster(xmn = 0, xmx = 10, ymn = 0, ymx = 10, vals = 1, res = 1)
  DEM87654 <- SpaDES::gaussMap(ras, var = 2, speedup = 1)
  names(DEM87654) <- "DEM87654"
  habitatQuality87654 <- SpaDES::gaussMap(ras, var = 2, speedup = 1)
  names(habitatQuality87654) <- "habitatQuality87654"
  landscape87654 <- raster::stack(DEM87654, habitatQuality87654)
  caribou87654 <- sp::SpatialPoints(
    coords = cbind(x = stats::runif(1e1, 0, 10), y = stats::runif(1e1, 0, 10))
  )

  # If any rearrangements are required, Plot searches for objects in Global Env
  # So all tests must run a clearPlot or a new=TRUE to be cleared to
  # prevent rearrangements
  clearPlot()
  expect_error(Plot(asdfd))
  clearPlot()
  expect_that(Plot(landscape87654), testthat::not(throws_error()))

  clearPlot()
  expect_that(Plot(caribou87654), testthat::not(throws_error()))

  # Test speedup > 0.1 for SpatialPoints
  clearPlot()
  expect_that(Plot(caribou87654, speedup = 2), testthat::not(throws_error()))

  #   # can add a plot to the plotting window
  clearPlot()
  expect_that(Plot(landscape87654), testthat::not(throws_error()))
  #   expect_that(Plot(caribou87654, new=FALSE), testthat::not(throws_error()))

  # Can add two maps with same name, if one is in a stack; they are given
  #  unique names based on object name
  clearPlot()
  expect_that(Plot(landscape87654, caribou87654, DEM87654),
              testthat::not(throws_error()))

  # can mix stacks, rasters, SpatialPoint*
  clearPlot()
  expect_that(Plot(landscape87654, habitatQuality87654, caribou87654),
              testthat::not(throws_error()))

  # can mix stacks, rasters, SpatialPoint*, and SpatialPolygons*
  clearPlot()
  expect_that(Plot(landscape87654, caribou87654), testthat::not(throws_error()))

  #expect_that(Plot(habitatQuality2, new=FALSE), not(throws_error()))
  Sr1 <- sp::Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
  Sr2 <- sp::Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
  Srs1 <- sp::Polygons(list(Sr1), "s1")
  Srs2 <- sp::Polygons(list(Sr2), "s2")
  SpP87654 <- sp::SpatialPolygons(list(Srs1, Srs2), 1:2)
  clearPlot()
  expect_that(Plot(SpP87654), testthat::not(throws_error()))
  clearPlot()
  expect_that(Plot(landscape87654, caribou87654, SpP87654, new = TRUE),
              testthat::not(throws_error()))

  Sr1 <- sp::Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
  Sr2 <- sp::Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
  Srs1 <- sp::Polygons(list(Sr1), "s1")
  Srs2 <- sp::Polygons(list(Sr2), "s2")
  SpP87 <- sp::SpatialPolygons(list(Srs1, Srs2), 1:2)

  # Test polygon with > 1e3 points to test the speedup parameter
  r <- 1
  N <- 1000
  cx <- 0
  cy <- 0
  a <- seq(0, 2*pi, length.out = N)
  x <- cx + r * cos(a)
  y <- cy + r * sin(a)
  Sr1 <- sp::Polygon(cbind(x, y))
  Sr2 <- sp::Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
  Srs1 <- sp::Polygons(list(Sr1), "s1")
  Srs2 <- sp::Polygons(list(Sr2), "s2")
  SpP87 <- sp::SpatialPolygons(list(Srs1, Srs2), 1:2)
  expect_that(Plot(SpP87, new = TRUE), testthat::not(throws_error()))

  # test SpatialLines
  l1 <- cbind(c(10, 2, 30), c(30, 2, 2))
  l1a <- cbind(l1[, 1] + .05, l1[, 2] + .05)
  l2 <- cbind(c(1, 20, 3), c(10, 1.5, 1))
  Sl1 <- sp::Line(l1)
  Sl1a <- sp::Line(l1a)
  Sl2 <- sp::Line(l2)
  S1 <- sp::Lines(list(Sl1, Sl1a), ID = "a")
  S2 <- sp::Lines(list(Sl2), ID = "b")
  Sl87654 <- sp::SpatialLines(list(S1, S2))
  expect_that(Plot(Sl87654), testthat::not(throws_error()))

  # Test polygon with > 1e3 points to test the speedup parameter
  r <- 1
  N <- 1000
  cx <- 0
  cy <- 0
  a <- seq(0, 2*pi, length.out = N)
  x <- cx + r * cos(a)
  y <- cy + r * sin(a)
  l1 <- cbind(x, y)
  l1a <- cbind(l1[, 1] + .05, l1[, 2] + .05)
  l2 <- cbind(c(1, 20, 3), c(10, 1.5, 1))
  Sl1 <- sp::Line(l1)
  Sl1a <- sp::Line(l1a)
  Sl2 <- sp::Line(l2)
  S1 <- sp::Lines(list(Sl1, Sl1a), ID = "a")
  S2 <- sp::Lines(list(Sl2), ID = "b")
  Sl87654 <- sp::SpatialLines(list(S1, S2))
  expect_that(Plot(Sl87654, new = TRUE), testthat::not(throws_error()))

  # test addTo
  expect_that(Plot(SpP87654, addTo = "landscape87654$habitatQuality87654",
                   gp = gpar(lwd = 2)), testthat::not(throws_error()))

  # test various arguments
  clearPlot()
  expect_that(Plot(caribou87654, new = TRUE, gpAxis = gpar(cex = 0.4), size = 1),
              testthat::not(throws_error()))
  clearPlot()
  expect_that(Plot(DEM87654, gpText = gpar(cex = 0.4)),
              testthat::not(throws_error()))

  # test colors
  clearPlot()
  expect_that(Plot(DEM87654, cols = c("blue", "red")),
              testthat::not(throws_error()))

  # test visualSqueeze
  expect_that(Plot(DEM87654, visualSqueeze = 0.2, new = TRUE),
              testthat::not(throws_error()))

  # test speedup
  caribou87 <- sp::SpatialPoints(
    coords = cbind(x = stats::runif(1.1e3, 0, 10), y = stats::runif(1e1, 0, 10))
  )
  expect_that(Plot(caribou87, speedup = 10, new = TRUE),
              testthat::not(throws_error()))

  # test ggplot2 and hist -- don't work unless invoke global environment
  clearPlot()
  hist87654 <- hist(stats::rnorm(1e3), plot = FALSE)
  expect_that(Plot(hist87654, new = TRUE), testthat::not(throws_error()))

  # test ggplot2 and hist -- don't work unless invoke global environment
  clearPlot()
  ggplot87654 <- ggplot2::qplot(stats::rnorm(1e3), binwidth = 0.3,
                                geom = "histogram")
  expect_that(Plot(ggplot87654, new = TRUE), testthat::not(throws_error()))

  # test rearrangements
  expect_that(Plot(caribou87654, new = TRUE), testthat::not(throws_error()))
  expect_that(Plot(DEM87654), testthat::not(throws_error()))
  expect_that(Plot(habitatQuality87654), testthat::not(throws_error()))

  testPlot <- Plot(habitatQuality87654)
  expect_that(Plot(testPlot), testthat::not(throws_error()))
  expect_message(Plot(ls(), habitatQuality87654),
                 "Plot can only plot objects of class .spadesPlottables")
  expect_message(Plot(habitatQuality87654, addTo = "test"),
                 "Plot called with 'addTo' argument specified")
  expect_error(Plot(ls()), "Not a plottable object")
  expect_that(rePlot(), testthat::not(throws_error()))
})

test_that("Unit tests for image content is not error-free", {
  skip_if_not_installed("visualTest")

  library(raster)
  library(visualTest)

  tmpdir <- file.path(tempdir(), "test_Plot_imageContent")
  dir.create(tmpdir, recursive = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    detach("package:raster")
    detach("package:visualTest")
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  })

  ncol <- 3
  nrow <- 4
  N <- ncol * nrow
  nLevels <- 4

  # Test legend with a factor raster
  set.seed(24334)
  ras <- raster(matrix(sample(1:nLevels, size = N, replace = TRUE),
                       ncol = ncol, nrow = nrow))
  levels(ras) <- data.frame(ID = 1:nLevels, Class = paste0("Level", 1:nLevels))
  png(file = "test.png", width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE)
  dev.off()

  #dput(getFingerprint(file = "test.png"))
  orig <- switch(
    Sys.info()["sysname"],
    Darwin = c(3L, 5L, 13L, 3L, 5L, 13L, 3L, 8L, 3L, 5L, 3L, 3L, 7L, 3L, 5L,
               3L, 5L, 5L, 11L, 4L, 5L, 4L, 7L, 25L, 5L, 4L, 5L, 8L, 7L, 6L,
               3L, 3L, 5L, 7L, 3L, 3L, 3L, 5L, 8L, 3L, 13L, 3L, 5L, 13L, 3L,
               5L),
    Linux = c(3L, 13L, 3L, 5L, 5L, 13L, 3L, 11L, 8L, 3L, 5L, 5L, 11L, 5L,
              16L, 7L, 4L, 8L, 18L, 8L, 8L, 4L, 15L, 5L, 11L, 5L, 5L, 8L, 3L,
              11L, 13L, 3L, 5L, 5L, 13L, 3L),
    Windows = c(3L, 5L, 13L, 3L, 5L, 8L, 3L, 5L, 5L, 5L, 6L, 5L, 3L, 8L, 5L,
                6L, 4L, 6L, 6L, 4L, 6L, 20L, 11L, 15L, 7L, 3L, 8L, 5L, 3L, 6L,
                7L, 6L, 3L, 7L, 6L, 5L, 3L, 5L, 8L, 3L, 5L, 13L, 3L, 5L)
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))

  # Test legend with a factor raster
  set.seed(24334)
  ras <- raster(matrix(sample(1:nLevels, size = N, replace = TRUE),
                       ncol = ncol, nrow = nrow))
  png(file = "test.png", width = 400, height = 300)
  clearPlot()
  Plot(ras)
  dev.off()

  #dput(getFingerprint(file = "test.png"))
  orig <- switch(Sys.info()["sysname"],
    Darwin = c(3L, 13L, 3L, 5L, 18L, 3L, 8L, 3L, 5L, 8L, 3L, 5L, 5L, 3L, 3L,
               5L, 5L, 8L, 3L, 16L, 24L, 16L, 8L, 3L, 5L, 5L, 3L, 3L, 5L, 5L,
               8L, 3L, 5L, 8L, 3L, 18L, 3L, 5L, 13L, 3L),
    Linux = c(3L, 13L, 3L, 5L, 18L, 3L, 8L, 3L, 5L, 3L, 3L, 5L, 5L, 8L, 3L,
              5L, 16L, 3L, 3L, 13L, 18L, 13L, 3L, 5L, 14L, 5L, 8L, 3L, 5L,
              5L, 3L, 3L, 5L, 8L, 3L, 18L, 3L, 5L, 13L, 3L),
    Windows = c(3L, 5L, 13L, 3L, 5L, 8L, 3L, 5L, 5L, 3L, 8L, 5L, 3L, 8L, 5L,
                3L, 7L, 6L, 6L, 5L, 7L, 4L, 5L, 5L, 7L, 9L, 4L, 5L, 7L, 4L, 4L,
                8L, 5L, 6L, 3L, 7L, 6L, 3L, 7L, 6L, 3L, 5L, 5L, 8L, 3L, 5L, 13L,
                3L, 5L)
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))

  # test non contiguous factor raster
  nLevels <- 6
  N <- ncol * nrow
  set.seed(24334)
  levs <- (1:nLevels)[-((nLevels - 2):(nLevels - 1))]
  ras <- raster(matrix(sample(levs, size = N, replace = TRUE),
                       ncol = ncol, nrow = nrow))
  levels(ras) <- data.frame(ID = levs, Class = paste0("Level", levs))
  ras <- setColors(ras, n = 4, c("red", "orange", "blue", "yellow"))

  png(file = "test.png", width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE)
  dev.off()

  #dput(getFingerprint(file = "test.png"))
  orig <- switch(Sys.info()["sysname"],
    Darwin = c(8L, 12L, 6L, 3L, 7L, 16L, 20L, 16L, 14L, 13L, 11L, 40L, 11L,
               8L, 14L, 20L, 16L, 16L, 6L, 5L, 4L, 13L, 8L),
    Linux = c(7L, 29L, 15L, 12L, 10L, 22L, 4L, 7L, 4L, 27L, 12L, 26L, 3L,
              7L, 4L, 23L, 9L, 13L, 15L, 29L, 7L),
    Windows = c(4L, 22L, 7L, 4L, 14L, 7L, 6L, 4L, 7L, 8L, 17L, 8L, 9L, 4L,
                7L, 3L, 10L, 11L, 5L, 3L, 7L, 4L, 12L, 6L, 17L, 8L, 7L, 3L, 7L,
                6L, 15L, 3L, 8L, 21L, 4L)
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))
})

test_that("Unit tests for plotting colors", {
  skip_if_not_installed("visualTest")

  library(raster)
  library(visualTest)

  tmpdir <- file.path(tempdir(), "test_Plot_colors")
  dir.create(tmpdir, recursive = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    detach("package:raster")
    detach("package:visualTest")
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  })

  ras <- raster(matrix(c(0, 0, 1, 2), ncol = 2))
  setColors(ras, n = 3) <- c("red", "blue", "green")

  png(file = "test.png", width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE)
  dev.off()

  #dput(getFingerprint(file = "test.png"))
  orig <- switch(Sys.info()["sysname"],
    Darwin = c(7L, 7L, 8L, 14L, 7L, 7L, 8L, 10L, 11L, 7L, 7L, 7L, 8L, 5L,
               8L, 9L, 12L, 13L, 8L, 9L, 5L, 8L, 7L, 7L, 7L, 10L, 11L, 8L, 7L,
               7L, 14L, 8L, 7L, 7L),
    Linux = c(7L, 7L, 12L, 10L, 7L, 8L, 6L, 8L, 8L, 7L, 8L, 8L, 6L, 8L, 20L,
              8L, 4L, 5L, 8L, 19L, 8L, 7L, 8L, 8L, 7L, 7L, 8L, 7L, 8L, 7L,
              9L, 13L, 7L, 7L),
    Windows = c(7L, 8L, 7L, 3L, 12L, 8L, 20L, 8L, 8L, 28L, 7L, 8L, 6L, 5L,
                14L, 5L, 7L, 8L, 6L, 29L, 8L, 8L, 20L, 8L, 11L, 3L, 8L, 8L, 7L)
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))
  unlink("test.png")

  ras2 <- raster(matrix(c(3, 1, 1, 2), ncol = 2))
  rasStack <- raster::stack(ras, ras2)
  names(rasStack) <- c("ras", "ras2")
  setColors(rasStack, n = 3) <- list(ras = c("black", "blue", "green"))
  png(file = "test.png", width = 400, height = 300)
  clearPlot()
  Plot(rasStack, new = TRUE)
  dev.off()

  #dput(getFingerprint(file = "test.png"))
  orig <- switch(Sys.info()["sysname"],
    Darwin = c(8L, 7L, 6L, 4L, 4L, 4L, 5L, 28L, 42L, 36L, 38L, 42L, 27L, 4L,
               4L, 4L, 5L, 7L, 9L),
    Linux = c(8L, 7L, 6L, 4L, 4L, 4L, 5L, 13L, 15L, 40L, 19L, 19L, 20L, 20L,
              40L, 14L, 13L, 4L, 4L, 4L, 5L, 7L, 9L),
    Windows = c(7L, 7L, 10L, 4L, 8L, 5L, 36L, 32L, 20L, 18L, 20L, 20L, 32L,
                35L, 5L, 7L, 5L, 13L, 7L)
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))
  unlink("test.png")

  # Test setColors
  ras <- setColors(ras, c("red", "purple", "orange"), n = 3)
  png(file = "test.png", width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE)
  dev.off()

  #dput(getFingerprint(file = "test.png"))
  orig <- switch(Sys.info()["sysname"],
    Darwin = c(7L, 9L, 8L, 5L, 7L, 7L, 7L, 8L, 14L, 7L, 7L, 7L, 7L, 7L, 8L,
               5L, 4L, 6L, 6L, 6L, 7L, 6L, 6L, 4L, 5L, 8L, 7L, 7L, 7L, 7L, 17L,
               5L, 7L, 7L, 7L, 8L, 5L, 9L, 7L),
    Linux = c(7L, 9L, 13L, 3L, 18L, 8L, 10L, 5L, 7L, 8L, 7L, 15L, 12L, 7L,
              9L, 4L, 5L, 8L, 8L, 11L, 16L, 7L, 8L, 7L, 4L, 12L, 7L, 17L, 8L,
              11L, 7L),
    Windows = c(7L, 22L, 7L, 9L, 3L, 5L, 7L, 5L, 9L, 7L, 6L, 14L, 8L, 7L, 8L,
                7L, 4L, 14L, 5L, 7L, 8L, 6L, 8L, 15L, 6L, 7L, 9L, 6L, 5L, 5L,
                6L, 7L, 22L, 7L)
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))
  unlink("test.png")

  ras <- setColors(ras, c("yellow", "orange"))
  png(file = "test.png", width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE)
  dev.off()

  #dput(getFingerprint(file = "test.png"))
  orig <- switch(Sys.info()["sysname"],
    Darwin = c(7L, 7L, 8L, 14L, 7L, 7L, 8L, 10L, 11L, 7L, 7L, 7L, 8L, 5L,
               8L, 9L, 5L, 7L, 8L, 5L, 8L, 9L, 5L, 8L, 7L, 7L, 7L, 10L, 11L,
               8L, 7L, 7L, 14L, 8L, 7L, 7L),
    Linux = c(7L, 7L, 12L, 10L, 7L, 8L, 6L, 8L, 8L, 7L, 8L, 8L, 6L, 8L, 20L,
              8L, 4L, 5L, 8L, 19L, 8L, 7L, 8L, 8L, 7L, 7L, 8L, 7L, 8L, 7L,
              9L, 13L, 7L, 7L),
    Windows = c(7L, 8L, 7L, 3L, 12L, 8L, 20L, 8L, 8L, 28L, 7L, 8L, 6L, 5L,
              14L, 5L, 7L, 8L, 6L, 29L, 8L, 8L, 20L, 8L, 11L, 3L, 8L, 8L, 7L)
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))
  unlink("test.png")
})

test_that("Unit tests for internal functions in Plot", {
  skip_if_not_installed("visualTest")

  library(raster)
  library(visualTest)

  tmpdir <- file.path(tempdir(), "test_Plot_internal")
  dir.create(tmpdir, recursive = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    detach("package:raster")
    detach("package:visualTest")
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  })

  # Test .makeColorMatrix for subsampled rasters
  # (i.e., where speedup is high compared to ncells)
  set.seed(1234)
  ras <- raster(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  setColors(ras, n = 3) <- c("red", "blue", "green")

  png(file = "test.png", width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE, speedup = 2e5)
  dev.off()

  #dput(getFingerprint(file = "test.png"))
  orig <- switch(Sys.info()["sysname"],
    Darwin = c(7L, 7L, 8L, 6L, 3L, 5L, 7L, 8L, 6L, 13L, 9L, 7L, 8L, 6L, 7L,
               7L, 5L, 4L, 6L, 7L, 6L, 7L, 7L, 6L, 4L, 5L, 7L, 7L, 6L, 8L, 7L,
               9L, 13L, 6L, 8L, 7L, 4L, 3L, 7L, 8L, 7L, 7L),
    Linux = c(7L, 8L, 8L, 13L, 8L, 7L, 7L, 4L, 4L, 7L, 7L, 8L, 7L, 8L, 12L,
              8L, 8L, 6L, 5L, 6L, 6L, 8L, 8L, 12L, 8L, 7L, 8L, 7L, 7L, 4L,
              4L, 7L, 7L, 8L, 13L, 8L, 8L, 7L),
    Windows = c(7L, 8L, 14L, 7L, 8L, 8L, 13L, 8L, 8L, 7L, 8L, 9L, 11L, 8L,
                8L, 7L, 3L, 3L, 8L, 8L, 8L, 11L, 9L, 8L, 7L, 8L, 8L, 13L, 8L,
                8L, 7L, 14L, 8L, 7L)
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))

  # Test that NA rasters plot correctly, i.e., with na.color only
  ras <- raster(matrix(NA, ncol = 3, nrow = 3))
  setColors(ras, n = 3) <- c("red", "blue", "green")

  png(file = "test.png", width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE, speedup = 2e5)
  dev.off()

  #dput(getFingerprint(file = "test.png"))
  orig <- switch(Sys.info()["sysname"],
    Darwin = c(7L, 7L, 8L, 14L, 7L, 8L, 7L, 14L, 7L, 7L, 7L, 7L, 7L, 7L, 5L,
               5L, 6L, 6L, 6L, 7L, 6L, 6L, 5L, 5L, 7L, 7L, 7L, 7L, 7L, 7L, 14L,
               7L, 8L, 7L, 14L, 8L, 7L, 7L),
    Linux = c(7L, 8L, 8L, 13L, 8L, 7L, 7L, 4L, 4L, 7L, 7L, 8L, 7L, 8L, 12L,
              8L, 8L, 6L, 5L, 6L, 6L, 8L, 8L, 12L, 8L, 7L, 8L, 7L, 7L, 4L,
              4L, 7L, 7L, 8L, 13L, 8L, 8L, 7L),
    Windows = c(7L, 8L, 14L, 7L, 8L, 8L, 13L, 8L, 8L, 7L, 8L, 9L, 11L, 8L,
                8L, 7L, 3L, 3L, 8L, 8L, 8L, 11L, 9L, 8L, 7L, 8L, 8L, 13L, 8L,
                8L, 7L, 14L, 8L, 7L)
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))

  # Test that NA rasters plot correctly, i.e., with na.color only, not default
  ras <- raster(matrix(NA, ncol = 3, nrow = 3))
  setColors(ras, n = 3) <- c("red", "blue", "green")

  png(file = "test.png", width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE, speedup = 2e5, na.color = "black")
  dev.off()

  #dput(getFingerprint(file = "test.png"))
  orig <- switch(Sys.info()["sysname"],
    Darwin = c(7L, 4L, 5L, 7L, 4L, 8L, 4L, 5L, 7L, 4L, 5L, 7L, 5L, 7L, 4L,
               5L, 7L, 4L, 4L, 5L, 7L, 4L, 4L, 4L, 5L, 4L, 3L, 3L, 3L, 3L, 3L,
               4L, 4L, 5L, 4L, 4L, 7L, 4L, 5L, 4L, 7L, 4L, 5L, 7L, 4L, 8L, 4L,
               5L, 7L, 4L, 5L, 7L, 5L, 7L, 4L, 5L, 7L, 4L),
    Linux =  c(7L, 4L, 5L, 7L, 4L, 8L, 4L, 5L, 7L, 4L, 5L, 4L, 4L, 7L, 4L,
               5L, 4L, 7L, 4L, 5L, 7L, 5L, 7L, 4L, 5L, 7L, 3L, 3L, 3L, 7L, 4L,
               5L, 7L, 4L, 8L, 4L, 5L, 7L, 4L, 4L, 5L, 7L, 4L, 4L, 4L, 5L, 7L,
               4L, 5L, 7L, 5L, 7L, 4L, 5L, 7L, 4L),
    Windows = c(7L, 4L, 5L, 7L, 8L, 4L, 4L, 5L, 7L, 4L, 8L, 5L, 7L, 4L, 5L,
                4L, 7L, 4L, 8L, 4L, 5L, 7L, 4L, 5L, 7L, 3L, 3L, 3L, 7L, 4L, 5L,
                7L, 4L, 5L, 7L, 5L, 7L, 4L, 4L, 5L, 7L, 4L, 8L, 5L, 7L, 4L, 5L,
                4L, 7L, 8L, 4L, 5L, 7L, 4L)
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))

  # Test legendRange in Plot
  set.seed(1234)
  ras <- raster(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  setColors(ras, n = 3) <- c("red", "blue", "green")

  png(file = "test.png", width = 400, height = 300)
  clearPlot()
  Plot(ras, legendRange = 0:5, new = TRUE)
  dev.off()

  #dput(getFingerprint(file = "test.png"))
  orig <- switch(Sys.info()["sysname"],
    Darwin = c(10L, 3L, 10L, 3L, 11L, 3L, 10L, 3L, 10L, 3L, 11L, 3L, 10L,
               5L, 17L, 5L, 3L, 10L, 22L, 10L, 3L, 5L, 19L, 3L, 10L, 4L, 10L,
               4L, 9L, 4L, 9L, 5L, 9L, 4L, 9L, 5L, 13L),
    Linux = c(13L, 14L, 3L, 5L, 3L, 6L, 7L, 10L, 10L, 7L, 6L, 8L, 5L, 5L,
              3L, 6L, 4L, 5L, 4L, 4L, 3L, 3L, 5L, 5L, 3L, 4L, 4L, 4L, 5L, 3L,
              6L, 3L, 6L, 4L, 8L, 6L, 7L, 11L, 9L, 7L, 6L, 5L, 3L, 10L, 13L,
              14L),
    Windows = c(10L, 5L, 8L, 9L, 4L, 4L, 10L, 6L, 5L, 8L, 7L, 4L, 8L, 8L, 6L,
                13L, 8L, 9L, 18L, 9L, 9L, 13L, 7L, 9L, 6L, 5L, 8L, 5L, 8L, 8L,
                5L, 5L, 9L, 5L, 8L, 5L)
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))
  unlink("test.png")
})

test_that("Plot is not error-free", {
  skip("This is a visual only test")
  tmpdir <- file.path(tempdir(), "test_Plot")
  dir.create(tmpdir, recursive = TRUE)
  cwd <- getwd()
  setwd(tmpdir)
  library(raster)
  library(SpaDES)

  on.exit({
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  })
  r<-raster(system.file("external/test.grd", package="raster"))
  message("These two plots should look similar")
  plot(r)
  dev()

  # 128 < vals < 1806
  Plot(r, new=TRUE)

  # -71 < vals < 1606
  r1 <- r-200
  Plot(r1, new=TRUE)

  # 0 < vals <= 1
  r1 <- r / maxValue(r)
  Plot(r1, new=TRUE)

  # 0 <= vals < 1
  r1 <- (r - min(getValues(r), na.rm= TRUE)) / max(getValues(r), na.rm=TRUE)
  Plot(r1, new=TRUE)

  # 0 <= vals <= 1
  r1 <- r - min(getValues(r), na.rm=TRUE)
  r1 <- r1/max(getValues(r1), na.rm=TRUE)
  Plot(r1, new=TRUE)

  # 0, 1, 2, 3
  r1 <- raster(ncol=3, nrow=3)
  r1[] <- sample(0:3, replace=TRUE, size = 9)
  Plot(r1, new=TRUE)

  # 0, 1 # Incorrect, presently because it is treating it as real
  r1 <- raster(ncol=3, nrow=3)
  r1[] <- sample(0:1, replace=TRUE, size = 9)
  Plot(r1, new=TRUE)

  # 0, 1, 2, 3, ... 30
  r1 <- raster(ncol=30, nrow=30)
  r1[] <- sample(0:30, replace=TRUE, size = 900)
  Plot(r1, new=TRUE)

  # 0, 1, 2, 3, 4, 5, 6
  r1 <- raster(ncol=30, nrow=30)
  r1[] <- sample(0:6, replace=TRUE, size = 900)
  Plot(r1, new=TRUE)

  # 1, 2, 3, 4, 5, 6
  r1 <- raster(ncol=30, nrow=30)
  r1[] <- sample(1:6, replace=TRUE, size = 900)
  Plot(r1, new=TRUE)

})
