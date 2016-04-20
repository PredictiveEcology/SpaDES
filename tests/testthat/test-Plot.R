test_that("Plot 1 is not error-free", {
  library(sp)
  library(raster)

  tmpdir <- file.path(tempdir(), "test_Plot1") %>% checkPath(create = TRUE)
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
  # So all tests must run a clearPlot or a new = TRUE to be cleared to
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

  # can add a plot to the plotting window
  clearPlot()
  expect_that(Plot(landscape87654), testthat::not(throws_error()))
  expect_that(Plot(caribou87654, new = FALSE), testthat::not(throws_error()))

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
  a <- seq(0, 2 * pi, length.out = N)
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

  # Should work with col as well as cols
  clearPlot()
  expect_that(Plot(DEM87654, col = c("blue", "red")),
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
  if (Sys.info()["sysname"] == "Windows") skip("Not working on Windows yet")
  skip_if_not_installed("visualTest")
  skip("Not reliable yet")

  library(raster)
  library(visualTest)

  tmpdir <- file.path(tempdir(), "test_Plot_imageContent") %>% checkPath(create = TRUE)
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
    Darwin = "BB1FC0E03E1E3B30",
    Linux = "BB1EC4E03E1E3B30",
    Windows = "E49B649B8A64DB64"
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
    Darwin = "BB1FC0E03E1E3B30",
    Linux = "BB1EC4E03E1E3B30",
    Windows = "A6997E268926D966"
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
    Darwin = "EEC0913E4AE16E2E",
    Linux = "EEC0913E4AE16E2E",
    Windows = "A49B659B8A64DB64"
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))
})

test_that("Unit tests for plotting colors", {
  if (Sys.info()["sysname"] == "Windows") skip("Not working on Windows yet")
  skip_if_not_installed("visualTest")
  skip("Not reliable yet")

  library(raster)
  library(visualTest)

  tmpdir <- file.path(tempdir(), "test_Plot_colors") %>% checkPath(create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    detach("package:raster")
    detach("package:visualTest")
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  })

  ras <- raster(matrix(c(1, 0, 1, 2), ncol = 2))
  setColors(ras, n = 3) <- c("red", "blue", "green")

  png(file = "test.png", width = 400, height = 300)
  clearPlot()
  Plot(ras, new = TRUE)
  dev.off()

  #dput(getFingerprint(file = "test.png"))
  orig <- switch(Sys.info()["sysname"],
    Darwin = "BF6AC491C0663B66",
    Linux = "BF6AC491C0663B36",
    Windows = "A0937D7C8224DF6C"
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.002))
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
    Darwin = "B93964CAC2C6939B",
    Linux = "B938649AC6C6939B",
    Windows = "9F1F33C0E0C0339F"
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
    Darwin = "BF6AC491C0663B66",
    Linux = "BF6AC491C06E3B26",
    Windows = "A0937D7C8224DF6C"
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
    Darwin = "AB95D06E84916F71",
    Linux = "BB95D06EC4916F30",
    Windows = "A0937D7C8224DF6C"
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 8))
  unlink("test.png")
})

test_that("Unit tests for internal functions in Plot", {
  if (Sys.info()["sysname"] == "Windows") skip("Not working on Windows yet")
  skip_if_not_installed("visualTest")
  skip("Not reliable yet")

  library(raster)
  library(visualTest)

  tmpdir <- file.path(tempdir(), "test_Plot_internal") %>% checkPath(create = TRUE)
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
    Darwin = "A0CB77708A30DF74",
    Linux = "A0CF75708A30DF74",
    Windows = "8000FFFE0000FFFF"
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
    Darwin = "A0CB77708A30DF74",
    Linux = "A0CF75708A30DF74",
    Windows = "A288FF760022DD77"
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
    Darwin = "AF8FD0F080303F75",
    Linux = "AF8FD0F0C0302F75",
    Windows = "A288FF760022DD77"
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
    Darwin = "AF99D0E4C0653F64",
    Linux = "AF9BD0E4C1253F60",
    Windows = "A49B7E648964DB64"
  )
  expect_true(isSimilar(file = "test.png", fingerprint = orig, threshold = 0.3))
  unlink("test.png")
})

test_that("Plot 2 is not error-free", {
  skip("This is a visual only test - see verbal expectations")
  tmpdir <- file.path(tempdir(), "test_Plot2") %>% checkPath(create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)
  library(raster)
  library(SpaDES)

  on.exit({
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  })

  r <- raster(system.file("external/test.grd", package = "raster"))
  message("These two plots should look similar")
  plot(r)
  dev()

  # 128 < vals < 1806
  Plot(r, new = TRUE) # Expect rainbow colors, lots of peach, little green

  # -71 < vals < 1606
  r1 <- r - 200
  Plot(r1, new = TRUE) # Expect legend from below 0 to just above 1500

  # 0 < vals <= 1
  r1 <- r / max(getValues(r), na.rm = TRUE)
  Plot(r1, new = TRUE) # Expect legend from below 0.2 to exactly 1

  # 0 <= vals < 1
  r1 <- (r - min(getValues(r), na.rm = TRUE)) / max(getValues(r), na.rm = TRUE)
  Plot(r1, new = TRUE)# Expect legend from exactly 0 to above 0.8

  # 0 <= vals <= 1
  r1 <- r - min(getValues(r), na.rm = TRUE)
  r1 <- r1/max(getValues(r1), na.rm = TRUE)
  Plot(r1, new = TRUE)# Expect legend from exactly 0 to exactly 1

  # 0, 1, 2, 3
  r1 <- raster(ncol = 3, nrow = 3)
  set.seed(234)
  r1[] <- sample(0:3, replace = TRUE, size = 9)
  Plot(r1, new = TRUE) # integers - 0, 1, 2 and 3 should line up with centre of
                      # each color, even though there is no peach in plot

  # 0, 1 #
  r1 <- raster(ncol = 3, nrow = 3)
  r1[] <- sample(0:1, replace = TRUE, size = 9)
  Plot(r1, new = TRUE) # Expect 0 and 1 lined up to middle of green and light grey
                       #  only Green and light grey
  Plot(r1, new = TRUE, zero.color = "black") # black zeros

  # 0, 1, 2, 3, ... 30
  r1 <- raster(ncol = 30, nrow = 30)
  r1[] <- sample(0:30, replace = TRUE, size = 900)
  Plot(r1, new = TRUE)
  Plot(r1, new = TRUE, zero.color = "black") # black zeros, some scattered

  # black zeros, plus legend -10 to 40
  Plot(r1, new = TRUE, zero.color = "black", legendRange = c(-10,40))

  # 0, 1, 2, 3, 4, 5, 6
  r1 <- raster(ncol = 30, nrow = 30)
  r1[] <- sample(0:6, replace = TRUE, size = 900)
  Plot(r1, new = TRUE)

  # 1, 2, 3, 4, 5, 6, ... 200
  r1 <- raster(ncol = 30, nrow = 30)
  r1[] <- sample(1:200, replace = TRUE, size = 900)
  Plot(r1, new = TRUE)

  # should be no black because no zeros
  Plot(r1, new = TRUE, zero.color = "black")

  # should be slim black in legend, none in fig
  Plot(r1, new = TRUE, zero.color = "black", legendRange = c(-10, 200))

  # 31, 32, ... 40
  r1 <- raster(ncol = 30, nrow = 30)
  r1[] <- sample(31:40, replace = TRUE, size = 900)
  Plot(r1, new = TRUE)
  Plot(r1, new = TRUE, legendRange = c(0,40)) # legend frmo 0 to 40, mostly green
  Plot(r1, new = TRUE, zero.color = "black") # no black
  Plot(r1, new = TRUE, zero.color = "black", legendRange = c(35,40)) # lots of white, legend from 35 to 40

  pixelGroupMap <- raster(xmn = 50, xmx = 50 + 3 * 100,
                          ymn = 50, ymx = 50 + 3 * 100,
                          res = c(100, 100), val = 1)
  pixelGroupMap[1] <- -1
  pixelGroupMap[2:6] <- 2
  Plot(pixelGroupMap, new = TRUE)

  # Should have all colors
  Plot(pixelGroupMap, new = TRUE, cols = c("red", "yellow", "green", "blue"))

  ### Test legend that is pre-set, even with various types of rasters
  # should be mostly empty raster, legend from 0 to 200
  Plot(r, legendRange = c(0, 200), new = TRUE, cols = c("red", "green"))

  # should be mostly red raster, a bit of green, legend below 0 to 2000
  Plot(r, legendRange = c(-200, 2000), new = TRUE, cols = c("red", "green"))

  # zero.color on Real numbers doesn't do anything - expect NO BLACK
  r1 <- r - 200
  Plot(r1, new = TRUE, zero.color = "black") # NO BLACK

  # zero.color on Integer numbers should work - expect BLACK both in legend and in a few cells
  r1 <- r - 1000
  r1 <- round(r1/300, 0)
  Plot(r1, new = TRUE, zero.color = "black")

  Plot(pixelGroupMap, zero.color = "red")
  Plot(r)

  Plot(pixelGroupMap, cols = "Blues", new = TRUE, legendRange = c(-3, 4))
  Plot(r)
  pixelGroupMap[] <- pixelGroupMap[] + 5
  Plot(pixelGroupMap, na.color = "white") # Should keep one dark Blue, rest white

  dev.off()
})

test_that("setColors is not error-free", {
  skip("Apparently color palettes are not universal")
  tmpdir <- file.path(tempdir(), "test_setColors") %>% checkPath(create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)
  library(raster)
  library(SpaDES)

  on.exit({
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  })
  set.seed(1234)

  ras1 <- raster(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  ras2 <- raster(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  rasStack <- stack(ras1, ras2)
  expect_error({
    setColors(rasStack, n = c(ras1 = 3, ras2 = 5)) <-
      list(ras1 = c("red", "blue", "green"), ras2 = c("purple", "yellow"))
  })
  names(rasStack) <- c("ras1", "ras2")
  expect_silent({
    setColors(rasStack, n = c(ras1 = 3, ras2 = 5)) <-
      list(ras1 = c("red", "blue", "green"), ras2 = c("purple", "yellow"))
  })

  expect_true(identical(
    getColors(rasStack),
    structure(list(ras1 = c("#FF0000FF", "#0000FFFF", "#00FF00FF"),
                   ras2 = c("#A020F0FF", "#B757B3FF", "#CF8F78FF", "#E7C73CFF",
                            "#FFFF00FF")),
              .Names = c("ras1", "ras2"))
  ))

  ras3 <- raster(matrix(sample(1:3, size = 100, replace = TRUE), ncol = 10))
  rasStack <- stack(rasStack, ras3)
  names(rasStack)[3] <- "ras3"

  expect_silent({
    setColors(rasStack, n = c(ras1 = 3, 5)) <- list(
      ras1 = c("red", "blue", "green"),
      ras2 = c("purple", "yellow"),
      ras3 = c("orange", "yellow")
    )
  })
  expect_true(identical(
    getColors(rasStack),
    structure(list(ras1 = c("#FF0000FF", "#0000FFFF", "#00FF00FF"),
                   ras2 = c("#A020F0FF", "#B757B3FF", "#CF8F78FF", "#E7C73CFF",
                            "#FFFF00FF"),
                   ras3 = c("#FFA500FF", "#FFBB00FF", "#FFD200FF", "#FFE800FF",
                            "#FFFF00FF")),
              .Names = c("ras1", "ras2", "ras3"))
  ))
})
