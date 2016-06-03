test_that("splitRaster and mergeRaster work on small in-memory rasters", {
  library(raster)
  on.exit(detach("package:raster"))

  b <- brick(system.file("external/rlogo.grd", package = "raster"))
  r <- b[[1]] # use first layer only
  nx <- 3
  ny <- 4
  expect_equal(xres(r), 1)
  expect_equal(yres(r), 1)

  # change the extent of r
  extent(r) <- extent(xmin(r) - 30, xmax(r) - 30, ymin(r) - 20, ymax(r) - 20)

  # no buffer
  y0 <- splitRaster(r, nx, ny)
  expect_equal(class(y0), "list")
  xextents <- c()
  yextents <- c()
  for (i in 1:length(y0)) {
    xextents <- c(xextents, xmin(y0[[i]]), xmax(y0[[i]]))
    yextents <- c(yextents, ymin(y0[[i]]), ymax(y0[[i]]))
  }
  expect_equal(sort(unique(xextents)), c(-30, 4, 37, 71))
  expect_equal(sort(unique(yextents)), c(-20, -1, 18, 38, 57))
  rm(xextents, yextents)

  expect_equal(length(unique(lapply(y0, crs))), 1L)
  expect_equal(unique(lapply(y0, crs))[[1]], crs(r))

  m0 <- mergeRaster(y0)
  expect_equal(dim(m0), dim(r))
  expect_equal(extent(m0), extent(r))
  #expect_equal(names(m0), names(r)) # TO DO (#283)
  expect_equal(res(m0), res(r))
  #expect_equal(max(values(m0)), max(values(r))) # TO DO (#283)
  #expect_equal(min(values(m0)), min(values(r))) # TO DO (#283)

  # with buffer (integer pixels)
  y1 <- splitRaster(r, nx, ny, c(3L, 4L))
  xextents <- c()
  yextents <- c()
  for (i in 1:length(y0)) {
    xextents <- c(xextents, xmin(y1[[i]]), xmax(y1[[i]]))
    yextents <- c(yextents, ymin(y1[[i]]), ymax(y1[[i]]))
  }
  expect_equal(sort(unique(xextents)), c(-30, 1, 7, 34, 40, 71))
  expect_equal(sort(unique(yextents)), c(-20, -5, 3, 14, 22, 34, 42, 57))
  rm(xextents, yextents)

  expect_equal(length(unique(lapply(y1, crs))), 1L)
  expect_equal(unique(lapply(y1, crs))[[1]], crs(r))

  m1 <- mergeRaster(y1)
  expect_equal(dim(m1), dim(r))
  expect_equal(extent(m1), extent(r))
  #expect_equal(names(m1), names(r)) # TO DO (#283)
  expect_equal(res(m1), res(r))
  #expect_equal(max(values(m1)), max(values(r))) # TO DO (#283)
  #expect_equal(min(values(10)), min(values(r))) # TO DO (#283)

  # with buffer (proportion of cells)
  y2 <- splitRaster(r, nx, ny, c(0.5, 0.3))
  xextents <- c()
  yextents <- c()
  for (i in 1:length(y0)) {
    xextents <- c(xextents, xmin(y2[[i]]), xmax(y2[[i]]))
    yextents <- c(yextents, ymin(y2[[i]]), ymax(y2[[i]]))
  }
  expect_equal(sort(unique(xextents)), c(-30, -13, 20, 21, 54, 71))
  expect_equal(sort(unique(yextents)), c(-20, -7, 5, 12, 24, 32, 44, 57))
  rm(xextents, yextents)

  expect_equal(length(unique(lapply(y2, crs))), 1L)
  expect_equal(unique(lapply(y2, crs))[[1]], crs(r))

  m2 <- mergeRaster(y2)
  expect_equal(dim(m2), dim(r))
  expect_equal(extent(m2), extent(r))
  #expect_equal(names(m2), names(r)) # TO DO (#283)
  expect_equal(res(m2), res(r))
  #expect_equal(max(values(m2)), max(values(r))) # TO DO (#283)
  #expect_equal(min(values(m2)), min(values(r))) # TO DO (#283)

  # different raster resolutions
  res(r) <- c(5, 6)
  y3 <- splitRaster(r, nx, ny)
  rows <- 1:ny
  for (j in c(0, 4, 8)) {
    colmin <- c()
    colmax <- c()
    for (i in rows + j) {
      colmin <- unique(c(colmin, xmin(y3[[i]])))
      colmax <- unique(c(colmax, xmax(y3[[i]])))
    }
    if (j > 0) {
      expect_true(colmin == colmaxtemp)
    }
    colmaxtemp <- colmax
  }

  cols <- c(1, 5, 9)
  for (j in 0:3) {
    rowmin <- c()
    rowmax <- c()
    for (i in cols + j) {
      rowmin <- unique(c(rowmin, ymin(y3[[i]])))
      rowmax <- unique(c(rowmax, ymax(y3[[i]])))
    }
    if (j > 0) {
      expect_true(rowmin == rowmaxtemp)
    }
    rowmaxtemp <- rowmax
  }
})

test_that("splitRaster and mergeRaster work on large on-disk rasters", {
  skip_on_cran()
  skip_on_travis()
  skip("This is very big")

  library(raster)

  on.exit(detach("package:raster"))

  ## use a large raster (1.3 GB)
  tmpdir <- file.path(tempdir(), "splitRaster-test") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  url <- "http://www.cec.org/sites/default/files/Atlas/Files/Land_Cover_2010/Land_Cover_2010_TIFF.zip"
  destfile <- file.path(tmpdir, basename(url))

  download.file(url, destfile) # 48.0 MB

  map <- "Land_Cover_2010_TIFF/LandCover_2010/data/NA_LandCover_2010_25haMMU.tif"
  unzip(destfile, exdir = tmpdir, files = map)

  rasterOptions(maxmemory = 1e9)
  r <- raster(file.path(tmpdir, map))

  # without buffer
  s <- splitRaster(r, 4, 4)
  expect_equal(length(unique(lapply(s, crs))), 1)
  expect_equal(unique(lapply(s, crs))[[1]], crs(r))

  e <- lapply(s, extent)
  u <- e[[1]]
  for (i in seq_along(e)[-1]) {
    u <- raster::union(e[[i]], u)
  }
  expect_equal(extent(r), u)

  m <- mergeRaster(s) # takes a while to run...
  expect_equal(dim(m), dim(r))
  expect_equal(extent(m), extent(r))
  #expect_equal(names(m), names(r)) # TO DO (#283)
  expect_equal(res(m), res(r))
  #expect_equal(max(values(m)), max(values(r))) # TO DO (#283)
  #expect_equal(min(values(m)), min(values(r))) # TO DO (#283)

  # with buffer
  s1 <- splitRaster(r, 4, 4, c(100, 100))
  expect_equal(length(unique(lapply(s, crs))), 1)
  expect_equal(unique(lapply(s, crs))[[1]], crs(r))

  e1 <- lapply(s1, extent)
  u1 <- e1[[1]]
  for (i in seq_along(e1)[-1]) {
    u1 <- raster::union(e1[[i]], u1)
  }
  expect_equal(extent(r), u1)

  m1 <- mergeRaster(s1) # takes a while to run...
  expect_equal(dim(m1), dim(r))
  expect_equal(extent(m1), extent(r))
  #expect_equal(names(m1), names(r)) # TO DO (#283)
  expect_equal(res(m1), res(r))
  #expect_equal(max(values(m1)), max(values(r))) # TO DO (#283)
  #expect_equal(min(values(m1)), min(values(r))) # TO DO (#283)
})
