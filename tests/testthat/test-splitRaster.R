test_that("splitRaster and mergeRaster work on small in-memory rasters", {
  library(raster); on.exit(detach("package:raster"), add = TRUE)

  owd <- getwd()
  tmpdir <- file.path(tempdir(), "splitRaster-test") %>% checkPath(create = TRUE)
  setwd(tmpdir)

  on.exit({
    setwd(owd)
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

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
  expect_true(unique(unlist(lapply(y0, fromDisk))))

  for (i in 1:12) {
    expect_true(file.exists(file.path(getwd(), "red", paste0("red_tile", i, ".grd"))))
  }

  xextents <- c()
  yextents <- c()
  for (i in seq_along(y0)) {
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
  expect_equal(names(m0), names(r))
  expect_equal(res(m0), res(r))
  expect_equal(max(values(m0)), max(values(r)))
  expect_equal(min(values(m0)), min(values(r)))

  # with buffer (integer pixels) and with specified path
  y1 <- splitRaster(r, nx, ny, c(3L, 4L), path = file.path(tmpdir, "red1"))
  expect_true(unique(unlist(lapply(y1, fromDisk))))

  for (i in 1:12) {
    expect_true(file.exists(file.path(tmpdir, "red1", paste0("red_tile", i, ".grd"))))
  }

  xextents <- c()
  yextents <- c()
  for (i in seq_along(y1)) {
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
  expect_equal(names(m1), names(r))
  expect_equal(res(m1), res(r))
  expect_equal(max(values(m1)), max(values(r)))
  expect_equal(min(values(m1)), min(values(r)))

  # with buffer (proportion of cells)
  y2 <- splitRaster(r, nx, ny, c(0.5, 0.3), path = file.path(tmpdir, "red2"))
  xextents <- c()
  yextents <- c()
  for (i in seq_along(y2)) {
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
  expect_equal(names(m2), names(r))
  expect_equal(res(m2), res(r))
  expect_equal(max(values(m2)), max(values(r)))
  expect_equal(min(values(m2)), min(values(r)))

  # different raster resolutions
  r1 <- r
  res(r1) <- c(5, 6) # no values assigned
  r1[] <- 1:ncell(r1)
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

test_that("splitRaster works in parallel", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  if (interactive()) {
    library(raster); on.exit(detach("package:raster"), add = TRUE)

    tmpdir <- file.path(tempdir(), "splitRaster-test-parallel") %>% checkPath(create = TRUE)

    on.exit({
      detach("package:raster")
      unlink(tmpdir, recursive = TRUE)
    }, add = TRUE)

    b <- brick(system.file("external/rlogo.grd", package = "raster"))
    r <- b[[1]] # use first layer only
    nx <- 3
    ny <- 4
    expect_equal(xres(r), 1)
    expect_equal(yres(r), 1)

    # change the extent of r
    extent(r) <- extent(xmin(r) - 30, xmax(r) - 30, ymin(r) - 20, ymax(r) - 20)

    # test parallel cropping
    n <- pmin(parallel::detectCores(), 4) # use up to 4 cores
    beginCluster(n)
    on.exit(raster::endCluster(), add = TRUE)

    cl <- getCluster()

    y11 <- splitRaster(r, nx, ny, c(3L, 4L), path = file.path(tmpdir, "red11"))
    expect_true(unique(unlist(lapply(y11, fromDisk))))

    for (i in 1:12) {
      expect_true(file.exists(file.path(tmpdir, "red11", paste0("red_tile", i, ".grd"))))
    }

    xextents <- c()
    yextents <- c()
    for (i in seq_along(y11)) {
      xextents <- c(xextents, xmin(y11[[i]]), xmax(y11[[i]]))
      yextents <- c(yextents, ymin(y11[[i]]), ymax(y11[[i]]))
    }
    expect_equal(sort(unique(xextents)), c(-30, 1, 7, 34, 40, 71))
    expect_equal(sort(unique(yextents)), c(-20, -5, 3, 14, 22, 34, 42, 57))
    rm(xextents, yextents)

    expect_equal(length(unique(lapply(y11, crs))), 1L)
    expect_equal(unique(lapply(y11, crs))[[1]], crs(r))

    m11 <- mergeRaster(y11)
    expect_equal(dim(m11), dim(r))
    expect_equal(extent(m11), extent(r))
    expect_equal(names(m11), names(r))
    expect_equal(res(m11), res(r))
    expect_equal(max(values(m11)), max(values(r)))
    expect_equal(min(values(m11)), min(values(r)))
    endCluster()
  }
})

test_that("splitRaster and mergeRaster work on large on-disk rasters", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip("This is very big.")

  tmpdir <- file.path(tempdir(), "splitRaster-test-large") %>% checkPath(create = TRUE)
    library(raster); on.exit(detach("package:raster"), add = TRUE)


  on.exit({
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  ## use a large raster (1.3 GB)
  url <- "http://www.cec.org/sites/default/files/Atlas/Files/Land_Cover_2010/Land_Cover_2010_TIFF.zip"
  destfile <- file.path(tmpdir, basename(url))

  download.file(url, destfile) # 48.0 MB

  map <- "Land_Cover_2010_TIFF/LandCover_2010/data/NA_LandCover_2010_25haMMU.tif"
  unzip(destfile, exdir = tmpdir, files = map)

  rasterOptions(maxmemory = 1e9)
  r <- raster(file.path(tmpdir, map))

  # without buffer
  s <- splitRaster(r, 4, 4, path = file.path(tmpdir, "s"))
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
  expect_equal(names(m), names(r))
  expect_equal(res(m), res(r))
  #expect_equal(maxValue(m)), maxValue(r))) ## Error: cannot allocate vector of size 4.8 Gb
  #expect_equal(min(values(m)), min(values(r))) ## Error: cannot allocate vector of size 4.8 Gb

  # with buffer
  s1 <- splitRaster(r, 4, 4, c(100, 100), path = file.path(tmpdir, "s1"))
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
  expect_equal(names(m1), names(r))
  expect_equal(res(m1), res(r))
  #expect_equal(max(values(m1)), max(values(r)))
  #expect_equal(min(values(m1)), min(values(r)))
})
