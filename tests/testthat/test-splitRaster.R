
test_that("split raster layer to mutiple tiles with or without buffer", {
  require(raster)
  b <- brick(system.file("external/rlogo.grd", package = "raster"))
  r <- b[[1]] # use first layer only
  nx <- 3
  ny <- 4
  expect_equal(xres(r), 1)
  expect_equal(yres(r), 1)
  # change the extent of r
  extent(r) <- extent(xmin(r)-30, xmax(r)-30, ymin(r)-20, ymax(r)-20)
  # test non buffer split
  y0 <- splitRaster(r, nx, ny) # without buffer
  expect_equal(class(y0), "list")
  xextents <- c()
  yextents <- c()
  for(i in 1:length(y0)){
    xextents <- c(xextents, xmin(y0[[i]]), xmax(y0[[i]]))
    yextents <- c(yextents, ymin(y0[[i]]), ymax(y0[[i]]))
  }
  expect_equal(sort(unique(xextents)), c(-30, 4, 37, 71))
  expect_equal(sort(unique(yextents)), c(-20, -1, 18, 38, 57))
  rm(xextents, yextents)

  # test with buffer 3 and 4 for x and y, respectively
  y1 <- splitRaster(r, nx, ny, c(3, 4)) # with 3 pixels buffer at horizontal scale
  #'                                       # and 4 pixels buffer at vertical scale
  xextents <- c()
  yextents <- c()
  for(i in 1:length(y0)){
    xextents <- c(xextents, xmin(y1[[i]]), xmax(y1[[i]]))
    yextents <- c(yextents, ymin(y1[[i]]), ymax(y1[[i]]))
  }
  expect_equal(sort(unique(xextents)), c(-30, 1, 7, 34, 40, 71))
  expect_equal(sort(unique(yextents)), c(-20, -5, 3, 14, 22, 34, 42, 57))
  rm(xextents, yextents)

  # test with buffer 0.5 and 0.3 for x and y, respectively,
  # this is relative scale
  y2 <- splitRaster(r, nx, ny, c(0.5, 0.3))
  xextents <- c()
  yextents <- c()
  for(i in 1:length(y0)){
    xextents <- c(xextents, xmin(y2[[i]]), xmax(y2[[i]]))
    yextents <- c(yextents, ymin(y2[[i]]), ymax(y2[[i]]))
  }
  expect_equal(sort(unique(xextents)), c(-30, -13, 20, 21, 54, 71))
  expect_equal(sort(unique(yextents)), c(-20, -7, 5, 12, 24, 32, 44, 57))
  rm(xextents, yextents)

  # for the raster layer of different resolution
  res(r) <- c(5, 6)
  y3 <- splitRaster(r, nx, ny)
  rows <- 1:ny
  for(j in c(0, 4, 8)){
    colmin <- c()
    colmax <- c()
    for(i in rows+j){
      colmin <- unique(c(colmin, xmin(y3[[i]])))
      colmax <- unique(c(colmax, xmax(y3[[i]])))
    }
    if(j>0){expect_true(colmin == colmaxtemp)}
    colmaxtemp <- colmax
  }

  cols <- c(1, 5, 9)
  for(j in 0:3){
    rowmin <- c()
    rowmax <- c()
    for(i in cols+j){
      rowmin <- unique(c(rowmin, ymin(y3[[i]])))
      rowmax <- unique(c(rowmax, ymax(y3[[i]])))
    }
    if(j>0){expect_true(rowmin == rowmaxtemp)}
    rowmaxtemp <- rowmax
  }
})



