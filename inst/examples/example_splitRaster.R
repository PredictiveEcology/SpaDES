library(raster)

# an example with dimensions:
# nrow = 77
# ncol = 101
# nlayers = 3
b <- brick(system.file("external/rlogo.grd", package = "raster"))
r <- b[[1]] # use first layer only
nx <- 1
ny <- 2

tmpdir <- file.path(tempdir(), "splitRaster-example")
dir.create(tmpdir)

y0 <- splitRaster(r, nx, ny, path = file.path(tmpdir, "y0")) # no buffer

# buffer: 10 pixels along both axes
y1 <- splitRaster(r, nx, ny, c(10, 10), path = file.path(tmpdir, "y1"))

# buffer: half the width and length of each tile
y2 <- splitRaster(r, nx, ny, c(0.5, 0.5), path = file.path(tmpdir, "y2"))

# parallel cropping
if (interactive()) {
  n <- pmin(parallel::detectCores(), 4) # use up to 4 cores
  beginCluster(n)
  y3 <- splitRaster(r, nx, ny, c(0.7, 0.7), path = file.path(tmpdir, "y3"))
  endCluster()
}

# the original raster:
if (interactive()) plot(r) # may require a call to `dev()` if using RStudio

# the split raster:
layout(mat = matrix(seq_len(nx * ny), ncol = nx, nrow = ny))
plotOrder <- c(4, 8, 12, 3, 7, 11, 2, 6, 10, 1, 5, 9)
if (interactive()) invisible(lapply(y0[plotOrder], plot))

# can be recombined using `raster::merge`
m0 <- do.call(merge, y0)
all.equal(m0, r) ## TRUE

m1 <- do.call(merge, y1)
all.equal(m1, r) ## TRUE

m2 <- do.call(merge, y2)
all.equal(m2, r) ## TRUE

# or recombine using SpaDES::mergeRaster
n0 <- mergeRaster(y0)
all.equal(n0, r) ## TRUE

n1 <- mergeRaster(y1)
all.equal(n1, r) ## TRUE

n2 <- mergeRaster(y2)
all.equal(n2, r) ## TRUE

unlink(tmpdir, recursive = TRUE)
