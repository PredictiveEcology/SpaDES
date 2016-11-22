library(raster)

N <- 2
distRas <- raster(extent(0,40,0,40), res = 1)
coords <- cbind(x = round(runif(N, xmin(distRas), xmax(distRas))) + 0.5,
                y = round(runif(N, xmin(distRas), xmax(distRas))) + 0.5)

# inverse distance weights
dists1 <- distanceFromEachPoint(coords, landscape = distRas)
indices <- cellFromXY(distRas,dists1[, c("x", "y")])
invDist <- tapply(dists1[, "dists"], indices, function(x) sum(1/(1 + x))) # idw function
distRas[] <- as.vector(invDist)
if (interactive()) {
  clearPlot()
  Plot(distRas)
}

# With iterative summing via cumulativeFn to keep memory use low, with same result
dists1 <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                landscape = distRas, cumulativeFn = `+`)
idwRaster <- raster(distRas)
idwRaster[] <- dists1[,"val"]
if (interactive()) Plot(idwRaster)

all(idwRaster[] == distRas[]) # TRUE

# A more complex example of cumulative inverse distance sums, weighted by the value
#  of the origin cell
ras <- raster(extent(0, 34, 0, 34), res = 1, val = 0)
rp <- randomPolygons(ras, numTypes = 10)^2
N <- 15
cells <- sample(ncell(ras), N)
coords <- xyFromCell(ras, cells)
distFn <- function(landscape, fromCell, dist) landscape[fromCell] / (1 + dist)

# beginCluster(3) # can do parallel
dists1 <- distanceFromEachPoint(coords[, c("x", "y"), drop = FALSE],
                                landscape = rp, distFn = distFn, cumulativeFn = `+`)
# endCluster() # if beginCluster was run

idwRaster <- raster(ras)
idwRaster[] <- dists1[,"val"]
if (interactive()) {
  clearPlot()
  Plot(rp, idwRaster)
  sp1 <- SpatialPoints(coords)
  Plot(sp1, addTo = "rp")
  Plot(sp1, addTo = "idwRaster")
}
