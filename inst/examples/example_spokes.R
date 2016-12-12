library(raster)
library(sp)

set.seed(1234)

Ras <- raster(extent(0, 10, 0, 10), res = 1, val = 0)
rp <- randomPolygons(Ras, numTypes = 10)
if (interactive()) {
  clearPlot()
  Plot(rp)
}
angles <- seq(0, pi * 2, length.out = 17)
angles <- angles[-length(angles)]
N <- 2
loci <- sample(ncell(rp), N)
coords <- SpatialPoints(xyFromCell(rp, loci))
stopRule <- function(landscape) landscape < 3
d2 <- spokes(rp, coords = coords, stopRule = stopRule,
             minRadius = 0, maxRadius = 50,
             returnAngles = TRUE, returnDistances = TRUE,
             allowOverlap = TRUE, angles = angles, returnIndices = TRUE)

# Assign values to the "patches" that were in the viewshed of a ray
rasB <- raster(Ras)
rasB[] <- 0
rasB[d2[d2[, "stop"] == 1, "indices"]] <- 1
if (interactive()) {
  Plot(rasB, addTo = "rp", zero.color = "transparent", cols = "red")
  # can plot it as raster or spatial points
  # Plot(rasB, addTo = "rp", zero.color = "transparent", cols = "black")
  if (NROW(d2) > 0) {
    sp1 <- SpatialPoints(d2[, c("x", "y")])
    Plot(sp1, addTo = "rp", pch = 19, size = 5, speedup = 0.1)
  }
  Plot(coords, addTo = "rp", pch = 19, size = 6, cols = "blue", speedup = 0.1)
}
