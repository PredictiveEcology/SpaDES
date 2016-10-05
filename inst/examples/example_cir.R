library(raster)
library(data.table)
library(sp)

# circle centred
Ras <- raster(extent(0, 15, 0, 15), res = 1, val = 0)
middleCircle <- cir(Ras)
Ras[middleCircle[, "indices"]] <- 1
circlePoints <- SpatialPoints(middleCircle[, c("x", "y")])
if (interactive()) {
  clearPlot()
  Plot(Ras)
  Plot(circlePoints, addTo = "Ras")
}

# circles non centred
Ras <- randomPolygons(Ras, numTypes = 4)
N <- 2
agent <- SpatialPoints(coords = cbind(x = stats::runif(N, xmin(Ras), xmax(Ras)),
                                      y = stats::runif(N, xmin(Ras), xmax(Ras))))

cirs <- cir(Ras, agent, maxRadius = 15, simplify = TRUE)
cirsSP <- SpatialPoints(coords = cirs[, c("x", "y")])
cirsRas <- raster(Ras)
cirsRas[] <- 0
cirsRas[cirs[, "indices"]] <- 1

if (interactive()) {
  clearPlot()
  Plot(Ras)
  Plot(cirsRas, addTo = "Ras", cols = c("transparent", "#00000055"))
  Plot(agent, addTo = "Ras")
  Plot(cirsSP, addTo = "Ras")
}

# Example comparing rings and cir
a <- raster(extent(0,30,0,30), res = 1)
hab <- gaussMap(a, speedup = 1) # if raster is large (>1e6 pixels), use speedup>1
radius <- 4
N <- 2
coords <- SpatialPoints(coords = cbind(x = stats::runif(N, xmin(hab), xmax(hab)),
                                       y = stats::runif(N, xmin(hab), xmax(hab))))

# cirs
cirs <- cir(hab, coords, maxRadius = rep(radius, length(coords)), simplify = TRUE)

# rings
loci <- cellFromXY(hab, coordinates(coords))
cirs2 <- rings(hab, loci, maxRadius = radius, minRadius = radius - 1, returnIndices = TRUE)

# Plot both
ras1 <- raster(hab)
ras1[] <- 0
ras1[cirs[, "indices"]] <- cirs[, "id"]

ras2 <- raster(hab)
ras2[] <- 0
ras2[cirs2$indices] <- cirs2$id
if (interactive()) {
  clearPlot()
  Plot(ras1, ras2)
}

a <- raster(extent(0, 100, 0, 100), res = 1)
hab <- gaussMap(a,speedup = 1)
cirs <- cir(hab, coords, maxRadius = 44, minRadius = 0)
ras1 <- raster(hab)
ras1[] <- 0
cirsOverlap <- data.table(cirs)[,list(sumIDs = sum(id)),by = indices]
ras1[cirsOverlap$indices] <- cirsOverlap$sumIDs
if (interactive()) {
  clearPlot()
  Plot(ras1)
}

# Provide a specific set of angles
Ras <- raster(extent(0, 330, 0, 330), res = 1)
Ras[] <- 0
N <- 2
coords <- cbind(x = stats::runif(N, xmin(Ras), xmax(Ras)),
                y = stats::runif(N, xmin(Ras), xmax(Ras)))
circ <- cir(Ras, coords, angles = seq(0, 2*pi, length.out = 21),
            maxRadius = 200, minRadius = 0, returnIndices = FALSE,
            allowOverlap = TRUE, returnAngles = TRUE)
