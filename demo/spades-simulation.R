#' SpaDES simulation demo
#'
#' randomLandscapes, caribouMovement, fireSpread
#'
# @demoTitle spades-simulation
#

library(SpaDES)
library(igraph)

if (require(rgdal, quietly = TRUE)) {
  filelist = data.frame(
    files = dir(system.file("maps", package = "quickPlot"),
                             full.names = TRUE, pattern = "tif"),
    functions = "rasterToMemory",
    packages = "quickPlot",
    stringsAsFactors = FALSE
  )

  stackName = "landscape"

  mySim <- simInit(
    times = list(start = 0.0, end = 100.00),
    params = list(
      .progress = list(type = "graphical", interval = 10),
      .globals = list(stackName = stackName, burnStats = "nPixelsBurned"),
      randomLandscapes = list(
        nx = 1e2, ny = 1e2, .saveObjects = stackName,
        .plotInitialTime = NA, .plotInterval = NA, inRAM = TRUE
      ),
      caribouMovement = list(
        N = 1e2, .saveObjects = c("caribou"),
        .plotInitialTime = 1, .plotInterval = 1, moveInterval = 1
      ),
      fireSpread = list(
        nFires = 1e1, spreadprob = 0.235, persistprob = 0, its = 1e6,
        returnInterval = 10, startTime = 0,
        .plotInitialTime = 0, .plotInterval = 10
      )
    ),
    #modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    modules = list("fireSpread", "caribouMovement"),
    inputs = filelist,
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
  )

  mySim$landscape <- stack(mySim$DEM, mySim$forestAge, mySim$habitatQuality, mySim$percentPine)
  if (interactive()) { dev() };
  mySim <- spades(mySim, debug = TRUE)
} else {
  warning("To see full demo, you need to install rgdal: install.packages(\"rgdal\")")
}

