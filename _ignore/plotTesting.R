devtools::load_all("~/GitHub/SpaDES")

dev()
clearPlot()
#library(SpaDES)
library(stringi)
times <- list(start = 0, end = 53, timeunit = "year")
parameters <- list(
  .globals = list(stackName = "landscape", burnStats = "burnStats"),
  .progress = list(type = "graphical", interval = 1),
  randomLandscapes = list(.plotInitialTime = times$start),
  fireSpread = list(.plotInitialTime = times$start, startTime = times$start),
  caribouMovement = list(N = 20, torus = TRUE, moveInitialTime = times$start,
                         .plotInitialTime = times$start, .plotInterval = 1))
modules <- list("fireSpread", "caribouMovement", "randomLandscapes")
path <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
#path <- list(modulePath = "~/GitHub/SpaDES.core/inst/sampleModules")

my <- simInit(times = times, params = parameters, modules = modules, path = path)
system.time(spades(my))
