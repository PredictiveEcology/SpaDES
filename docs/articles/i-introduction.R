## ----install-fastshp, echo=TRUE, eval=FALSE------------------------------
#  install.packages("fastshp", repos = "http://rforge.net", type = "source")

## ----SpaDES-demo, eval=FALSE, echo=TRUE----------------------------------
#  library("SpaDES")
#  demo("spades-simulation", package="SpaDES")

## ----SpaDES-modules, eval=FALSE, echo=TRUE-------------------------------
#  downloadModule(name = "moduleName")

## ----view-sim, eval=FALSE, echo=TRUE-------------------------------------
#  # full simulation details:
#  #  simList object info + simulation data
#  mySim
#  
#  # less detail:
#  # simList object isn't shown; object details are
#  ls.str(mySim)
#  
#  # least detail:
#  # simList object isn't shown; object names only
#  ls(mySim)

## ----view-dependencies, eval=FALSE, echo=TRUE----------------------------
#  library(igraph)
#  depsEdgeList(mySim, FALSE)  # data.frame of all object dependencies
#  moduleDiagram(mySim)        # plots simplified module (object) dependency graph
#  objectDiagram(mySim)        # plots object dependency diagram

## ----view-event-sequences, eval=FALSE, echo=TRUE-------------------------
#  options(spades.nCompleted = 50)   # default: store 1000 events in the completed event list
#  mySim <- simInit(...)             # initialize a simulation using valid parameters
#  mySim <- spades(mySim)            # run the simulation, returning the completed sim object
#  eventDiagram(mySim)               # visualize the sequence of events for all modules

## ----experiment, eval=FALSE, echo=TRUE-----------------------------------
#  ?experiment # will give many more examples than here
#  
#  # Copy Example 5 here:
#   mySim <- simInit(
#      times = list(start = 0.0, end = 2.0, timeunit = "year"),
#      params = list(.globals = list(stackName = "landscape", burnStats = "nPixelsBurned")),
#      modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#      paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
#                   outputPath = tmpdir),
#      # Save final state of landscape and caribou
#      outputs = data.frame(objectName = c("landscape", "caribou"), stringsAsFactors = FALSE)
#    )
#  
#  sims <- experiment(mySim, replicates = 2)
#  attr(sims, "experiment")$expDesign # shows 2 replicates of same experiment

