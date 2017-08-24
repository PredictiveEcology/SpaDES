## ----module-metadata, eval=FALSE, echo=TRUE------------------------------
#  ## sample module metadata for the default `randomLandscapes` module
#  ## NOTE: long lines have been truncated
#  defineModule(sim, list(
#    name = "randomLandscapes",
#    description = "Generate RasterStack of random maps representative of a forest landsc...",
#    keywords = c("random map", "random landscape"),
#    authors = c(person(c("Alex", "M"), "Chubaty",
#                       email = "alexander.chubaty@canada.ca",
#                       role = c("aut", "cre")),
#                person(c("Eliot", "J", "B"), "McIntire",
#                       email = "eliot.mcintire@canada.ca",
#                       role = c("aut", "cre"))),
#    version = numeric_version("0.2.0"),
#    spatialExtent = raster::extent(rep(NA_real_, 4)),
#    timeframe = as.POSIXlt(c(NA, NA)),
#    timeunit = NA_real_,
#    citation = list(),
#    reqdPkgs = list("raster", "RColorBrewer", "tkrplot", "RandomFields"),
#    parameters = rbind(
#      defineParameter("stackName", "character", "randomLandscape", NA, NA, "..."),
#      defineParameter("nx", "numeric", 100L, NA, NA, "size of map (number ..."),
#      defineParameter("ny", "numeric", 100L, NA, NA, "size of map (number ..."),
#      defineParameter("inRAM", "logical", FALSE, NA, NA, "should the raster ..."),
#      defineParameter(".plotInitialTime", "numeric", 0, NA, NA, "time to ..."),
#      defineParameter(".plotInterval", "numeric", 1, NA, NA, "time interval ..."),
#      defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, "time ..."),
#      defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, "time ...")
#    ),
#    inputObjects = bind_rows(
#      expectsInput(objectName = NA_character_, objectClass = NA_character_,
#                   desc = NA_character_, sourceURL = NA_character_, other = NA_character_)
#    ),
#    outputObjects = bind_rows(
#      createsOutput(objectName = globals(sim)$stackName, objectClass = "RasterStack",
#                    desc = NA_character_, other = NA_character_)
#    )
#  ))

## ----passing-params, eval=FALSE, echo=TRUE-------------------------------
#  library(SpaDES)
#  
#  outputDir <- file.path(tempdir(), "simOutputs")
#  times <- list(start = 0.0, end = 20.0)
#  parameters <- list(
#    .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#    .progress = list(NA),
#    randomLandscapes = list(nx = 100L, ny = 100L, inRAM = TRUE),
#    fireSpread = list(
#      nFires = 10L, spreadprob = 0.225, its = 1e6, persistprob = 0,
#      returnInterval = 10, startTime = 0,
#      .plotInitialTime = 0, .plotInterval = 10
#    ),
#    caribouMovement = list(
#      N = 100L, moveInterval = 1, torus = TRUE,
#      .plotInitialTime = 1, .plotInterval = 1
#    )
#  )
#  modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
#  objects <- list()
#  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES"),
#                outputPath = outputDir)
#  
#  mySim <- simInit(times = times, params = parameters, modules = modules,
#                   objects = objects, paths = paths)

## ----event-types, echo=TRUE, eval=FALSE----------------------------------
#  ## sample event type definitions from the default `randomLandscapes` module
#  doEvent.randomLandscapes <- function(sim, eventTime, eventType, debug = FALSE) {
#    if (eventType == "init") {
#      # do stuff for this event
#      sim <- randomLandscapesInit(sim)
#  
#      # schedule the next events
#      sim <- scheduleEvent(sim, params(sim)$randomLandscapes$.plotInitialTime,
#                           "randomLandscapes", "plot")
#      sim <- scheduleEvent(sim, params(sim)$randomLandscapes$.saveInitialTime,
#                           "randomLandscapes", "save")
#  
#    } else if (eventType=="plot") {
#      # do stuff for this event
#      Plot(sim[[globals(sim)$stackName]])
#  
#      # schedule the next event
#      sim <- scheduleEvent(sim, time(sim) +
#                             params(sim)$randomLandscapes$.plotInterval,
#                           "randomLandscapes", "plot")
#    } else if (eventType == "save") {
#      # do stuff for this event
#      saveFiles(sim)
#  
#      # schedule the next event
#      sim <- scheduleEvent(sim, time(sim) +
#                             params(sim)$randomLandscapes$.saveInterval,
#                           "randomLandscapes", "save")
#  
#    } else {
#      warning(paste("Undefined event type: \'",
#                    events(sim)[1, "eventType", with = FALSE],
#                    "\' in module \'",
#                    events(sim)[1, "moduleName", with = FALSE],
#                    "\'", sep = ""))
#    }
#    return(invisible(sim))
#  }

## ----event-functions, echo=TRUE, eval=FALSE------------------------------
#  ## sample event functions from the default `randomLandscapes` module
#  library(raster)
#  randomLandscapesInit <- function(sim) {
#    if (is.null(params(sim)$randomLandscapes$inRAM)) {
#      inMemory <- FALSE
#    } else {
#      inMemory <- params(sim)$randomLandscapes$inRAM
#    }
#    # Give dimensions of dummy raster
#    nx <- params(sim)$randomLandscapes$nx
#    ny <- params(sim)$randomLandscapes$ny
#    r <- raster(nrows = ny, ncols = nx, xmn = -nx/2, xmx = nx/2,
#                ymn = -ny/2, ymx = ny/2)
#    speedup <- max(1, nx/5e2)
#    # Make dummy maps for testing of models
#    DEM <- gaussMap(template, scale = 300, var = 0.03,
#                    speedup = speedup, inMemory = inMemory)
#    DEM[] <- round(getValues(DEM), 1) * 1000
#    forestAge <- gaussMap(template, scale = 10, var = 0.1,
#                          speedup = speedup, inMemory = inMemory)
#    forestAge[] <- round(getValues(forestAge), 1) * 20
#    percentPine <- gaussMap(template, scale = 50, var = 1,
#                            speedup = speedup, inMemory = inMemory)
#    percentPine[] <- round(getValues(percentPine), 1)
#  
#    # Scale them as needed
#    forestAge <- forestAge / maxValue(forestAge) * 100
#    percentPine <- percentPine / maxValue(percentPine) * 100
#  
#    # Make layers that are derived from other layers
#    habitatQuality <- (DEM + 10 + (forestAge + 2.5) * 10) / 100
#    habitatQuality <- habitatQuality / maxValue(habitatQuality)
#  
#    # Stack them into a single stack and assign to sim envir
#    mapStack <- stack(DEM, forestAge, habitatQuality, percentPine)
#    names(mapStack) <- c("DEM", "forestAge", "habitatQuality", "percentPine")
#    setColors(mapStack) <- list(DEM = brewer.pal(9, "YlOrBr"),
#                                forestAge = brewer.pal(9, "BuGn"),
#                                habitatQuality = brewer.pal(8, "Spectral"),
#                                percentPine = brewer.pal(9, "Greens"))
#    sim[[globals(sim)$stackName]] <- mapStack
#    return(invisible(sim))
#  }

## ----sim-eventDiagram, eval=TRUE, echo=FALSE, message=FALSE, warning=FALSE----
library(igraph) # for %>%
library(SpaDES)
parameters <- list(
  .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
  .progress = list(NA),
  randomLandscapes = list(nx = 100L, ny = 100L, inRAM = TRUE),
  fireSpread = list(nFires = 10L, spreadprob = 0.225, its = 1e6,
                    persistprob = 0, returnInterval = 1, startTime = 0,
                    .plotInitialTime = 0, .plotInterval = 10),
  caribouMovement = list(N = 100L, moveInterval = 1, torus = TRUE,
                         .plotInitialTime = 1, .plotInterval = 1)
)

ftmp <- file.path(tempdir(), "spades_vignetteOutputs")
pdf(ftmp)
clearPlot()
mySim <- simInit(
  times = list(start = 0.0, end = 2.0, timeunit = "year"),
  params = parameters,
  modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
  objects = list(),
  paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
  ) %>% 
  spades()
dev.off()
unlink(ftmp)

## ----eventDiagram, eval=TRUE, fig.height=10, fig.width=7-----------------
mySim <- spades(mySim) # runs the simulation

# overview of the events in the simulation
eventDiagram(mySim, "0000-06-01", n = 200, width = 720)

## ----module-object-diagrams, eval=TRUE, echo=TRUE, message=FALSE, fig.width=7----
library(SpaDES)

times <- list(start = 0.0, end = 20)
parameters <- list(
  .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
)
modules <- list("SpaDES_sampleModules")
paths <- list(modulePath = system.file("sampleModules", package = "SpaDES"))

mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths)

## examine simulation module (object) dependencies
depsEdgeList(mySim, FALSE)      # all object dependency relationships
clearPlot()
moduleDiagram(mySim)            # simplified visual representation of modules

clearPlot()
moduleDiagram(mySim, showParents = TRUE) # similar, but showing parent module grouping

# detailed visual representation of objects
objectDiagram(mySim, width = 720)

## ----checkpoints, echo=TRUE, eval=TRUE, message=FALSE--------------------

# initialize a new simulation, setting the checkpoint interval and filename.
times <- list(start = 0, end = 30)
parameters <- list(
  .globals = list(stackName = "landscape"),
  .checkpoint = list(interval = 10, file = "chkpnt.RData")
)
modules <- list("randomLandscapes", "caribouMovement")
paths <- list(
  modulePath = system.file("sampleModules", package = "SpaDES")
)

mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths)

# retrieve the checkpoint params from the simulation object
checkpointFile(mySim)
checkpointInterval(mySim)

## ----progress, echo=TRUE, eval=TRUE, message=FALSE-----------------------
# initialize a new simulation, setting the progress parameters
mySim <- simInit(times = list(start = 0.0, end = 100.0),
                 params = list(.globals = list(stackName = "landscape"),
                               .progress = list(type = "text", interval = 10)),
                 modules = list("randomLandscapes"),
                 paths = list(modulePath = system.file("sampleModules", package = "SpaDES")))

# retrieve the checkpoint params from the simulation object
progressType(mySim)
progressInterval(mySim)

## ----load-save, echo=TRUE, eval=TRUE, message=FALSE----------------------
# initialize a new simulation, setting the load and save parameters
library(data.table)

outputDir <- file.path(tempdir(), "simOutputs")
mySim <- simInit(times = list(start = 0.0, end = 10),
                 params = list(
                   .globals = list(stackName = "landscape"),
                   randomLandscapes = list(
                     .saveInitialTime = 0, .saveInterval = 10,
                     .saveObjects = c("landscape"),
                     .savePath = file.path(outputDir, "randomLandscapes"))
                   ),
                 modules = list("randomLandscapes"),
                 paths = list(modulePath = system.file("sampleModules", package = "SpaDES"),
                              outputPath = outputDir),
                 outputs = data.frame(objectName = "landscape")
)

# retrieve the load and save params from the simulation object
inputs(mySim)    # shows all files and objects that are "inputs"
outputs(mySim)   # shows all files and objects that are "outputs"

params(mySim)$randomLandscapes$.saveInitialTime
params(mySim)$randomLandscapes$.saveInterval
params(mySim)$randomLandscapes$.saveObjects
params(mySim)$randomLandscapes$.savePath
ftmp <- file.path(tempdir(), "spades_vignetteOutputs")
pdf(ftmp)
clearPlot()
mySim2 <- spades(mySim)

# More sophisticated, passing arguments to outputs()
outputs(mySim) <- data.frame(
  objectName = "landscape", fun = "writeRaster", package = "raster",
  saveTime = c(3,6), arguments = I(lapply(c(3,6), function(x) {
    list(dataType = "FLT4S", format = "raster", overwrite = TRUE)
})))
mySim2 <- spades(mySim)
dev.off()
unlink(ftmp)

## ----save-events, echo=TRUE, eval=FALSE, message=FALSE-------------------
#  ### WITHIN A MODULE:
#  
#  # schedule a recurring save event
#  nextSave <- time(mySim) + params(mySim)$randomLandscapes$.saveInterval
#  sim <- scheduleEvent(mySim, nextSave, "randomLandscapes", "save")

## ----plotting, echo=TRUE, eval=FALSE, message=FALSE----------------------
#  # initialize a new simulation, setting the load and save parameters
#  mySim <- simInit(times = list(start = 0.0, end = 100),
#                   params = list(
#                     .globals = list(stackName = "landscape"),
#                     randomLandscapes = list(.plotInitialTime = 0, .plotInterval = 1)
#                   ),
#                   modules = list("randomLandscapes"),
#                   paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
#  )
#  
#  # retrieve the plotting params from the simulation object
#  params(mySim)$randomLandscapes$.plotInitialTime
#  params(mySim)$randomLandscapes$.plotInterval

## ----plot-events, echo=TRUE, eval=FALSE, message=FALSE-------------------
#  ### WITHIN A MODULE:
#  
#  # schedule a recurring plot event
#  nextPlot <- time(mySim) + params(mySim)$randomLandscapes$.plotInterval
#  mySim <- scheduleEvent(mySim, nextPlot, "randomLandscapes", "save")

## ----caribouMovement, echo=TRUE, eval=FALSE------------------------------
#  openModules(system.file("sampleModules", package = "SpaDES"), "moduleName")

## ----download-module, echo=TRUE, eval=FALSE------------------------------
#  downloadModule("moduleName")

## ----create-new-module, eval=FALSE, echo=TRUE, message=FALSE-------------
#  # create a new module called "randomLandscape" in the "custom-modules" subdirectory
#  # and open the resulting file immediately for editing.
#  newModule(name = "randomLandscapes", path = "custom-modules", open = TRUE)

## ----module-group-init, eval=FALSE---------------------------------------
#  library(DiagrammeR)
#  library(SpaDES)
#  
#  outputDir <- file.path(tempdir(), "simOutputs")
#  times <- list(start = 0.0, end = 20.0)
#  parameters <- list(
#    .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#    .progress = list(NA),
#    randomLandscapes = list(nx = 100L, ny = 100L, inRAM = TRUE),
#    fireSpread = list(
#      nFires = 10L, spreadprob = 0.225, its = 1e6, persistprob = 0,
#      returnInterval = 10, startTime = 0,
#      .plotInitialTime = 0, .plotInterval = 10
#    ),
#    caribouMovement = list(
#      N = 100L, moveInterval = 1, torus = TRUE,
#      .plotInitialTime = 1, .plotInterval = 1
#    )
#  )
#  modules <- list("SpaDES_sampleModules")
#  objects <- list()
#  paths <- list(
#    modulePath = system.file("sampleModules", package = "SpaDES"),
#    outputPath = outputDir
#  )
#  
#  mySim <- simInit(times = times, params = parameters, modules = modules,
#                   objects = objects, paths = paths)
#  
#  modules(mySim) # note the child modules are initialized

## ----module-group-dl, eval=FALSE-----------------------------------------
#  downloadModule("SpaDES_sampleModules")

## ----cleanup, eval=TRUE, echo=FALSE--------------------------------------
unlink(outputDir, recursive = TRUE)

