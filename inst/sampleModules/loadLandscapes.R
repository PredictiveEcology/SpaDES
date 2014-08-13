################################################
###
### loadLandscapes MODULE
###
###############################################

doEvent.loadLandscapes <- function(sim, eventTime, eventType, debug=FALSE) {
    if (eventType=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends <- "NONE" # list module names here

        if (reloadModuleLater(sim, depends)) {
            sim <- scheduleEvent(sim, currentTime(sim), "loadLandscapes", "init")
        } else {
            sim <- loadLandscapesInit(sim)
        }
        sim <- scheduleEvent(sim, simParams(sim)$loadLandscapes$plotInitialTime, "loadLandscapes", "plot")
        sim <- scheduleEvent(sim, simParams(sim)$loadLandscapes$saveInitialTime, "loadLandscapes", "save")

    } else if (eventType=="plot") {
      # do stuff for this event
      simPlot(habitat, col=.cols[6:2])

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$loadLandscapes$plotInterval, "loadLandscapes", "plot")
    } else if (eventType=="save") {

      # do stuff for this event
      simSave(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$loadLandscapes$saveInterval, "loadLandscapes", "save")

    } else {
      warning(paste("Undefined event type: \'", simEvents(sim)[1,"eventType",with=FALSE],
                    "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE] ,"\'", sep=""))
    }
    return(sim)
}

loadLandscapesInit <- function(sim) {
  ### load any required packages
  pkgs <- list("raster") # list required packages here
  loadPackages(pkgs)

  # the map layers used here are read in by the load module during simInit,
  # so they should exist in the global environment.
  habitat <<- stack(DEM, forestAge, forestCover, habitatQuality, percentPine)
  names(habitat) <<- c("DEM", "forestAge", "forestCover", "habitatQuality", "percentPine")

  # last thing to do is add module name to the loaded list
  simLoaded(sim) <- append(simLoaded(sim), "loadLandscapes")

  return(sim)
}
