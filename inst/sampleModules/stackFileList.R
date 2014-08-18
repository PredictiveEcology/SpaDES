###
### MODULE: stackFileList
###
### DESCRIPTION: stack the RasterLayers loaded from files inta a single object
###               - DEM, forestAge, forestCover, habitatQuality, percentPine
###

### load any required packages
### (use `loadPackages` or similar)
pkgs <- list("SpaDES")
loadPackages(pkgs)

### event functions
doEvent.stackFileList <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for module dependencies:
    ### (use NULL if no dependencies exist)
    depends <- NULL

    ### check for object dependencies:
    ### (use `checkObject` or similar)


    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(sim, depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), "stackFileList", "init")
    } else {
      objectNames <- simObjectsLoaded(sim)

      mapStack <- stack(mget(objectNames, envir=.GlobalEnv))
      names(mapStack) <- objectNames

      assign(simGlobls(sim)$mapName, mapStack, envir=.GlobalEnv)

#      simPlot(mapStack, col = .cols[5:1])
    }

  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1,"eventType",with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE] ,"\'", sep=""))
  }
  return(sim)
}
