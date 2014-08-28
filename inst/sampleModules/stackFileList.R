###
### MODULE: stackFileList
###
### DESCRIPTION: stack the RasterLayers loaded from files inta a single object
###               - DEM, forestAge, forestCover, habitatQuality, percentPine
###

### load any required packages
### (use `loadPackages` or similar)
pkgs <- list("SpaDES", "RColorBrewer")
loadPackages(pkgs)
rm(pkgs)

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

      mapStack <- stack(mget(unlist(objectNames), envir=.GlobalEnv))
      names(mapStack) <- objectNames
      setColors(mapStack) <- list(DEM=terrain.colors(100),
                                  forestAge=brewer.pal(9,"BuGn"),
                                  forestCover=brewer.pal(8,"BrBG"),
                                  habitatQuality=brewer.pal(8,"Spectral"),
                                  percentPine=brewer.pal(9,"Greens"))

      assign(simGlobals(sim)$.stackName, mapStack, envir=.GlobalEnv)
    }

  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1,"eventType",with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE] ,"\'", sep=""))
  }
  return(invisible(sim))
}
