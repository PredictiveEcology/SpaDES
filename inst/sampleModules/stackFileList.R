################################################
###
### stackFileList MODULE
###
###############################################

doEvent.stackFileList <- function(sim, eventTime, eventType, debug=FALSE) {
    if (eventType=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later if not loaded
        depends <- "NONE" # list module names here

        # Check object dependencies... stack will only work on RasterLayers
        # To Do


        if (reloadModuleLater(sim, depends)) {
            sim <- scheduleEvent(sim, currentTime(sim), "stackFileList", "init")
        } else {

          objectNames <- simObjectsLoaded(sim)

          mapStack <- stack(mget(objectNames, envir=.GlobalEnv))
          names(mapStack) <- objectNames

          assign(simParams(sim)$globals$mapName, mapStack, envir=.GlobalEnv)

          simPlot(mapStack, col = .cols[5:1])

        }

    } else {
      warning(paste("Undefined event type: \'", simEvents(sim)[1,"eventType",with=FALSE],
                    "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE] ,"\'", sep=""))
    }
    return(sim)
}

