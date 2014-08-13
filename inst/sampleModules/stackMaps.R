################################################
###
### stackMaps MODULE
###
###############################################

doEvent.stackMaps <- function(sim, eventTime, eventType, debug=FALSE) {
    if (eventType=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends <- "NONE" # list module names here

        if (reloadModuleLater(sim, depends)) {
            sim <- scheduleEvent(sim, currentTime(sim), "stackMaps", "init")
        } else {
            landscapes <<- stack(DEM, forestCover, forestAge, percentPine,
                                 habitatQuality)
            names(landscapes) <<- ("DEM", "forestCover", "forestAge", "percentPine",
                                  "habitatQuality")
        }

    } else {
      warning(paste("Undefined event type: \'", simEvents(sim)[1,"eventType",with=FALSE],
                    "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE] ,"\'", sep=""))
    }
    return(sim)
}
