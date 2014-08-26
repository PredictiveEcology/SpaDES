###
### MODULE: forestAge
###
### DESCRIPTION: a basic forest age module
###               - land cover classes (2005) for Canada
###               - etc.
###

### load any required packages
### (use `loadPackages`, or `library` directly)
pkgs <- list("SpaDES", "raster", "RColorBrewer")
loadPackages(pkgs)
rm(pkgs)

### event functions
doEvent.forestAge <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {

    ### check for module dependencies:
    ### (use or NULL if no dependencies exist)
    depends <- NULL

    ### check for object dependencies:
    ### (use `checkObject` or similar)
    checkObject(simGlobals(sim)$mapName, layer="habitatQuality")

    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(deparse(sim), depends)) {
        sim <- scheduleEvent(sim, simCurrentTime(sim), "forestAge", "init")
    } else {
        # do stuff for this event
        sim <- forestAgeInit(sim)

        # schedule the next event
        sim <- scheduleEvent(sim, 0.6, "forestAge", "age")
    }
  } else if (eventType=="age") {
      # do stuff for this event
      sim <- forestAgeAge(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim)+1.0, "forestAge", "age")
  } else if (eventType=="plot") {
    # do stuff for this event
    sim <- forestAgePlot(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim)+1.0, "forestAge", "plot")
  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE], "\'", sep=""))
  }
  return(sim)
}

forestAgeInit <- function(sim) {
#  beginCluster()
  ageMap <<- projectRaster(raster("C:/shared/data/shared/age/age.asc"), to=vegMap)
#  endCluster()

#  assign(x=get(simParams(sim)$age$rasterLayerName),
#         value=raster(simParams(sim)$age$inputFile),
#         envir=.GlobalEnv)

 # last thing to do is add module name to the loaded list
  simModulesLoaded(sim) <- append(simModulesLoaded(sim), "forestAge")

  return(sim)
}

forestAgeAge <- function(sim) {
#    assign(x=get(simParams(sim)$age$rasterStackName),
#           value=agingFunction(get(simParams(sim)$age$rasterStackName)),
#           envir=.GlobalEnv)
    ageMap[] <<- setValues(ageMap, pmin(200, getValues(ageMap) + 1))
#    ageMap[1] <<- 0
    return(sim)
}

forestAgePlot <- function(sim) {
  simPlot(ageMap)

  return(sim)
}

