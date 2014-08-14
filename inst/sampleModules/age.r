################################################
###
### AGE MODULE
### - change the age of cells
###
###############################################
### load any required packages
### (use `loadPackages` or similar)
pkgs <- list("raster", "RColorBrewer")
loadPackages(pkgs)

doEvent.age <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {

    ### check for module dependencies:
    ### (use or NULL if no dependencies exist)
    depends <- NULL

    ### check for object dependencies:
    ### (use `checkObject` or similar)
    checkObject(simParams(sim)$globals$mapName, layer="habitatQuality")

    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(deparse(sim), depends)) {
        sim <- scheduleEvent(sim, simCurrentTime(sim), "age", "init")
    } else {
        # do stuff for this event
        sim <- ageInit(sim)

        # schedule the next event
        sim <- scheduleEvent(sim, 0.6, "age", "age")
    }
  } else if (eventType=="age") {
      # do stuff for this event
      sim <- ageAge(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim)+1.0, "age", "age")
  } else if (eventType=="plot") {
    # do stuff for this event
    sim <- agePlot(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim)+1.0, "age", "plot")
  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE], "\'", sep=""))
  }
  return(sim)
}

ageInit <- function(sim) {
#  beginCluster()
  ageMap <<- projectRaster(raster("C:/shared/data/shared/age/age.asc"), to=vegMap)
#  endCluster()

#  assign(x=get(simParams(sim)$age$rasterLayerName),
#         value=raster(simParams(sim)$age$inputFile),
#         envir=.GlobalEnv)

 # last thing to do is add module name to the loaded list
  simModulesLoaded(sim) <- append(simModulesLoaded(sim), "age")

  return(sim)
}

ageAge <- function(sim) {
#    assign(x=get(simParams(sim)$age$rasterStackName),
#           value=agingFunction(get(simParams(sim)$age$rasterStackName)),
#           envir=.GlobalEnv)
    ageMap[] <<- pmin(200,ageMap[] + 1)
#    ageMap[1] <<- 0
    return(sim)
}

agePlot <- function(sim) {
  simPlot(ageMap)

  return(sim)
}

