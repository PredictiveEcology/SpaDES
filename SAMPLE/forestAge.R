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
    depends <- "forestSuccession"

    ### check for object dependencies:
    ### (use `checkObject` or similar)

    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(sim, depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), "fireAge", "init")
    } else {
        # do stuff for this event
        sim <- forestAgeInit(sim)

        # schedule the next event
        sim <- scheduleEvent(sim, simParams(sim)$forestAge$startTime, "forestAge", "age")
        sim <- scheduleEvent(sim, simParams(sim)$forestAge$.saveInterval, "forestAge", "save")
        sim <- scheduleEvent(sim, simParams(sim)$forestAge$.plotInitialTime, "forestAge", "plot.init")
#        sim <- scheduleEvent(sim, 0.6, "forestAge", "age")
    }
  } else if (eventType=="age") {
      # do stuff for this event
      sim <- forestAgeAge(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) +
                             simParams(sim)$forestAge$returnInterval,
                           "forestAge", "age")
  } else if (eventType=="plot.init") {
    # do stuff for this event
    Plot(ageMap, legendRange=c(0,200))
    #grid.text(paste0("age, time=",simCurrentTime(sim)),y=1.05)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim)+ simParams(sim)$forestAge$.plotInterval,
                         "forestAge", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(ageMap, legendRange=c(0,200))
    #grid.text(paste0("age, time=",simCurrentTime(sim)),y=1.05)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim)+ simParams(sim)$forestAge$.plotInterval,
                         "forestAge", "plot")
  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE], "\'", sep=""))
  }
  return(invisible(sim))
}

forestAgeInit <- function(sim) {
#  beginCluster()
#  ageMap <- projectRaster(age, to=lcc05.cr, method="ngb")
  setColors(ageMap,n=201) <- colorRampPalette(c("LightGreen","DarkGreen"))(50)
  name(ageMap) <- "ageMap"
  assign("ageMap", ageMap, envir=.GlobalEnv)
  #  endCluster()

#  assign(x=get(simParams(sim)$age$rasterLayerName),
#         value=raster(simParams(sim)$age$inputFile),
#         envir=.GlobalEnv)

 # last thing to do is add module name to the loaded list
  simModulesLoaded(sim) <- append(simModulesLoaded(sim), "forestAge")

  return(invisible(sim))
}

forestAgeAge <- function(sim) {
#    assign(x=get(simParams(sim)$age$rasterStackName),
#           value=agingFunction(get(simParams(sim)$age$rasterStackName)),
#           envir=.GlobalEnv)


    ageMap <- setValues(ageMap, pmin(200, getValues(ageMap)+1))
    ageMap[Fires>0] <- 1
    name(ageMap) <- "ageMap"
    setColors(ageMap,n=201) <- colorRampPalette(c("LightGreen","darkgreen"))(50)
    assign("ageMap", ageMap, envir=.GlobalEnv)
#    ageMap[1] <<- 0
    return(invisible(sim))
}

