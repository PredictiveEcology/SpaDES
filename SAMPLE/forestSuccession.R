###
### MODULE: forestSuccession
###
### DESCRIPTION: a basic forest succession module
###               - land cover classes (2005) for Canada
###               - etc.
###

### load any required packages
### (use `loadPackages` or similar)
pkgs <- list("SpaDES", "raster", "RColorBrewer")
loadPackages(pkgs)
rm(pkgs)

### event functions
doEvent.forestSuccession <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {

    ### check for module dependencies:
    ### (use NULL if no dependencies exist)
    depends <- NULL

    ### check for object dependencies:
    ### (use `checkObject` or similar)


    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(sim, depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), "fireSuccession", "init")
    } else {
        # do stuff for this event
        sim <- forestSuccessionSuccession(sim)

        # schedule the next event
#        sim <- scheduleEvent(sim, 0.5, "forestSuccession", "succession")
        sim <- scheduleEvent(sim, simParams(sim)$forestSuccession$startTime,
                             "forestSuccession", "succession")
        sim <- scheduleEvent(sim, simParams(sim)$forestSuccession$.saveInterval,
                             "forestSuccession", "save")
        sim <- scheduleEvent(sim, simParams(sim)$forestSuccession$.plotInitialTime,
                             "forestSuccession", "plot.init")

    }
  } else if (eventType=="succession") {
    # do stuff for this event
    sim <- forestSuccessionSuccession(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) +
                           simParams(sim)$forestSuccession$returnInterval,
                         "forestSuccession", "succession")
#    sim <- scheduleEvent(sim, simCurrentTime(sim)+1.0, "forestSuccession", "succession")
  } else if (eventType=="plot.init") {
    # do stuff for this event
    #setColors(vegMap) <- vegMapColors[minValue(vegMap):maxValue(vegMap)]
    Plot(vegMap)
    Plot(trajMap)
#     grid.rect(just="topright", y=1.05, width=0.3, height=0.1, gp=gpar(fill="white", col="white"))
#     grid.text(paste("vegMap: Time",simCurrentTime(sim)),y=1.05)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim)+ simParams(sim)$forestSuccession$.plotInterval,
                         "forestSuccession", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(vegMap)
    if(simCurrentTime(sim)<=3) {
      seekViewport("vegMap")
      grid.text(x=1.1,y=((minValue(vegMap)-1):(maxValue(vegMap)-1)/(maxValue(vegMap)-1)-0.5)/2.2+0.5,just="left",
                as.character(lcc05VegReclass[minValue(vegMap):maxValue(vegMap),3]),
                gp=gpar(cex=0.5))
    }
    Plot(trajMap)
    #     grid.rect(y=1.05, width=0.3, height=0.1, gp=gpar(fill="white", col="white"))
#     grid.text(paste("vegMap: Time",simCurrentTime(sim)),y=1.05)
    dev(5); hist(getValues(vegMap)); dev(4)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim)+ simParams(sim)$forestSuccession$.plotInterval,
                         "forestSuccession", "plot")
  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1,"eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE], "\'", sep=""))
  }
  return(invisible(sim))
}

forestSuccessionInit <- function(sim) {

  simModulesLoaded(sim) <- append(simModulesLoaded(sim), "forestSuccession")

  return(invisible(sim))
}

forestSuccessionSuccession <- function(sim) {

  ageMap.v <- round(getValues(ageMap))+1
  trajMap.v <- getValues(trajMap)
    vegMap.v <- trajObj[cbind(ageMap.v,trajMap.v)]
#    vegMap <- raster(ageMap)
    vegMap <- RasterLayerNamed(setValues(vegMap,vegMap.v),name="vegMap")
  setColors(vegMap, n=12 ) <- vegMapColors

    assign("vegMap", vegMap, envir=.GlobalEnv)
  fireSpreadProb <<- RasterLayerNamed(reclassify(x=vegMap,
                               rcl=cbind(1:11,
                                         c(0.22,0.22,0.21,0.15,0.15,0.18,0.1,0.1,0,0,0))),
                               name="fireSpreadProb")



    #vegMap[indStatics] <<- valsStatics

    return(invisible(sim))
}

