################################################
###
### fireSpread MODULE
###
###############################################

doEvent.fireSpread <- function(sim, eventTime, eventType, debug=FALSE) {
    if (eventType=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends <- "NONE"  # list package names here

        if (reloadModuleLater(sim, depends)) {
            sim <- scheduleEvent(sim, simCurrentTime(sim), "fireSpread", "init")
        } else {
            # do stuff for this event
            sim <- fireSpreadInit(sim)

            # schedule the next event
            sim <- scheduleEvent(sim, simParams(sim)$fireSpread$startTime, "fireSpread", "burn")
            sim <- scheduleEvent(sim, simParams(sim)$fireSpread$.plotInitialTime, "fireSpread", "plot.init")
            sim <- scheduleEvent(sim, simParams(sim)$fireSpread$.saveInterval, "fireSpread", "save")
        }
    } else if (eventType=="burn") {
        # do stuff for this event
        sim <- fireSpreadBurn(sim)

        # schedule the next event
        sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$interval, "fireSpread", "burn")
    } else if (eventType=="plot.init") {
      # do stuff for this event

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$.plotInterval, "fireSpread", "plot")
    } else if (eventType=="plot") {
      # do stuff for this event
      simPlot(Fires, add=TRUE, on.which.to.plot = 6, col=.cols[[2]], add.legend=TRUE, delete.previous = FALSE)

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$.plotInterval, "fireSpread", "plot")
    } else if (eventType=="save") {
      # do stuff for this event
      simSave(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$.saveInterval, "fireSpread", "save")
    } else {
      warning(paste("Undefined event type: \'",simEvents(sim)[1, "eventType", with=FALSE],
                    "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE],"\'", sep=""))
    }
    return(sim)
}

fireSpreadInit <- function(sim) {
    ### load any required packages
    pkgs <- list("raster", "RColorBrewer") # list required packages here
    loadPackages(pkgs)

    landscapes <- get(simParams(sim)$globals$mapName, envir=.GlobalEnv)

    ### create burn map that tracks fire locations over time
    Fires <<- raster(extent(landscapes), ncol=ncol(landscapes), nrow=nrow(landscapes), vals=0)
    names(Fires) <<- "Fires"
    Fires[] <<- 0


    # last thing to do is add module name to the loaded list
    simModulesLoaded(sim) <- append(simModulesLoaded(sim), "fireSpread")

    return(sim)
}


fireSpreadBurn <- function(sim) {
    # random fire start locations, but could be based on hab:
    landscapes <- get(simParams(sim)$globals$mapName, envir=.GlobalEnv)

    Fires <<- spread(landscapes[[1]],
                  loci=as.integer(sample(1:ncell(landscapes), simParams(sim)$fireSpread$nFires)),
                  #spreadProb=0.225,
                  spreadProb=simParams(sim)$fireSpread$spreadprob,
                  persistance=simParams(sim)$fireSpread$persistprob,
                  mapFireID=TRUE,
                  mask=NULL,
                  maxSize=1e8,
                  directions=8,
                  iterations=simParams(sim)$fireSpread$its,
                  plot.it=FALSE,
                  mapID=TRUE)
    names(Fires) <<- "Fires" # do we need this again here??


    return(sim)
}
