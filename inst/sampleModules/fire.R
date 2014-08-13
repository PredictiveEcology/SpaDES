################################################
###
### A SIMPLE FIRE MODULE
###
###############################################

doEvent.fire <- function(sim, eventTime, eventType, debug=FALSE) {
    if (eventType=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends <- "NONE"#c("habitat") # list package names here

        if (reloadModuleLater(sim, depends)) {
            sim <- scheduleEvent(sim, simCurrentTime(sim), "fire", "init")
        } else {
            # do stuff for this event
            sim <- fireInit(sim)

            # schedule the next event
            sim <- scheduleEvent(sim, simParams(sim)$fire$startTime, "fire", "burn")
            sim <- scheduleEvent(sim, simParams(sim)$fire$plotInitialTime, "fire", "plot.init")
            sim <- scheduleEvent(sim, simParams(sim)$fire$saveInterval, "fire", "save")
        }
    } else if (eventType=="burn") {
        # do stuff for this event
        sim <- fireBurn(sim)

        # schedule the next event
        sim <- scheduleEvent(sim, simCurrentTime(sim)+simParams(sim)$fire$interval, "fire", "burn")
    } else if (eventType=="plot.init") {
      # do stuff for this event
      map <<- stack(DEM, forestAge, forestCover, habitatQuality, percentPine)
      names(map) <<- c("DEM", "forestAge", "forestCover", "habitatQuality", "percentPine")

      simPlot(stack(map, Fires), add=FALSE, col=.cols[c(2:5,3,2)], add.legend=TRUE)

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fire$plotInterval, "fire", "plot")
    } else if (eventType=="plot") {
      # do stuff for this event
      simPlot(Fires, add=TRUE, on.which.to.plot = 6, col=.cols[[2]], add.legend=TRUE, delete.previous = FALSE)

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fire$plotInterval, "fire", "plot")
    } else if (eventType=="save") {
      # do stuff for this event
      simSave(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fire$saveInterval, "fire", "save")
    } else {
      warning(paste("Undefined event type: \'",simEvents(sim)[1,"eventType",with=FALSE],
                    "\' in module \'", simEvents(sim)[1,"moduleName",with=FALSE],"\'",sep=""))
    }
    return(sim)
}

fireInit <- function(sim) {
    ### load any required packages
    pkgs <- list("raster", "RColorBrewer") # list required packages here
    loadPackages(pkgs)

    ras <- get(simParams(sim)$fire$raster)
    ### create burn map that tracks fire locations over time
    Fires <<- raster(extent(ras), ncol=ncol(ras), nrow=nrow(ras), vals=0)
    names(Fires) <<- "Fires"
    Fires[] <<- 0


    # last thing to do is add module name to the loaded list
    simLoaded(sim) <- append(simLoaded(sim), "fire")

    return(sim)
}


fireBurn <- function(sim) {
    # random fire start locations, but could be based on hab:
    ras <- get(simParams(sim)$fire$raster)
    Fires <<- spread(ras,
                  loci=as.integer(sample(1:ncell(ras), simParams(sim)$fire$nFires)),
                  #spreadProb=0.225,
                  spreadProb=simParams(sim)$fire$spreadprob,
                  persistance=simParams(sim)$fire$persistprob,
                  mapFireID=TRUE,
                  mask=NULL,
                  maxSize=1e8,
                  directions=8,
                  iterations=simParams(sim)$fire$its,
                  plot.it=FALSE,
                  mapID=TRUE)
    names(Fires) <<- "Fires" # do we need this again here??


    return(sim)
}
