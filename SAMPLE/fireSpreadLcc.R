###
### MODULE: fireSpreadLcc
###
### DESCRIPTION: simulate fire ignition and spread on a landscape
###               - spread probability varies according to percent pine
###               - stats re: fire size collected immediately after each burn event
###

### load any required packages
### (use `loadPackages` or similar)
pkgs <- list("SpaDES", "raster", "RColorBrewer")
loadPackages(pkgs)
rm(pkgs)

### event functions
doEvent.fireSpreadLcc <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for module dependencies:
    ### (use NULL if no dependencies exist)
    depends <- "forestSuccession"

    ### check for object dependencies:
    ### (use `checkObject` or similar)
    checkObject(name="fireSpreadProb")

    if (!exists(simGlobals(sim)$burnStats, envir=.GlobalEnv)) {
      assign(simGlobals(sim)$burnStats, numeric(), envir=.GlobalEnv)
    } else {
      npix <- get(simGlobals(sim)$burnStats, envir=.GlobalEnv)
      stopifnot("numeric" %in% is(npix), "vector" %in% is(npix))
      if (length(npix)>0) {
        message(paste0("Object `", simGlobals(sim)$burnStats, "` already exists and will be appended."))
      }
    }

    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(sim, depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), "fireSpreadLcc", "init")
    } else {
      # do stuff for this event
      sim <- fireSpreadLccInit(sim)


      # schedule the next event
      sim <- scheduleEvent(sim, simParams(sim)$fireSpreadLcc$startTime, "fireSpreadLcc", "burn")
      sim <- scheduleEvent(sim, simParams(sim)$fireSpreadLcc$.saveInterval, "fireSpreadLcc", "save")
      sim <- scheduleEvent(sim, simParams(sim)$fireSpreadLcc$.plotInitialTime, "fireSpreadLcc", "plot.init")
    }
  } else if (eventType=="burn") {
    # do stuff for this event
    sim <- fireSpreadLccBurn(sim)

    # schedule the next events
    sim <- scheduleEvent(sim, simCurrentTime(sim), "fireSpreadLcc", "stats") # do stats immediately following burn
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpreadLcc$returnInterval, "fireSpreadLcc", "burn")
  } else if (eventType=="stats") {
    # do stuff for this event
    sim <- fireSpreadLccStats(sim)

    # schedule the next event
    ## stats scheduling done by burn event
  } else if (eventType=="plot.init") {
    # do stuff for this event
    #maps <- get(simGlobals(sim)$.stackName, envir=.GlobalEnv)
    #setColors(maps) <- list(DEM=terrain.colors(100),
    #                            forestAge=brewer.pal(9,"BuGn"),
    #                            forestCover=brewer.pal(8,"BrBG"),
    #                            habitatQuality=brewer.pal(8,"Spectral"),
    #                            percentPine=brewer.pal(9,"Greens"),
    #                            Fires=c("#FFFFFF", rev(heat.colors(9)))
    #                        )
    #Plot(Fires, legendRange=c(0,simParams(sim)$fireSpreadLcc$nFires))
    #assign(simGlobals(sim)$.stackName, maps, envir=.GlobalEnv)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpreadLcc$.plotInterval, "fireSpreadLcc", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(Fires, legendRange=c(0,simParams(sim)$fireSpreadLcc$nFires))

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpreadLcc$.plotInterval, "fireSpreadLcc", "plot")
  } else if (eventType=="save") {
    # do stuff for this event
    saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpreadLcc$.saveInterval, "fireSpreadLcc", "save")
  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE],"\'", sep=""))
  }
  return(invisible(sim))
}

fireSpreadLccInit <- function(sim) {
  #landscapes <- get(simGlobals(sim)$.stackName, envir=.GlobalEnv)

  ### create burn map that tracks fire locations over time
  Fires <- raster(extent(fireSpreadProb), ncol=ncol(fireSpreadProb),
                  nrow=nrow(fireSpreadProb), vals=0)
  name(Fires) <- "Fires"
  setColors(Fires,n=10) <- c("#FFFFFF", rev(heat.colors(9)))
  Fires <<- setValues(Fires, 0)

  # add Fires map to global$.stackName stack
  #  assign(simGlobals(sim)$.stackName, addLayer(landscapes,Fires), envir=.GlobalEnv)
  assign("Fires", Fires, envir=.GlobalEnv)

  # last thing to do is add module name to the loaded list
  simModulesLoaded(sim) <- append(simModulesLoaded(sim), "fireSpreadLcc")

  return(invisible(sim))
}


fireSpreadLccBurn <- function(sim) {
 # landscapes <- get(simGlobals(sim)$.stackName, envir=.GlobalEnv)

  Fires <- spread(fireSpreadProb,
                   loci=as.integer(sample(1:ncell(fireSpreadProb), simParams(sim)$fireSpreadLcc$nFires)),
                   spreadProb=fireSpreadProb,
                   persistance=simParams(sim)$fireSpreadLcc$persistprob,
                   mask=NULL,
                   maxSize=1e8,
                   directions=8,
                   iterations=simParams(sim)$fireSpreadLcc$its,
                   plot.it=FALSE,
                   mapID=TRUE)
  names(Fires) <- "Fires"
  setColors(Fires,n=10) <- c("#FFFFFF", rev(heat.colors(9)))
#  landscapes$Fires <- Fires

  assign("Fires", Fires, envir=.GlobalEnv)

  return(invisible(sim))
}

fireSpreadLccStats <- function(sim) {
  npix <- get(simGlobals(sim)$burnStats, envir=.GlobalEnv)

  #landscapes <- get(simGlobals(sim)$.stackName, envir=.GlobalEnv)

  assign("nPixelsBurned", c(npix, length(which(values(Fires)>0))), envir=.GlobalEnv)

  return(invisible(sim))
}

