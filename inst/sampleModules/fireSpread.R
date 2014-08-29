###
### MODULE: fireSpread
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
doEvent.fireSpread <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for module dependencies:
    ### (use NULL if no dependencies exist)
    depends <- NULL

    ### check for object dependencies:
    ### (use `checkObject` or similar)
    checkObject(simGlobals(sim)$.stackName, layer="habitatQuality")

    if (!exists(simGlobals(sim)$burnStats, envir=.GlobalEnv)) {
      assign(simGlobals(sim)$burnStats, numeric(), envir=.GlobalEnv)
    } else {
      npix <- get(simGlobals(sim)$burnStats, envir=.GlobalEnv)
      stopifnot("numeric" %in% is(npix), "vector" %in% is(npix))
      if (length(npix)>0) {
        message(paste0("Object `", simGlobals(sim)$burnStats, "` already exists and will be overwritten."))
      }
    }

    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(sim, depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), "fireSpread", "init")
    } else {
      # do stuff for this event
      sim <- fireSpreadInit(sim)


      # schedule the next event
      sim <- scheduleEvent(sim, simParams(sim)$fireSpread$startTime, "fireSpread", "burn")
      sim <- scheduleEvent(sim, simParams(sim)$fireSpread$.saveInterval, "fireSpread", "save")
      sim <- scheduleEvent(sim, simParams(sim)$fireSpread$.plotInitialTime, "fireSpread", "plot.init")
    }
  } else if (eventType=="burn") {
    # do stuff for this event
    sim <- fireSpreadBurn(sim)

    # schedule the next events
    sim <- scheduleEvent(sim, simCurrentTime(sim), "fireSpread", "stats") # do stats immediately following burn
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$returnInterval, "fireSpread", "burn")
  } else if (eventType=="stats") {
    # do stuff for this event
    sim <- fireSpreadStats(sim)

    # schedule the next event
    ## stats scheduling done by burn event
  } else if (eventType=="plot.init") {
    # do stuff for this event
    maps <- get(simGlobals(sim)$.stackName, envir=.GlobalEnv)
    setColors(maps) <- list(DEM=terrain.colors(100),
                                forestAge=brewer.pal(9,"BuGn"),
                                forestCover=brewer.pal(8,"BrBG"),
                                habitatQuality=brewer.pal(8,"Spectral"),
                                percentPine=brewer.pal(9,"Greens"),
                                Fires=c("#FFFFFF", rev(heat.colors(9)))
                            )
    Plot(maps)
    assign(simGlobals(sim)$.stackName, maps, envir=.GlobalEnv)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$.plotInterval, "fireSpread", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(get(simGlobals(sim)$.stackName)$Fires, add=TRUE)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$.plotInterval, "fireSpread", "plot")
  } else if (eventType=="save") {
    # do stuff for this event
    saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$.saveInterval, "fireSpread", "save")
  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE],"\'", sep=""))
  }
  return(invisible(sim))
}

fireSpreadInit <- function(sim) {
  landscapes <- get(simGlobals(sim)$.stackName, envir=.GlobalEnv)

  ### create burn map that tracks fire locations over time
  Fires <- raster(extent(landscapes), ncol=ncol(landscapes), nrow=nrow(landscapes), vals=0)
  names(Fires) <- "Fires"
  setColors(Fires,10) <- c("#FFFFFF", rev(heat.colors(9)))
  Fires <- setValues(Fires, 0)

  # add Fires map to global$.stackName stack
  assign(simGlobals(sim)$.stackName, addLayer(landscapes,Fires), envir=.GlobalEnv)

  # last thing to do is add module name to the loaded list
  simModulesLoaded(sim) <- append(simModulesLoaded(sim), "fireSpread")

  return(invisible(sim))
}


fireSpreadBurn <- function(sim) {
  landscapes <- get(simGlobals(sim)$.stackName, envir=.GlobalEnv)

  Fires <- spread(landscapes[[1]],
                   loci=as.integer(sample(1:ncell(landscapes), simParams(sim)$fireSpread$nFires)),
                   spreadProb=simParams(sim)$fireSpread$spreadprob,
                   persistance=simParams(sim)$fireSpread$persistprob,
                   mask=NULL,
                   maxSize=1e8,
                   directions=8,
                   iterations=simParams(sim)$fireSpread$its,
                   plot.it=FALSE,
                   mapID=TRUE)
  names(Fires) <- "Fires"
  setColors(Fires,10) <- c("#FFFFFF", rev(heat.colors(9)))
  landscapes$Fires <- Fires

  assign(simGlobals(sim)$.stackName, landscapes, envir=.GlobalEnv)

  return(invisible(sim))
}

fireSpreadStats <- function(sim) {
  npix <- get(simGlobals(sim)$burnStats, envir=.GlobalEnv)

  landscapes <- get(simGlobals(sim)$.stackName, envir=.GlobalEnv)

  assign("nPixelsBurned", c(npix, length(which(values(landscapes$Fires)>0))), envir=.GlobalEnv)

  return(invisible(sim))
}

fireSpreadStats <- function(sim) {
  npix <- get(simGlobals(sim)$burnStats, envir=.GlobalEnv)

  landscapes <- get(simGlobals(sim)$.stackName, envir=.GlobalEnv)

  assign("nPixelsBurned", c(npix, length(which(values(landscapes$Fires)>0))), envir=.GlobalEnv)

  return(sim)
}
