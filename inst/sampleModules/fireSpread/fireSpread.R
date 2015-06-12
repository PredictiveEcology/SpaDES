stopifnot(packageVersion("SpaDES") >= "0.99.0")

defineModule(sim, list(
  name="fireSpread",
  description="Simulate fire ignition and spread on a landscape, where spread probability varies according to percent pine. Fire size statistics are collected immediately after each burn event. Requires a global simulation parameter `stackName` be set.",
  keywords=c("fire", "percolation model", "spread algorithm"),
  authors=c(person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person("Steve", "Cumming", email="Steve.Cumming@sbf.ulaval.ca", role=c("aut"))),
  version=numeric_version("1.0.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timestepUnit="year",
  citation=list(),
  reqdPkgs=list("methods", "raster", "RColorBrewer"),
  parameters=rbind(
    defineParameter("nFires", "numeric", 10L, NA, NA),
    defineParameter("its", "numeric", 1e6, NA, NA),
    defineParameter("persistprob", "numeric", 0.00, 0, 1),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA),
    defineParameter("spreadprob", "numeric", 0.225, 0, 1),
    defineParameter("startTime", "numeric", 1.0, 0, NA),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA),
    defineParameter(".plotInterval", "numeric", 1, NA, NA),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA)),
  inputObjects=data.frame(objectName=c(simGlobals(sim)$stackName,
                                       simGlobals(sim)$burnStats),
                          objectClass=c("RasterStack", "numeric"),
                          other=c(NA_character_, NA_character_),
                          stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c(simGlobals(sim)$stackName,
                                        simGlobals(sim)$burnStats),
                           objectClass=c("RasterStack", "numeric"),
                           other=c(NA_character_, NA_character_),
                           stringsAsFactors=FALSE)
))

## event types
doEvent.fireSpread <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more object dependencies:
    ### (use `checkObject` or similar)
    checkObject(sim, simGlobals(sim)$stackName, layer="habitatQuality")
    #    checkObject(sim, simGlobals(sim)$burnStats)

    if (is.null(sim[[simGlobals(sim)$burnStats]])) {
      sim[[simGlobals(sim)$burnStats]] <- numeric()
    } else {
      npix <- sim[[(simGlobals(sim)$burnStats)]]
      stopifnot("numeric" %in% is(npix), "vector" %in% is(npix))
    }

    # do stuff for this event
    sim <- fireSpreadInit(sim)

    # schedule the next events
    sim <- scheduleEvent(sim, simParams(sim)$fireSpread$startTime, "fireSpread", "burn")
    sim <- scheduleEvent(sim, simParams(sim)$fireSpread$.saveInterval, "fireSpread", "save")
    sim <- scheduleEvent(sim, simParams(sim)$fireSpread$.plotInitialTime, "fireSpread", "plot.init")

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
    setColors(sim[[simGlobals(sim)$stackName]]) <- list(DEM=terrain.colors(100),
                                forestAge=brewer.pal(9,"BuGn"),
                                forestCover=brewer.pal(8,"BrBG"),
                                habitatQuality=brewer.pal(8,"Spectral"),
                                percentPine=brewer.pal(9,"Greens"),
                                Fires=c("white", rev(heat.colors(9)))
                            )

    Plot(sim[[simGlobals(sim)$stackName]], new=TRUE)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpread$.plotInterval, "fireSpread", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(sim[[simGlobals(sim)$stackName]]$Fires, new=FALSE)

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

## event functions
fireSpreadInit <- function(sim) {
  landscapes <- sim[[simGlobals(sim)$stackName]]

  ### create burn map that tracks fire locations over time
  Fires <- raster(extent(landscapes), ncol=ncol(landscapes), nrow=nrow(landscapes), vals=0)
  names(Fires) <- "Fires"
  setColors(Fires) <- c("white", rev(heat.colors(9)))
  Fires <- setValues(Fires, 0)

  # add Fires map to global$stackName stack
  landscapes$Fires <- Fires
  sim[[simGlobals(sim)$stackName]] <- landscapes

  return(invisible(sim))
}


fireSpreadBurn <- function(sim) {
  landscapes <- sim[[simGlobals(sim)$stackName]]

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
  setColors(Fires) <- c("white", rev(heat.colors(9)))
  landscapes$Fires <- Fires

  sim[[simGlobals(sim)$stackName]] <- landscapes

  return(invisible(sim))
}

fireSpreadStats <- function(sim) {
  npix <- sim[[simGlobals(sim)$burnStats]]

  landscapes <- sim[[simGlobals(sim)$stackName]]

  sim[[simGlobals(sim)$burnStats]] <- c(npix, length(which(values(landscapes$Fires)>0)))

  return(invisible(sim))
}
