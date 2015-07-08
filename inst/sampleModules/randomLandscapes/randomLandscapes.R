stopifnot(packageVersion("SpaDES") >= "0.99.0")

defineModule(sim, list(
  name="randomLandscapes",
  description="Generate RasterStack of random maps representative of a forest landscape (DEM, forestAge, forestCover, habitatQuality, percentPine). Requires a global simulation parameter `stackName` be set.",
  keywords=c("random map", "random landscape"),
  authors=c(person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre"))),
  version=numeric_version("1.0.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit="year",
  citation=list(),
  reqdPkgs=list("raster", "RColorBrewer", "tkrplot", "RandomFields"),
  parameters=rbind(
    defineParameter("stackName", "character", "randomLandscape", NA, NA, "name of the RasterStack"),
    defineParameter("nx", "numeric", 100L, NA, NA, "size of map (number of pixels) in the x dimension"),
    defineParameter("ny", "numeric", 100L, NA, NA, "size of map (number of pixels) in the y dimension"),
    defineParameter("inRAM", "logical", FALSE, NA, NA, "should the raster be stored in memory?"),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA, "time to schedule first plot event"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, "time to schedule first save event"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, "time interval between save events")),
  inputObjects=data.frame(objectName=character(),
                          objectClass=character(),
                          other=character(), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=globals(sim)$stackName,
                           objectClass="RasterStack",
                           other=NA_character_, stringsAsFactors=FALSE)
))

## event types
doEvent.randomLandscapes <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    # do stuff for this event
    sim <- randomLandscapesInit(sim)

    # schedule the next events
    sim <- scheduleEvent(sim, params(sim)$randomLandscapes$.plotInitialTime,
                         "randomLandscapes", "plot")
    sim <- scheduleEvent(sim, params(sim)$randomLandscapes$.saveInitialTime,
                         "randomLandscapes", "save")

  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(sim[[globals(sim)$stackName]])

  } else if (eventType=="save") {
    # do stuff for this event
    sim <- saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$randomLandscapes$.saveInterval,
                         "randomLandscapes", "save")

  } else {
    warning(paste("Undefined event type: \'", events(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", events(sim)[1, "moduleName", with=FALSE] , "\'", sep=""))
  }
  return(invisible(sim))
}

## event functions
randomLandscapesInit <- function(sim) {
  if (is.null(params(sim)$randomLandscapes$inRAM)) {
    inMemory <- FALSE
  } else {
    inMemory <- params(sim)$randomLandscapes$inRAM
  }
  # Give dimensions of dummy raster
  nx <- params(sim)$randomLandscapes$nx
  ny <- params(sim)$randomLandscapes$ny
  template <- raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn=-ny/2, ymx=ny/2)
  speedup <- max(1, nx/5e2)

  # Make dummy maps for testing of models
  DEM <- round(gaussMap(template, scale=300, var=0.03, speedup=speedup, inMemory=inMemory), 1)*1000
  forestAge <- round(gaussMap(template, scale=10, var=0.1, speedup=speedup, inMemory=inMemory), 1)*20
  percentPine <- round(gaussMap(template, scale=50, var=1, speedup=speedup, inMemory=inMemory), 1)

  # Scale them as needed
  forestAge <- forestAge/maxValue(forestAge)*100
  percentPine <- percentPine/maxValue(percentPine)*100

  # Make layers that are derived from other layers
  habitatQuality <- (DEM+10 + (forestAge+2.5)*10)/100
  habitatQuality <- habitatQuality/maxValue(habitatQuality)

  # Stack them into a single stack and assign to global env
  mapStack <- stack(DEM, forestAge, habitatQuality, percentPine)
  names(mapStack) <- c("DEM", "forestAge", "habitatQuality", "percentPine")

  setColors(mapStack) <- list(DEM=terrain.colors(100),
                              forestAge=brewer.pal(9,"BuGn"),
                              habitatQuality=brewer.pal(8,"Spectral"),
                              percentPine=brewer.pal(9,"Greens"))
  sim[[globals(sim)$stackName]] <- mapStack
  return(invisible(sim))
}
