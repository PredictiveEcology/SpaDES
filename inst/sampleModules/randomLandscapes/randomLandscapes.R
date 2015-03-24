###
### Specify module (and dependencies) definitions:
###
### name:         randomLandscapes
###
### description:  Generate RasterStack of random maps representative of a forest landscape
###               (DEM, forestAge, habitatQuality, percentPine).
###               Requires a global simulation parameter `.stackName` be set.
###
### keywords:     random map; random landscape
###
### authors:      Alex M. Chubaty <Alexander.Chubaty@NRCan.gc.ca>
###               Eliot J. B. McIntire <Eliot.McIntire@NRCan.gc.ca>
###
### version:      0.2.0
###
### spatialExtent: NA
###
### timeframe:    NA
###
### timestep:     NA
###
### citation:     NA
###
### reqdPkgs:     raster; RColorBrewer; tkrplot; RandomFields
###
### inputObjects: NA
###
### outputObjects: objectName: simGlobals(sim)$.stackName
###                objectClass: RasterStack
###                other: NA
###
### randomLandscapes module metadata
defineModule(sim, list(
  name="randomLandscapes",
  description="Generate RasterStack of random maps representative of a forest landscape (DEM, forestAge, forestCover, habitatQuality, percentPine). Requires a global simulation parameter `.stackName` be set.",
  keywords=c("random map", "random landscape"),
  authors=c(person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre"))),
  version=numeric_version("0.2.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timestep=NA_real_,
  citation=list(),
  reqdPkgs=list("raster", "RColorBrewer", "tkrplot", "RandomFields"),
  inputObjects=data.frame(objectName=NA_character_, objectClass=NA_character_, other=list(NA), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=simGlobals(sim)$.stackName, objectClass="RasterStack", other=list(NA), stringsAsFactors=FALSE)
))

### event functions
doEvent.randomLandscapes <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    # do stuff for this event
    sim <- randomLandscapesInit(sim)

    # schedule the next events
    sim <- scheduleEvent(sim, simParams(sim)$randomLandscapes$.plotInitialTime, "randomLandscapes", "plot")
    sim <- scheduleEvent(sim, simParams(sim)$randomLandscapes$.saveInitialTime, "randomLandscapes", "save")

  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(getGlobal(simGlobals(sim)$.stackName))

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$randomLandscapes$.plotInterval,
                         "randomLandscapes", "plot")
  } else if (eventType=="save") {

    # do stuff for this event
    saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$randomLandscapes$.saveInterval,
                         "randomLandscapes", "save")

  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE] , "\'", sep=""))
  }
  return(invisible(sim))
}

randomLandscapesInit <- function(sim) {
  if (is.null(simParams(sim)$randomLandscapes$inRAM)) {
    inMemory <- FALSE
  } else {
    inMemory <- simParams(sim)$randomLandscapes$inRAM
  }
  # Give dimensions of dummy raster
  nx <- simParams(sim)$randomLandscapes$nx
  ny <- simParams(sim)$randomLandscapes$ny
  template <- raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn=-ny/2, ymx=ny/2)
  speedup <- max(1, nx/5e2)
  # Make dummy maps for testing of models
  DEM <- round(GaussMap(template, scale=300, var=0.03, speedup=speedup, inMemory=inMemory), 1)*1000
  forestAge <- round(GaussMap(template, scale=10, var=0.1, speedup=speedup, inMemory=inMemory), 1)*20
#  forestCover <- round(GaussMap(template, scale=50, var=1, speedup=speedup, inMemory=inMemory),2)*10
  percentPine <- round(GaussMap(template, scale=50, var=1, speedup=speedup, inMemory=inMemory),1)

  # Scale them as needed
  forestAge <- forestAge/maxValue(forestAge)*100
  percentPine <- percentPine/maxValue(percentPine)*100

  # Make layers that are derived from other layers
  habitatQuality <- (DEM+10 + (forestAge+2.5)*10)/100
  habitatQuality <- habitatQuality/maxValue(habitatQuality)

  # Stack them into a single stack and assign to global env
  mapStack <- stack(DEM, forestAge, habitatQuality, percentPine)
  names(mapStack)<-c("DEM", "forestAge", "habitatQuality", "percentPine")
  #mapStack <- stack(DEM, forestAge, forestCover, habitatQuality, percentPine)
  #names(mapStack)<-c("DEM", "forestAge", "forestCover", "habitatQuality", "percentPine")

  setColors(mapStack) <- list(DEM=terrain.colors(100),
                              forestAge=brewer.pal(9,"BuGn"),
                              #forestCover=brewer.pal(8,"BrBG"),
                              habitatQuality=brewer.pal(8,"Spectral"),
                              percentPine=brewer.pal(9,"Greens"))
  assignGlobal(simGlobals(sim)$.stackName, mapStack)

  # last thing to do is add module name to the loaded list
  simModulesLoaded(sim) <- append(simModulesLoaded(sim), "randomLandscapes")

  return(invisible(sim))
}
