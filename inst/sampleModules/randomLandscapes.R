###
### MODULE: randomLandscapes
###
### DESCRIPTION: generate RasterStack of random maps representative of a forest landscape
###               - DEM, forestAge, forestCover, habitatQuality, percentPine
###

### load any required packages
### (use `loadPackages` or similar)
pkgs <- list("SpaDES", "raster")
loadPackages(pkgs)


doEvent.randomLandscapes <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for module dependencies:
    ### (use NULL if no dependencies exist)
    depends <- NULL

    ### check for object dependencies:
    ### (use `checkObject` or similar)


    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(sim, depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), "randomLandscapes", "init")
    } else {
      sim <- randomLandscapesInit(sim)
    }
    sim <- scheduleEvent(sim, simParams(sim)$randomLandscapes$.plotInitialTime, "randomLandscapes", "plot")
    sim <- scheduleEvent(sim, simParams(sim)$randomLandscapes$.saveInitialTime, "randomLandscapes", "save")

  } else if (eventType=="plot") {
    # do stuff for this event
    simPlot(get(simParams(sim)$.globals$mapName, envir=.GlobalEnv))

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$randomLandscapes$.plotInterval, "randomLandscapes", "plot")
  } else if (eventType=="save") {

    # do stuff for this event
    simSave(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$randomLandscapes$.saveInterval, "randomLandscapes", "save")

  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE] , "\'", sep=""))
  }
  return(sim)
}

randomLandscapesInit <- function(sim) {
  # Give dimensions of dummy raster
  nx <- simParams(sim)$randomLandscapes$nx
  ny <- simParams(sim)$randomLandscapes$ny
  template <- raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn=-ny/2, ymx=ny/2)
  speedup <- nx/5e1
  # Make dummy maps for testing of models
  DEM <- round(GaussMap(template, scale=300, var=0.03, speedup=speedup), 1)*1000
  forestAge <- round(GaussMap(template, scale=10, var=0.1, speedup=speedup), 1)*20
  forestCover <- round(GaussMap(template, scale=50, var=1, speedup=speedup),2)*10
  percentPine <- round(GaussMap(template, scale=50, var=1, speedup=speedup),1)

  # Scale them as needed
  forestAge <- forestAge/maxValue(forestAge)*100
  percentPine <- percentPine/maxValue(percentPine)*100

  # Make layers that are derived from other layers
  habitatQuality <- (DEM+10 + (forestAge+2.5)*10)/100
  habitatQuality <- habitatQuality/maxValue(habitatQuality)

  # Stack them into a single stack and assign to global env
  mapStack <- stack(DEM, forestAge, forestCover, habitatQuality, percentPine)
  names(mapStack)<-c("DEM","forestAge","forestCover","habitatQuality","percentPine")
  assign(simParams(sim)$.globals$mapName, mapStack, envir=.GlobalEnv)

  # last thing to do is add module name to the loaded list
  simModulesLoaded(sim) <- append(simModulesLoaded(sim), "randomLandscapes")

  return(sim)
}
