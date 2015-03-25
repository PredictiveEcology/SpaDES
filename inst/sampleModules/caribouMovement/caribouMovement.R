###
### Specify module (and dependencies) definitions:
###
### name:         caribouMovement
###
### description:  Simulate caribou movement via correlated random walk.
###
### keywords:     caribou; individual based movement model; correlated random walk
###
### authors:      Eliot J. B. McIntire <Eliot.McIntire@NRCan.gc.ca>
###
### version:      0.2.0
###
### spatialExtent: NA
###
### timeframe:    NA
###
### timestep:     31557600 (1 year)
###
### citation:     NA
###
### reqdPkgs:     dplyr; grid; raster; sp
###
### inputObjects: objectName: simGlobals(sim)$.stackName
###               objectClass: RasterStack
###               other: layerName="habitatQuality"
###
###               objectName: simParams(sim)$caribouMovementLcc$moveInterval
###               objectClass: numeric
###               other: NA
###
###               objectName: simParams(sim)$caribouMovement$N
###               objectClass: numeric (integer)
###               other: NA
###
### outputObjects: objectName: simGlobals(sim)$.stackName
###                objectClass: RasterStack
###                other: layerName="habitatQuality"
###
###                objectName: caribou
###                objectClass: SpatialPointsDataFrame
###                other: NA
###
### caribouMovement module metadata
defineModule(sim, list(
  name="caribouMovement",
  description="Simulate caribou movement via correlated random walk.",
  keywords=c("caribou", "individual based movement model", "correlated random walk"),
  authors=c(person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre"))),
  version=numeric_version("0.2.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timestep=NA_real_,
  citation=list(),
  reqdPkgs=list("dplyr", "grid", "raster", "sp"),
  inputObjects=data_frame(objectName=c(simGlobals(sim)$.stackName,
                                       "simParams(sim)$caribouMovementLcc$moveInterval",
                                       "simParams(sim)$caribouMovement$N"),
                          objectClass=c("RasterStack", "numeric", "numeric"),
                          other=c("layername=\"habitatQuality\"", rep(NA_character_, 2L))),
  outputObjects=data_frame(objectName=c(simGlobals(sim)$.stackName, "caribou"),
                           objectClass=c("RasterStack", "SpatialPointsDataFrame"),
                           other=c("layername=\"habitatQuality\"", NA_character_))
))

### event functions
doEvent.caribouMovement <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)
    checkObject(simGlobals(sim)$.stackName, layer="habitatQuality")

    # do stuff for this event
    sim <- caribouMovementInit(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, 1.00, "caribouMovement", "move")
    sim <- scheduleEvent(sim, simParams(sim)$caribouMovement$.plotInitialTime, "caribouMovement", "plot.init")
    sim <- scheduleEvent(sim, simParams(sim)$caribouMovement$.saveInitialTime, "caribouMovement", "save")
  } else if (eventType=="move") {
    # do stuff for this event
    sim <- caribouMovementMove(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovement$moveInterval, "caribouMovement", "move")
  } else if (eventType=="plot.init") {
    # do stuff for this event
    Plot(caribou, addTo="landscape.habitatQuality", new=FALSE, size=0.2, pch=19, gp=gpar(cex=0.6))

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovement$.plotInterval, "caribouMovement", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(caribou, addTo="landscape.habitatQuality", new=FALSE, pch=19, size=0.2, gp=gpar(cex=0.6))
    Plot(caribou, new=FALSE, pch=19, size=0.1, gp=gpar(cex=0.6))

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovement$.plotInterval, "caribouMovement", "plot")
  } else if (eventType=="save") {
    # do stuff for this event
    saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovement$.saveInterval, "caribouMovement", "save")

  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1,"eventType",with=FALSE],
                  "\' in module \'", simEvents(sim)[1,"moduleName",with=FALSE],"\'",sep=""))
  }
  return(invisible(sim))
}

caribouMovementInit <- function(sim) {
  landscape <- getGlobal(simGlobals(sim)$.stackName)

  yrange <- c(ymin(landscape), ymax(landscape))
  xrange <- c(xmin(landscape), xmax(landscape))

  # initialize caribou agents
  N <- simParams(sim)$caribouMovement$N
  IDs <- as.character(1:N)
  sex <- sample(c("female", "male"), N, replace=TRUE)
  age <- round(rnorm(N, mean=8, sd=3))
  x1 <- rep(0, N)
  y1 <- rep(0, N)
  starts <- cbind(x=runif(N, xrange[1],xrange[2]),
                  y=runif(N, yrange[1],yrange[2]))

  # create the caribou agent object
  caribou <<- SpatialPointsDataFrame(coords=starts,
                                     data=data.frame(x1, y1, sex, age))
  row.names(caribou) <<- IDs # alternatively, add IDs as column in data.frame above

  return(invisible(sim))
}

caribouMovementMove <- function(sim) {
  landscape <- getGlobal(simGlobals(sim)$.stackName)

  # crop any caribou that went off maps
  caribou <<- crop(caribou, landscape)
  if(length(caribou)==0) stop("All agents are off map")

  # find out what pixels the individuals are on now
  ex <- landscape[["habitatQuality"]][caribou]

  # step length is a function of current cell's habitat quality
  sl <- 0.25/ex

  ln <- rlnorm(length(ex), sl, 0.02) # log normal step length
  sd <- 30 # could be specified globally in params

  caribou <<- move("crw", caribou, stepLength=ln, stddev=sd, lonlat=FALSE)

  return(invisible(sim))
}
