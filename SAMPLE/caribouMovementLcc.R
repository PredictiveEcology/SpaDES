###
### MODULE: caribouMovement
###
### DESCRIPTION: simulate caribou movement via correlated random walk
###               - requires a RasterStack object whose name is specified
###                 by `simGlobals(sim)$.stackName`, containing a RasterLayer
###                 named `habitatQuality`
###

### load any required packages
### (use `loadPackages`, or `library` directly)
pkgs <- list("SpaDES", "grid", "raster", "sp")
loadPackages(pkgs)
rm(pkgs)

### event functions
doEvent.caribouMovementLcc <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for module dependencies:
    ### (use NULL if no dependencies exist)
    depends <- NULL

    ### check for object dependencies:
    ### (use `checkObject` or similar)
    checkObject("vegMap")

    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(sim, depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), "caribouMovementLcc", "init")
    } else  {
      # do stuff for this event
      sim <- caribouMovementInit(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, 1.00, "caribouMovementLcc", "move")
      sim <- scheduleEvent(sim, simParams(sim)$caribouMovementLcc$.plotInitialTime, "caribouMovementLcc", "plot.init")
      sim <- scheduleEvent(sim, simParams(sim)$caribouMovementLcc$.saveInitialTime, "caribouMovementLcc", "save")
    }
  } else if (eventType=="move") {
    # do stuff for this event
    sim <- caribouMovementMove(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovementLcc$moveInterval, "caribouMovementLcc", "move")
  } else if (eventType=="plot.init") {
    # do stuff for this event
    Plot(caribou, addTo="forestAge", add=TRUE, size=1, pch=19, gp=gpar(cex=0.01))

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovementLcc$.plotInterval, "caribouMovementLcc", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(caribou, addTo="forestAge", add=TRUE, pch=19, size=1, gp=gpar(cex=0.01))

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovementLcc$.plotInterval, "caribouMovementLcc", "plot")
  } else if (eventType=="save") {
    # do stuff for this event
    saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovementLcc$.saveInterval, "caribouMovementLcc", "save")

  } else {
    warning(paste("Undefined event type: \'",simEvents(sim)[1,"eventType",with=FALSE],
                  "\' in module \'", simEvents(sim)[1,"moduleName",with=FALSE],"\'",sep=""))
  }
  return(invisible(sim))
}

caribouMovementInit <- function(sim) {
  cellsFromXY <- cellFromXY # the raster Package has a bug
  caribouRas <<- RasterLayerNamed(raster(extent(vegMap), ncol=ncol(vegMap),
                                                nrow=nrow(vegMap), vals=0),name="caribouRas")

  yrange <- c(ymin(vegMap), ymax(vegMap))
  xrange <- c(xmin(vegMap), xmax(vegMap))
#    best <- max(values(vegMap))
#    worst <- min(values(vegMap))
#    good <- Which(vegMap>0.8*best)
#
#   al <- agentLocation(good)    # good vegMap, from above
#   initialCoords <- probInit(vegMap, al)

  # initialize caribou agents
  N <- simParams(sim)$caribouMovementLcc$N
  IDs <- as.character(1:N)
  sex <- sample(c("female", "male"), N, replace=TRUE)
  age <- round(rnorm(N, mean=8, sd=3))
  x1 <- rep(0, N)
  y1 <- rep(0, N)
  starts <- cbind(x=runif(N, xrange[1],xrange[2]),
                  y=runif(N, yrange[1],yrange[2]))

  # create the caribou agent object
  caribou <<- SpatialPointsDataFrameNamed(coords=starts,
                                     data=data.frame(x1, y1, sex, age),
                                     name="caribou")
  row.names(caribou) <- IDs # alternatively, add IDs as column in data.frame above
  caribouRas[caribou] <- caribouRas[caribou]+1
  return(invisible(sim))
}

caribouMovementMove <- function(sim) {
  # crop any caribou that went off maps

    caribou <<- crop(caribou, vegMap)
  if(length(caribou)==0) stop("All agents are off map")

  # find out what pixels the individuals are on now
  ex <- ageMap[caribou]

  # step length is a function of current cell's habitat quality
  sl <- 20/log(ex+5)

  ln <- rlnorm(length(ex), sl, 0.02) # log normal step length
  sd <- 30 # could be specified globally in params

  caribou <<- move("crw", caribou, stepLength=ln, stddev=sd, lonlat=FALSE)
  caribouRas[caribou] <<- caribouRas[caribou] + 1
  setColors(caribouRas,maxValue(caribouRas)+1) <- c("#FFFFFF", rev(heat.colors(maxValue(caribouRas))))
  assign("caribouRas",caribouRas,envir=.GlobalEnv)

#     #rads <- sample(10:30, length(caribou), replace=TRUE)
#     #rings <- cir(caribou, radiuses=rads, vegMap, 1)
#     #points(rings$x, rings$y, col=rings$ids, pch=19, cex=0.1)
#

    return(invisible(sim))
}
