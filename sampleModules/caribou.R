################################################
###
### CARIBOU MODULE
### - requires habitat map from habitat module
### - create a bunch of caribou agents
### - move the caribou around the map
###
###############################################

doEvent.caribou <- function(sim, eventTime, eventType, debug=FALSE) {
    if (eventType=="init") {
        ### check for module dependencies:
        depends <- "NONE"#c("habitat") # list package names here

        # if a required module isn't loaded yet,
        # reschedule this module init for later
        if (reloadModuleLater(sim, depends)) {
            sim <- scheduleEvent(sim, simCurrentTime(sim), "caribou", "init")
        } else {
            # do stuff for this event
            sim <- caribouInit(sim)

            # schedule the next event
            sim <- scheduleEvent(sim, 1.00, "caribou", "move")
            sim <- scheduleEvent(sim, simParams(sim)$caribou$plotInitialTime, "caribou", "plot.init")
            sim <- scheduleEvent(sim, simParams(sim)$caribou$saveInitialTime, "caribou", "save")
        }
    } else if (eventType=="move") {
        # do stuff for this event
        sim <- caribouMove(sim)

        # schedule the next event
        sim <- scheduleEvent(sim, simCurrentTime(sim) + 1.00, "caribou", "move")
    } else if (eventType=="plot.init") {
      # do stuff for this event
      simPlot(caribou, on.which.to.plot="forestAge", add=TRUE, pch=19,gp=gpar(cex=0.01),
              delete.previous=TRUE)

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribou$plotInterval, "caribou", "plot")
    } else if (eventType=="plot") {
      # do stuff for this event
      simPlot(caribou, on.which.to.plot="forestAge", add=TRUE, pch=19,gp=gpar(cex=0.01),
              delete.previous=TRUE)

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribou$plotInterval, "caribou", "plot")
    } else if (eventType=="save") {
      # do stuff for this event
      simSave(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribou$saveInterval, "caribou", "save")

    } else {
      warning(paste("Undefined event type: \'",simEvents(sim)[1,"eventType",with=FALSE],
                    "\' in module \'", simEvents(sim)[1,"moduleName",with=FALSE],"\'",sep=""))
    }
    return(sim)
}

caribouInit <- function(sim) {
    ### load any required packages
    pkgs <- list("raster","grid") # list required packages here
    loadPackages(pkgs)

    yrange <- c(ymin(habitat),ymax(habitat))
    xrange <- c(xmin(habitat),xmax(habitat))
#    best <- max(values(habitat))
#    worst <- min(values(habitat))
#    good <- Which(habitat>0.8*best)
#
#   al <- agentLocation(good)    # good habitat, from above
#   initialCoords <- probInit(habitat, al)

    # initialize caribou agents
    N <- simParams(sim)$caribou$N
    IDs <- as.character(1:N)
    sex <- sample(c("female", "male"), N, replace=TRUE)
    age <- round(rnorm(N, mean=8, sd=3))
    prevX <- rep(0, N)
    prevY <- rep(0, N)
    starts <- cbind(x=runif(N, xrange[1],xrange[2]),
                    y=runif(N, yrange[1],yrange[2]))

    # create the caribou agent object
    caribou <<- SpatialPointsDataFrame(coords=starts,
                                       data=data.frame(prevX, prevY, sex, age))
    row.names(caribou) <<- IDs # alternatively, add IDs as column in data.frame above

    #simPlot(caribou, add=TRUE, pch=".")

    # save output list to track caribou over time
#    outputs$caribou[[1]] <<- caribou
#    saveRDS(caribou, paste("../data/caribou_0.rds"))

    # last thing to do is add module name to the loaded list
    simLoaded(sim) <- append(simLoaded(sim), "caribou")
    return(sim)
}

caribouMove <- function(sim) {
  #crop any caribou that went off maps
  caribou <<- crop(caribou, habitat)
  if(length(caribou)==0) stop("All agents are off map")

  # find out what pixels the individuals are on now
  ex <- habitat[["habitatQuality"]][caribou]

  #step length is a function of current cell's habitat quality
  sl <- 0.25/ex

  ln <- rlnorm(length(ex), sl, 0.02) # log normal step length
  sd <- 30 # could be specified globally in params

  caribou <<- move("crw", caribou, stepLength=ln, stddev=sd, lonlat=FALSE)

#     #rads <- sample(10:30, length(caribou), replace=TRUE)
#     #rings <- cir(caribou, radiuses=rads, habitat, 1)
#     #points(rings$x, rings$y, col=rings$ids, pch=19, cex=0.1)
#

    return(sim) # technically, sim isn't updated in this function
}
