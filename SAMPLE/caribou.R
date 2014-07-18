################################################
###
### CARIBOU MODULE
### - requires habitat map from habitat module
### - create a bunch of caribou agents
### - move the caribou around the map
###
###############################################



### event functions:
#   - follow the naming convention `moduleName.eventType()`;
#   - `module.NAME.init()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.
doEvent.caribou = function(sim, eventTime, eventType, debug=FALSE) {
    if (eventType=="init") {
        ### check for module dependencies:
        depends = c("habitat") # list package names here
        
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        if (reloadModuleLater(sim, depends)) {
            sim <- scheduleEvent(sim, currentTime(sim), "caribou", "init")
        } else {
            # do stuff for this event
            sim <- caribouInit(sim)

            # schedule the next event
            sim <- scheduleEvent(sim, 1.00, "caribou", "move")
        }
    } else if (eventType=="move") {
        # do stuff for this event
        sim <- caribouMove(sim)
        
        # schedule the next event
        sim <- scheduleEvent(sim, currentTime(sim) + 1.00, "caribou", "move")
    } else {
        # do stuff for this event
        print("polar bears. grr!")
        
        # schedule the next event
#        sim <- scheuleEvent(sim, EVENT.TIME, "MODULE.NAME", "EVENT.TYPE")
    }
    return(sim)
}

caribouInit = function(sim) {
    ### load any required packages
    pkgs = list("raster","grid") # list required packages here
    loadPackages(pkgs)
    
    best = max(habitat@data@values)
    worst = min(habitat@data@values)
    good = Which(habitat>0.2*best)
    
    al = agentLocation(good)    # good habitat, from above
    pri = probInit(habitat, al)
    
    # initialize caribou agents
    N <- simParams(sim)$caribou$N
    IDs <- as.character(1:N)
    sex <- sample(c("female", "male"), N, replace=TRUE)
    age <- round(rnorm(N, mean=8, sd=3))
    
    # create the caribou agent object
    caribou <- SpatialPointsDataFrame(coords=al,
                                      data=data.frame(sex=sex, age=age))
    row.names(caribou) <- IDs # alternatively, add IDs as column in data.frame above
        
    simPlot(caribou, ext=extent(habitat), on.which.to.plot=1, add=TRUE, pch=19, gp=gpar(cex=0.1))
    
    # save output list to track caribou over time
#    outputs$caribou[[1]] <<- caribou
#    saveRDS(caribou, paste("../data/caribou_0.rds"))
    
    # last thing to do is add module name to the loaded list
    sim.loaded(sim) <- append(simLoaded(sim), "caribou")
    return(sim)
}

caribouMove = function(sim) {
    ex =  habitat[coordinates(caribou)] # find out what pixels the individuals are on now
    wh = which(!is.na(ex))
    if (length(wh)==0) stop(paste("all agents off map at time", currentTime(sim)))
    sl = 10/ex
    sl[-wh] = 1
    
    ln = rlnorm(length(ex), sl, 0.02) # log normal step length
    sd = 30 # could be specified globally in params
    
    caribou <<- crw(caribou, stepLength=ln, sd=sd, lonlat=FALSE)
    simPlot(caribou, ext=extent(habitat), on.which.to.plot=1, add=TRUE, pch=19,
            gp=gpar(cex=0.1), delete.previous=FALSE)
    
    # update caribou list
#    outputs$caribou[[currentTime(sim)+1]] <<- caribou
    
    #rads = sample(10:30, length(caribou), replace=TRUE)
    #rings = cir(caribou, radiuses=rads, habitat, 1)
    #points(rings$x, rings$y, col=rings$ids, pch=19, cex=0.1)
    
#    saveRDS(list(caribou, rings), paste("../data/caribou_", currentTime(sim), ".rds", sep=""))

    return(sim) # technically, sim isn't updated in this function
}
