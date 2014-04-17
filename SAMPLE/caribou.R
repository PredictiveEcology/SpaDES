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
do.event.caribou = function(sim, event.time, event.type, debug=FALSE) {
    if (event.type=="init") {
        ### check for module dependencies:
        depends = c("habitat") # list package names here
        
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        if (reload.module.later(sim, depends)) {
            sim <- schedule.event(sim, currentTime(sim), "caribou", "init")
        } else {
            # do stuff for this event
            sim <- caribou.init(sim)

            # schedule the next event
            sim <- schedule.event(sim, 1.00, "caribou", "move")
        }
    } else if (event.type=="move") {
        # do stuff for this event
        sim <- caribou.move(sim)
        
        # schedule the next event
        time.next.move = currentTime(sim) + 1.00
        sim <- schedule.event(sim, time.next.move, "caribou", "move")
    } else {
        # do stuff for this event
        print("polar bears. grr!")
        
        # schedule the next event
#        sim <- schedule.event(sim, EVENT.TIME, "MODULE.NAME", "EVENT.TYPE")
    }
    return(sim)
}

caribou.init = function(sim) {
    ### load any required packages
    pkgs = list("raster") # list required packages here
    load.packages(pkgs)
    
    best = max(hab@data@values)
    worst = min(hab@data@values)
    good = Which(hab>0.8*best)
    
    al = AgentLocation(good)    # good habitat, from above
    pri = ProbInit(hab, al)
    na = NumAgents(100)         # could be specified globally in params
    
    # initialize caribou agents
    caribou <<- new("mobileAgent", agentlocation=al, numagents=sim.params(sim)$Ncaribou, probinit=pri)
    points(caribou, pch=19, cex=0.1)
    
    # save output list to track caribou over time
#    outputs$caribou[[1]] <<- caribou
#    saveRDS(caribou, paste("../data/caribou_0.rds"))
    
    # last thing to do is add module name to the loaded list
    sim.loaded(sim) <- append(sim.loaded(sim), "caribou")
    return(sim)
}

caribou.move = function(sim) {
    ex =  hab[agentPosition(caribou)] # find out what pixels the individuals are on now
    wh = which(!is.na(ex))
    if (length(wh)==0) stop(paste("all agents off map at time", currentTime(sim)))
    sl = ex/10
    sl[-wh] = 1
    
    ln = rlnorm(length(ex), sl, 0.02) # log normal step length
    dir.sd = 30 # could be specified globally in params
    
    caribou <<- crw(caribou, step.len=ln, dir.sd=dir.sd, lonlat=FALSE)
    points(caribou, pch=19, cex=0.1)
    
    # update caribou list
#    outputs$caribou[[currentTime(sim)+1]] <<- caribou
    
    rads = sample(10:30, length(caribou), replace=TRUE)
    rings = cir(caribou, radiuses=rads, hab, 1)
    points(rings$x, rings$y, col=rings$ids, pch=19, cex=0.1)
    
#    saveRDS(list(caribou, rings), paste("../data/caribou_", currentTime(sim), ".rds", sep=""))

    return(sim) # technically, sim isn't updated in this function
}
