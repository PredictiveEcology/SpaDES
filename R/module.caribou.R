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
react.event.caribou = function(event.time, event.type) {
    if (event.type=="init") {
        # do stuff for this event
        module.caribou.init()
        
        # schedule the next event
        schedule.event(1.00, "caribou", "move")
    } else if (event.type=="move") {
        # do stuff for this event
        module.caribou.move()
        
        # schedule the next event
        time.next.move = sim$currtime + 1.00
        schedule.event(time.next.move, "caribou", "move")
    } else {
        # do stuff for this event
        print("polar bears. grr!")
        
        # schedule the next event
#        schedule.event(EVENT.TIME, "MODULE.NAME", "EVENT.TYPE", list(OPTIONAL.ITEMS))
    }
}

module.caribou.init = function() {
    ### load any required packages
    pkgs = list("raster") # list required packages here
    load.required.pkgs(pkgs)
    
    hab = get.habitat.map() # from habitat module
    best = max(hab@data@values)
    worst = min(hab@data@values)
    good = Which(hab>0.8*best)
    
    al = AgentLocation(good)    # good habitat, from above
    pri = ProbInit(hab, al)
    na = NumAgents(100)         # could be specified globally in params
    
    # initialize caribou agents
    caribou = new("mobileAgent", agentlocation=al, numagents= na, probinit=pri)
    points(caribou, pch=19, cex=0.1)
    
    ### module parameters
    #   - export module params to global list
    globals$params[["caribou"]] <<- list(population=caribou)
    
    #   -  export data structure for module stats
#    globals$modulestats[["caribou"]] <<- list()
}

module.caribou.move = function() {
    hab = get.habitat.map() # from habitat module
    caribou = get.caribou.population() # see below
    
    ex =  hab[position(caribou)] # find out what pixels the individuals are on now
    wh = which(!is.na(ex))
    if (length(wh)==0) stop(paste("all agents off map at time", sim$currtime))
    sl = ex/10
    sl[-wh] = 1
    
    ln = rlnorm(length(ex), sl, 0.02) # log normal step length
    dir.sd = 30 # could be specified globally in params
    
    caribou = crw(caribou, step.len=ln , dir.sd=dir.sd)
    points(caribou, pch=19, cex = 0.1)
    
    rads = sample(10:30, length(caribou), replace=TRUE)
    rings = cir(caribou, radiuses=rads, hab, 1)
    points(rings$x, rings$y, col=rings$ids, pch=19, cex=0.1)
    
    globals$params$caribou$population <<- caribou
}

### user-defined subroutines

get.caribou.population = function() {
    pop = globals$params$caribou$population
    return(pop)
}