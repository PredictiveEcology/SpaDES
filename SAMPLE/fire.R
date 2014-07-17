################################################
###
### FIRE MODULE
### - burn some habitat; scare caribou away
###
###############################################



### event functions:
#   - follow the naming convention `moduleName.eventType()`;
#   - `moduleName.init()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.
doEvent.fire = function(sim, eventTime, eventType, debug=FALSE) {
    if (eventType=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends = "habitat" # list module names here
        
        if (reloadModuleLater(sim, depends)) {
            sim <- scheduleEvent(sim, currentTime(sim), "fire", "init")
        } else {
            # do stuff for this event
            sim <- fireInit(sim)
            
            # schedule the next event
            sim <- scheduleEvent(sim, 0.5, "fire", "burn")
        }
    } else if (eventType=="burn") {
        # do stuff for this event
        sim <- fireBurn(sim)
        
        # schedule the next event
        sim <- scheduleEvent(sim, currentTime(sim)+1.0, "fire", "burn")
    } else {
        print("polar bears. grr!")
    }
    return(sim)
}

fireInit = function(sim) {
    ### load any required packages
    pkgs = list("raster", "RColorBrewer") # list required packages here
    loadPackages(pkgs)
    
    ### create burn map that tracks fire locations over time
    tmp = raster(extent(hab), ncol=ncol(hab), nrow=nrow(hab), vals=0)
    names(tmp) = "area.burned"
    burned <<- tmp
    
    simPlot(stack(hab, burned), speedup=10,add=F, 
            col=list(brewer.pal(9,"YlGnBu"),brewer.pal(10,"Set3")))
    
    # last thing to do is add module name to the loaded list
    sim.loaded(sim) <- append(sim.loaded(sim), "fire")
    
    return(sim)
}


fire.burn = function(sim) {
    # random fire start locations, but could be based on hab:
    loci = sample(1:ncell(hab), size=simParams(sim)$fire$num)
    tmp = spread(hab, loci=loci, spreadProb=simParams(sim)$fire$spreadprob,
                       persistance=simParams(sim)$fire$persistprob, iterations=simParams(sim)$fire$its)
    
    values(burned) <<- values(burned) + values(tmp)
#    burned <- burned+tmp
    burnedNoNA <- burned
    burnedNoNA[burned==0] <- NA
    simPlot(burnedNoNA, on.which.to.plot="area.burned", add=TRUE, speedup=20, 
            col=brewer.pal(10,"Set3"))
    
    return(sim)
}
