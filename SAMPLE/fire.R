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
do.event.fire = function(sim, event.time, event.type, debug=FALSE) {
    if (event.type=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends = "habitat" # list module names here
        
        if (reload.module.later(sim, depends)) {
            sim <- schedule.event(sim, currentTime(sim), "fire", "init")
        } else {
            # do stuff for this event
            sim <- fire.init(sim)
            
            # schedule the next event
            sim <- schedule.event(sim, 0.5, "fire", "burn")
        }
    } else if (event.type=="burn") {
        # do stuff for this event
        sim <- fire.burn(sim)
        
        # schedule the next event
        sim <- schedule.event(sim, currentTime(sim)+1.0, "fire", "burn")
    } else {
        print("polar bears. grr!")
    }
    return(sim)
}

fire.init = function(sim) {
    ### load any required packages
    pkgs = list("raster", "RColorBrewer") # list required packages here
    load.packages(pkgs)
    
    ### create burn map that tracks fire locations over time
    tmp = raster(extent(hab), ncol=ncol(hab), nrow=nrow(hab), vals=0)
    names(tmp) = "area.burned"
    burned <<- tmp
    
    simplot(stack(hab, burned), speedup=10,add=F, 
            col=list(brewer.pal(9,"YlGnBu"),brewer.pal(10,"Set3")))
    
    # last thing to do is add module name to the loaded list
    sim.loaded(sim) <- append(sim.loaded(sim), "fire")
    
    return(sim)
}


fire.burn = function(sim) {
    # random fire start locations, but could be based on hab:
    loci = sample(1:ncell(hab), size=sim.params(sim)$fire$num)
    tmp = SpreadEvents(hab, loci=loci, spreadProb=sim.params(sim)$fire$spreadprob,
                       persistance=sim.params(sim)$fire$persistprob, iterations=sim.params(sim)$fire$its)
    
    values(burned) <<- values(burned) + values(tmp)
#    burned <- burned+tmp
    burnedNoNA <- burned
    burnedNoNA[burned==0] <- NA
    simplot(burnedNoNA, on.which.to.plot="area.burned", add=TRUE, speedup=20, 
            col=brewer.pal(10,"Set3"))
    
    return(sim)
}
