################################################
###
### HABITAT MODULE
### - create random habitat
###
###############################################



### event functions:
#   - follow the naming convention `moduleName.eventType()`;
#   - `module.NAME.init()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.
do.event.habitat = function(sim, event.time, event.type) {
    if (event.type=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends = "NONE" # list module names here
        
        if (reload.module.later(sim, depends)) {
            sim <- schedule.event(sim, currentTime(sim), "habitat", "init")
        } else {
            # do stuff for this event
            sim <- habitat.init(sim)
            
            # schedule the next event
            #sim <- schedule.event(sim, EVENT.TIME, "MODULE.NAME", "EVENT.TYPE",)
        }
    } else {
        # do stuff for this event
        print("polar bears. grr!")
        
        # schedule the next event
        #sim <- schedule.event(sim, EVENT.TIME, "MODULE.NAME", "EVENT.TYPE")
    }
    return(sim)
}

habitat.init = function(sim) {
    ### load any required packages
    pkgs = list("raster", "shiny") # list required packages here
    load.packages(pkgs)
        
    ### initialize habitat
    nx = 5e2 # could be specified globally in params
    ny = 5e2 # could be specified globally in params
    hab <<- raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn =-ny/2, ymx=ny/2)
    hab <<- round(GaussMap(extent(hab), speedup=10), 1)
    
    ### add map to outputs list
#    outputs <- list(caribou=list(), habitat=list())
#    outputs$habitat[["map"]] <<- hab

#    saveRDS(hab, "../data/habitat.rds")
    
    newPlot()
    plot(hab)
    
    # last thing to do is add module name to the loaded list
    sim.loaded(sim) <- append(sim.loaded(sim), "habitat")
    
    return(sim)
}


### user-defined subroutines
