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
do.event.habitat = function(event.time, event.type) {
    if (event.type=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends = "NONE" # list package names here
        
        if (reload.module.later(depends)) {
            schedule.event(sim.time(sim), "habitat", "init")
        } else {
            # do stuff for this event
            habitat.init()
            
            # schedule the next event
#            schedule.event(EVENT.TIME, "MODULE.NAME", "EVENT.TYPE", list(OPTIONAL.ITEMS))
        }
    } else {
        # do stuff for this event
        print("polar bears. grr!")
        
        # schedule the next event
 #       schedule.event(EVENT.TIME, "MODULE.NAME", "EVENT.TYPE", list(OPTIONAL.ITEMS))
    }
}

habitat.init = function() {
    ### load any required packages
    pkgs = list("raster") # list required packages here
    load.packages(pkgs)
        
    ### initialize habitat
    nx = 5e2 # could be specified globally in params
    ny = 5e2 # could be specified globally in params
    hab <<- raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn = -ny/2, ymx = ny/2)
    hab <<- round(GaussMap(extent(hab), speedup=10), 1)
    plot(hab)
    dev.flush()
    
    # last thing to do is add module name to the loaded list
    sim.loaded(sim) <<- append(sim.loaded(sim), "habitat")
}


### user-defined subroutines
