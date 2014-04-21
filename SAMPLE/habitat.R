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
do.event.habitat = function(sim, event.time, event.type, debug=FALSE) {
    if (event.type=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends = "NONE" # list module names here
        
        if (reload.module.later(sim, depends)) {
            sim <- schedule.event(sim, currentTime(sim), "habitat", "init")
        } else {
            sim <- habitat.init(sim)
        }
    } else {
        print("polar bears. grr!")
    }
    return(sim)
}

habitat.init = function(sim) {
    ### load any required packages
    pkgs = list("raster") # list required packages here
    load.packages(pkgs)
        
    ### initialize habitat
    nx = 5e2 # could be specified globally in params
    ny = 5e2 # could be specified globally in params
    tmp = raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn =-ny/2, ymx=ny/2)
    tmp = round(GaussMap(extent(tmp), speedup=10), 1)
    names(tmp) = "habitat"
    hab <<- tmp
    
    ### add map to outputs list
#    outputs <- list(caribou=list(), habitat=list())
#    outputs$habitat[["map"]] <<- hab

#    saveRDS(hab, "../data/habitat.rds")
    
    # last thing to do is add module name to the loaded list
    sim.loaded(sim) <- append(sim.loaded(sim), "habitat")
    
    return(sim)
}
