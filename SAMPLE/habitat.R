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
doEvent.habitat = function(sim, eventTime, eventType, debug=FALSE) {
    if (eventType=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends = "NONE" # list module names here
        
        if (reloadModuleLater(sim, depends)) {
            sim <- scheduleEvent(sim, currentTime(sim), "habitat", "init")
        } else {
            sim <- habitatInit(sim)
        }
    } else {
        print("polar bears. grr!")
    }
    return(sim)
}

habitatInit = function(sim) {
    ### load any required packages
    pkgs = list("raster") # list required packages here
    loadPackages(pkgs)
        
    ### initialize habitat
    nx = 1e3 # could be specified globally in params
    ny = 1e3 # could be specified globally in params
    tmp = raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn =-ny/2, ymx=ny/2)
    tmp = round(GaussMap(tmp, speedup=10), 1)
    names(tmp) = "habitat"
    hab <<- tmp
    
    ### add map to outputs list
#    outputs <- list(caribou=list(), habitat=list())
#    outputs$habitat[["map"]] <<- hab

#    saveRDS(hab, "../data/habitat.rds")
    
    # last thing to do is add module name to the loaded list
    simLoaded(sim) <- append(simLoaded(sim), "habitat")
    
    return(sim)
}
