################################################
###
### AGE MODULE
### - change the age of cells
###
###############################################



### event functions:
#   - follow the naming convention `moduleName.eventType()`;
#   - `moduleName.init()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.
do.event.age = function(sim, event.time, event.type, debug=FALSE) {
    if (event.type=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends = "" # list module names here
        
        if (reload.module.later(sim, depends)) {
            sim <- schedule.event(sim, currentTime(sim), "age", "init")
        } else {
            # do stuff for this event
            sim <- age.init(sim)
            
            # schedule the next event
            sim <- schedule.event(sim, 0.6, "age", "age")
        }
    } else if (event.type=="age") {
        # do stuff for this event
        sim <- age(sim)
        
        # schedule the next event
        sim <- schedule.event(sim, currentTime(sim)+1.0, "age", "age")
    } else {
        print("polar bears. grr!")
    }
    return(sim)
}

age.init = function(sim) {
    ### load any required packages
    pkgs = list("raster", "RColorBrewer") # list required packages here
    load.packages(pkgs)
    ageMap.full <- raster("C:/shared/data/shared/age/age.asc")
    beginCluster()
    ageMap <- projectRaster(ageMap.full,to=vegMap)
    endCluster()
    
#    assign(x=get(sim.params(sim)$age$rasterLayerName),
#           value=raster(sim.params(sim)$age$inputFile),
#           envir=.GlobalEnv)

    
    # last thing to do is add module name to the loaded list
    sim.loaded(sim) <- append(sim.loaded(sim), "age")
    
    return(sim)
}

age.age = function(sim) {
#    assign(x=get(sim.params(sim)$age$rasterStackName),
#           value=agingFunction(get(sim.params(sim)$age$rasterStackName)),
#           envir=.GlobalEnv)
    ageMap[] <- ageMap[] + 1
    return(sim)
}
