################################################
###
### HABITAT MODULE
### - create random habitat
###
###############################################



### event functions:
#   - follow the naming convention `module.eventType()`;
#   - `moduleInit()` function is required for initiliazation;
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
    # Give dimensions of dummy raster
    nx = simParams(sim)$habitat$nx
    ny = simParams(sim)$habitat$ny
    template = raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn =-ny/2, ymx=ny/2)
    speedup = nx/5e1
    # Make dummy maps for testing of models
    DEM = round(GaussMap(template, scale = 300, var = 0.03, speedup=speedup), 1)*1000
    Age = round(GaussMap(template, scale = 10, var = 0.1, speedup=speedup), 1)*20
    Forest_Cover = round(GaussMap(template, scale = 50, var = 1, speedup=speedup),2)*10
    Pct_Pine = round(GaussMap(template, scale = 50, var = 1, speedup=speedup),1)
    
    # Scale them as needed
    Age = Age/maxValue(Age)*100
    Pct_Pine = Pct_Pine/maxValue(Pct_Pine)*100
    
    # Make layers that are derived from other layers
    HabitatQuality = (DEM+10 + (Forest_Cover+5)*10)/100 
    HabitatQuality = HabitatQuality/maxValue(HabitatQuality)  
    
    # Stack them into a single stack for plotting
    habitat <<- stack(list(DEM,Age,Forest_Cover,HabitatQuality,Pct_Pine))
    
    names(habitat) <<- c("DEM","Age", "Forest_Cover", "HabitatQuality", "Pct_Pin")
    
    ### add map to outputs list
#    outputs <- list(caribou=list(), habitat=list())
#    outputs$habitat[["map"]] <<- hab

#    saveRDS(hab, "../data/habitat.rds")
    
    # last thing to do is add module name to the loaded list
    simLoaded(sim) <- append(simLoaded(sim), "habitat")
    
    return(sim)
}
