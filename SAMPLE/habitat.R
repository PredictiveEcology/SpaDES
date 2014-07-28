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
            simPlot(habitat, col=cols[c(2:5,3)])
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
    DEM = round(GaussMap(template, scale=300, var=0.03, speedup=speedup), 1)*1000
    forestAge = round(GaussMap(template, scale=10, var=0.1, speedup=speedup), 1)*20
    forestCover = round(GaussMap(template, scale=50, var=1, speedup=speedup),2)*10
    percentPine = round(GaussMap(template, scale=50, var=1, speedup=speedup),1)

    # Scale them as needed
    forestAge = forestAge/maxValue(forestAge)*100
    percentPine = percentPine/maxValue(percentPine)*100

    # Make layers that are derived from other layers
    habitatQuality = (DEM+10 + (forestCover+5)*10)/100
    habitatQuality = habitatQuality/maxValue(habitatQuality)

    # Stack them into a single stack for plotting
    habitat <<- stack(list(DEM, forestAge, forestCover, habitatQuality, percentPine))

    names(habitat) <<- c("DEM","forestAge", "forestCover", "habitatQuality", "percentPine")

    ### add map to outputs list
#    outputs <- list(caribou=list(), habitat=list())
#    outputs$habitat[["map"]] <<- hab

#    saveRDS(hab, "../data/habitat.rds")

    # last thing to do is add module name to the loaded list
    simLoaded(sim) <- append(simLoaded(sim), "habitat")

    return(sim)
}
