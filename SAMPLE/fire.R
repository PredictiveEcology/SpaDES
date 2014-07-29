################################################
###
### A SIMPLE FIRE MODULE
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
            sim <- scheduleEvent(sim, simCurrentTime(sim), "fire", "init")
        } else {
            # do stuff for this event
            sim <- fireInit(sim)
            simPlot(stack(habitat,Fires),col=cols[c(2:5,3,2)],add.legend=TRUE)
            
            # schedule the next event
            sim <- scheduleEvent(sim, 10, "fire", "burn")
            sim <- scheduleEvent(sim, 0, "fire", "plot")
        }
    } else if (eventType=="burn") {
        # do stuff for this event
        sim <- fireBurn(sim)
        
        # schedule the next event
        sim <- scheduleEvent(sim, simCurrentTime(sim)+10, "fire", "burn")
    } else if (eventType=="plot") {
      # do stuff for this event
      simPlot(stack(habitat, Fires), add=FALSE, 
              col=cols[c(2:5,3,2)], add.legend=TRUE)
      
      # schedule the next event
      sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fire$plotFreq, "fire", "plot")
    } else {
      warning(paste("Undefined event type: \'",simEvents(sim)[1,"eventType",with=FALSE],
                    "\' in module \'", simEvents(sim)[1,"moduleName",with=FALSE],"\'",sep=""))
    }
    return(sim)
}

fireInit = function(sim) {
    ### load any required packages
    pkgs = list("raster", "RColorBrewer") # list required packages here
    loadPackages(pkgs)
    
    ### create burn map that tracks fire locations over time
    Fires <<- raster(extent(habitat), ncol=ncol(habitat), nrow=nrow(habitat), vals=0)
    names(Fires) <<- "fire"
    Fires[] <- 0
    
    
    # last thing to do is add module name to the loaded list
    simLoaded(sim) <- append(simLoaded(sim), "fire")
    
    return(sim)
}


fireBurn = function(sim) {
    # random fire start locations, but could be based on hab:
    Fires <<- spread(habitat[[1]],
                  loci=as.integer(sample(1:ncell(habitat), simParams(sim)$fires$nFires)),
                  #spreadProb = 0.225,
                  spreadProb = simParams(sim)$fire$spreadprob,
                  persistance = simParams(sim)$fire$persistprob,
                  mapFireID = TRUE,
                  mask = NULL,
                  maxSize = 1e8,
                  directions = 8,
                  iterations = simParams(sim)$fire$its,
                  plot.it = FALSE,
                  mapID = TRUE)
    names(Fires)<<-"Fires"
  
    #values(burned) <<- values(burned) + values(tmp)
#    burned <- burned+tmp
    #burnedNoNA <- burned
    #burnedNoNA[burned==0] <- NA
    
    return(sim)
}
