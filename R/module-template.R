# autogenerate a skeleton for a new module
newModule = function(name, path) {
    path <- check.path(path)
    filename <- paste(path, name, sep="")

    # - need to produce code that replaces TEMPLATE with module name
    # cat(, file=filename, fill=FALSE, sep="\n")

    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
    
    ### event functions:
    #   - follow the naming convention `moduleName.eventType()`;
    #   - `moduleName.init()` function is required for initiliazation;
    #   - keep event functions short and clean, modularize by calling
    #       subroutines from section below.
    
    ### template event
    doEvent.TEMPLATE = function(sim, eventTime, eventType, debug=FALSE) {
      if (eventType=="init") {
        ### check for module dependencies
        # if a required module isn't loaded yet,
        # reschedule this module init for later
        depends <- "NONE" # list module names here

        if (reloadModuleLater(sim, depends)) {
          sim <- scheduleEvent(sim, currentTime(sim), "habitat", "init")
        } else {
          sim <- habitatInit(sim)
        }
      } else if (eventType=="templateEvent") {
        # ! ----- EDIT BELOW ----- ! #
        # do stuff for this event

        # schedule future event(s)
        sim <- scheduleEvent(sim, currentTime(sim), "habitat", "init")

        # ! ----- STOP EDITING ----- ! #
        } else {
          warning(paste("Undefined event type: \'",simEvents(sim)[1,"eventType",with=FALSE],
                        "\' in module \'", simEvents(sim)[1,"moduleName",with=FALSE],"\'",sep=""))
        }
      return(sim)
    }

    ### template initilization
    templateInit = function(sim) {
      # load any required packages
      pkgs <- list("raster") # list required packages here
      loadPackforestAges(pkgs)

      # # ! ----- EDIT BELOW ----- ! #


      # ! ----- STOP EDITING ----- ! #

      # last thing to do is add module name to the loaded list
      simLoaded(sim) <- append(simLoaded(sim), "TEMPLATE")

      return(sim)
    }

    ### template for your module function
    templateFunction = function(sim) {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # schedule future event(s)
      sim <- scheduleEvent(sim, currentTime(sim), "TEMPLATE", "function")

      # ! ----- STOP EDITING ----- ! #
      return(sim)
    }

    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
}
