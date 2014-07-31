# autogenerate a skeleton for a new module
newModule = function(name, path) {
    path <- check.path(path)
    filename <- paste(path, name, sep="")

    # - need to produce code that replaces TEMPLATE with module name
    # cat(, file=filename, fill=FALSE, sep="")

    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

    cat("
        ### event functions:
        #   - follow the naming convention `modulenameEventtype()`;
        #   - `modulenameInit()` function is required for initiliazation;
        #   - module name and this filename must match;
        #   - keep event functions short and clean, modularize by calling
        #       subroutines from section below.

        ### template event
        doEvent.", name, " = function(simname, eventTime, eventType, debug=FALSE) {
          sim <- get(simname, envir=globalenv())
          if (eventType==\"init\") {
            ### check for module dependencies
            # if a required module isn't loaded yet,
            # reschedule this module init for later
            depends <- \"NONE\" # list module names here

            if (reloadModuleLater(sim, depends)) {
              scheduleEvent(sim, currentTime(sim), \"", name, "\", \"init\")
            } else {
              sim <- ", name, "Init(sim)
            }
          } else if (eventType==\"templateEvent\") {
            # ! ----- EDIT BELOW ----- ! #
            # do stuff for this event

            # schedule future event(s)
            sim <- scheduleEvent(sim, currentTime(sim), \"", name, "\", \"init\")

            # ! ----- STOP EDITING ----- ! #
            } else {
              warning(paste(\"Undefined event type: \'\", simEvents(sim)[1, \"eventType\", with=FALSE],
                            \"\' in module \'\", simEvents(sim)[1, \"moduleName\", with=FALSE], \"\'\", sep=\"\"))
            }
          return(sim)
        }

        ### template initilization
        ", name, "Init = function(sim) {
          # load any required packages
          pkgs <- list(\"raster\") # list required packages here
          loadPackforestAges(pkgs)

          # # ! ----- EDIT BELOW ----- ! #


          # ! ----- STOP EDITING ----- ! #

          # last thing to do is add module name to the loaded list
          simLoaded(sim) <- append(simLoaded(sim), \"", name, "\")

          return(sim)
        }

        ### template for your module function
        ", name, "Function = function(sim) {
          # ! ----- EDIT BELOW ----- ! #
          # do stuff for this event

          # schedule future event(s)
          sim <- scheduleEvent(sim, currentTime(sim), \"", name, "\", \"function\")

          # ! ----- STOP EDITING ----- ! #
          return(sim)
        }",
        file=filename, fill=FALSE, sep="")
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
}
