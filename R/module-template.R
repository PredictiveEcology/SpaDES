##############################################################
#' Create new module from template.
#'
#' Autogenerate a skeleton for a new SpaDES module.
#'
#' @param name  Character string. Your module's name.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @param open  Logical. Should the new module file be opened after creation?
#'              Default \code{TRUE}.
#'
#' @return Nothing is returned. The new module is created at \code{path/name.R}.
#'
#' @export
#' @docType methods
#' @rdname newModule-method
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{## create a "fastfood" module in the "modules" subdirectory.}
#' \dontrun{newModule("fastfood", "modules")}
#'
setGeneric("newModule", function(name, path, open) {
  standardGeneric("newModule")
})

#' @rdname newModule-method
setMethod("newModule",
          signature=c(name="character", path="character", open="logical"),
          definition = function(name, path, open) {
            path <- checkPath(path, create=TRUE)
            filename <- file.path(path, paste0(name, ".R"))

            cat("
### MODULE: ", name, "
###
### DESCRIPTION: enter a brief description of your module here
###
###

### load any required packages
### (use `loadPackages`, or `library` directly)
pkgs <- list(\"SpaDES\")
loadPackages(pkgs)
rm(pkgs)

### event functions:
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - module name and this filename must match;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.

### template event
doEvent.", name, " = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType==\"init\") {
    ### check for module dependencies:
    ### (use or NULL if no dependencies exist)
    depends <- NULL

    ### check for object dependencies:
    ### (use `checkObject` or similar)


    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(sim, depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), \"", name, "\", \"init\")
    } else {
      sim <- ", name, "Init(sim)
    }
    sim <- scheduleEvent(sim, simParams(sim)$habitat$.plotInitialTime, \"", name, "\", \"plot\")
    sim <- scheduleEvent(sim, simParams(sim)$habitat$.saveInitialTime, \"", name, "\", \"save\")
  } else if (eventType==\"templateEvent\") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # schedule future event(s)
    sim <- scheduleEvent(sim, simCurrentTime(sim), \"", name, "\", \"init\")

    # ! ----- STOP EDITING ----- ! #
    } else {
      warning(paste(\"Undefined event type: \'\", simEvents(sim)[1, \"eventType\", with=FALSE],
                    \"\' in module \'\", simEvents(sim)[1, \"moduleName\", with=FALSE], \"\'\", sep=\"\"))
    }
  return(invisible(sim))
}

### template initilization
", name, "Init = function(sim) {

  # # ! ----- EDIT BELOW ----- ! #


  # ! ----- STOP EDITING ----- ! #

  # last thing to do is add module name to the loaded list
  simModulesLoaded(sim) <- append(simModulesLoaded(sim), \"", name, "\")

  return(invisible(sim))
}

### template for save events
", name, "Save = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  saveFiles(sim)

  # schedule future event(s)
  sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$", name, "$.saveInterval, \"", name, "\", \"save\")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
", name, "Event1 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  Plot()

  # schedule future event(s)
  sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$", name, "$.plotInterval, \"", name, "\", \"plot\")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
", name, "Event1 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event

  # schedule future event(s)
  sim <- scheduleEvent(sim, simCurrentTime(sim), \"", name, "\", \"event1\")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
", name, "Event2 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event

  # schedule future event(s)
  sim <- scheduleEvent(sim, simCurrentTime(sim), \"", name, "\", \"event2\")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above\n",
            file=filename, fill=FALSE, sep="")
            if(open) file.edit(filename)
})

#' @rdname newModule-method
setMethod("newModule",
          signature=c(name="character", path="missing", open="logical"),
          definition = function(name, open) {
            newModule(name=name, path=".", open=open)
})

#' @rdname newModule-method
setMethod("newModule",
          signature=c(name="character", path="character", open="missing"),
          definition = function(name, path) {
            newModule(name=name, path=path, open=TRUE)
})

#' @rdname newModule-method
setMethod("newModule",
          signature=c(name="character", path="missing", open="missing"),
          definition = function(name) {
            newModule(name=name, path=".", open=TRUE)
})
