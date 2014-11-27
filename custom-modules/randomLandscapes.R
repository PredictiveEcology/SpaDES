
### MODULE: randomLandscapes
###
### DESCRIPTION: enter a brief description of your module here
###
###

### load any required packages
### (use `loadPackages`, or `library` directly)
pkgs <- list("SpaDES")
loadPackages(pkgs)
rm(pkgs)

### event functions:
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - module name and this filename must match;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.

### template event
doEvent.randomLandscapes = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for module dependencies:
    ### (use or NULL if no dependencies exist)
    depends <- NULL

    ### check for object dependencies:
    ### (use `checkObject` or similar)


    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(sim, depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), "randomLandscapes", "init")
    } else {
      sim <- randomLandscapesInit(sim)
    }
    sim <- scheduleEvent(sim, simParams(sim)$habitat$.plotInitialTime, "randomLandscapes", "plot")
    sim <- scheduleEvent(sim, simParams(sim)$habitat$.saveInitialTime, "randomLandscapes", "save")
  } else if (eventType=="templateEvent") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # schedule future event(s)
    sim <- scheduleEvent(sim, simCurrentTime(sim), "randomLandscapes", "init")

    # ! ----- STOP EDITING ----- ! #
    } else {
      warning(paste("Undefined event type: '", simEvents(sim)[1, "eventType", with=FALSE],
                    "' in module '", simEvents(sim)[1, "moduleName", with=FALSE], "'", sep=""))
    }
  return(invisible(sim))
}

### template initilization
randomLandscapesInit = function(sim) {

  # # ! ----- EDIT BELOW ----- ! #


  # ! ----- STOP EDITING ----- ! #

  # last thing to do is add module name to the loaded list
  simModulesLoaded(sim) <- append(simModulesLoaded(sim), "randomLandscapes")

  return(invisible(sim))
}

### template for save events
randomLandscapesSave = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  saveFiles(sim)

  # schedule future event(s)
  sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$randomLandscapes$.saveInterval, "randomLandscapes", "save")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
randomLandscapesEvent1 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  Plot()

  # schedule future event(s)
  sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$randomLandscapes$.plotInterval, "randomLandscapes", "plot")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
randomLandscapesEvent1 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event

  # schedule future event(s)
  sim <- scheduleEvent(sim, simCurrentTime(sim), "randomLandscapes", "event1")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
randomLandscapesEvent2 = function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event

  # schedule future event(s)
  sim <- scheduleEvent(sim, simCurrentTime(sim), "randomLandscapes", "event2")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
