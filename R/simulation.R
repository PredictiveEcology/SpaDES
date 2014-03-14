# DES application:  agent-based modelling simulation

# to run, use this function call:
#   dosim(abm.globals.init, abm.react.event, abm.print.results,
#       maxsimtime=10000.0,
#       params=list(FILL.THIS.IN),
#       modules=list(FILL.THIS.IN.TOO),
#       debug=FALSE)

# initializes global variables specific to this app
abm.globals.init <- function(params, modules) {
   abm.globals <<- list()
   
   # simulation parameters (global)
   abm.globals$params <<- params
   
   # load simulation modules
   abm.globals$modules <<- modules # this should be a list of module names
   for (m in modules) source(paste("module.", m, ".R", sep="")) # source each module from file
   
   # statistics
   #    some will be "hard coded" here as global stats (e.g., execution time)
   abm.globals$globalstats <<- list()   # name and init these accordingly
   
   # set up first event(s): all first events should be initialization events e.g. from modules
   #    schedule.event(EVENT.TIME, "MODULE.NAME", "EVENT.TYPE", list(OPTIONAL.ITEMS))
   time.init = 1e-8
   for (m in modules) {
       schedule.event(time.init, m, "init")
       time.init = time.init + 1e-8
   }
}

# event processing function called by dosim() in the general DES library 
abm.react.event <- function(head) {
    # instead of having a massive list of ifelse cases for each event type,
    # we should have the cases processed by the submodule;
    # this makes things more modular, since we can add/remove modules without
    # having to worry about updating this (hardcoded) list.
    
    module.call <- paste(head$module.name, "react.event", sep=".")
    check.validity(module.call) # do it here, otherwise user must do it
                                # per module in the react.event function?
    get(module.call)(head$event.time, head$event.type)
    
    # e.g., this would produce the following call to the fire module:
    #   module.fire.react.event(TIME, "TYPE")
}

abm.print.results <- function(modules, debug) {
    # THIS NEEDS ATTENTION!!
    #  it should print all global and module-specific stats.
    #  it should print these to file unless in debug mode
    if (debug) {
        # print to file
    } else {
        print(abm.globals$globalstats)
        print(abm.globals$modulestats)
    }
}


