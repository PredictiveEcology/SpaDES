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
   abm.globals$param1 <<- params$param1 # do this for each parameter
   
   # simulation modules
   abm.globals$modules <<- modules # this should be a list of module names
   for (m in modules) source(paste(m, ".R", sep="")) # load each module from file
   
   # other simulation parameters (e.g., from modules)
   abm.globals$module1.param1 <<- params$param1 # do this for each module and each parameter
   
   # statistics
   #    some will be "hard coded" here as global stats, but most will be module-specific
   abm.globals$globalstats <<- list()   # name and init these accordingly
   abm.globals$modulestats <<- list()   # name and init these accordingly for each module
   
   # set up first event
   schedule.event(EVENT.TIME, "MODULE.NAME", "EVENT.TYPE", list(OPTIONAL.ITEMS)) # modify this accordingly
}

# event processing function called by dosim() in the general DES library 
abm.react.event <- function(head) {
    # instead of having a massive list of ifelse cases for each event type,
    # we should have the cases processed by the submodule;
    # this makes things more modular, since we can add/remove modules without
    # having to worry about updating this (hardcoded) list.
    
    module.call <- paste(head$module.name, "react.event", sep=".")
    get(module.call)(head$event.time, head$event.type)
    
    # e.g., this would produce the following call to the fire module:
    #   module.fire.react.event(TIME, "TYPE")

#     if (head$evnttype == "arrv") {  # arrival
#       # if server free, start service, else add to queue (added to queue
#       # even if empty, for convenience)
#       if (length(abm.globals$srvq) == 0) {
#          abm.globals$srvq <<- head$arrvtime
#          srvdonetime <- sim$currtime + rexp(1,abm.globals$srvrate)
#          schedule.event(srvdonetime,"srvdone",list(arrvtime=head$arrvtime))
#       } else abm.globals$srvq <<- c(abm.globals$srvq,head$arrvtime)
#       # generate next arrival
#       arrvtime <- sim$currtime + rexp(1,abm.globals$arrvrate)
#       schedule.event(arrvtime,"arrv",list(arrvtime=arrvtime))
#    } else {  # service done
#       # process job that just finished
#       # do accounting
#       abm.globals$njobsdone <<- abm.globals$njobsdone + 1
#       abm.globals$totwait <<- 
#          abm.globals$totwait + sim$currtime - head$arrvtime
#       # remove from queue
#       abm.globals$srvq <<- abm.globals$srvq[-1]
#       # more still in the queue?
#       if (length(abm.globals$srvq) > 0) {
#          # schedule new service
#          srvdonetime <- sim$currtime + rexp(1,abm.globals$srvrate)
#          schedule.event(srvdonetime,"srvdone",list(arrvtime=abm.globals$srvq[1]))
#       }
#    }
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


