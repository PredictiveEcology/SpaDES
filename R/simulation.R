#######################################################################
###     DES-ABM.R:  R routines for discrete-event simulation (DES)  ###
###                                                                 ###
###     Modified from Matloff (2009):                               ###
###     - uses `data.table` instead of `data.frame`                 ###
###     - implemented in a more modular fashion so it's easier      ###
###       to add submodules to the simulation                       ###
#######################################################################

require(data.table)

##  OVERVIEW:
##
##  A global list named "sim" holds:
##       events: the events data.table;
##       currtime: the current simulated time;
##       debug: indicates debugging mode
##
##  Each event is represented by a data.table row consisting of:
##      event.time: the time the event is to occur;
##      module.name: the module from which the event is taken;
##      event.type: a character string for the programmer-defined event type;
##      : optional application-specific components.
##



# initializes global simulation variables
globals.init <- function(params, modules) {
    globals <<- list()
    
    # simulation parameters (global)
    globals$params <<- params
    
    # load simulation modules
    globals$.loaded <<- list()  # this keeps track of already loaded modules;
                                # add name of module to this list after loading.
    
    globals$modules <<- modules # this should be a list of module names that will be loaded
    for (m in modules) source(paste("module.", m, ".R", sep="")) # source each module from file
    
    # agents (loaded by modules)
    globals$agents <<- list()
    
    # data (loaded by modules)
    globals$data <<- list()
    
    # maps (loaded by modules)
    globals$maps <<- list()

    # statistics
    #    some will be "hard coded" here as global stats (e.g., execution time)
    globals$globalstats <<- list()   # name and init these accordingly
    
    # set up first event(s): all first events should be initialization events e.g. from modules
    #    schedule.event(EVENT.TIME, "MODULE.NAME", "EVENT.TYPE", list(OPTIONAL.ITEMS))
    time.init = 1e-8
    for (m in modules) {
        schedule.event(time.init, m, "init")
        time.init = time.init + 1e-8
    }
}

# print results of simulation
print.results <- function(modules, debug) {
    # THIS NEEDS ATTENTION!!
    #  it should print all global and module-specific stats.
    #  it should print these to file unless in debug mode
    print(globals$globalstats, debug)
#    print.module.stats(debug) # should be done by each module
}

# event processing function called by dosim() below
do.event <- function(head) {
    # instead of having a massive list of ifelse cases for each event type,
    # we should have the cases processed by the submodule;
    # this makes things more modular, since we can add/remove modules without
    # having to worry about updating this (hardcoded) list.
    
    module.call <- paste("do.event", head$module.name, sep=".")
#    check.validity(module.call) # do it here, otherwise user must do it
    # per module in the do.event function?
    get(module.call)(head$event.time, head$event.type)
    
    # e.g., this would produce the following call to the fire module:
    #   do.event.fire(TIME, "TYPE")
}

# insert event with time `time.event` and type `type.event` into event list;
# other.info is an optional set of application-specific traits of this event,
# specified in the form a list with named components
schedule.event <- function(event.time, module.name, event.type, other.info=NULL) {
    # forms a row for an event of type `type.event` from the module `name.module`
    #  that will occur at time `time.event`;
    # see comments in `schedule.event()` regarding `other.info`
    new.event <- as.data.table(c(list(event.time=event.time,
                                      module.name=module.name,
                                      event.type=event.type),
                                 other.info)) # `other.info` should be a named list
    setkey(new.event, event.time)
    
    # if the event list is empty, set it to consist of evnt and return
    if (length(sim$events)==0) {
        sim$events <<- new.event
        return()
    }
    
    # otherwise, "insert" by reconstructing the data frame;
    # find what portion of the current matrix should come before the new event,
    # and what portion should come after it, then bind everything together.
    before <- sim$events[event.time<new.event$event.time[1]]
    after <- sim$events[event.time>new.event$event.time[1]]
    sim$events <<- setkey(rbind(before,new.event,after), event.time)
}

# start to process next event;
#  second half done by application programmer via call to `do.event()`
get.next.event <- function() {
    head <- sim$events[1,]
    # delete head
    if (nrow(sim$events) == 1) {
        sim$events <<- NULL
    } else sim$events <<- sim$events[-1,]
    return(head)
}

# simulation body, takes the following arguments:
#   globals.init:  application-specific initialization function;
#                   inits globals to statistical totals for the app, etc.;
#                   records params in globals;
#                   schedules the first event.
#   do.event:       application-specific event handling function, coding the
#                   proper action for each type of event.
#   print.results: prints application-specific results.
#   params:        list of application-specific parameters.
#   modules:       list of module names used in the simulation.
#   maxsimtime:    simulation will be run until this simulated time.
#   debug:         logical flag determines whether sim debug info will be printed.
dosim <- function(globals.init, do.event, print.results, maxsimtime, params=NULL, modules=NULL, debug=FALSE) {
    sim <<- list()
    sim$currtime <<- 0.0  # current simulated time
    sim$events <<- NULL  # events data table (filled in by `schedule.event()`)
    sim$debug <<- debug
    
    globals.init(params, modules)
    while(sim$currtime < maxsimtime) {  
        head <- get.next.event()
        sim$currtime <<- head$event.time  # update current simulated time
        do.event(head)  # process this event 
        
        # print debugging info
        #  this can, and should, be more sophisticated;
        #  i.e., don't simply print the entire object
        if (debug) {
            print(sim)
        }
    }
    
    # print simulation results
    print.results(modules, debug)
}

# to run, use this function call:
#   dosim(globals.init, do.event, print.results,
#       maxsimtime=10000.0,
#       params=list(FILL.THIS.IN),
#       modules=list(FILL.THIS.IN.TOO),
#       debug=FALSE)
