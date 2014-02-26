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
#  second half done by application programmer via call to `react.event()`
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
#   react.event:   application-specific event handling function, coding the
#                   proper action for each type of event.
#   print.results: prints application-specific results.
#   params:        list of application-specific parameters.
#   modules:       list of module names used in the simulation.
#   maxsimtime:    simulation will be run until this simulated time.
#   debug:         logical flag determines whether sim debug info will be printed.
dosim <- function(globals.init, react.event, print.results, maxsimtime, params=NULL, modules=NULL, debug=FALSE) {
    sim <<- list()
    sim$currtime <<- 0.0  # current simulated time
    sim$events <<- NULL  # events data table (filled in by `schedule.event()`)
    sim$debug <<- debug
    
    globals.init(params)
    while(sim$currtime < maxsimtime) {  
        head <- get.next.event()
        sim$currtime <<- head$event.time  # update current simulated time
        react.event(head)  # process this event 
        
        # print debugging info
        #  this can, and should, be more sophisticated;
        #  i.e., don't simply print the enitre object
        if (debug) {
            print(sim)
        }
    }
    
    # print simulation results
    print.results(modules, debug)
}
