#######################################################################
###     Modified from Matloff (2009):                               ###
###     - uses S4 classes for the sim objects                       ###
###     - uses `data.table` instead of `data.frame`                 ###
###     - implemented in a more modular fashion so it's easier      ###
###       to add submodules to the simulation                       ###
#######################################################################


###
### SimList and SimData methods
###

# initialize methods
setMethod("initialize",
          signature = "SimList",
          definition = function(.Object) {
              sim.events(.Object) = as.data.table(NULL)
              sim.time(.Object) = 0.0
              sim.debug(.Object) = FALSE
              return(.Object)
})

### show is already defined in the methods package
#' @title Show an Object
#' @name show
#' @rdname show-methods
#' @aliases show,SimList
#' @importMethodsFrom methods show
#' @export
#' 
setMethod("show",
          signature = "SimList",
          definition = function(object) {
              show = list()
#              show[["Modules Required:"]] = as.character(sim.modules(object))
#              show[["Modules Loaded:"]] = as.character(sim.loaded(object))
#              show[["Simulation Parameters:"]] = as.data.frame(sim.params(object))
#              show[["Current Simulation Time:"]] = sim.time(object)
#              show[["Next 5 Scheduled Events:"]] = head(sim.events(object), 5)
#              show[["Debugging Mode:"]] = sim.debug(object)

              show[["Modules Required:"]] = as.character(slot(object, "modules"))
              show[["Modules Loaded:"]] = as.character(slot(object, ".loaded"))
              show[["Simulation Parameters:"]] = as.data.frame(slot(object, "params"))
              show[["Current Simulation Time:"]] = slot(object, "simtime")
              show[["Next 5 Scheduled Events:"]] = head(slot(object, "events"), 5)
              show[["Debugging Mode:"]] = slot(object, "debug")
              print(show)
})

### get slot values using `slot(object, "slotname")`
### set slot values using `slot(object, "slotname") <- value`

# accessor methods for SimList slots
setGeneric("sim.modules", function(object) {
    standardGeneric("sim.modules")
})

setMethod("sim.modules",
          signature = "SimList",
          definition = function(object) {
              return(object@modules)
})

setGeneric("sim.modules<-",
           function(object, value) {
               standardGeneric("sim.modules<-")
})

setReplaceMethod("sim.modules",
                 signature="SimList",
                 function(object, value) {
                     object@modules <- value
                     validObject(object)
                     return(object)
})

setGeneric("sim.loaded", function(object) {
    standardGeneric("sim.loaded")
})

setMethod("sim.loaded",
          signature = "SimList",
          definition = function(object) {
              return(object@.loaded)
})

setGeneric("sim.loaded<-",
           function(object, value) {
               standardGeneric("sim.loaded<-")
})

setReplaceMethod("sim.loaded",
                 signature="SimList",
                 function(object, value) {
                     object@.loaded <- value
                     validObject(object)
                     return(object)
})

setGeneric("sim.params", function(object) {
    standardGeneric("sim.params")
})

setMethod("sim.params",
          signature = "SimList",
          definition = function(object) {
              return(object@params)
})

setGeneric("sim.params<-",
           function(object, value) {
               standardGeneric("sim.params<-")
})

setReplaceMethod("sim.params",
                 signature="SimList",
                 function(object, value) {
                     object@params <- value
                     validObject(object)
                     return(object)
})

setGeneric("sim.time", function(object) {
    standardGeneric("sim.time")
})

setMethod("sim.time",
          signature = "SimList",
          definition = function(object) {
              return(object@simtime)
})

setGeneric("sim.time<-",
           function(object, value) {
               standardGeneric("sim.time<-")
})

setReplaceMethod("sim.time",
                 signature="SimList",
                 function(object, value) {
                     object@simtime <- value
                     validObject(object)
                     return(object)
})

setGeneric("sim.events", function(object) {
    standardGeneric("sim.events")
})

setMethod("sim.events",
          signature = "SimList",
          definition = function(object) {
              return(object@events)
})

setGeneric("sim.events<-",
           function(object, value) {
               standardGeneric("sim.events<-")
})

setReplaceMethod("sim.events",
                 signature="SimList",
                 function(object, value) {
                     object@events <- value
                     validObject(object)
                     return(object)
})

setGeneric("sim.debug", function(object) {
    standardGeneric("sim.debug")
})

setMethod("sim.debug",
          signature = "SimList",
          definition = function(object) {
              return(object@debug)
})

setGeneric("sim.debug<-",
           function(object, value) {
               standardGeneric("sim.debug<-")
})

setReplaceMethod("sim.debug",
                 signature="SimList",
                 function(object, value) {
                     object@debug <- value
                     validObject(object)
                     return(object)
})

###
### initializes simulation variables
###
sim.init <- function(params, modules, path) {
    path <- check.path(path)
    
    sim <<- new("SimList")
    
    # load simulation parameters and modules
    sim.params(sim) <<- params
    sim.modules(sim) <<- modules # this should be a list of module names that will be loaded

    for (m in modules) {
        source(paste(path, m, ".R", sep="")) # source each module from file
    }
    # set up first event(s): all first events should be initialization events e.g. from modules
    #    schedule.event(EVENT.TIME, "MODULE.NAME", "EVENT.TYPE", list(OPTIONAL.ITEMS))
    for (m in modules) {
        schedule.event(0.00, m, "init")
    }
}

# print results of simulation
print.results <- function(modules, debug) {
    # THIS NEEDS ATTENTION!!
    #  it should print all global and module-specific stats.
    #  it should print these to file unless in debug mode
    print("Hi there, I don't do anything yet. Sorry!")

    ### 
    ### the way this is set up currently likely will (should) change:
    ### - outputs are schedules events;
    ### - all module outputs should be scheduled in the modules;
    ### - need method defs for `print()` of class `SimData`.
    ### 
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
schedule.event <- function(event.time, module.name, event.type) {
    new.event <- as.data.table(list(event.time=event.time,
                            module.name=module.name,
                            event.type=event.type))
    
    # if the event list is empty, set it to consist of evnt and return
    if (length(sim.events(sim))==0) {
        sim.events(sim) <<- new.event
        return()
    }
    
    # otherwise, "insert" by reconstructing the data frame;
    # find what portion of the current matrix should come before the new event,
    # and what portion should come after it, then bind everything together.
    before <- sim.events(sim)[event.time<=new.event$event.time[1]]
    after <- sim.events(sim)[event.time>new.event$event.time[1]]
    revised.list <- rbindlist(list(before,new.event,after))
    sim.events(sim) <<- setkey(revised.list, event.time)
}

# start to process next event;
#  second half done by application programmer via call to `do.event()`
get.next.event <- function() {
    head <- sim.events(sim)[1,]
    # delete head
    sim.events(sim) <<- sim.events(sim)[-1,]
    return(head)
}

#####################################################################################
# simulation body, takes the following arguments:
#   params:        list of application-specific parameters.
#   modules:       list of module names used in the simulation.
#   maxsimtime:    simulation will be run until this simulated time.
#   debug:         logical flag determines whether sim debug info will be printed.
dosim <- function(maxsimtime, params=list(), modules=list(), path="./", debug=FALSE) {
    # initialize the simulation
    sim.init(params, modules, path=path)
    
    # run the discrete event simulation
    while(sim.time(sim) < maxsimtime) {  
        head <- get.next.event()
        sim.time(sim) <<- head$event.time  # update current simulated time
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
