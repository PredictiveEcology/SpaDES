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
          definition = function(.Object, times=list(start=0.0, stop=10.0)) {
              sim.events(.Object) = as.data.table(NULL)
              sim.times(.Object) = list(current=times$start, start=times$start, stop=times$stop)
              sim.debug(.Object) = FALSE
              return(.Object)
})

### show is already defined in the methods package
#' show SimList
#' @export
#' 
setMethod("show",
          signature = "SimList",
          definition = function(object) {
              show = list()
#              show[["Modules Required:"]] = as.character(sim.modules(object))
#              show[["Modules Loaded:"]] = as.character(sim.loaded(object))
#              show[["Simulation Parameters:"]] = as.data.frame(sim.params(object))
#              show[["Current Simulation Time:"]] = sim.times(object)
#              show[["Next 5 Scheduled Events:"]] = head(sim.events(object), 5)
#              show[["Debugging Mode:"]] = sim.debug(object)

              show[["Modules Required:"]] = as.character(slot(object, "modules"))
              show[["Modules Loaded:"]] = as.character(slot(object, ".loaded"))
              show[["Simulation Parameters:"]] = as.data.frame(slot(object, "params"))
              show[["Simulation Times:"]] = slot(object, "simtimes")
              show[["Next 5 Scheduled Events:"]] = head(slot(object, "events"), 5)
              show[["Debugging Mode:"]] = slot(object, "debug")
              print(show)
})

### get slot values using `slot(object, "slotname")`
### set slot values using `slot(object, "slotname") <- value`

##### accessor methods for SimList slots
### modules
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

### .loaded
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

### params
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

### simulation times
setGeneric("sim.times", function(object) {
    standardGeneric("sim.times")
})

setMethod("sim.times",
          signature = "SimList",
          definition = function(object) {
              return(object@simtimes)
})

setGeneric("sim.times<-",
           function(object, value) {
               standardGeneric("sim.times<-")
})

setReplaceMethod("sim.times",
                 signature="SimList",
                 function(object, value) {
                     object@simtimes <- value
                     validObject(object)
                     return(object)
})

# current simulation time
setGeneric("currentTime", function(object) {
    standardGeneric("currentTime")
})

setMethod("currentTime",
          signature = "SimList",
          definition = function(object) {
              return(object@simtimes$current)
})

setGeneric("currentTime<-",
           function(object, value) {
               standardGeneric("currentTime<-")
})

setReplaceMethod("currentTime",
                 signature="SimList",
                 function(object, value) {
                     object@simtimes$current <- value
                     validObject(object)
                     return(object)
})

# simulation start time
setGeneric("startTime", function(object) {
    standardGeneric("startTime")
})

setMethod("startTime",
          signature = "SimList",
          definition = function(object) {
              return(object@simtimes$start)
          })

setGeneric("startTime<-",
           function(object, value) {
               standardGeneric("startTime<-")
           })

setReplaceMethod("startTime",
                 signature="SimList",
                 function(object, value) {
                     object@simtimes$start <- value
                     validObject(object)
                     return(object)
                 })

# simulation stop time
setGeneric("stopTime", function(object) {
    standardGeneric("stopTime")
})

setMethod("stopTime",
          signature = "SimList",
          definition = function(object) {
              return(object@simtimes$stop)
})

setGeneric("stopTime<-",
           function(object, value) {
               standardGeneric("stopTime<-")
})

setReplaceMethod("stopTime",
                 signature="SimList",
                 function(object, value) {
                     object@simtimes$stop <- value
                     validObject(object)
                     return(object)
})

### events list
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

### debug
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


### check validity of module call
check.validity = function(sim, module.call) {
    # this function should check to make sure:
    # - the module is currently loaded
    # - it is structures correctly
    return(module.call)
}





### initializes simulation variables
#
#   times:      named list of simulation start and stop times
#   params:     named list of application-specific parameters.
#   modules:    named list of module names used in the simulation.
#
sim.init <- function(times, params, modules, path) {
    # check to make sure times are valid:
    if ( is.numeric(times$start) && is.numeric(times$stop) ) {
        if (times$start >= times$stop) {
            stop("ERROR: simulation start time should occur before stop time.")
        }
    } else {
        stop("ERROR: simulation times should be numeric.")
    }
    
    path <- check.path(path, create=TRUE)
    
    sim <- new("SimList", times=times)
    
    # load simulation parameters and modules
    sim.params(sim) <- params
    sim.modules(sim) <- modules # this should be a list of module names that will be loaded

    for (m in modules) {
        # source the code from each module's R file
        source(paste(path, "/", m, ".R", sep=""))
        
        # schedule each module's init event:
        #    sim <- schedule.event(sim, EVENT.TIME, "MODULE.NAME", "EVENT.TYPE")
        sim <- schedule.event(sim, 0.00, m, "init")
    }
    
    return(sim)
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
do.event <- function(sim) {
    # get next event
    next.event <- sim.events(sim)[1,]       # extract the next event from list
    sim.events(sim) <- sim.events(sim)[-1,] # remove this event from the list
            
    # update current simulated time
    currentTime(sim) <- next.event$event.time

    # call the module responsible for processing this event
    module.call <- paste("do.event", next.event$module.name, sep=".")
    module.call <- check.validity(sim, module.call)
    sim <- get(module.call)(sim, next.event$event.time, next.event$event.type)
    # e.g., this would produce the following call to the fire module:
    #   do.event.fire(TIME, "TYPE")
    
    return(sim)
}

# insert event with time `time.event` and type `type.event` into event list;
# other.info is an optional set of application-specific traits of this event,
# specified in the form a list with named components
schedule.event <- function(sim, event.time, module.name, event.type) {
    new.event <- as.data.table(list(event.time=event.time,
                            module.name=module.name,
                            event.type=event.type))
    
    # if the event list is empty, set it to consist of evnt and return;
    # otherwise, "insert" by reconstructing the data frame.
    if (length(sim.events(sim))==0) {
        sim.events(sim) <- new.event
    } else {
        # find what portion of the current matrix should come before the new event,
        # and what portion should come after it, then bind everything together.
        before <- sim.events(sim)[event.time<=new.event$event.time[1]]
        after <- sim.events(sim)[event.time>new.event$event.time[1]]
        revised.list <- rbindlist(list(before,new.event,after))
        sim.events(sim) <- setkey(revised.list, event.time)
    }
    return(sim)
}

#####################################################################################
# simulation body, takes the following arguments:
#   sim:    SimList object
#   debug:  logical flag determines whether sim debug info will be printed.
dosim <- function(sim, debug=FALSE) {

    # run the discrete event simulation
    while(currentTime(sim) < stopTime(sim)) {  
        sim <- do.event(sim)  # process the next event 
        
        # print debugging info
        #  this can, and should, be more sophisticated;
        #  i.e., don't simply print the entire object
        if (debug) {
            print(sim)
        }
    }
    
    # print simulation results
    print.results(modules, debug)
    
    return(sim)
}
