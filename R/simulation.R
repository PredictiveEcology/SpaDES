###########################################################################
###                                                                     ###
###     Based on code from chapter 7.8.3 on discrete event simulation:  ###
###         Matloff, N. (2011). The Art of R Programming (373 pp.).     ###
###             San Fransisco, CA: No Starch Press, Inc.                ###
###             Retrieved from http://www.nostarch.com/artofr.htm       ###
###                                                                     ###
###     - implemented using S4 classes and methods                      ###
###     - uses `data.table` instead of `data.frame`                     ###
###     - implemented in a more modular fashion so it's easier          ###
###       to add submodules to the simulation                           ###
###                                                                     ###
###########################################################################

#' The \code{SimList} class
#'
#' This class contains the minimum components of a simulation.
#'
#' @slot .loaded    List of character names specifying which modules are currently loaded.
#' 
#' @slot modules    List of character names specifying which modules to load.
#' 
#' @slot params     Named list of potentially other lists specifying simulation parameters.
#' 
#' @slot events     The list of scheduled events, as a data.table class. This is implemented
#'                  such that the data.table is always sorted (keyed) by time, making it easy
#'                  to insert new events into the table.
#' 
#' @slot simtimes    List of numerical values describing the simulation start and stop timos,
#'                  and the current simulation time.
#' 
#' @slot debug      Logical value specifying whether to run simulation in debugging mode.
#'
#' @note Each event is represented by a data.table row consisting of:
#'          event.time: the time the event is to occur;
#'          module.name: the module from which the event is taken;
#'          event.type: a character string for the programmer-defined event type.
#' 
#' @seealso \code{\link{data.table}}
#' 
#' @name SimList
#' @aliases SimList-class
#' @rdname SimList-class
#' @import data.table
#' @exportClass SimList
#' 
setClass("SimList",
         slots=list(.loaded="list", modules="list", params="list",
                    events="data.table", simtimes="list", debug="logical"
))

### initialize is already defined in the methods package
#' initialize SimList
#' 
#' @param times     A named list of simulation start and stop times
#'                  (e.g., \code{times=list(start=0.00, stop=10.00)}).
#' 
#' @export
setMethod("initialize",
          signature = "SimList",
          definition = function(.Object, ..., times=list(start=0.00, stop=NA_real_)) {
              # check for valid sim times and make default list
              if (is.na(times$stop)) {
                  stop("ERROR: simulation stop time must be specified.")
              } else {
                  if (times$start >= times$stop) {
                      stop("ERROR: simulation start time should occur before stop time.")
                  } else {
                      simtimes <- list(current=times$start, start=times$start, stop=times$stop)
                  }
              }
                            
              # set default slot values
              sim.events(.Object) = as.data.table(NULL)
              sim.times(.Object) = simtimes # validated list of sim times
              sim.debug(.Object) = FALSE
              .Object <- callNextMethod(.Object, ..., simtimes=simtimes)
              return(.Object)
})

### show is already defined in the methods package
#' show SimList
#' 
#' @export
setMethod("show",
          signature = "SimList",
          definition = function(object) {
              show = list()
              show[["Modules Required:"]] = as.character(sim.modules(object))
              show[["Modules Loaded:"]] = as.character(sim.loaded(object))
              show[["Simulation Parameters:"]] = as.list(sim.params(object))
              show[["Current Simulation Time:"]] = sim.times(object)
              show[["Next 5 Scheduled Events:"]] = head(sim.events(object), 5)
              show[["Debugging Mode:"]] = sim.debug(object)
              print(show)
})


##############################################################
#' Accessor methods for \code{SimList} object slots
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#' 
#' Additonal methods are provided to access the current, start, and stop times of the
#' simulation: \code{currentTime(sim)}, \code{startTime(sim)}, \code{stopTime(sim)}.
#' 
#' @param object A \code{SimList} simulation object.
#' 
#' @param value The object to be stored at the slot.
#' 
#' @return Returns or sets the value of the slot from the \code{SimList} object.
#' 
#' @export
#' @docType methods
#' @rdname simulation-accessor-methods
#' 
setGeneric("sim.modules", function(object) {
    standardGeneric("sim.modules")
})

#' get list of simulation modules
#' 
setMethod("sim.modules",
          signature = "SimList",
          definition = function(object) {
              return(object@modules)
})

#' set list of simulation modules
#' @export
#' @rdname simulation-accessor-methods
setGeneric("sim.modules<-",
           function(object, value) {
               standardGeneric("sim.modules<-")
})

#' set list of simulation modules
#' @name <-
#' @rdname simulation-accessor-methods
setReplaceMethod("sim.modules",
                 signature="SimList",
                 function(object, value) {
                     object@modules <- value
                     validObject(object)
                     return(object)
})

#' get list of loaded simulation modules
#' @export
#' @rdname simulation-accessor-methods
setGeneric("sim.loaded", function(object) {
    standardGeneric("sim.loaded")
})

#' get list of loaded simulation modules
#' @rdname simulation-accessor-methods
setMethod("sim.loaded",
          signature = "SimList",
          definition = function(object) {
              return(object@.loaded)
})

#' set list of loaded simulation modules
#' @export
#' @rdname simulation-accessor-methods
setGeneric("sim.loaded<-",
           function(object, value) {
               standardGeneric("sim.loaded<-")
})

#' set list of loaded simulation modules
#' @name <-
#' @rdname simulation-accessor-methods
setReplaceMethod("sim.loaded",
                 signature="SimList",
                 function(object, value) {
                     object@.loaded <- value
                     validObject(object)
                     return(object)
})

#' get list of simulation parameters
#' @export
#' @rdname simulation-accessor-methods
setGeneric("sim.params", function(object) {
    standardGeneric("sim.params")
})

#' get list of simulation parameters
#' @rdname simulation-accessor-methods
setMethod("sim.params",
          signature = "SimList",
          definition = function(object) {
              return(object@params)
})

#' set list of simulation parameters
#' @export
#' @rdname simulation-accessor-methods
setGeneric("sim.params<-",
           function(object, value) {
               standardGeneric("sim.params<-")
})

#' set list of simulation parameters
#' @name <-
#' @rdname simulation-accessor-methods
setReplaceMethod("sim.params",
                 signature="SimList",
                 function(object, value) {
                     object@params <- value
                     validObject(object)
                     return(object)
})

#' get list of simulation times
#' @export
#' @rdname simulation-accessor-methods
setGeneric("sim.times", function(object) {
    standardGeneric("sim.times")
})

#' get list of simulation times
#' @rdname simulation-accessor-methods
setMethod("sim.times",
          signature = "SimList",
          definition = function(object) {
              return(object@simtimes)
})

#' set list of simulation times
#' @export
#' @rdname simulation-accessor-methods
setGeneric("sim.times<-",
           function(object, value) {
               standardGeneric("sim.times<-")
})

#' set list of simulation times
#' @name <-
#' @rdname simulation-accessor-methods
setReplaceMethod("sim.times",
                 signature="SimList",
                 function(object, value) {
                     object@simtimes <- value
                     validObject(object)
                     return(object)
})

#' get the current simulation time
#' @export
#' @rdname simulation-accessor-methods
setGeneric("currentTime", function(object) {
    standardGeneric("currentTime")
})

#' get the current simulation time
#' @rdname simulation-accessor-methods
setMethod("currentTime",
          signature = "SimList",
          definition = function(object) {
              return(object@simtimes$current)
})

#' set the current simulation time
#' @export
#' @rdname simulation-accessor-methods
setGeneric("currentTime<-",
           function(object, value) {
               standardGeneric("currentTime<-")
})

#' set the current simulation time
#' @name <-
#' @rdname simulation-accessor-methods
setReplaceMethod("currentTime",
                 signature="SimList",
                 function(object, value) {
                     object@simtimes$current <- value
                     validObject(object)
                     return(object)
})

#' get the simulation start time
#' @export
#' @rdname simulation-accessor-methods
setGeneric("startTime", function(object) {
    standardGeneric("startTime")
})

#' get the simulation start time
#' @rdname simulation-accessor-methods
setMethod("startTime",
          signature = "SimList",
          definition = function(object) {
              return(object@simtimes$start)
          })

#' set the simulation start time
#' @export
#' @rdname simulation-accessor-methods
setGeneric("startTime<-",
           function(object, value) {
               standardGeneric("startTime<-")
           })

#' set the simulation start time
#' @name <-
#' @rdname simulation-accessor-methods
setReplaceMethod("startTime",
                 signature="SimList",
                 function(object, value) {
                     object@simtimes$start <- value
                     validObject(object)
                     return(object)
                 })

#' get the simulation stop time
#' @export
#' @rdname simulation-accessor-methods
setGeneric("stopTime", function(object) {
    standardGeneric("stopTime")
})

#' get the simulation stop time
#' @rdname simulation-accessor-methods
setMethod("stopTime",
          signature = "SimList",
          definition = function(object) {
              return(object@simtimes$stop)
})

#' set the simulation stop time
#' @export
#' @rdname simulation-accessor-methods
setGeneric("stopTime<-",
           function(object, value) {
               standardGeneric("stopTime<-")
})

#' set the simulation stop time
#' @name <-
#' @rdname simulation-accessor-methods
setReplaceMethod("stopTime",
                 signature="SimList",
                 function(object, value) {
                     object@simtimes$stop <- value
                     validObject(object)
                     return(object)
})

#' get the simulation event queue
#' @export
#' @rdname simulation-accessor-methods
setGeneric("sim.events", function(object) {
    standardGeneric("sim.events")
})

#' get the simulation event queue
#' @rdname simulation-accessor-methods
setMethod("sim.events",
          signature = "SimList",
          definition = function(object) {
              return(object@events)
})

#' set the simulation event queue
#' @export
#' @rdname simulation-accessor-methods
setGeneric("sim.events<-",
           function(object, value) {
               standardGeneric("sim.events<-")
})

#' set the simulation event queue
#' @name <-
#' @rdname simulation-accessor-methods
setReplaceMethod("sim.events",
                 signature="SimList",
                 function(object, value) {
                     object@events <- value
                     validObject(object)
                     return(object)
})

#' get the simulation debug toggle
#' @export
#' @rdname simulation-accessor-methods
setGeneric("sim.debug", function(object) {
    standardGeneric("sim.debug")
})

#' get the simulation debug toggle
#' @rdname simulation-accessor-methods
setMethod("sim.debug",
          signature = "SimList",
          definition = function(object) {
              return(object@debug)
})

#' set the simulation debug toggle
#' @export
#' @rdname simulation-accessor-methods
setGeneric("sim.debug<-",
           function(object, value) {
               standardGeneric("sim.debug<-")
})

#' set the simulation debug toggle
#' @name <-
#' @rdname simulation-accessor-methods
setReplaceMethod("sim.debug",
                 signature="SimList",
                 function(object, value) {
                     object@debug <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Initialize a new simulation
#'
#' Create a new simulation object, preloaded with parameters,
#' modules, times, etc.
#' 
#' @param times A named list of numeric simulation start and stop times,
#' of the form \code{times=list(start=0.0, stop=10.0)}.
#'
#' @param params A named list of simulation parameters and their values.
#' 
#' @param modules A named list of character strings specfying the names
#' of modules to be loaded for the simulation. **Note:** the module name
#' should correspond to the R source file from which the module is loaded.
#' Example: a module named "caribou" will be sourced form the file \code{caribou.R},
#' located at the specified \code{path} (see below).
#' 
#' @param path An optional character string specifying the location of the module source files.
#' If no path is specified, it defaults to the current working directory.
#'
#' @return A \code{SimList} simulation object, pre-initialized from values specified
#' in the arguments supplied.
#' 
#' @seealso \code{\link{dosim}}.
#' 
#' @export
#' @docType methods
#' @rdname simulation-init-method
#'
#' @examples
#' \dontrun{mySim <- sim.init(times=list(start=0.0, stop=10.0), params=list(Ncaribou=100),
#' modules=list("habitat", "caribou"), path="/path/to/my/modules/")}
#' \dontrun{mySim}
setGeneric("sim.init", function(times, params, modules, path) {
    standardGeneric("sim.init")
})

#' sim.init
#' @rdname simulation-init-method
setMethod("sim.init",
          signature(times="list", params="list", modules="list", path="character"),
          definition = function(times, params, modules, path) {
              # check validity of all inputs
              path <- check.path(path, create=TRUE)
              #params <- check.params(params)
              #modules <- check.modules(modules)
              
              # create new SimList object
              sim <- new("SimList", times=times)
              
              # default/built-in modules:  (should we be hardcoding this??)
              defaults <- list("checkpoint")
              
              sim.modules(sim) <- append(defaults, modules)
              sim.params(sim) <- params
              
              # load user-defined modules
              for (m in modules) {
                  # source the code from each module's R file
                  source(paste(path, "/", m, ".R", sep=""))
                  
                  # schedule each module's init event:
                  sim <- schedule.event(sim, 0.00, m, "init")
              }
              
              # load "default" modules (should we be hardcoding this??)
              for (d in defaults) {
                  # schedule each module's init event:
                  sim <- schedule.event(sim, 0.00, d, "init")
              }
              
              return(sim)
})

#' sim.init
#' @rdname simulation-init-method
setMethod("sim.init",
          signature(times="list", params="list", modules="list", path="missing"),
          definition = function(times, params, modules) {
              sim <- sim.init(times=times, params=params, modules=modules, path="./")
              return(sim)
})

##############################################################
#' Process a simulation event
#'
#' Internal function called from \code{dosim}.
#' 
#' Calls the module corresponding to the event call, and executes the event.
#' E.g., if the next event in the SimList event queue was:
#' 
#' \code{}
#' 
#' the following call would to the "fire" module would be produced:
#' 
#' \code{do.event.fire(sim, TIME, "TYPE", debug)}.
#' 
#' @param sim A \code{SimList} simulation object.
#' 
#' @param debug Optional logical flag determines whether sim debug info will be printed.
#' If not specified, the default is \code{debug=FALSE}.
#'
#' @return Returns the modified \code{SimList} object.
#' 
#' @import data.table
#' @export
#' @docType methods
#' @rdname simulation-do-event-method
#' 
setGeneric("do.event", function(sim, debug) {
    standardGeneric("do.event")
})

#' do.event
#' @rdname simulation-do-event-method
setMethod("do.event",
          signature(sim="SimList", debug="logical"),
          definition = function(sim, debug) {
              # get next event
              next.event <- sim.events(sim)[1,]       # extract the next event from queue
              sim.events(sim) <- sim.events(sim)[-1,] # remove this event from the queue
              
              # update current simulated time
              currentTime(sim) <- next.event$event.time
              
              # call the module responsible for processing this event
              module.call <- paste("do.event", next.event$module.name, sep=".")
              
              # check the module call for validity
              if(next.event$module.name %in% sim.modules(sim)) {
                  sim <- get(module.call)(sim, next.event$event.time, next.event$event.type, debug)
              } else {
                  error.msg <- paste("ERROR: Invalid module call. The module ",
                                     next.event$module.name,
                                     " wasn't specified to be loaded.", sep="")
                  stop(error.msg)
              }  
              
              return(sim)
})

#' do.event
#' @rdname simulation-do-event-method
setMethod("do.event",
          signature(sim="SimList", debug="missing"),
          definition = function(sim) {
              sim <- do.event(sim, debug=FALSE)
              return(sim)
})

##############################################################
#' Schedule a simulation event
#'
#' Adds a new event to the simulation's event queue, updating the simulation object.
#' 
#' @param sim           A \code{SimList} simulation object.
#' 
#' @param event.time    A numeric specifying the time of the next event.
#' 
#' @param module.name   A character string specifying the module from which to call the event.
#' 
#' @param event.type    A character string specifying the type of event from within the module.
#'
#' @return Returns the modified \code{SimList} object.
#' 
# @seealso \code{\link{SIMULATION-MODULES}}.
#' 
#' @export
#' @docType methods
#' @rdname simulation-schedule-event-method
#'
#' @examples
#' \dontrun{schedule.event(x, 10.5, "firemodule", "burn")}
setGeneric("schedule.event", function(sim, event.time, module.name, event.type) {
    standardGeneric("schedule.event")
})

#' schedule event
#' @rdname simulation-schedule-event-method
setMethod("schedule.event",
          signature(sim="SimList", event.time="numeric", module.name="character", event.type="character"),
          definition = function(sim, event.time, module.name, event.type) {
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
})

##############################################################
#' Process a simulation event
#'
#' Internal function called from \code{dosim}.
#' 
#' Calls the module corresponding to the event call, and executes the event.
#' 
#' @param sim A \code{SimList} simulation object.
#' 
#' @param debug Optional logical flag determines whether sim debug info will be printed.
#' If not specified, the default is \code{debug=FALSE}.
#'
#' @return Returns the modified \code{SimList} object.
#' 
#' @seealso \code{\link{sim.init}}.
#' 
#' @note The debug option is primarily intended to facilitate building simulation
#' models by the user. Setting \code{debug=TRUE} allows the user to toggle debugging
#' statements in their own modules.
#' 
#' @export
#' @docType methods
#' @rdname simulation-dosim-method
#'
#' @examples
#' \dontrun{mySim <- sim.init(times=list(start=0.0, stop=10.0), params=list(Ncaribou=100),
#' modules=list("habitat", "caribou"), path="/path/to/my/modules/)}
#' \dontrun{dosim{mySim}}
setGeneric("dosim", function(sim, debug) {
    standardGeneric("dosim")
})

#' dosim
#' @rdname simulation-dosim-method-method
setMethod("dosim",
          signature(sim="SimList", debug="logical"),
          definition = function(sim, debug) {
              # run the discrete event simulation
              while(currentTime(sim) < stopTime(sim)) {
                  sim <- do.event(sim, debug)  # process the next event
                  
                  # print debugging info
                  #  this can, and should, be more sophisticated;
                  #  i.e., don't simply print the entire object
                  if (debug) {
                      print(sim)
                  }
              }
              
              return(sim)
})

#' dosim
#' @rdname simulation-dosim-method
setMethod("dosim",
          signature(sim="SimList", debug="missing"),
          definition = function(sim) {
              sim <- dosim(sim, debug=FALSE)
              return(sim)
})
