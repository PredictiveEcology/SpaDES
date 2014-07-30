###########################################################################
#' The \code{simList} class
#'
#' This class contains the minimum components of a simulation.
#'
#' Based on code from Matloff (2011) ch. 7.8.3 on discrete event simulation:
#' - implemented using S4 classes and methods;
#' - uses `data.table` instead of `data.frame` (which is much faster);
#' - implemented in a more modular fashion so it's easier
#'   to add submodules to the simulation.
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
#' @slot completed  The list of completed events, as a data.table class.
#'
#' @slot simtimes   List of numerical values describing the simulation start and stop timos,
#'                  and the current simulation time.
#'
#' @slot debug      Logical value specifying whether to run simulation in debugging mode.
#'
#' @note Each event is represented by a data.table row consisting of:
#'          eventTime: the time the event is to occur;
#'          moduleName: the module from which the event is taken;
#'          eventType: a character string for the programmer-defined event type.
#'
#' @seealso \code{\link{data.table}}
#'
#' @name simList
#' @aliases simList-class
#' @rdname simList-class
#' @import data.table
#' @exportClass simList
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (373 pp.). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
setClass("simList",
         slots=list(.loaded="list", modules="list", params="list",
                    events="data.table", completed="ANY",
                    simtimes="list", debug="logical"
))

### initialize is already defined in the methods package
#' initialize simList
#'
#' @param times     A named list of simulation start and stop times
#'                  (e.g., \code{times=list(start=0.00, stop=10.00)}).
#'
#' @export
#'
#' @author Alex Chubaty
#'
setMethod("initialize",
          signature = "simList",
          definition = function(.Object, ..., times=list(start=0.00, stop=NA_real_)) {
            # check for valid sim times and make default list
            if (is.na(times$stop)) {
              stop("simulation stop time must be specified.")
            } else {
              if (times$start >= times$stop) {
                stop("simulation start time should occur before stop time.")
              } else {
                simtimes <- list(current=times$start, start=times$start, stop=times$stop)
              }
            }

            # set default slot values
            simEvents(.Object) <- as.data.table(NULL)
            simEventsCompleted(.Object) <- as.data.table(NULL)
            simTimes(.Object) <- simtimes # validated list of sim times
            simDebug(.Object) <- FALSE
            .Object <- callNextMethod(.Object, ..., simtimes=simtimes)
            return(.Object)
})

### show is already defined in the methods package
#' show simList
#'
#' @export
setMethod("show",
          signature = "simList",
          definition = function(object) {
              show = list()
              show[["Modules Required:"]] = as.character(simModules(object))
              show[["Modules Loaded:"]] = as.character(simLoaded(object))
              show[["Simulation Parameters:"]] = as.list(simParams(object))
              show[["Current Simulation Time:"]] = simTimes(object)
              show[["Past 5 Completed Events:"]] = tail(simEventsCompleted(object), 5)
              show[["Next 5 Scheduled Events:"]] = head(simEvents(object), 5)
              show[["Debugging Mode:"]] = simDebug(object)
              print(show)
})


##############################################################
#' Accessor methods for \code{simList} object slots
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' Additonal methods are provided to access the current, start, and stop times of the
#' simulation: \code{simCurrentTime(sim)}, \code{simStartTime(sim)}, \code{simStopTime(sim)}.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simModules-accessor-methods
#'
#' @author Alex Chubaty
#'
setGeneric("simModules", function(object) {
    standardGeneric("simModules")
})

#' get list of simulation modules
#' @rdname simModules-accessor-methods
setMethod("simModules",
          signature = "simList",
          definition = function(object) {
              return(object@modules)
})

#' set list of simulation modules
#' @export
#' @rdname simModules-accessor-methods
setGeneric("simModules<-",
           function(object, value) {
               standardGeneric("simModules<-")
})

#' set list of simulation modules
#' @name <-
#' @rdname simModules-accessor-methods
setReplaceMethod("simModules",
                 signature="simList",
                 function(object, value) {
                     object@modules <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Accessor methods for \code{simList} object slots
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' Additonal methods are provided to access the current, start, and stop times of the
#' simulation: \code{simCurrentTime(sim)}, \code{simStartTime(sim)}, \code{simStopTime(sim)}.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simLoaded-accessor-methods
#'
#' @export
#'
#' @author Alex Chubaty
#'
setGeneric("simLoaded", function(object) {
    standardGeneric("simLoaded")
})

#' get list of loaded simulation modules
#' @rdname simLoaded-accessor-methods
setMethod("simLoaded",
          signature = "simList",
          definition = function(object) {
              return(object@.loaded)
})

#' set list of loaded simulation modules
#' @export
#' @rdname simLoaded-accessor-methods
setGeneric("simLoaded<-",
           function(object, value) {
               standardGeneric("simLoaded<-")
})

#' set list of loaded simulation modules
#' @name <-
#' @rdname simLoaded-accessor-methods
setReplaceMethod("simLoaded",
                 signature="simList",
                 function(object, value) {
                     object@.loaded <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Accessor methods for \code{simList} object slots
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' Additonal methods are provided to access the current, start, and stop times of the
#' simulation: \code{simCurrentTime(sim)}, \code{simStartTime(sim)}, \code{simStopTime(sim)}.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simParams-accessor-methods
#'
#' @export
#'
#' @author Alex Chubaty
#'
setGeneric("simParams", function(object) {
    standardGeneric("simParams")
})

#' get list of simulation parameters
#' @rdname simParams-accessor-methods
setMethod("simParams",
          signature = "simList",
          definition = function(object) {
              return(object@params)
})

#' set list of simulation parameters
#' @export
#' @rdname simParams-accessor-methods
setGeneric("simParams<-",
           function(object, value) {
               standardGeneric("simParams<-")
})

#' set list of simulation parameters
#' @name <-
#' @rdname simParams-accessor-methods
setReplaceMethod("simParams",
                 signature="simList",
                 function(object, value) {
                     object@params <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Accessor methods for \code{simList} object slots
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' Additonal methods are provided to access the current, start, and stop times of the
#' simulation: \code{simCurrentTime(sim)}, \code{simStartTime(sim)}, \code{simStopTime(sim)}.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simParams-accessor-methods
#'
#' @export
#'
#' @author Alex Chubaty
#'
setGeneric("simTimes", function(object) {
    standardGeneric("simTimes")
})

#' get list of simulation times
#' @rdname simTimes-accessor-methods
setMethod("simTimes",
          signature = "simList",
          definition = function(object) {
              return(object@simtimes)
})

#' set list of simulation times
#' @export
#' @rdname simTimes-accessor-methods
setGeneric("simTimes<-",
           function(object, value) {
               standardGeneric("simTimes<-")
})

#' set list of simulation times
#' @name <-
#' @rdname simTimes-accessor-methods
setReplaceMethod("simTimes",
                 signature="simList",
                 function(object, value) {
                     object@simtimes <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Accessor methods for \code{simList} object slots
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' Additonal methods are provided to access the current, start, and stop times of the
#' simulation: \code{simCurrentTime(sim)}, \code{simStartTime(sim)}, \code{simStopTime(sim)}.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simCurrentTime-accessor-methods
#'
#' @export
#'
#' @author Alex Chubaty
#'
setGeneric("simCurrentTime", function(object) {
    standardGeneric("simCurrentTime")
})

#' get the current simulation time
#' @rdname simCurrentTime-accessor-methods
setMethod("simCurrentTime",
          signature = "simList",
          definition = function(object) {
              return(object@simtimes$current)
})

#' set the current simulation time
#' @export
#' @rdname simCurrentTime-accessor-methods
setGeneric("simCurrentTime<-",
           function(object, value) {
               standardGeneric("simCurrentTime<-")
})

#' set the current simulation time
#' @name <-
#' @rdname simCurrentTime-accessor-methods
setReplaceMethod("simCurrentTime",
                 signature="simList",
                 function(object, value) {
                     object@simtimes$current <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Accessor methods for \code{simList} object slots
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' Additonal methods are provided to access the current, start, and stop times of the
#' simulation: \code{simCurrentTime(sim)}, \code{simStartTime(sim)}, \code{simStopTime(sim)}.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simStartTime-accessor-methods
#'
#' @author Alex Chubaty
#'
setGeneric("simStartTime", function(object) {
    standardGeneric("simStartTime")
})

#' get the simulation start time
#' @rdname simStartTime-accessor-methods
setMethod("simStartTime",
          signature = "simList",
          definition = function(object) {
              return(object@simtimes$start)
          })

#' set the simulation start time
#' @export
#' @rdname simStartTime-accessor-methods
setGeneric("simStartTime<-",
           function(object, value) {
               standardGeneric("simStartTime<-")
           })

#' set the simulation start time
#' @name <-
#' @rdname simStartTime-accessor-methods
setReplaceMethod("simStartTime",
                 signature="simList",
                 function(object, value) {
                     object@simtimes$start <- value
                     validObject(object)
                     return(object)
                 })

##############################################################
#' Accessor methods for \code{simList} object slots
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' Additonal methods are provided to access the current, start, and stop times of the
#' simulation: \code{simCurrentTime(sim)}, \code{simStartTime(sim)}, \code{simStopTime(sim)}.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simStopTime-accessor-methods
#'
#' @author Alex Chubaty
#'
setGeneric("simStopTime", function(object) {
    standardGeneric("simStopTime")
})

#' get the simulation stop time
#' @rdname simStopTime-accessor-methods
setMethod("simStopTime",
          signature = "simList",
          definition = function(object) {
              return(object@simtimes$stop)
})

#' set the simulation stop time
#' @export
#' @rdname simStopTime-accessor-methods
setGeneric("simStopTime<-",
           function(object, value) {
               standardGeneric("simStopTime<-")
})

#' set the simulation stop time
#' @name <-
#' @rdname simStopTime-accessor-methods
setReplaceMethod("simStopTime",
                 signature="simList",
                 function(object, value) {
                     object@simtimes$stop <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Accessor methods for \code{simList} object slots
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' Additonal methods are provided to access the current, start, and stop times of the
#' simulation: \code{simCurrentTime(sim)}, \code{simStartTime(sim)}, \code{simStopTime(sim)}.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simEvents-accessor-methods
#'
#' @author Alex Chubaty
#'
setGeneric("simEvents", function(object) {
    standardGeneric("simEvents")
})

#' get the simulation event queue
#' @rdname simEvents-accessor-methods
setMethod("simEvents",
          signature = "simList",
          definition = function(object) {
              return(object@events)
})

#' set the simulation event queue
#' @export
#' @rdname simEvents-accessor-methods
setGeneric("simEvents<-",
           function(object, value) {
               standardGeneric("simEvents<-")
})

#' set the simulation event queue
#' @name <-
#' @rdname simEvents-accessor-methods
setReplaceMethod("simEvents",
                 signature="simList",
                 function(object, value) {
                     object@events <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Accessor methods for \code{simList} object slots
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' Additonal methods are provided to access the current, start, and stop times of the
#' simulation: \code{simCurrentTime(sim)}, \code{simStartTime(sim)}, \code{simStopTime(sim)}.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simEventsCompleted-accessor-methods
#'
#' @author Eliot McIntire
#'
setGeneric("simEventsCompleted", function(object) {
  standardGeneric("simEventsCompleted")
})

#' get the simulation event queue
#' @rdname simEventsCompleted-accessor-methods
setMethod("simEventsCompleted",
          signature="simList",
          definition=function(object) {
            return(object@completed)
})

#' set the simulation event queue
#' @export
#' @rdname simEventsCompleted-accessor-methods
setGeneric("simEventsCompleted<-",
           function(object, value) {
             standardGeneric("simEventsCompleted<-")
})

#' set the simulation event queue
#' @name <-
#' @rdname simEventsCompleted-accessor-methods
setReplaceMethod("simEventsCompleted",
                 signature="simList",
                 function(object, value) {
                   object@completed <- value
                   validObject(object)
                   return(object)
})

##############################################################
#' Accessor methods for \code{simList} object slots
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' Additonal methods are provided to access the current, start, and stop times of the
#' simulation: \code{simCurrentTime(sim)}, \code{simStartTime(sim)}, \code{simStopTime(sim)}.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simDebug-accessor-methods
#'
#' @author Alex Chubaty
#'
setGeneric("simDebug", function(object) {
    standardGeneric("simDebug")
})

#' get the simulation debug toggle
#' @rdname simDebug-accessor-methods
setMethod("simDebug",
          signature = "simList",
          definition = function(object) {
              return(object@debug)
})

#' set the simulation debug toggle
#' @export
#' @rdname simDebug-accessor-methods
setGeneric("simDebug<-",
           function(object, value) {
               standardGeneric("simDebug<-")
})

#' set the simulation debug toggle
#' @name <-
#' @rdname simDebug-accessor-methods
setReplaceMethod("simDebug",
                 signature="simList",
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
#' Based on code from Matloff (2011) ch. 7.8.3 on discrete event simulation:
#' - implemented using S4 classes and methods;
#' - uses `data.table` instead of `data.frame` (which is much faster);
#' - implemented in a more modular fashion so it's easier
#'   to add submodules to the simulation.
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
#' @return A \code{simList} simulation object, pre-initialized from values specified
#' in the arguments supplied.
#'
#' @seealso \code{\link{doSim}}.
#'
#' @export
#' @docType methods
#' @rdname simInit-method
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (373 pp.). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{mySim <- simInit(times=list(start=0.0, stop=10.0), params=list(Ncaribou=100),
#' modules=list("habitat", "caribou"), path="/path/to/my/modules/")}
#' \dontrun{mySim}
setGeneric("simInit", function(times, params, modules, path) {
    standardGeneric("simInit")
})

#' simInit
#' @rdname simInit-method
#'
setMethod("simInit",
          signature(times="list", params="list", modules="list", path="character"),
          definition = function(times, params, modules, path) {
              # check validity of all inputs
              path <- checkPath(path, create=TRUE)
              #params <- checkParams(params)
              #modules <- checkModules(modules)

              # create new simList object
              sim <- new("simList", times=times)

              # default/built-in modules:  (should we be hardcoding this??)

              defaults <- list("checkpoint","save","progress")

              simModules(sim) <- append(defaults, modules)
              simParams(sim) <- params

              # load user-defined modules
              for (m in modules) {
                  # source the code from each module's R file
                  source(paste(path, "/", m, ".R", sep=""))

                  # schedule each module's init event:
                  sim <- scheduleEvent(sim, 0.00, m, "init")
              }

              # load "default" modules (should we be hardcoding this??)
              for (d in defaults) {
                  # schedule each module's init event:
                  sim <- scheduleEvent(sim, 0.00, d, "init")
              }

              return(sim)
})

#' simInit
#' @rdname simInit-method
setMethod("simInit",
          signature(times="list", params="list", modules="list", path="missing"),
          definition = function(times, params, modules) {
              return(simInit(times=times, params=params, modules=modules, path="./"))
})

##############################################################
#' Load modules for simulation.
#'
#' Checks the dependencies of the current module on other modules.
#' These dependencies need to be loaded first, so if they are not
#' already loaded, hold off loading the current module until after
#' dependencies are loaded.
#'
#' @param sim An object of class \code{simList}.
#'
#' @param depends A list of character strings specifying the names
#'                of modules upon which the current module depends.
#'
#' @return \code{Logical}.
#'
#' @seealso \code{\link{library}}.
#'
#' @export
#' @docType methods
#' @rdname loadmodules
#'
#' @author Alex Chubaty
#'
# @examples
# need examples
setGeneric("reloadModuleLater", function(sim, depends) {
  standardGeneric("reloadModuleLater")
})

#' @rdname loadmodules
setMethod("reloadModuleLater",
          signature(depends="character"),
          definition = function(sim, depends) {
            if (depends=="NONE") {
              return(FALSE)
            } else {
              return(!all(depends %in% simLoaded(sim)))
            }
})

##############################################################
#' Process a simulation event
#'
#' Internal function called from \code{doSim}.
#'
#' Calls the module corresponding to the event call, and executes the event.
#'
#' Based on code from Matloff (2011) ch. 7.8.3 on discrete event simulation:
#' - implemented using S4 classes and methods;
#' - uses `data.table` instead of `data.frame` (which is much faster);
#' - implemented in a more modular fashion so it's easier
#'   to add submodules to the simulation.
#'
#' @param sim A \code{simList} simulation object.
#'
#' @param debug Optional logical flag determines whether sim debug info
#'              will be printed (default is \code{debug=FALSE}).
#'
#' @return Returns the modified \code{simList} object.
#'
#' @import data.table
#' @export
#' @docType methods
#' @rdname doEvent-method
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (373 pp.). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
setGeneric("doEvent", function(sim, debug) {
    standardGeneric("doEvent")
})

#' doEvent
#' @rdname doEvent-do-event-method
setMethod("doEvent",
          signature(sim="simList", debug="logical"),
          definition = function(sim, debug) {
              # get next event
              nextEvent <- simEvents(sim)[1,]       # extract the next event from queue

              # update current simulated time
              simCurrentTime(sim) <- nextEvent$eventTime

              # call the module responsible for processing this event
              moduleCall <- paste("doEvent", nextEvent$moduleName, sep=".")

              # check the module call for validity
              if(nextEvent$moduleName %in% simModules(sim)) {
                  sim <- get(moduleCall)(sim, nextEvent$eventTime, nextEvent$eventType, debug)
              } else {
                  errormsg <- paste("ERROR: Invalid module call. The module ",
                                     nextEvent$moduleName,
                                     " wasn't specified to be loaded.", sep="")
                  stop(errormsg)
              }

              # now that it is run, without error, remove it from the queue
              simEvents(sim) <- simEvents(sim)[-1,]

              # add to list of completed events
              if(length(simEventsCompleted(sim))==0) {
                simEventsCompleted(sim) <- setkey(nextEvent, eventTime)
              } else {
                simEventsCompleted(sim) <- setkey(rbindlist(list(simEventsCompleted(sim), nextEvent)), eventTime)
              }
              return(sim)
})

#' doEvent
#' @rdname doEvent-do-event-method
setMethod("doEvent",
          signature(sim="simList", debug="missing"),
          definition = function(sim) {
              sim <- doEvent(sim, debug=FALSE)
              return(sim)
})

##############################################################
#' Schedule a simulation event
#'
#' Adds a new event to the simulation's event queue, updating the simulation object.
#'
#' Based on code from Matloff (2011) ch. 7.8.3 on discrete event simulation:
#' - implemented using S4 classes and methods;
#' - uses `data.table` instead of `data.frame` (which is much faster);
#' - implemented in a more modular fashion so it's easier
#'   to add submodules to the simulation.
#'
#' @param sim           A \code{simList} simulation object.
#'
#' @param eventTime    A numeric specifying the time of the next event.
#'
#' @param moduleName   A character string specifying the module from which to call the event.
#'
#' @param eventType    A character string specifying the type of event from within the module.
#'
#' @return Returns the modified \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname scheduleEvent-method
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (373 pp.). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{scheduleEvent(x, 10.5, "firemodule", "burn")}
setGeneric("scheduleEvent", function(sim, eventTime, moduleName, eventType) {
    standardGeneric("scheduleEvent")
})

#' schedule event
#' @rdname scheduleEvent-method
setMethod("scheduleEvent",
          signature(sim="simList", eventTime="numeric",
                    moduleName="character", eventType="character"),
          definition = function(sim, eventTime, moduleName, eventType) {
              newEvent <- as.data.table(list(eventTime=eventTime,
                                              moduleName=moduleName,
                                              eventType=eventType))

              # if the event list is empty, set it to consist of evnt and return;
              # otherwise, insert new event and re-sort (rekey).
              if (length(simEvents(sim))==0) {
                  simEvents(sim) <- setkey(newEvent, eventTime)
              } else {
                  simEvents(sim) <- setkey(rbindlist(list(simEvents(sim), newEvent)), eventTime)
              }
              return(sim)
})

##############################################################
#' Process a simulation event
#'
#' Internal function called from \code{doSim}.
#'
#' Calls the module corresponding to the event call, and executes the event.
#'
#' Based on code from Matloff (2011) ch. 7.8.3 on discrete event simulation:
#' - implemented using S4 classes and methods;
#' - uses `data.table` instead of `data.frame` (which is much faster);
#' - implemented in a more modular fashion so it's easier
#'   to add submodules to the simulation.
#'
#' @param sim A \code{simList} simulation object.
#'
#' @param debug Optional logical flag determines whether sim debug info
#'              will be printed (default is \code{debug=FALSE}).
#'
#' @return Returns the modified \code{simList} object.
#'
#' @seealso \code{\link{simInit}}.
#'
#' @note The debug option is primarily intended to facilitate building simulation
#' models by the user. Setting \code{debug=TRUE} allows the user to toggle debugging
#' statements in their own modules.
#'
#' @export
#' @docType methods
#' @rdname doSim-method
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (373 pp.). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{mySim <- simInit(times=list(start=0.0, stop=10.0), params=list(Ncaribou=100),
#' modules=list("habitat", "caribou"), path="/path/to/my/modules/)}
#' \dontrun{doSim{mySim}}
setGeneric("doSim", function(sim, debug) {
    standardGeneric("doSim")
})

#' doSim
#' @rdname doSim-method
setMethod("doSim",
          signature(sim="simList", debug="logical"),
          definition = function(sim, debug) {
              # run the discrete event simulation

              while(simCurrentTime(sim) <= simStopTime(sim)) {
                  sim <- doEvent(sim, debug)  # process the next event

                  # print debugging info
                  #  this can, and should, be more sophisticated;
                  #  i.e., don't simply print the entire object
                  if (debug) {
                      print(sim)
                  }
              }
#              close(pb)
              return(sim)
})

#' doSim
#' @rdname doSim-method
setMethod("doSim",
          signature(sim="simList", debug="missing"),
          definition = function(sim) {
              sim <- doSim(sim, debug=FALSE)
              return(sim)
})
