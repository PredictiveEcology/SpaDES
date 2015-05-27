################################################################################
#' The \code{simList} class
#'
#' Contains the minimum components of a SpaDES simulation.
#'
#' Based on code from chapter 7.8.3 of Matloff (2011): "Discrete event simulation".
#' Here, we implement a discrete event simulation in a more modular fashion so it's
#' easier to add simulation components (i.e., "simulation modules").
#'
#' We use S4 classes and methods, and use \code{\link{data.table}} instead of
#' \code{\link{data.frame}} to implement the event queue (because it is much faster).
#'
#' @slot .loadOrder Character vector of names specifying the order in which modules are to be loaded.
#'
#' @slot .loaded    List of character names specifying which modules and objects are currently loaded.
#'
#' @slot modules    List of character names specifying which modules to load.
#'
#' @slot params     Named list of potentially other lists specifying simulation parameters.
#'
#' @slot events     The list of scheduled events (i.e., event queue), as a \code{data.table}.
#'                  This event queue is always sorted (keyed) by time,
#'                  making it easy to insert new events into the queue.
#'
#' @slot completed  The list of completed events, as a \code{data.table}.
#'
#' @slot depends    A \code{.simDeps} list of \code{.moduleDeps} objects containing
#'                  module object dependency information.
#'
#' @slot simtimes   List of numerical values describing the simulation start and stop times;
#'                  as well as the current simulation time.
#'
#' @note Each event is represented by a \code{\link{data.table}} row consisting of:
#'        \tabular{ll}{
#'          \code{eventTime} \tab The time the event is to occur.\cr
#'          \code{moduleName} \tab The module from which the event is taken.\cr
#'          \code{eventType} \tab A character string for the programmer-defined event type.\cr
#'        }
#'
#' @seealso \code{\link{data.table}}.
#'
#' @include module-dependencies-class.R
#' @aliases simList
#' @rdname simList-class
#' @import data.table
#' @exportClass simList
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
#' @author Alex Chubaty
#'
setClass("simList",
         slots=list(.loadOrder="character", .loaded="list",
                    modules="list", params="list",
                    events="data.table", completed="data.table",
                    depends=".simDeps", simtimes="list"),
         prototype=list(.loadOrder=character(),
                        .loaded=list(modules=as.list(NULL), objects=as.list(NULL)),
                        modules=as.list(NULL),
                        params=list(.checkpoint=list(interval=NA_real_, file=NULL),
                                    .progress=list(graphical=NULL, interval=NULL)),
                        events=as.data.table(NULL), completed=as.data.table(NULL),
                        depends=new(".simDeps", dependencies=list(NULL)),
                        simtimes=list(current=0.00, start=0.00, stop=1.00)),
         validity=function(object) {
           # check for valid sim times
           if (is.na(object@simtimes$stop)) {
             stop("simulation stop time must be specified.")
           } else {
             if (object@simtimes$start >= object@simtimes$stop) {
               stop("simulation start time should occur before stop time.")
             }
           }
})



################################################################################
#' Simulation modules and dependencies
#'
#' Accessor functions for the \code{depends}, \code{modules}, \code{.loaded},
#' and \code{.loadOrder} slots in a \code{simList} object, within a \code{simEnv} object.
#' These are included for advanced users.
#' \tabular{ll}{
#'    \code{\link{simDepends}} \tab List of simulation module dependencies. (advanced) \cr
#'    \code{\link{simModules}} \tab List of simulation modules to be loaded. (advanced) \cr
#'    \code{\link{simModulesLoaded}} \tab List of loaded simulation modules. (advanced) \cr
#'    \code{\link{simModulesLoadOrder}} \tab List specifying the order in which to load modules. (advanced) \cr
#'    \code{\link{simObjectsLoaded}} \tab List of loaded objects used in simulation. (advanced) \cr
#' }
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @param object A \code{simEnv} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{simList-class}},
#'          \code{\link{simEnv-accessors-params}},
#'          \code{\link{simEnv-accessors-events}},
#'          \code{\link{simEnv-accessors-times}}.
#' @export
#' @docType methods
#' @aliases simEnv-accessors-modules
#' @rdname simEnv-accessors-modules
#'
#' @author Alex Chubaty
#'
setGeneric("simModules", function(object) {
  standardGeneric("simModules")
})

#' @rdname simEnv-accessors-modules
setMethod("simModules",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@modules)
})

#' @export
#' @rdname simEnv-accessors-modules
setGeneric("simModules<-",
           function(object, value) {
             standardGeneric("simModules<-")
})

#' @name simModules<-
#' @aliases simModules<-,simEnv-method
#' @rdname simEnv-accessors-modules
setReplaceMethod("simModules",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@modules <- value
                   validObject(object)
                   return(object)
 })

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simEnv-accessors-modules
#'
setGeneric("simModulesLoaded", function(object) {
  standardGeneric("simModulesLoaded")
})

#' @rdname simEnv-accessors-modules
setMethod("simModulesLoaded",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@.loaded$modules)
})

#' @export
#' @rdname simEnv-accessors-modules
setGeneric("simModulesLoaded<-",
           function(object, value) {
             standardGeneric("simModulesLoaded<-")
})

#' @name simModulesLoaded<-
#' @aliases simModulesLoaded<-,simEnv-method
#' @rdname simEnv-accessors-modules
setReplaceMethod("simModulesLoaded",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@.loaded$modules <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simEnv-accessors-modules
#'
setGeneric("simModulesLoadOrder", function(object) {
  standardGeneric("simModulesLoadOrder")
})

#' @rdname simEnv-accessors-modules
setMethod("simModulesLoadOrder",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@.loadOrder)
})

#' @export
#' @rdname simEnv-accessors-modules
setGeneric("simModulesLoadOrder<-",
           function(object, value) {
             standardGeneric("simModulesLoadOrder<-")
 })

#' @name simModulesLoadOrder<-
#' @aliases simModulesLoadOrder<-,simEnv-method
#' @rdname simEnv-accessors-modules
setReplaceMethod("simModulesLoadOrder",
                 signature="simEnv",
                 function(object, value) {
                   if (!is.null(value)) {
                     object$.sim@.loadOrder <- value
                   } else {
                     object$.sim@.loadOrder <- character()
                   }
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simEnv-accessors-modules
#'
setGeneric("simDepends", function(object) {
  standardGeneric("simDepends")
})

#' @rdname simEnv-accessors-modules
setMethod("simDepends",
          signature("simEnv"),
          definition=function(object) {
            return(object$.sim@depends)
})

#' @export
#' @rdname simEnv-accessors-modules
setGeneric("simDepends<-",
           function(object, value) {
             standardGeneric("simDepends<-")
})

#' @name simDepends<-
#' @aliases simDepends<-,simEnv-method
#' @rdname simEnv-accessors-modules
setReplaceMethod("simDepends",
                 signature("simEnv"),
                 function(object, value) {
                   object$.sim@depends <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @param ... arguments passed to \code{ls.str}, allowing, e.g. \code{all.names=TRUE}
#' @export
#' @docType methods
#' @rdname simEnv-accessors-modules
#'
setGeneric("simObjects", function(object, ...) {
  standardGeneric("simObjects")
})

#' @rdname simEnv-accessors-modules
setMethod("simObjects",
          signature="simEnv",
          definition=function(object, ...) {
            return(ls.str(object, ...))
})

#' @export
#' @rdname simEnv-accessors-modules
setGeneric("simObjects<-",
           function(object, value) {
             standardGeneric("simObjects<-")
})

#' @name simObjects<-
#' @aliases simObjects<-,simEnv-method
#' @rdname simEnv-accessors-modules
setReplaceMethod("simObjects",
                 signature="simEnv",
                 function(object, value) {
                   if (is.list(value)) {
                     lapply(value, function(x) {
                       object[[names(x)]] <- x
                     })
                   } else {
                     object[[names(value)]] <- value
                   }
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simEnv-accessors-modules
#'
setGeneric("simObjectsLoaded", function(object) {
  standardGeneric("simObjectsLoaded")
})

#' @rdname simEnv-accessors-modules
setMethod("simObjectsLoaded",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@.loaded$objects)
})

#' @export
#' @rdname simEnv-accessors-modules
setGeneric("simObjectsLoaded<-",
           function(object, value) {
             standardGeneric("simObjectsLoaded<-")
})

#' @name simObjectsLoaded<-
#' @aliases simObjectsLoaded<-,simEnv-method
#' @rdname simEnv-accessors-modules
setReplaceMethod("simObjectsLoaded",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@.loaded$objects <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' Get and set simulation parameters.
#'
#' Accessor functions for the \code{params} slot of a \code{simList} object
#' and its elements, within a \code{simEnv} object.
#' Additonal methods are provided to access core module and global parameters:
#' Commonly used
#' \tabular{ll}{
#'    \code{simGlobals} \tab List of global simulation parameters.\cr
#'    \code{simParams} \tab Nested list of all simulation parameter.\cr
#' }
#' Advanced use
#' \tabular{lll}{
#'    Accessor method \tab Module \tab Description \cr
#'    \code{simCheckpointFile} \tab \code{.checkpoint} \tab Name of the checkpoint file. (advanced)\cr
#'    \code{simCheckpointInterval} \tab \code{.checkpoint} \tab The simulation checkpoint interval. (advanced)\cr
#'    \code{simGlobalsOutputPath} \tab \code{NA} \tab Global simulation output path. (advanced)\cr
#'    \code{simFileList} \tab \code{.load} \tab List of files to load for the simulation. (advanced)\cr
#'    \code{simObjectsLoaded} \tab \code{.load} \tab List of loaded simulation objects. (advanced)\cr
#'    \code{simProgressGraphical} \tab \code{.progress} \tab Type of graphical progress bar used. (advanced)\cr
#'    \code{simProgressInterval} \tab \code{.progress} \tab Interval for the progress bar. (advanced)\cr
#' }
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @param object A \code{simEnv} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{simList-class}},
#'          \code{\link{simEnv-accessors-modules}},
#'          \code{\link{simEnv-accessors-events}},
#'          \code{\link{simEnv-accessors-times}}.
#' @export
#' @docType methods
#' @aliases simEnv-accessors-params
#' @rdname simEnv-accessors-params
#'
setGeneric("simParams", function(object) {
  standardGeneric("simParams")
})

#' @rdname simEnv-accessors-params
setMethod("simParams",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@params)
})

#' @export
#' @rdname simEnv-accessors-params
setGeneric("simParams<-",
           function(object, value) {
             standardGeneric("simParams<-")
})

#' @name simParams<-
#' @aliases simParams<-,simEnv-method
#' @rdname simEnv-accessors-params
setReplaceMethod("simParams",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@params <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simEnv-accessors-params
#'
setGeneric("simCheckpointFile", function(object) {
  standardGeneric("simCheckpointFile")
})

#' @rdname simEnv-accessors-params
setMethod("simCheckpointFile",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@params$.checkpoint$file)
})

#' @rdname simEnv-accessors-params
setGeneric("simCheckpointFile<-",
           function(object, value) {
             standardGeneric("simCheckpointFile<-")
})

#' @name simCheckpointFile<-
#' @aliases simCheckpointFile<-,simEnv-method
#' @rdname simEnv-accessors-params
setReplaceMethod("simCheckpointFile",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@params$.checkpoint$file <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simEnv-accessors-params
#'
setGeneric("simCheckpointInterval", function(object) {
  standardGeneric("simCheckpointInterval")
})

#' @rdname simEnv-accessors-params
setMethod("simCheckpointInterval",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@params$.checkpoint$interval)
})

#' @export
#' @rdname simEnv-accessors-params
setGeneric("simCheckpointInterval<-",
           function(object, value) {
             standardGeneric("simCheckpointInterval<-")
})

#' @name simCheckpointInterval<-
#' @aliases simCheckpointInterval<-,simEnv-method
#' @rdname simEnv-accessors-params
setReplaceMethod("simCheckpointInterval",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@params$.checkpoint$interval <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simEnv-accessors-params
#'
setGeneric("simFileList", function(object) {
  standardGeneric("simFileList")
})

#' @rdname simEnv-accessors-params
setMethod("simFileList",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@params$.load$fileList)
})

#' @export
#' @rdname simEnv-accessors-params
setGeneric("simFileList<-",
           function(object, value) {
             standardGeneric("simFileList<-")
})

#' @name simFileList<-
#' @aliases simFileList<-,simEnv-method
#' @rdname simEnv-accessors-params
setReplaceMethod("simFileList",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@params$.load$fileList <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simEnv-accessors-params
#'
setGeneric("simProgressGraphical", function(object) {
  standardGeneric("simProgressGraphical")
})

#' @rdname simEnv-accessors-params
setMethod("simProgressGraphical",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@params$.progress$graphical)
})

#' @rdname simEnv-accessors-params
setGeneric("simProgressGraphical<-",
           function(object, value) {
             standardGeneric("simProgressGraphical<-")
})

#' @name simProgressGraphical<-
#' @aliases simProgressGraphical<-,simEnv-method
#' @rdname simEnv-accessors-params
setReplaceMethod("simProgressGraphical",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@params$.progress$graphical <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simEnv-accessors-params
#'
setGeneric("simProgressInterval", function(object) {
  standardGeneric("simProgressInterval")
})

#' @rdname simEnv-accessors-params
setMethod("simProgressInterval",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@params$.progress$interval)
})

#' @export
#' @rdname simEnv-accessors-params
setGeneric("simProgressInterval<-",
           function(object, value) {
             standardGeneric("simProgressInterval<-")
})

#' @name simProgressInterval<-
#' @aliases simProgressInterval<-,simEnv-method
#' @rdname simEnv-accessors-params
setReplaceMethod("simProgressInterval",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@params$.progress$interval <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simEnv-accessors-params
#'
setGeneric("simGlobals", function(object) {
  standardGeneric("simGlobals")
})

#' @rdname simEnv-accessors-params
setMethod("simGlobals",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@params$.globals)
})

#' @export
#' @rdname simEnv-accessors-params
setGeneric("simGlobals<-",
           function(object, value) {
             standardGeneric("simGlobals<-")
})

#' @name simGlobals<-
#' @aliases simGlobals<-,simEnv-method
#' @rdname simEnv-accessors-params
setReplaceMethod("simGlobals",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@params$.globals <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simEnv-accessors-params
#'
setGeneric("simGlobalsOutputPath", function(object) {
  standardGeneric("simGlobalsOutputPath")
})

#' @rdname simEnv-accessors-params
setMethod("simGlobalsOutputPath",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@params$.globals$.outputPath)
})

#' @export
#' @rdname simEnv-accessors-params
setGeneric("simGlobalsOutputPath<-",
           function(object, value) {
             standardGeneric("simGlobalsOutputPath<-")
})

#' @name simGlobalsOutputPath<-
#' @aliases simGlobalsOutputPath<-,simEnv-method
#' @rdname simEnv-accessors-params
setReplaceMethod("simGlobalsOutputPath",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@params$.globals$.outputPath <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' Get and set simulation times.
#'
#' Accessor functions for the \code{simtimes} slot of a \code{simEnv} object
#' and its elements, within a \code{simEnv} object.
#' Additonal methods are provided to access the current, start, and stop times
#' of the simulation.
#' \tabular{ll}{
#'    \code{simCurrentTime} \tab Current simulation time.\cr
#'    \code{simStartTime} \tab Simulation start time.\cr
#'    \code{simStopTime} \tab Simulation stop time.\cr
#'    \code{simTimes} \tab List of all simulation times (current, start, stop).\cr
#' }
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @param object A \code{simEnv} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{simList-class}},
#'          \code{\link{simEnv-accessors-params}},
#'          \code{\link{simEnv-accessors-modules}},
#'          \code{\link{simEnv-accessors-events}}.
#' @export
#' @docType methods
#' @aliases simEnv-accessors-times
#' @rdname simEnv-accessors-times
#'
#' @author Alex Chubaty
#'
setGeneric("simTimes", function(object) {
  standardGeneric("simTimes")
})

#' @rdname simEnv-accessors-times
setMethod("simTimes",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@simtimes)
})

#' @export
#' @rdname simEnv-accessors-times
setGeneric("simTimes<-",
           function(object, value) {
             standardGeneric("simTimes<-")
})

#' @name simTimes<-
#' @aliases simTimes<-,simEnv-method
#' @rdname simEnv-accessors-times
setReplaceMethod("simTimes",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@simtimes <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simTimes
#' @export
#' @docType methods
#' @rdname simEnv-accessors-times
#'
setGeneric("simCurrentTime", function(object) {
  standardGeneric("simCurrentTime")
})

#' @rdname simEnv-accessors-times
setMethod("simCurrentTime",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@simtimes$current)
})

#' @export
#' @rdname simEnv-accessors-times
setGeneric("simCurrentTime<-",
           function(object, value) {
             standardGeneric("simCurrentTime<-")
})

#' @name simCurrentTime<-
#' @aliases simCurrentTime<-,simEnv-method
#' @rdname simEnv-accessors-times
setReplaceMethod("simCurrentTime",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@simtimes$current <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simTimes
#' @export
#' @docType methods
#' @rdname simEnv-accessors-times
#'
setGeneric("simStartTime", function(object) {
  standardGeneric("simStartTime")
})

#' @rdname simEnv-accessors-times
setMethod("simStartTime",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@simtimes$start)
})

#' @export
#' @rdname simEnv-accessors-times
setGeneric("simStartTime<-",
           function(object, value) {
             standardGeneric("simStartTime<-")
})

#' @name simStartTime<-
#' @aliases simStartTime<-,simEnv-method
#' @rdname simEnv-accessors-times
setReplaceMethod("simStartTime",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@simtimes$start <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simTimes
#' @export
#' @docType methods
#' @rdname simEnv-accessors-times
#'
setGeneric("simStopTime", function(object) {
  standardGeneric("simStopTime")
})

#' @rdname simEnv-accessors-times
setMethod("simStopTime",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@simtimes$stop)
})

#' @export
#' @rdname simEnv-accessors-times
setGeneric("simStopTime<-",
           function(object, value) {
             standardGeneric("simStopTime<-")
})

#' @name simStopTime<-
#' @aliases simStopTime<-,simEnv-method
#' @rdname simEnv-accessors-times
setReplaceMethod("simStopTime",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@simtimes$stop <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' Simulation event lists
#'
#' Accessor functions for the \code{events} and \code{completed} slots of a
#' \code{simList} object, within a \code{simEnv} object.
#' By default, the event lists are shown when the \code{simEnv} object is printed,
#' thus most users will not require direct use of these methods.
#' \tabular{ll}{
#'    \code{simEvents} \tab Scheduled simulation events (the event queue).\cr
#'    \code{simCompleted} \tab Completed simulation events.\cr
#' }
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @note Each event is represented by a \code{\link{data.table}} row consisting of:
#'        \tabular{ll}{
#'          \code{eventTime} \tab The time the event is to occur.\cr
#'          \code{moduleName} \tab The module from which the event is taken.\cr
#'          \code{eventType} \tab A character string for the programmer-defined event type.\cr
#'        }
#'
#' @param object A \code{simEnv} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{simList-class}},
#'          \code{\link{simEnv-accessors-params}},
#'          \code{\link{simEnv-accessors-modules}},
#'          \code{\link{simEnv-accessors-times}}.
#' @export
#' @docType methods
#' @aliases simEnv-accessors-events
#' @rdname simEnv-accessors-events
#'
setGeneric("simEvents", function(object) {
  standardGeneric("simEvents")
})

#' @rdname simEnv-accessors-events
setMethod("simEvents",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@events)
})

#' @export
#' @rdname simEnv-accessors-events
setGeneric("simEvents<-",
           function(object, value) {
             standardGeneric("simEvents<-")
})

#' @name simEvents<-
#' @aliases simEvents<-,simEnv-method
#' @rdname simEnv-accessors-events
setReplaceMethod("simEvents",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@events <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simEvents
#' @export
#' @docType methods
#' @rdname simEnv-accessors-events
#'
setGeneric("simCompleted", function(object) {
  standardGeneric("simCompleted")
})

#' @rdname simEnv-accessors-events
setMethod("simCompleted",
          signature="simEnv",
          definition=function(object) {
            return(object$.sim@completed)
})

#' @export
#' @rdname simEnv-accessors-events
setGeneric("simCompleted<-",
           function(object, value) {
             standardGeneric("simCompleted<-")
})

#' @name simCompleted<-
#' @aliases simCompleted<-,simEnv-method
#' @rdname simEnv-accessors-events
setReplaceMethod("simCompleted",
                 signature="simEnv",
                 function(object, value) {
                   object$.sim@completed <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' Add simulation dependencies
#'
#' Internal function.
#' Adds a \code{.moduleDeps} object to the simulation dependency list.
#'
#' @param sim A \code{simEnv} object.
#'
#' @param x   A named list containing the parameters used to construct a new
#'            \code{.moduleDeps} object.
#'
#' @return A \code{simEnv} object.
#'
#' @export
#' @docType methods
#' @rdname addSimDepends
#'
#' @author Alex Chubaty
#'
setGeneric(".addSimDepends", function(sim, x) {
  standardGeneric(".addSimDepends")
})

#' @rdname addSimDepends
setMethod(".addSimDepends",
          signature(sim="simEnv", x=".moduleDeps"),
          definition=function(sim, x) {
            deps <- simDepends(sim)
            n <- length(deps@dependencies)
            if (n==1L) {
              if (is.null(deps@dependencies[[1L]])) n <- 0L
            }
            deps@dependencies[[n+1L]] <- x
            dupes <- which(duplicated(deps@dependencies))
            if (length(dupes)) deps@dependencies <- deps@dependencies[-dupes]
            simDepends(sim) <- deps
            return(sim)
})

################################################################################
#' Define a new module.
#'
#' Specify a new module's metadata as well as object and package dependecies.
#' Packages are loaded during this call.
#'
#' @inheritParams .addSimDepends
#'
#' @return Updated \code{simEnv} object.
#'
#' @export
#' @docType methods
#' @rdname defineModule
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#'   moduleInfo <- list(...)
#'   defineModule(sim, moduleInfo)
#' }
#'
setGeneric("defineModule", function(sim, x) {
  standardGeneric("defineModule")
})

#' @rdname defineModule
setMethod("defineModule",
          signature(sim="simEnv", x="list"),
          definition=function(sim, x) {
            loadPackages(x$reqdPkgs)
            m <- do.call(new, c(".moduleDeps", x))
            return(.addSimDepends(sim, m))
})

################################################################################
#' Define a parameter used in a module
#'
#' Used to specify a parameter's name, value, and set a default.
#'
#' @param name      Character string giving the parameter name.
#' @param class     Character string giving the parameter class.
#' @param default   The default value to use when none is specified by the user.
#'                  Non-standard evaluation is used for the expression.
#'
#' @return data.frame
#'
#' @export
#' @docType methods
#' @rdname defineParameter
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#'   rbind(
#'    defineParameter("lambda", "numeric", 1.23),
#'    defineParameter("mu", "numeric", 1e-3),
#'    defineParameter("nu", "numeric", 1e-2)
#'   )
#' }
#'
setGeneric("defineParameter", function(name, class, default) {
  standardGeneric("defineParameter")
})

#' @rdname defineParameter
setMethod("defineParameter",
          signature(name="character", class="character", default="ANY"),
          definition=function(name, class, default) {
            df <- data.frame(name=name, class=class,
                             default=I(list(substitute(default))),
                             stringsAsFactors=FALSE)
            return(df)
})
