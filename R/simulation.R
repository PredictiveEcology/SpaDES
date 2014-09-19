###########################################################################
#' The \code{simList} class
#'
#' This class contains the minimum components of a simulation.
#'
#' Based on code from chapter 7.8.3 of Matloff (2011): "Discrete event simulation".
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods, and use `data.table`
#' instead of `data.frame` to implement the event queue (because it is much faster).
#'
#' @slot .loaded    List of character names specifying which modules and objects are currently loaded.
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
#' @note Each event is represented by a data.table row consisting of:
#'          eventTime: the time the event is to occur;
#'          moduleName: the module from which the event is taken;
#'          eventType: a character string for the programmer-defined event type.
#'
#' @seealso \code{\link{data.table}}
#'
#' @rdname simList-class
#' @import data.table
#' @exportClass simList
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
setClass("simList",
         slots=list(.loaded="list", modules="list", params="list",
                    events="data.table", completed="data.table", simtimes="list"),
         prototype=list(.loaded=list(modules=as.list(NULL), objects=as.list(NULL)),
                        modules=as.list(NULL), params=as.list(NULL),
                        events=as.data.table(NULL), completed=as.data.table(NULL),
                        simtimes=list(current=0.00, start=0.00, stop=1.00)),
         validity=function(object) {
           # check for valid sim times and make default list
           if (is.na(object@simtimes$stop)) {
             stop("simulation stop time must be specified.")
           } else {
             if (object@simtimes$start >= object@simtimes$stop) {
               stop("simulation start time should occur before stop time.")
             }
           }
         }
)

### show is already defined in the methods package
#' show simList
#' @export
setMethod("show",
          signature="simList",
          definition=function(object) {
            out = list()

            ### hr
            out[[1]] = capture.output(cat(rep("=", getOption("width"), sep=""), "\n", sep=""))

            ### simtimes
            out[[2]] = capture.output(cat(">> Simulation times:\n"))
            out[[3]] = capture.output(print(rbind(simTimes(object))))
            out[[4]] = capture.output(cat("\n"))

            ### modules loaded
            out[[5]] = capture.output(cat(">> Modules:\n"))
            out[[6]] = capture.output(print(cbind(ModuleName=simModules(object),
                          IsLoaded=simModules(object) %in%
                            simModulesLoaded(object)),
                          quote=FALSE, row.names=FALSE))
            out[[7]] = capture.output(cat("\n"))

            ### file/objects loaded
            files = simFileList(object)[["files"]]
            if (!is.null(simFileList(object)[["files"]])) {
              if (is.null(simFileList(object)[["objectNames"]])) {
                names = fileName(files)
              } else {
                names = objectNames
              }
            }
            out[[8]] = capture.output(cat(">> Objects Loaded:\n"))
            out[[9]] = capture.output(print(cbind(ObjectName=simObjectsLoaded(object)#,
                                                  #Contains=names(get(simObjectsLoaded(object), envir=.GlobalEnv))
                                                  ),
                                            quote=FALSE, row.names=FALSE))
            out[[10]] = capture.output(cat("\n"))
            ### params
            omit = which(names(simParams(object))==".loadFileList" |
                           names(simParams(object))==".progress")

            p = mapply(function(x, y) {
                        data.frame(Module=x, Parameter=names(y), Value=unlist(y),
                                   stringsAsFactors=FALSE, row.names=NULL)
                        },
                       x=names(simParams(object))[-omit], y=simParams(object)[-omit],
                       USE.NAMES=TRUE, SIMPLIFY=FALSE)
            if (length(p)>0) {
              q = do.call(rbind, p)
              q = q[order(q$Module, q$Parameter),]
            } else {
              q = cbind(Module=list(), Parameter=list())
            }
            out[[11]] = capture.output(cat(">> Parameters:\n"))
            out[[12]] = capture.output(print(q, row.names=FALSE))
            out[[13]] = capture.output(cat("\n"))

            ### completed events
            out[[14]] = capture.output(cat(">> Completed Events:\n"))
            out[[15]] = capture.output(print(simCompleted(object)))
            out[[16]] = capture.output(cat("\n"))

            ### scheduled events
            out[[17]] = capture.output(cat(">> Scheduled Events:\n"))
            out[[18]] = capture.output(print(simEvents(object)))
            out[[19]] = capture.output(cat("\n"))

            ### print result
            cat(unlist(out), fill=FALSE, sep="\n")
})

##############################################################
#' Get and set the list of modules to be loaded for a simulation.
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simParams}} for accessing the list of simulation parameters;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#'          to access the simulation current, start, and stop times individually).
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
          signature="simList",
          definition=function(object) {
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
#' @name simModules<-
#' @aliases simModules<-,simList-method
#' @rdname simModules-accessor-methods
setReplaceMethod("simModules",
                 signature="simList",
                 function(object, value) {
                     object@modules <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Get and set the modules loaded for the simulation.
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simModulesLoaded-accessor-methods
#'
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simParams}} for accessing the list of simulation parameters;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#'          to access the simulation current, start, and stop times individually).
#'
#' @author Alex Chubaty
#'
setGeneric("simModulesLoaded", function(object) {
    standardGeneric("simModulesLoaded")
})

#' get list of loaded simulation modules
#' @rdname simModulesLoaded-accessor-methods
setMethod("simModulesLoaded",
          signature="simList",
          definition=function(object) {
              return(object@.loaded$modules)
})

#' set list of loaded simulation modules
#' @export
#' @rdname simModulesLoaded-accessor-methods
setGeneric("simModulesLoaded<-",
           function(object, value) {
               standardGeneric("simModulesLoaded<-")
})

#' set list of loaded simulation modules
#' @name simModulesLoaded<-
#' @aliases simModulesLoaded<-,simList-method
#' @rdname simModulesLoaded-accessor-methods
setReplaceMethod("simModulesLoaded",
                 signature="simList",
                 function(object, value) {
                     object@.loaded$modules <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Get and set the objects loaded for the simulation.
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simObjectsLoaded-accessor-methods
#'
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simParams}} for accessing the list of simulation parameters;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#'          to access the simulation current, start, and stop times individually).
#'
#' @author Alex Chubaty
#'
setGeneric("simObjectsLoaded", function(object) {
  standardGeneric("simObjectsLoaded")
})

#' get list of loaded simulation modules
#' @rdname simObjectsLoaded-accessor-methods
setMethod("simObjectsLoaded",
          signature="simList",
          definition=function(object) {
            return(object@.loaded$objects)
})

#' set list of loaded simulation modules
#' @export
#' @rdname simObjectsLoaded-accessor-methods
setGeneric("simObjectsLoaded<-",
           function(object, value) {
             standardGeneric("simObjectsLoaded<-")
})

#' set list of loaded simulation modules
#' @name simObjectsLoaded<-
#' @aliases simObjectsLoaded<-,simList-method
#' @rdname simObjectsLoaded-accessor-methods
setReplaceMethod("simObjectsLoaded",
                 signature="simList",
                 function(object, value) {
                   object@.loaded$objects <- value
                   validObject(object)
                   return(object)
})


##############################################################
#' Get and set the simulation parameters list.
#'
#' Currently, only get and set methods are defined. Subset methods are not.
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
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#'          to access the simulation current, start, and stop times individually).
#'
#' @author Alex Chubaty
#'
setGeneric("simParams", function(object) {
    standardGeneric("simParams")
})

#' get list of simulation parameters
#' @rdname simParams-accessor-methods
setMethod("simParams",
          signature="simList",
          definition=function(object) {
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
#' @name simParams<-
#' @aliases simParams<-,simList-method
#' @rdname simParams-accessor-methods
setReplaceMethod("simParams",
                 signature="simList",
                 function(object, value) {
                     object@params <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Get and set the simulation checkpoint filename.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simCheckpointFile-accessor-methods
#'
#' @seealso \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#'          to access the simulation current, start, and stop times individually).
#'
#' @author Alex Chubaty
#'
setGeneric("simCheckpointFile", function(object) {
  standardGeneric("simCheckpointFile")
})

#' get list of simulation parameters
#' @rdname simCheckpointFile-accessor-methods
setMethod("simCheckpointFile",
          signature="simList",
          definition=function(object) {
            return(object@params$.checkpoint$file)
})

#' set list of simulation parameters
#' @export
#' @rdname simCheckpointFile-accessor-methods
setGeneric("simCheckpointFile<-",
           function(object, value) {
             standardGeneric("simCheckpointFile<-")
})

#' set list of simulation parameters
#' @name simCheckpointFile<-
#' @aliases simCheckpointFile<-,simList-method
#' @rdname simCheckpointFile-accessor-methods
setReplaceMethod("simCheckpointFile",
                 signature="simList",
                 function(object, value) {
                   object@params$.checkpoint$file <- value
                   validObject(object)
                   return(object)
})

##############################################################
#' Get and set the simulation checkpoint interval.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simCheckpointInterval-accessor-methods
#'
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#'          to access the simulation current, start, and stop times individually).
#'
#' @author Alex Chubaty
#'
setGeneric("simCheckpointInterval", function(object) {
  standardGeneric("simCheckpointInterval")
})

#' get list of simulation parameters
#' @rdname simCheckpointInterval-accessor-methods
setMethod("simCheckpointInterval",
          signature="simList",
          definition=function(object) {
            return(object@params$.checkpoint$interval)
})

#' set list of simulation parameters
#' @export
#' @rdname simCheckpointInterval-accessor-methods
setGeneric("simCheckpointInterval<-",
           function(object, value) {
             standardGeneric("simCheckpointInterval<-")
})

#' set list of simulation parameters
#' @name simCheckpointInterval<-
#' @aliases simCheckpointInterval<-,simList-method
#' @rdname simCheckpointInterval-accessor-methods
setReplaceMethod("simCheckpointInterval",
                 signature="simList",
                 function(object, value) {
                   object@params$.checkpoint$interval <- value
                   validObject(object)
                   return(object)
})

##############################################################
#' Get and set the filelist to be loaded during simulation.
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simFileList-accessor-methods
#'
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simParams}} for accessing the list of simulation parameters;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#'          to access the simulation current, start, and stop times individually).
#'
#' @author Alex Chubaty
#'
setGeneric("simFileList", function(object) {
  standardGeneric("simFileList")
})

#' get .loadFileList from simulation parameters
#' @rdname simFileList-accessor-methods
setMethod("simFileList",
          signature="simList",
          definition=function(object) {
            return(object@params$.loadFileList)
})

#' set .loadFileList in simulation parameters
#' @export
#' @rdname simFileList-accessor-methods
setGeneric("simFileList<-",
           function(object, value) {
             standardGeneric("simFileList<-")
})

#' set .loadFileList in simulation parameters
#' @name simFileList<-
#' @aliases simFileList<-,simList-method
#' @rdname simFileList-accessor-methods
setReplaceMethod("simFileList",
                 signature="simList",
                 function(object, value) {
                   object@params$.loadFileList <- value
                   validObject(object)
                   return(object)
})

##############################################################
#' Get and set the list of global simulation parmeters.
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simGlobals-accessor-methods
#'
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simParams}} for accessing the list of simulation parameters;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#'          to access the simulation current, start, and stop times individually).
#'
#' @author Alex Chubaty
#'
setGeneric("simGlobals", function(object) {
  standardGeneric("simGlobals")
})

#' get .globals from simulation parameters
#' @rdname simGlobals-accessor-methods
setMethod("simGlobals",
          signature="simList",
          definition=function(object) {
            return(object@params$.globals)
})

#' set .globals in simulation parameters
#' @export
#' @rdname simGlobals-accessor-methods
setGeneric("simGlobals<-",
           function(object, value) {
             standardGeneric("simGlobals<-")
})

#' set .globals in simulation parameters
#' @name simGlobals<-
#' @aliases simGlobals<-,simList-method
#' @rdname simGlobals-accessor-methods
setReplaceMethod("simGlobals",
                 signature="simList",
                 function(object, value) {
                   object@params$.globals <- value
                   validObject(object)
                   return(object)
})

##############################################################
#' Get and set the output path from global simulation parmeters.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simGlobalsOutputPath-accessor-methods
#'
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simParams}} for accessing the list of simulation parameters;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#'          to access the simulation current, start, and stop times individually).
#'
#' @author Alex Chubaty
#'
setGeneric("simGlobalsOutputPath", function(object) {
  standardGeneric("simGlobalsOutputPath")
})

#' get .globals$.outputPath from simulation parameters
#' @rdname simGlobalsOutputPath-accessor-methods
setMethod("simGlobalsOutputPath",
          signature="simList",
          definition=function(object) {
            return(object@params$.globals$.outputPath)
})

#' set .globals$.outputPath in simulation parameters
#' @export
#' @rdname simGlobalsOutputPath-accessor-methods
setGeneric("simGlobalsOutputPath<-",
           function(object, value) {
             standardGeneric("simGlobalsOutputPath<-")
})

#' set .globals$.outputPath in simulation parameters
#' @name simGlobalsOutputPath<-
#' @aliases simGlobalsOutputPath<-,simList-method
#' @rdname simGlobalsOutputPath-accessor-methods
setReplaceMethod("simGlobalsOutputPath",
                 signature="simList",
                 function(object, value) {
                   object@params$.globals$.outputPath <- value
                   validObject(object)
                   return(object)
})

##############################################################
#' Get and set the list of simulation times.
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
#' @rdname simTimes-accessor-methods
#'
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simParams}} for accessing the list of simulation parameters;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#'          to access the simulation current, start, and stop times individually.
#'
#' @author Alex Chubaty
#'
setGeneric("simTimes", function(object) {
    standardGeneric("simTimes")
})

#' get list of simulation times
#' @rdname simTimes-accessor-methods
setMethod("simTimes",
          signature="simList",
          definition=function(object) {
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
#' @name simTimes<-
#' @aliases simTimes<-,simList-method
#' @rdname simTimes-accessor-methods
setReplaceMethod("simTimes",
                 signature="simList",
                 function(object, value) {
                     object@simtimes <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Get and set the current simulation time.
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
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simParams}} for accessing the list of simulation parameters;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simStartTime}, and \code{simStopTime}
#'          to access the simulation start, and stop times individually).
#'
#' @author Alex Chubaty
#'
setGeneric("simCurrentTime", function(object) {
    standardGeneric("simCurrentTime")
})

#' get the current simulation time
#' @rdname simCurrentTime-accessor-methods
setMethod("simCurrentTime",
          signature="simList",
          definition=function(object) {
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
#' @name simCurrentTime<-
#' @aliases simCurrentTime<-,simList-method
#' @rdname simCurrentTime-accessor-methods
setReplaceMethod("simCurrentTime",
                 signature="simList",
                 function(object, value) {
                     object@simtimes$current <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Get and set the simulation start time.
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
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simParams}} for accessing the list of simulation parameters;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime} and \code{simStopTime}
#'          to access the simulation current and stop times individually).
#'
#' @author Alex Chubaty
#'
setGeneric("simStartTime", function(object) {
    standardGeneric("simStartTime")
})

#' get the simulation start time
#' @rdname simStartTime-accessor-methods
setMethod("simStartTime",
          signature="simList",
          definition=function(object) {
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
#' @name simStartTime<-
#' @aliases simStartTime<-,simList-method
#' @rdname simStartTime-accessor-methods
setReplaceMethod("simStartTime",
                 signature="simList",
                 function(object, value) {
                     object@simtimes$start <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Get and set the simulation stop time.
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
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simParams}} for accessing the list of simulation parameters;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime} and \code{simStartTime}
#'          to access the simulation current and start times individually).
#'
#' @author Alex Chubaty
#'
setGeneric("simStopTime", function(object) {
    standardGeneric("simStopTime")
})

#' get the simulation stop time
#' @rdname simStopTime-accessor-methods
setMethod("simStopTime",
          signature="simList",
          definition=function(object) {
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
#' @name simStopTime<-
#' @aliases simStopTime<-,simList-method
#' @rdname simStopTime-accessor-methods
setReplaceMethod("simStopTime",
                 signature="simList",
                 function(object, value) {
                     object@simtimes$stop <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Get and set the simluation event list (aka event queue).
#'
#' The event queue is the list of scheduled (upcoming) events. All completed events
#' are moved to to a sepaparte list accessed via \code{simCompleted}.
#'
#' Currently, only get and set methods are defined. Subset methods are not.
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
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simParams}} for accessing the list of simulation parameters;
#'          \code{\link{simCompleted}} for accessing the list of completed simulation events;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#'          to access the simulation current, start, and stop times individually).
#'
#' @author Alex Chubaty
#'
setGeneric("simEvents", function(object) {
    standardGeneric("simEvents")
})

#' get the simulation event queue
#' @rdname simEvents-accessor-methods
setMethod("simEvents",
          signature="simList",
          definition=function(object) {
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
#' @name simEvents<-
#' @aliases simEvents<-,simList-method
#' @rdname simEvents-accessor-methods
setReplaceMethod("simEvents",
                 signature="simList",
                 function(object, value) {
                     object@events <- value
                     validObject(object)
                     return(object)
})

##############################################################
#' Get and set the list of completed simulation events.
#'
#' Upcaming (scheduled) events are stored in the events slot, accessible via
#' \code{simEvents}.
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simCompleted-accessor-methods
#'
#' @seealso \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#'          \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#'          \code{\link{simGlobals}} for accessing the global simulation parameters;
#'          \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#'          \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#'          \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#'          \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#'          \code{\link{simParams}} for accessing the list of simulation parameters;
#'          \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#'          \code{\link{simTimes}} for accessing the list of simulation times
#'          (\code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#'          to access the simulation current, start, and stop times individually).
#'
#' @author Eliot McIntire
#'
setGeneric("simCompleted", function(object) {
  standardGeneric("simCompleted")
})

#' get the simulation completed events list
#' @rdname simCompleted-accessor-methods
setMethod("simCompleted",
          signature="simList",
          definition=function(object) {
            return(object@completed)
})

#' set the simulation completed events list
#' @export
#' @rdname simCompleted-accessor-methods
setGeneric("simCompleted<-",
           function(object, value) {
             standardGeneric("simCompleted<-")
})

#' set the simulation completed events list
#' @name simCompleted<-
#' @aliases simCompleted<-,simList-method
#' @rdname simCompleted-accessor-methods
setReplaceMethod("simCompleted",
                 signature="simList",
                 function(object, value) {
                   object@completed <- value
                   validObject(object)
                   return(object)
})


##############################################################
#' Initialize a new simulation
#'
#' Create a new simulation object, preloaded with parameters,
#' modules, times, etc.
#'
#' Based on code from chapter 7.8.3 of Matloff (2011): "Discrete event simulation".
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods, and use `data.table`
#' instead of `data.frame` to implement the event queue (because it is much faster).
#'
#' @param times A named list of numeric simulation start and stop times
#'        (e.g., \code{times=list(start=0.0, stop=10.0)}).
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
#' @seealso \code{\link{spades}}.
#'
#' @export
#' @docType methods
#' @rdname simInit-method
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{mySim <- simInit(times=list(start=0.0, stop=10.0), params=list(Ncaribou=100),
#' modules=list("habitat", "caribou"), path="/path/to/my/modules/")}
#' \dontrun{mySim}
setGeneric("simInit", function(times, params, modules, path) {
    standardGeneric("simInit")
})

#' @rdname simInit-method
#'
setMethod("simInit",
          signature(times="list", params="list", modules="list", path="character"),
          definition=function(times, params, modules, path) {
            path <- checkPath(path, create=TRUE)

            # Delete any previous Plot information at initialization
            rm(list = (ls(all.names = TRUE,
                          envir = .GlobalEnv)[grep(".spadesArr",
                                                   ls(all.names = TRUE,
                                                      envir = .GlobalEnv))]),
               envir=.GlobalEnv)

            # default modules
            defaults <- list("checkpoint", "save", "progress", "load")

            # parameters for default modules
            dotParamsReal = list(".saveInterval", ".saveInitialTime",
                                 ".plotInterval", ".plotInitialTime")
            dotParamsChar = list(".savePath", ".saveObjects")
            dotParams = append(dotParamsChar, dotParamsReal)

            # create new simList object
            sim <- new("simList", simtimes=list(current=times$start,
                                                start=times$start,
                                                stop=times$stop))
            simModules(sim) <- modules
            simParams(sim) <- params

            # load "default" modules
            for (d in defaults) {
              ### sourcing the code in each module is already done
              ### because they are loaded with the package

              # add default module name to the loaded list:
              ### add module name to the loaded list
              simModulesLoaded(sim) <- append(simModulesLoaded(sim), d)

              # schedule each module's init event:
              sim <- scheduleEvent(sim, 0.00, d, "init")
            }

            # load user-defined modules
            for (m in simModules(sim)) {
                # source the code from each module's R file
                source(paste(path, "/", m, ".R", sep=""),local=.GlobalEnv)

                # schedule each module's init event:
                sim <- scheduleEvent(sim, 0.00, m, "init")

                ### add module name to the loaded list
                simModulesLoaded(sim) <- append(simModulesLoaded(sim), m)

                ### add NAs to any of the dotParams that are not specified by user
                # ensure the modules sublist exists by creating a tmp value in it
                if(is.null(simParams(sim)[[m]])) {
                  simParams(sim)[[m]] <- list(.tmp=NA_real_)
                }

                # add the necessary values to the sublist
                for(x in dotParamsReal) {
                  if (is.null(simParams(sim)[[m]][[x]])) {
                    simParams(sim)[[m]][[x]] <- NA_real_
                  } else if (is.na(simParams(sim)[[m]][[x]])) {
                    simParams(sim)[[m]][[x]] <- NA_real_
                  }
                }

                # remove the tmp value from the module sublist
                simParams(sim)[[m]]$.tmp <- NULL

                ### Currently, everything in dotParamsChar is being checked for NULL
                ### values where used (i.e., in save.R).
            }

            simModules(sim) <- append(defaults, modules)

            # load files in the filelist
            if (is.null(simFileList(sim))) {
              sim <- loadFiles(sim, usedFileList=TRUE)
            } else {
              sim <- loadFiles(sim)
            }

            # check the parameters supplied by the user
            checkParams(sim, defaults, dotParams, path) # returns invisible TRUE/FALSE

            return(invisible(sim))
})

#' @rdname simInit-method
setMethod("simInit",
          signature(times="list", params="list", modules="list", path="missing"),
          definition=function(times, params, modules) {
            simInit(times=times, params=params, modules=modules, path="./")
            return(invisible(sim))
})

##############################################################
#' Load modules for simulation.
#'
#' Checks the dependencies of the current module on other modules.
#' These dependencies need to be loaded first, so if they are not
#' already loaded, hold off loading the current module until after
#' dependencies are loaded.
#'
#' @param sim     A \code{simList} simulation object.
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
setGeneric("reloadModuleLater", function(sim, depends) {
  standardGeneric("reloadModuleLater")
})

#' @rdname loadmodules
setMethod("reloadModuleLater",
          signature(sim="simList", depends="NULL"),
          definition=function(sim, depends) {
            return(FALSE)
})

#' @rdname loadmodules
setMethod("reloadModuleLater",
          signature(sim="simList", depends="character"),
          definition=function(sim, depends) {
            return(!all(depends %in% simModulesLoaded(sim)))
})

##############################################################
#' Process a simulation event
#'
#' Internal function called from \code{spades}.
#'
#' Calls the module corresponding to the event call, and executes the event.
#'
#' Based on code from chapter 7.8.3 of Matloff (2011): "Discrete event simulation".
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods, and use `data.table`
#' instead of `data.frame` to implement the event queue (because it is much faster).
#'
#' @param sim Character string for the \code{simList} simulation object.
#'
#' @param debug Optional logical flag determines whether sim debug info
#'              will be printed (default is \code{debug=FALSE}).
#'
#' @return Returns the modified \code{simList} object.
#'
#' @import data.table
#' @export
#' @keywords internal
#' @docType methods
#' @rdname doEvent-method
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
setGeneric("doEvent", function(sim, debug) {
    standardGeneric("doEvent")
})

#' @rdname doEvent-method
setMethod("doEvent",
          signature(sim="simList", debug="logical"),
          definition=function(sim, debug) {
            # get next event
            nextEvent <- simEvents(sim)[1, ] # extract the next event from queue

            # Catches the situation where no future event is scheduled, but StopTime is not reached
             if(any(is.na(nextEvent))) {
               simCurrentTime(sim) <- simStopTime(sim) + 1e-10
             } else {
              if (nextEvent$eventTime <= simStopTime(sim)) {
                # update current simulated time
                simCurrentTime(sim) <- nextEvent$eventTime

                # call the module responsible for processing this event
                moduleCall <- paste("doEvent", nextEvent$moduleName, sep=".")

                # check the module call for validity
                if(nextEvent$moduleName %in% simModules(sim)) {
                  sim <- get(moduleCall)(sim, nextEvent$eventTime, nextEvent$eventType, debug)
                } else {
                  stop(paste("Invalid module call. The module `", nextEvent$moduleName,
                             "` wasn't specified to be loaded."))
                }

                # now that it is run, without error, remove it from the queue
                simEvents(sim) <- simEvents(sim)[-1,]

                # add to list of completed events
                if(length(simCompleted(sim))==0) {
                  simCompleted(sim) <- setkey(nextEvent, eventTime)
                } else {
                  simCompleted(sim) <- setkey(rbindlist(list(simCompleted(sim), nextEvent)), eventTime)
                }
              } else {
                # update current simulated time to
                simCurrentTime(sim) <- simStopTime(sim) + 1e-10 # .Machine$double.eps
              }
            }
          return(invisible(sim))
})

#' @rdname doEvent-method
setMethod("doEvent",
          signature(sim="simList", debug="missing"),
          definition=function(sim) {
            return(doEvent(sim, debug=FALSE))
})

##############################################################
#' Schedule a simulation event
#'
#' Adds a new event to the simulation's event queue, updating the simulation object.
#'
#' Based on code from chapter 7.8.3 of Matloff (2011): "Discrete event simulation".
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods, and use `data.table`
#' instead of `data.frame` to implement the event queue (because it is much faster).
#'
#' @param sim          A \code{simList} simulation object.
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
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{scheduleEvent(x, 10.5, "firemodule", "burn")}
setGeneric("scheduleEvent", function(sim, eventTime, moduleName, eventType) {
    standardGeneric("scheduleEvent")
})

#' @rdname scheduleEvent-method
setMethod("scheduleEvent",
          signature(sim="simList", eventTime="numeric",
                    moduleName="character", eventType="character"),
          definition=function(sim, eventTime, moduleName, eventType) {
            if (length(eventTime)>0) {
              if (!is.na(eventTime)) {
                  newEvent <- as.data.table(list(eventTime=eventTime,
                                                moduleName=moduleName,
                                                eventType=eventType))

                # if the event list is empty, set it to consist of newEvent and return;
                # otherwise, add newEvent and re-sort (rekey).
                if (length(simEvents(sim))==0) {
                  simEvents(sim) <- setkey(newEvent, eventTime)
                } else {
                  simEvents(sim) <- setkey(rbindlist(list(simEvents(sim), newEvent)), eventTime)
                }
              }
            } else {
                warning(paste("Invalid or missing eventTime. This is usually",
                                "caused by an attempt to scheduleEvent at an empty eventTime",
                                "or by using an undefined parameter."))
            }


            return(invisible(sim))
})

#' @rdname scheduleEvent-method
setMethod("scheduleEvent",
          signature(sim="simList", eventTime="NULL",
                    moduleName="character", eventType="character"),
          definition=function(sim, eventTime, moduleName, eventType) {
            warning(paste("Invalid or missing eventTime. This is usually",
                          "caused by an attempt to scheduleEvent at time NULL",
                          "or by using an undefined parameter."))
            return(invisible(sim))
})

##############################################################
#' Run a spatial discrete event simulation
#'
#' Based on code from chapter 7.8.3 of Matloff (2011): "Discrete event simulation".
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods, and use `data.table`
#' instead of `data.frame` to implement the event queue (because it is much faster).
#'
#' @param sim Character string for the \code{simList} simulation object.
#'
#' @param debug Optional logical flag determines whether sim debug info
#'              will be printed (default is \code{debug=FALSE}).
#'
#' @return Invisibly returns the modified \code{simList} object.
#'
#' @seealso \code{\link{simInit}}.
#'
#' @note The debug option is primarily intended to facilitate building simulation
#' models by the user. Will print additional outputs informing the user of updates
#' to the values of various simList slot components.
#'
#' @export
#' @docType methods
#' @rdname spades-method
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{
#' mySim <- simInit(times=list(start=0.0, stop=10.0), params=list(Ncaribou=100),
#'                  modules=list("habitat", "caribou"), path="/path/to/my/modules/)
#' spades{mySim}
#' }
#'
setGeneric("spades", function(sim, debug) {
    standardGeneric("spades")
})

#' @rdname spades-method
setMethod("spades",
          signature(sim="simList", debug="logical"),
          definition=function(sim, debug) {
            while(simCurrentTime(sim) <= simStopTime(sim)) {
              sim <- doEvent(sim, debug)  # process the next event

              # print debugging info
              #  this can, and should, be more sophisticated;
              #  i.e., don't simply print the entire object
              if (debug) {
                  print(sim)
              }
            }
          return(invisible(sim))
})

#' @rdname spades-method
setMethod("spades",
          signature(sim="simList", debug="missing"),
          definition=function(sim) {
            return(spades(sim, debug=FALSE))
})
