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
#' @slot events     The list of scheduled events (aka event queue), as a \code{data.table}.
#'                  This event queue is always sorted (keyed) by time,
#'                  making it easy to insert new events into the queue.
#'
#' @slot completed  The list of completed events, as a \code{data.table}.
#'
#' @slot depends    A \code{simDeps} list of \code{moduleDeps} objects containing
#'                  module object dependency information.
#'
#' @slot simtimes   List of numerical values describing the simulation start and stop times;
#'                  as well as the current simulation time.
#'
#' @note Each event is represented by a data.table row consisting of:
#'          eventTime: the time the event is to occur;
#'          moduleName: the module from which the event is taken;
#'          eventType: a character string for the programmer-defined event type.
#'
#' @seealso \code{\link{data.table}}.
#'
#' @include module-dependencies-class.R
#' @rdname simList-class
#' @aliases simList
#' @import data.table
#' @exportClass simList
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
setClass("simList",
         slots=list(.loadOrder="character", .loaded="list",
                    modules="list", params="list",
                    events="data.table", completed="data.table",
                    depends="simDeps", simtimes="list"),
         prototype=list(.loadOrder=character(),
                        .loaded=list(modules=as.list(NULL), objects=as.list(NULL)),
                        modules=as.list(NULL),
                        params=list(.checkpoint=list(interval=NA_real_, file=NULL),
                                    .progress=list(graphical=NULL, interval=NULL)),
                        events=as.data.table(NULL), completed=as.data.table(NULL),
                        depends=new("simDeps", dependencies=list(NULL)),
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

### show is already defined in the methods package
#' show simList
#'
#' @param object  Any R object
#'
#' @export
setMethod("show",
          signature="simList",
          definition=function(object) {
            out = list()

            ### hr
            out[[1]] = capture.output(cat(rep("=", getOption("width"), sep=""), "\n", sep=""))

            ### simulation dependencies
            out[[2]] = capture.output(cat(">> Simulation dependencies:\n"))
            out[[3]] = "use `simDepends(sim)` to view dependencies for each module"
            out[[4]] = capture.output(cat("\n"))

            ### simtimes
            out[[5]] = capture.output(cat(">> Simulation times:\n"))
            out[[6]] = capture.output(print(rbind(simTimes(object))))
            out[[7]] = capture.output(cat("\n"))

            ### modules loaded
            out[[8]] = capture.output(cat(">> Modules:\n"))
            out[[9]] = capture.output(print(cbind(ModuleName=simModules(object),
                                                  IsLoaded=simModules(object) %in%
                                                    simModulesLoaded(object)),
                                            quote=FALSE, row.names=FALSE))
            out[[10]] = capture.output(cat("\n"))

            ### file/objects loaded
            files = simFileList(object)[["files"]]
            if (!is.null(simFileList(object)[["files"]])) {
              if (is.null(simFileList(object)[["objectNames"]])) {
                names = fileName(files)
              } else {
                names = objectNames
              }
            }
            out[[11]] = capture.output(cat(">> Objects Loaded:\n"))
            out[[12]] = capture.output(print(cbind(ObjectName=simObjectsLoaded(object)),
                                            quote=FALSE, row.names=FALSE))
            out[[13]] = capture.output(cat("\n"))

            ### params
            omit = which(names(simParams(object))==".load" |
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
            out[[14]] = capture.output(cat(">> Parameters:\n"))
            out[[15]] = capture.output(print(q, row.names=FALSE))
            out[[16]] = capture.output(cat("\n"))

            ### completed events
            out[[17]] = capture.output(cat(">> Completed Events:\n"))
            out[[18]] = capture.output(print(simCompleted(object)))
            out[[19]] = capture.output(cat("\n"))

            ### scheduled events
            out[[20]] = capture.output(cat(">> Scheduled Events:\n"))
            out[[21]] = capture.output(print(simEvents(object)))
            out[[22]] = capture.output(cat("\n"))

            ### print result
            cat(unlist(out), fill=FALSE, sep="\n")
})

################################################################################
#' Get and set slots and elements in a simList object
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' \code{\link{simCheckpointFile}} for accessing the name of the checkpoint file;
#' \code{\link{simCheckpointInterval}} for accessing the simulation checkpoint interval;
#' \code{\link{simGlobals}} for accessing the global simulation parameters;
#' \code{\link{simGlobalsOutputPath}} for accessing the global simulation output path;
#' \code{\link{simModules}} for accessing the list of simulation modules to be loaded;
#' \code{\link{simModulesLoaded}} for accessing the list of loaded simulation modules;
#' \code{\link{simObjectsLoaded}} for accessing the list of loaded simulation objects;
#' \code{\link{simParams}} for accessing the list of simulation parameters;
#' \code{\link{simProgressGraphical}} for accessing the graphical progress bar parameter;
#' \code{\link{simProgressInterval}} for accessing the interval for the progress bar;
#' \code{\link{simEvents}} for accessing the scheduled simulation event queue;
#' \code{\link{simCompleted}} for accessing the list of completed simulation events;
#' \code{\link{simTimes}} for accessing the list of simulation times
#' (\code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#' to access the simulation current, start, and stop times individually).
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
#' @author Alex Chubaty
#'
setGeneric("simModules", function(object) {
  standardGeneric("simModules")
})

#' @rdname simList-accessors
setMethod("simModules",
          signature="simList",
          definition=function(object) {
            return(object@modules)
})

#' @export
#' @rdname simList-accessors
setGeneric("simModules<-",
           function(object, value) {
             standardGeneric("simModules<-")
})

#' @name simModules<-
#' @aliases simModules<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simModules",
                 signature="simList",
                 function(object, value) {
                   object@modules <- value
                   validObject(object)
                   return(object)
 })

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simModulesLoadOrder", function(object) {
  standardGeneric("simModulesLoadOrder")
})

#' @rdname simList-accessors
setMethod("simModulesLoadOrder",
          signature="simList",
          definition=function(object) {
            return(object@.loadOrder)
})

#' @export
#' @rdname simList-accessors
setGeneric("simModulesLoadOrder<-",
           function(object, value) {
             standardGeneric("simModulesLoadOrder<-")
 })

#' @name simModulesLoadOrder<-
#' @aliases simModulesLoadOrder<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simModulesLoadOrder",
                 signature="simList",
                 function(object, value) {
                   if (!is.null(value)) {
                     object@.loadOrder <- value
                   } else {
                     object@.loadOrder <- character()
                   }
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simModulesLoaded", function(object) {
  standardGeneric("simModulesLoaded")
})

#' @rdname simList-accessors
setMethod("simModulesLoaded",
          signature="simList",
          definition=function(object) {
            return(object@.loaded$modules)
})

#' @export
#' @rdname simList-accessors
setGeneric("simModulesLoaded<-",
           function(object, value) {
             standardGeneric("simModulesLoaded<-")
})

#' @name simModulesLoaded<-
#' @aliases simModulesLoaded<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simModulesLoaded",
                 signature="simList",
                 function(object, value) {
                   object@.loaded$modules <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simObjectsLoaded", function(object) {
  standardGeneric("simObjectsLoaded")
})

#' @rdname simList-accessors
setMethod("simObjectsLoaded",
          signature="simList",
          definition=function(object) {
            return(object@.loaded$objects)
})

#' @export
#' @rdname simList-accessors
setGeneric("simObjectsLoaded<-",
           function(object, value) {
             standardGeneric("simObjectsLoaded<-")
})

#' @name simObjectsLoaded<-
#' @aliases simObjectsLoaded<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simObjectsLoaded",
                 signature="simList",
                 function(object, value) {
                   object@.loaded$objects <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simParams", function(object) {
  standardGeneric("simParams")
})

#' @rdname simList-accessors
setMethod("simParams",
          signature="simList",
          definition=function(object) {
            return(object@params)
})

#' @export
#' @rdname simList-accessors
setGeneric("simParams<-",
           function(object, value) {
             standardGeneric("simParams<-")
})

#' @name simParams<-
#' @aliases simParams<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simParams",
                 signature="simList",
                 function(object, value) {
                   object@params <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simCheckpointFile", function(object) {
  standardGeneric("simCheckpointFile")
})

#' @rdname simList-accessors
setMethod("simCheckpointFile",
          signature="simList",
          definition=function(object) {
            return(object@params$.checkpoint$file)
})

#' @rdname simList-accessors
setGeneric("simCheckpointFile<-",
           function(object, value) {
             standardGeneric("simCheckpointFile<-")
})

#' @name simCheckpointFile<-
#' @aliases simCheckpointFile<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simCheckpointFile",
                 signature="simList",
                 function(object, value) {
                   object@params$.checkpoint$file <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simCheckpointInterval", function(object) {
  standardGeneric("simCheckpointInterval")
})

#' @rdname simList-accessors
setMethod("simCheckpointInterval",
          signature="simList",
          definition=function(object) {
            return(object@params$.checkpoint$interval)
})

#' @export
#' @rdname simList-accessors
setGeneric("simCheckpointInterval<-",
           function(object, value) {
             standardGeneric("simCheckpointInterval<-")
})

#' @name simCheckpointInterval<-
#' @aliases simCheckpointInterval<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simCheckpointInterval",
                 signature="simList",
                 function(object, value) {
                   object@params$.checkpoint$interval <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simFileList", function(object) {
  standardGeneric("simFileList")
})

#' @rdname simList-accessors
setMethod("simFileList",
          signature="simList",
          definition=function(object) {
            return(object@params$.load$fileList)
})

#' @export
#' @rdname simList-accessors
setGeneric("simFileList<-",
           function(object, value) {
             standardGeneric("simFileList<-")
})

#' @name simFileList<-
#' @aliases simFileList<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simFileList",
                 signature="simList",
                 function(object, value) {
                   object@params$.load$fileList <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simProgressGraphical", function(object) {
  standardGeneric("simProgressGraphical")
})

#' @rdname simList-accessors
setMethod("simProgressGraphical",
          signature="simList",
          definition=function(object) {
            return(object@params$.progress$graphical)
})

#' @rdname simList-accessors
setGeneric("simProgressGraphical<-",
           function(object, value) {
             standardGeneric("simProgressGraphical<-")
})

#' @name simProgressGraphical<-
#' @aliases simProgressGraphical<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simProgressGraphical",
                 signature="simList",
                 function(object, value) {
                   object@params$.progress$graphical <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simProgressInterval", function(object) {
  standardGeneric("simProgressInterval")
})

#' @rdname simList-accessors
setMethod("simProgressInterval",
          signature="simList",
          definition=function(object) {
            return(object@params$.progress$interval)
})

#' @rdname simList-accessors
setGeneric("simProgressInterval<-",
           function(object, value) {
             standardGeneric("simProgressInterval<-")
})

#' @name simProgressInterval<-
#' @aliases simProgressInterval<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simProgressInterval",
                 signature="simList",
                 function(object, value) {
                   object@params$.progress$interval <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simGlobals", function(object) {
  standardGeneric("simGlobals")
})

#' @rdname simList-accessors
setMethod("simGlobals",
          signature="simList",
          definition=function(object) {
            return(object@params$.globals)
})

#' @export
#' @rdname simList-accessors
setGeneric("simGlobals<-",
           function(object, value) {
             standardGeneric("simGlobals<-")
})

#' @name simGlobals<-
#' @aliases simGlobals<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simGlobals",
                 signature="simList",
                 function(object, value) {
                   object@params$.globals <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simGlobalsOutputPath", function(object) {
  standardGeneric("simGlobalsOutputPath")
})

#' @rdname simList-accessors
setMethod("simGlobalsOutputPath",
          signature="simList",
          definition=function(object) {
            return(object@params$.globals$.outputPath)
})

#' @export
#' @rdname simList-accessors
setGeneric("simGlobalsOutputPath<-",
           function(object, value) {
             standardGeneric("simGlobalsOutputPath<-")
})

#' @name simGlobalsOutputPath<-
#' @aliases simGlobalsOutputPath<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simGlobalsOutputPath",
                 signature="simList",
                 function(object, value) {
                   object@params$.globals$.outputPath <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' Get and set simulation times.
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' Additonal methods are provided to access the current, start, and stop times of the
#' simulation: \code{simCurrentTime}, \code{simStartTime}, and \code{simStopTime}
#' to access the simulation current, start, and stop times individually.
#'
#' @inheritParams simModules
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname simTimes-accessor
#'
#' @author Alex Chubaty
#'
setGeneric("simTimes", function(object) {
  standardGeneric("simTimes")
})

#' @rdname simTimes-accessor
setMethod("simTimes",
          signature="simList",
          definition=function(object) {
            return(object@simtimes)
})

#' @export
#' @rdname simTimes-accessor
setGeneric("simTimes<-",
           function(object, value) {
             standardGeneric("simTimes<-")
})

#' @name simTimes<-
#' @aliases simTimes<-,simList-method
#' @rdname simTimes-accessor
setReplaceMethod("simTimes",
                 signature="simList",
                 function(object, value) {
                   object@simtimes <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simTimes-accessor
#'
setGeneric("simCurrentTime", function(object) {
  standardGeneric("simCurrentTime")
})

#' @rdname simTimes-accessor
setMethod("simCurrentTime",
          signature="simList",
          definition=function(object) {
            return(object@simtimes$current)
})

#' @export
#' @rdname simTimes-accessor
setGeneric("simCurrentTime<-",
           function(object, value) {
             standardGeneric("simCurrentTime<-")
})

#' @name simCurrentTime<-
#' @aliases simCurrentTime<-,simList-method
#' @rdname simTimes-accessor
setReplaceMethod("simCurrentTime",
                 signature="simList",
                 function(object, value) {
                   object@simtimes$current <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simTimes-accessor
#'
setGeneric("simStartTime", function(object) {
  standardGeneric("simStartTime")
})

#' @rdname simTimes-accessor
setMethod("simStartTime",
          signature="simList",
          definition=function(object) {
            return(object@simtimes$start)
})

#' @export
#' @rdname simTimes-accessor
setGeneric("simStartTime<-",
           function(object, value) {
             standardGeneric("simStartTime<-")
})

#' @name simStartTime<-
#' @aliases simStartTime<-,simList-method
#' @rdname simTimes-accessor
setReplaceMethod("simStartTime",
                 signature="simList",
                 function(object, value) {
                   object@simtimes$start <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simTimes-accessor
#'
setGeneric("simStopTime", function(object) {
  standardGeneric("simStopTime")
})

#' @rdname simTimes-accessor
setMethod("simStopTime",
          signature="simList",
          definition=function(object) {
            return(object@simtimes$stop)
})

#' @export
#' @rdname simTimes-accessor
setGeneric("simStopTime<-",
           function(object, value) {
             standardGeneric("simStopTime<-")
})

#' @name simStopTime<-
#' @aliases simStopTime<-,simList-method
#' @rdname simTimes-accessor
setReplaceMethod("simStopTime",
                 signature="simList",
                 function(object, value) {
                   object@simtimes$stop <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simEvents", function(object) {
  standardGeneric("simEvents")
})

#' @rdname simList-accessors
setMethod("simEvents",
          signature="simList",
          definition=function(object) {
            return(object@events)
})

#' @export
#' @rdname simList-accessors
setGeneric("simEvents<-",
           function(object, value) {
             standardGeneric("simEvents<-")
})

#' @name simEvents<-
#' @aliases simEvents<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simEvents",
                 signature="simList",
                 function(object, value) {
                   object@events <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simCompleted", function(object) {
  standardGeneric("simCompleted")
})

#' @rdname simList-accessors
setMethod("simCompleted",
          signature="simList",
          definition=function(object) {
            return(object@completed)
})

#' @export
#' @rdname simList-accessors
setGeneric("simCompleted<-",
           function(object, value) {
             standardGeneric("simCompleted<-")
})

#' @name simCompleted<-
#' @aliases simCompleted<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simCompleted",
                 signature="simList",
                 function(object, value) {
                   object@completed <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors
#'
setGeneric("simDepends", function(object) {
  standardGeneric("simDepends")
})

#' @rdname simList-accessors
#'
setMethod("simDepends",
          signature("simList"),
          definition=function(object) {
            return(object@depends)
})

#' @export
#' @rdname simList-accessors
setGeneric("simDepends<-",
           function(object, value) {
             standardGeneric("simDepends<-")
})

#' @name simDepends<-
#' @aliases simDepends<-,simList-method
#' @rdname simList-accessors
setReplaceMethod("simDepends",
                 signature("simList"),
                 function(object, value) {
                   object@depends <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' Add simulation dependencies
#'
#' Adds a \code{moduleDeps} object to the simulation dependency list.
#'
#' @param sim A \code{simList} object.
#'
#' @param x   A named list containing the parameters used to construct a new
#'            \code{moduleDeps} object.
#'
#' @return A \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname addSimDepends
#'
#' @author Alex Chubaty
#'
setGeneric("addSimDepends", function(sim, x) {
  standardGeneric("addSimDepends")
})

#' @rdname addSimDepends
#'
setMethod("addSimDepends",
          signature(sim="simList", x="moduleDeps"),
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
#' @inheritParams addSimDepends
#'
#' @return Updated \code{simList} object.
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
#'
setMethod("defineModule",
          signature(sim="simList", x="list"),
          definition=function(sim, x) {
            loadPackages(x$reqdPkgs)
            m <- do.call(new, c("moduleDeps", x))
            return(addSimDepends(sim, m))
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
#'   defineParameter("lambda", "numeric", 1e-3)
#' }
#'
setGeneric("defineParameter", function(name, class, default) {
  standardGeneric("defineParameter")
})

#' @rdname defineParameter
#'
setMethod("defineParameter",
          signature(name="character", class="character", default="ANY"),
          definition=function(name, class, default) {
            df <- data.frame(name=name, class=class,
                             default=I(list(substitute(default))),
                             stringsAsFactors=FALSE)
            return(df)
})
