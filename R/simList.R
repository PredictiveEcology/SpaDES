################################################################################
#' The \code{simList} class
#'
#' Contains the minimum components of a \code{SpaDES} simulation.
#'
#' Based on code from chapter 7.8.3 of Matloff (2011): "Discrete event simulation".
#' Here, we implement a discrete event simulation in a more modular fashion so it's
#' easier to add simulation components (i.e., "simulation modules").
#' We use S4 classes and methods, and use \code{\link{data.table}} instead of
#' \code{\link{data.frame}} to implement the event queue (because it is much
#' more efficient).
#'
#' Various slot accessor methods (i.e., get and set functions) are provided
#' (see 'Accessor Methods' below).
#'
#' @note The \code{simList} class extends the \code{.simList} superclass by adding
#' a slot \code{.envir} to store the simulation environment containing references
#' to simulation objects.
#' The \code{\link{simList_}} class extends the \code{.simList} superclass, by
#' adding a slot \code{.list} containing the simulation objects.
#' Thus, \code{simList} is identical to \code{simList_}, except that the former
#' uses an environment for objects and the latter uses a list.
#' The class \code{simList_} is only used internally.
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
#'                  See 'Event Lists' for more information.
#'
#' @slot completed  The list of completed events, as a \code{data.table}.
#'                  See 'Event Lists' for more information.
#'
#' @slot depends    A \code{.simDeps} list of \code{.moduleDeps} objects
#'                  containing module object dependency information.
#'
#' @slot simtimes   List of numerical values describing the simulation start
#'                  and stop times; as well as the current simulation time.
#'
#' @section Accessor Methods:
#'
#' Several slot (and sub-slot) accessor methods are provided for use, and
#' categorized into separate help pages:
#' \tabular{ll}{
#'   \code{\link{simList-accessors-envir}} \tab Simulation enviroment and objects. \cr
#'   \code{\link{simList-accessors-events}} \tab Scheduled and completed events. \cr
#'   \code{\link{simList-accessors-modules}} \tab Modules loaded and used; module dependencies. \cr
#'   \code{\link{simList-accessors-params}} \tab Global and module-specific parameters. \cr
#'   \code{\link{simList-accessors-times}} \tab Simulation times. \cr
#' }
#'
#' @section Event Lists:
#'
#' Event lists are sorted (keyed) by time.
#' Each event is represented by a \code{\link{data.table}} row consisting of:
#' \tabular{ll}{
#'   \code{eventTime} \tab The time the event is to occur.\cr
#'   \code{moduleName} \tab The module from which the event is taken.\cr
#'   \code{eventType} \tab A character string for the programmer-defined event type.\cr
#' }
#'
#' @include module-dependencies-class.R
#' @aliases .simList
#' @rdname simList-class
#' @import data.table
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
#' @author Alex Chubaty
#'
setClass(".simList",
         slots=list(.loadOrder="character", .loaded="list",
                    modules="list", params="list",
                    events="data.table", completed="data.table",
                    depends=".simDeps", simtimes="list"),
         prototype=list(.loadOrder=character(),
                        .loaded=list(modules=as.list(NULL)),
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
#'
#' @inheritParams .simList
#'
#' @slot .envir     Environment referencing the objects used in the simulation.
#'                  Several "shortcuts" to accessing objects referenced by this
#'                  environment are provided, and can be used on the
#'                  \code{simList} object directly instead of specifying the
#'                  \code{.envir} slot: \code{$}, \code{[[}, \code{ls},
#'                  \code{ls.str}, \code{simObjects}. See examples.
#'
#' @aliases simList
#' @rdname simList-class
#' @exportClass simList
#'
setClass("simList",
         contains=".simList",
         slots=list(.envir="environment"),
         prototype=list(.envir=new.env(parent=emptyenv()))
)

################################################################################
#' The \code{simList_} class
#'
#' Internal use only. Used when saving/loading a \code{simList}.
#'
#' This is identical to class \code{simList}, except that the \code{.envir} slot
#' is replaced by a \code{.list} containing a list to store the objects from the
#' environment contained within the \code{simList}.
#' Saving/loading a list behaves more reliably than saving/loading an environment.
#'
#' @inheritParams .simList
#'
#' @seealso \code{\link{simList}}
#'
#' @aliases simList_
#' @rdname simList_-class
#'
#' @author Alex Chubaty
#'
setClass("simList_",
         contains=".simList",
         slots=list(.list="list"),
         prototype=list(.list=list())
)

setAs(from="simList_", to="simList", def=function(from) {
  x <- as(as(from, ".simList"), "simList")
  x@.envir <- as.environment(from@.list)
  return(x)
})

setAs(from="simList", to="simList_", def=function(from) {
  x <- as(as(from, ".simList"), "simList_")
  x@.list <- as.list(simEnv(from))
  return(x)
})

### `initialize` generic is already defined in the methods package
#' Generate a \code{simList} object
#'
#' Given the name or the definition of a class, plus optionally data to be
#' included in the object, \code{new} returns an object from that class.
#'
#' @param .Object  A \code{simList} object.
#' @include misc-methods.R
#' @export
#' @docType methods
#' @rdname initialize-method
setMethod("initialize",
          signature(.Object = "simList"),
          definition=function(.Object) {
            .Object@.envir <- new.env(parent=.GlobalEnv)
            return(.Object)
})

### `show` generic is already defined in the methods package
#' Show an Object
#'
#' @param object  \code{simList}
#'
#' @export
#' @docType methods
#' @rdname show-method
setMethod("show",
          signature="simList",
          definition=function(object) {

            out <- list()

            ### hr
            out[[1]] <- capture.output(cat(rep("=", getOption("width"), sep=""), "\n", sep=""))

            ### simulation dependencies
            out[[2]] <- capture.output(cat(">> Simulation dependencies:\n"))
            out[[3]] <- "use `simDepends(sim)` to view dependencies for each module"
            out[[4]] <- capture.output(cat("\n"))

            ### simtimes
            out[[5]] <- capture.output(cat(">> Simulation times:\n"))
            out[[6]] <- capture.output(print(rbind(simTimes(object))))
            out[[7]] <- capture.output(cat("\n"))

            ### modules loaded
            out[[8]] <- capture.output(cat(">> Modules:\n"))
            out[[9]] <- capture.output(print(cbind(ModuleName=simModules(object),
                                                   IsLoaded=simModules(object) %in%
                                                     simModulesLoaded(object)),
                                             quote=FALSE, row.names=FALSE))
            out[[10]] <- capture.output(cat("\n"))

            ### objects loaded
            out[[11]] <- capture.output(cat(">> Objects Loaded:\n"))
            out[[12]] <- capture.output(print(cbind(ObjectName=simObjectsLoaded(object)),
                                              quote=FALSE, row.names=FALSE))
            out[[13]] <- capture.output(cat("\n"))

            ### params
            omit <- which(names(simParams(object))==".load" |
                            names(simParams(object))==".progress")

            p <- mapply(function(x, y) {
              data.frame(Module=x, Parameter=names(y), Value=unlist(y),
                         stringsAsFactors=FALSE, row.names=NULL)
            },
            x=names(simParams(object))[-omit], y=simParams(object)[-omit],
            USE.NAMES=TRUE, SIMPLIFY=FALSE)
            if (length(p)) {
              q = do.call(rbind, p)
              q = q[order(q$Module, q$Parameter),]
            } else {
              q = cbind(Module=list(), Parameter=list())
            }
            out[[14]] <- capture.output(cat(">> Parameters:\n"))
            out[[15]] <- capture.output(print(q, row.names=FALSE))
            out[[16]] <- capture.output(cat("\n"))

            ### completed events
            out[[17]] <- capture.output(cat(">> Completed Events:\n"))
            out[[18]] <- capture.output(print(simCompleted(object)))
            out[[19]] <- capture.output(cat("\n"))

            ### scheduled events
            out[[20]] <- capture.output(cat(">> Scheduled Events:\n"))
            out[[21]] <- capture.output(print(simEvents(object)))
            out[[22]] <- capture.output(cat("\n"))

            ### list stored objects
            out[[23]] <- capture.output(cat(">> Objects stored:\n"))
            out[[24]] <- capture.output(print(ls.str(simEnv(object))))
            out[[25]] <- capture.output(cat("\n"))

            ### print result
            cat(unlist(out), fill=FALSE, sep="\n")
})

### `ls` generic is already defined in the base package
#' List simulation objects
#'
#' Return a vector of character strings giving the names of the objects in the
#' specified simulation environment.
#' Can be used with a \code{simList} object, because the method for this class
#' is simply a wrapper for calling \code{ls} on the simulation environment
#' stored in the \code{simList} object.
#'
#' @param name  A \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname ls-method
ls.simList <- function(name) {
  ls(simEnv(name))
}

#' @rdname ls-method
setMethod("ls",
          signature(name="simList"),
          definition=function(name) {
            ls.simList(name)
})

### `ls.str` generic is already defined in the utils package
#' List simulation objects and their structure
#'
#' A variation of applying \code{\link{str}} to each matched name.
#' Can be used with a \code{simList} object, because the method for this class
#' is simply a wrapper for calling \code{ls} on the simulation environment
#' stored in the \code{simList} object.

#' @param name  A \code{simList} object.
#'
#' @export
#' @docType methods
#' @rdname ls_str-method
ls.str.simList <- function(name) {
  ls.str(simEnv(name))
}

#' @rdname ls_str-method
setMethod("ls.str",
          signature(pos="missing", name="simList"),
          definition=function(name) {
            ls.str.simList(name=name)
})

###############################################################################
#' Extract or replace parts of an object from the simulation environment
#'
#' @param x      object from which to extract element(s) or in which to replace element(s).
#' @param i      indices specifying elements to extract or replace.
#' @param j      see \code{i}.
#' @param ...    see \code{i}.
#' @param name   A literal character string or a \code{\link{name}}.
#' @param drop   not implemented.
#' @param value  Any R object.
#'
#' @export
#' @name [[
#' @aliases [[,simList,ANY,ANY-method
#' @docType methods
#' @rdname simList-extract-methods
setMethod("[[", signature(x="simList", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., drop) {
            return(x@.envir[[i]])
})

#' @export
#' @name [[<-
#' @aliases [[<-,simList,ANY,ANY-method
#' @rdname simList-extract-methods
setReplaceMethod("[[", signature(x="simList", value="ANY"),
                 definition=function(x, i, value) {
                   assign(i, value, envir=x@.envir, inherits=FALSE)
                   return(x)
})

#' @export
#' @name $
#' @aliases $,simList-method
#' @rdname simList-extract-methods
setMethod("$", signature(x="simList"),
          definition=function(x, name) {
            return(x@.envir[[name]])
})

#' @export
#' @name $<-
#' @aliases $<-,simList-method
#' @rdname simList-extract-methods
setReplaceMethod("$", signature(x="simList", value="ANY"),
                 definition=function(x, name, value) {
                   x@.envir[[name]] <- value
                   return(x)
})

################################################################################
#' Simulation environment
#'
#' Accessor functions for the \code{.envir} slot in a \code{simList} object.
#' These are included for advanced users.
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{simList-class}},
#'          \code{\link{simList-accessors-modules}},
#'          \code{\link{simList-accessors-params}},
#'          \code{\link{simList-accessors-events}},
#'          \code{\link{simList-accessors-times}}.
#' @export
#' @docType methods
#' @aliases simList-accessors-envir
#' @rdname simList-accessors-envir
#'
#' @author Alex Chubaty
#'
setGeneric("simEnv", function(object) {
  standardGeneric("simEnv")
})

#' @rdname simList-accessors-envir
setMethod("simEnv",
          signature="simList",
          definition=function(object) {
            return(object@.envir)
})

#' @export
#' @rdname simList-accessors-envir
setGeneric("simEnv<-",
           function(object, value) {
             standardGeneric("simEnv<-")
})

#' @name simEnv<-
#' @aliases simEnv<-,simList-method
#' @rdname simList-accessors-envir
setReplaceMethod("simEnv",
                 signature="simList",
                 function(object, value) {
                   object@.envir <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' Simulation modules and dependencies
#'
#' Accessor functions for the \code{depends}, \code{modules}, \code{.loaded},
#' and \code{.loadOrder} slots in a \code{simList} object.
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
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{simList-class}},
#'          \code{\link{simList-accessors-envir}},
#'          \code{\link{simList-accessors-params}},
#'          \code{\link{simList-accessors-events}},
#'          \code{\link{simList-accessors-times}}.
#' @export
#' @docType methods
#' @aliases simList-accessors-modules
#' @rdname simList-accessors-modules
#'
#' @author Alex Chubaty
#'
setGeneric("simModules", function(object) {
  standardGeneric("simModules")
})

#' @rdname simList-accessors-modules
setMethod("simModules",
          signature="simList",
          definition=function(object) {
            return(object@modules)
})

#' @export
#' @rdname simList-accessors-modules
setGeneric("simModules<-",
           function(object, value) {
             standardGeneric("simModules<-")
})

#' @name simModules<-
#' @aliases simModules<-,simList-method
#' @rdname simList-accessors-modules
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
#' @rdname simList-accessors-modules
#'
setGeneric("simModulesLoaded", function(object) {
  standardGeneric("simModulesLoaded")
})

#' @rdname simList-accessors-modules
setMethod("simModulesLoaded",
          signature="simList",
          definition=function(object) {
            return(object@.loaded$modules)
})

#' @export
#' @rdname simList-accessors-modules
setGeneric("simModulesLoaded<-",
           function(object, value) {
             standardGeneric("simModulesLoaded<-")
})

#' @name simModulesLoaded<-
#' @aliases simModulesLoaded<-,simList-method
#' @rdname simList-accessors-modules
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
#' @rdname simList-accessors-modules
#'
setGeneric("simModulesLoadOrder", function(object) {
  standardGeneric("simModulesLoadOrder")
})

#' @rdname simList-accessors-modules
setMethod("simModulesLoadOrder",
          signature="simList",
          definition=function(object) {
            return(object@.loadOrder)
})

#' @export
#' @rdname simList-accessors-modules
setGeneric("simModulesLoadOrder<-",
           function(object, value) {
             standardGeneric("simModulesLoadOrder<-")
 })

#' @name simModulesLoadOrder<-
#' @aliases simModulesLoadOrder<-,simList-method
#' @rdname simList-accessors-modules
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
#' @rdname simList-accessors-modules
#'
setGeneric("simDepends", function(object) {
  standardGeneric("simDepends")
})

#' @rdname simList-accessors-modules
setMethod("simDepends",
          signature("simList"),
          definition=function(object) {
            return(object@depends)
})

#' @export
#' @rdname simList-accessors-modules
setGeneric("simDepends<-",
           function(object, value) {
             standardGeneric("simDepends<-")
})

#' @name simDepends<-
#' @aliases simDepends<-,simList-method
#' @rdname simList-accessors-modules
setReplaceMethod("simDepends",
                 signature("simList"),
                 function(object, value) {
                   object@depends <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simModules
#' @export
#' @docType methods
#' @rdname simList-accessors-modules
#'
setGeneric("simObjectsLoaded", function(object) {
  standardGeneric("simObjectsLoaded")
})

#' @rdname simList-accessors-modules
setMethod("simObjectsLoaded",
          signature="simList",
          definition=function(object) {
            return(object@.loaded$objects)
})

#' @export
#' @rdname simList-accessors-modules
setGeneric("simObjectsLoaded<-",
           function(object, value) {
             standardGeneric("simObjectsLoaded<-")
})

#' @name simObjectsLoaded<-
#' @aliases simObjectsLoaded<-,simList-method
#' @rdname simList-accessors-modules
setReplaceMethod("simObjectsLoaded",
                 signature="simList",
                 function(object, value) {
                   object@.loaded$objects <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' Show objects referenced in the simulation environment
#'
#' @inheritParams simEnv
#'
#' @param ... arguments passed to \code{ls}, allowing, e.g. \code{all.names=TRUE}
#'
#' @return Returns or sets a list of objects in the \code{simList} environment.
#'
#' @seealso \code{\link{simList-class}},
#'          \code{\link{simList-accessors-modules}},
#'          \code{\link{simList-accessors-params}},
#'          \code{\link{simList-accessors-events}},
#'          \code{\link{simList-accessors-times}}.
#'
#' @export
#' @docType methods
#' @rdname simList-accessors-envir
setGeneric("simObjects", function(object, ...) {
  standardGeneric("simObjects")
})

#' @rdname simList-accessors-envir
setMethod("simObjects",
          signature="simList",
          definition=function(object, ...) {
            x <- lapply(ls(simEnv(object), ...), function(x) {
              eval(parse(text=x), envir=simEnv(object))
            })
            names(x) <- ls(simEnv(object), ...)
            return(x)
})

#' @export
#' @rdname simList-accessors-envir
setGeneric("simObjects<-",
           function(object, value) {
             standardGeneric("simObjects<-")
})

#' @name simObjects<-
#' @aliases simObjects<-,simList-method
#' @rdname simList-accessors-envir
setReplaceMethod("simObjects",
                 signature="simList",
                 function(object, value) {
                   if (is.list(value)) {
                     lapply(value, function(x) {
                       object@.envir[[names(x)]] <- x
                     })
                   } else {
                     object@.envir[[names(value)]] <- value
                   }
                   validObject(object)
                   return(object)
})

################################################################################
#' Get and set simulation parameters.
#'
#' Accessor functions for the \code{params} slot of a \code{simList} object
#' and its elements.
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
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{simList-class}},
#'          \code{\link{simList-accessors-modules}},
#'          \code{\link{simList-accessors-events}},
#'          \code{\link{simList-accessors-times}}.
#' @export
#' @docType methods
#' @aliases simList-accessors-params
#' @rdname simList-accessors-params
#'
setGeneric("simParams", function(object) {
  standardGeneric("simParams")
})

#' @rdname simList-accessors-params
setMethod("simParams",
          signature="simList",
          definition=function(object) {
            return(object@params)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("simParams<-",
           function(object, value) {
             standardGeneric("simParams<-")
})

#' @name simParams<-
#' @aliases simParams<-,simList-method
#' @rdname simList-accessors-params
setReplaceMethod("simParams",
                 signature="simList",
                 function(object, value) {
                   object@params <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("simCheckpointFile", function(object) {
  standardGeneric("simCheckpointFile")
})

#' @rdname simList-accessors-params
setMethod("simCheckpointFile",
          signature="simList",
          definition=function(object) {
            return(object@params$.checkpoint$file)
})

#' @rdname simList-accessors-params
setGeneric("simCheckpointFile<-",
           function(object, value) {
             standardGeneric("simCheckpointFile<-")
})

#' @name simCheckpointFile<-
#' @aliases simCheckpointFile<-,simList-method
#' @rdname simList-accessors-params
setReplaceMethod("simCheckpointFile",
                 signature="simList",
                 function(object, value) {
                   object@params$.checkpoint$file <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("simCheckpointInterval", function(object) {
  standardGeneric("simCheckpointInterval")
})

#' @rdname simList-accessors-params
setMethod("simCheckpointInterval",
          signature="simList",
          definition=function(object) {
            return(object@params$.checkpoint$interval)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("simCheckpointInterval<-",
           function(object, value) {
             standardGeneric("simCheckpointInterval<-")
})

#' @name simCheckpointInterval<-
#' @aliases simCheckpointInterval<-,simList-method
#' @rdname simList-accessors-params
setReplaceMethod("simCheckpointInterval",
                 signature="simList",
                 function(object, value) {
                   object@params$.checkpoint$interval <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("simFileList", function(object) {
  standardGeneric("simFileList")
})

#' @rdname simList-accessors-params
setMethod("simFileList",
          signature="simList",
          definition=function(object) {
            return(object@params$.load$fileList)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("simFileList<-",
           function(object, value) {
             standardGeneric("simFileList<-")
})

#' @name simFileList<-
#' @aliases simFileList<-,simList-method
#' @rdname simList-accessors-params
setReplaceMethod("simFileList",
                 signature="simList",
                 function(object, value) {
                   object@params$.load$fileList <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("simProgressGraphical", function(object) {
  standardGeneric("simProgressGraphical")
})

#' @rdname simList-accessors-params
setMethod("simProgressGraphical",
          signature="simList",
          definition=function(object) {
            return(object@params$.progress$graphical)
})

#' @rdname simList-accessors-params
setGeneric("simProgressGraphical<-",
           function(object, value) {
             standardGeneric("simProgressGraphical<-")
})

#' @name simProgressGraphical<-
#' @aliases simProgressGraphical<-,simList-method
#' @rdname simList-accessors-params
setReplaceMethod("simProgressGraphical",
                 signature="simList",
                 function(object, value) {
                   object@params$.progress$graphical <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("simProgressInterval", function(object) {
  standardGeneric("simProgressInterval")
})

#' @rdname simList-accessors-params
setMethod("simProgressInterval",
          signature="simList",
          definition=function(object) {
            return(object@params$.progress$interval)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("simProgressInterval<-",
           function(object, value) {
             standardGeneric("simProgressInterval<-")
})

#' @name simProgressInterval<-
#' @aliases simProgressInterval<-,simList-method
#' @rdname simList-accessors-params
setReplaceMethod("simProgressInterval",
                 signature="simList",
                 function(object, value) {
                   object@params$.progress$interval <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("simGlobals", function(object) {
  standardGeneric("simGlobals")
})

#' @rdname simList-accessors-params
setMethod("simGlobals",
          signature="simList",
          definition=function(object) {
            return(object@params$.globals)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("simGlobals<-",
           function(object, value) {
             standardGeneric("simGlobals<-")
})

#' @name simGlobals<-
#' @aliases simGlobals<-,simList-method
#' @rdname simList-accessors-params
setReplaceMethod("simGlobals",
                 signature="simList",
                 function(object, value) {
                   object@params$.globals <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simParams
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("simGlobalsOutputPath", function(object) {
  standardGeneric("simGlobalsOutputPath")
})

#' @rdname simList-accessors-params
setMethod("simGlobalsOutputPath",
          signature="simList",
          definition=function(object) {
            return(object@params$.globals$.outputPath)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("simGlobalsOutputPath<-",
           function(object, value) {
             standardGeneric("simGlobalsOutputPath<-")
})

#' @name simGlobalsOutputPath<-
#' @aliases simGlobalsOutputPath<-,simList-method
#' @rdname simList-accessors-params
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
#' Accessor functions for the \code{simtimes} slot of a \code{simList} object
#' and its elements.
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
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{simList-class}},
#'          \code{\link{simList-accessors-envir}},
#'          \code{\link{simList-accessors-modules}},
#'          \code{\link{simList-accessors-params}},
#'          \code{\link{simList-accessors-events}}.
#'
#' @export
#' @docType methods
#' @aliases simList-accessors-times
#' @rdname simList-accessors-times
#'
#' @author Alex Chubaty
#'
setGeneric("simTimes", function(object) {
  standardGeneric("simTimes")
})

#' @rdname simList-accessors-times
setMethod("simTimes",
          signature="simList",
          definition=function(object) {
            return(object@simtimes)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("simTimes<-",
           function(object, value) {
             standardGeneric("simTimes<-")
})

#' @name simTimes<-
#' @aliases simTimes<-,simList-method
#' @rdname simList-accessors-times
setReplaceMethod("simTimes",
                 signature="simList",
                 function(object, value) {
                   object@simtimes <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simTimes
#' @export
#' @docType methods
#' @rdname simList-accessors-times
#'
setGeneric("simCurrentTime", function(object) {
  standardGeneric("simCurrentTime")
})

#' @rdname simList-accessors-times
setMethod("simCurrentTime",
          signature="simList",
          definition=function(object) {
            return(object@simtimes$current)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("simCurrentTime<-",
           function(object, value) {
             standardGeneric("simCurrentTime<-")
})

#' @name simCurrentTime<-
#' @aliases simCurrentTime<-,simList-method
#' @rdname simList-accessors-times
setReplaceMethod("simCurrentTime",
                 signature="simList",
                 function(object, value) {
                   object@simtimes$current <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simTimes
#' @export
#' @docType methods
#' @rdname simList-accessors-times
#'
setGeneric("simStartTime", function(object) {
  standardGeneric("simStartTime")
})

#' @rdname simList-accessors-times
setMethod("simStartTime",
          signature="simList",
          definition=function(object) {
            return(object@simtimes$start)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("simStartTime<-",
           function(object, value) {
             standardGeneric("simStartTime<-")
})

#' @name simStartTime<-
#' @aliases simStartTime<-,simList-method
#' @rdname simList-accessors-times
setReplaceMethod("simStartTime",
                 signature="simList",
                 function(object, value) {
                   object@simtimes$start <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simTimes
#' @export
#' @docType methods
#' @rdname simList-accessors-times
#'
setGeneric("simStopTime", function(object) {
  standardGeneric("simStopTime")
})

#' @rdname simList-accessors-times
setMethod("simStopTime",
          signature="simList",
          definition=function(object) {
            return(object@simtimes$stop)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("simStopTime<-",
           function(object, value) {
             standardGeneric("simStopTime<-")
})

#' @name simStopTime<-
#' @aliases simStopTime<-,simList-method
#' @rdname simList-accessors-times
setReplaceMethod("simStopTime",
                 signature="simList",
                 function(object, value) {
                   object@simtimes$stop <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' Simulation event lists
#'
#' Accessor functions for the \code{events} and \code{completed} slots of a
#' \code{simList} object.
#' By default, the event lists are shown when the \code{simList} object is printed,
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
#' @param object A \code{simList} simulation object.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{simList-class}},
#'          \code{\link{simList-accessors-envir}},
#'          \code{\link{simList-accessors-modules}},
#'          \code{\link{simList-accessors-params}},
#'          \code{\link{simList-accessors-times}}.
#'
#' @export
#' @docType methods
#' @aliases simList-accessors-events
#' @rdname simList-accessors-events
#'
setGeneric("simEvents", function(object) {
  standardGeneric("simEvents")
})

#' @rdname simList-accessors-events
setMethod("simEvents",
          signature="simList",
          definition=function(object) {
            return(object@events)
})

#' @export
#' @rdname simList-accessors-events
setGeneric("simEvents<-",
           function(object, value) {
             standardGeneric("simEvents<-")
})

#' @name simEvents<-
#' @aliases simEvents<-,simList-method
#' @rdname simList-accessors-events
setReplaceMethod("simEvents",
                 signature="simList",
                 function(object, value) {
                   object@events <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams simEvents
#' @export
#' @docType methods
#' @rdname simList-accessors-events
#'
setGeneric("simCompleted", function(object) {
  standardGeneric("simCompleted")
})

#' @rdname simList-accessors-events
setMethod("simCompleted",
          signature="simList",
          definition=function(object) {
            return(object@completed)
})

#' @export
#' @rdname simList-accessors-events
setGeneric("simCompleted<-",
           function(object, value) {
             standardGeneric("simCompleted<-")
})

#' @name simCompleted<-
#' @aliases simCompleted<-,simList-method
#' @rdname simList-accessors-events
setReplaceMethod("simCompleted",
                 signature="simList",
                 function(object, value) {
                   object@completed <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' Add simulation dependencies
#'
#' Internal function.
#' Adds a \code{.moduleDeps} object to the simulation dependency list.
#'
#' @param sim A \code{simList} object.
#'
#' @param x   A named list containing the parameters used to construct a new
#'            \code{.moduleDeps} object.
#'
#' @return A \code{simList} object.
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
          signature(sim="simList", x=".moduleDeps"),
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
#' Get simulation package dependencies
#'
#' Internal function.
#'
#' @param sim A \code{simList} object.
#'
#' @return A sorted character vector of package names.
#'
#' @importFrom magrittr '%>%'
#' @export
#' @docType methods
#' @rdname simReqdPkgs
#'
#' @author Alex Chubaty
#'
setGeneric(".simReqdPkgs", function(sim) {
  standardGeneric(".simReqdPkgs")
})

#' @rdname simReqdPkgs
setMethod(".simReqdPkgs",
          signature(sim="simList"),
          definition=function(sim) {
            pkgs <- lapply(simDepends(sim)@dependencies, function(x) {
              x@reqdPkgs
              }) %>%
              unlist %>%
              append("SpaDES") %>%
              unique %>%
              sort
            return(pkgs)
})

################################################################################
#' Define a new module.
#'
#' Specify a new module's metadata as well as object and package dependecies.
#' Packages are loaded during this call.
#'
#' @inheritParams .addSimDepends
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
setMethod("defineModule",
          signature(sim="simList", x="list"),
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
