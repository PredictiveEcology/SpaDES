if (getRversion() >= "3.1.0")  utils::globalVariables("eventTime")

### `show` generic is already defined in the methods package
#' Show an Object
#'
#' @param object  \code{simList}
#'
#' @export
#' @include simList-class.R
#' @importFrom dplyr mutate
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
            out[[6]] <- capture.output(print(rbind(times(object))))
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

            ### list stored objects
            out[[14]] <- capture.output(cat(">> Objects stored:\n"))
            out[[15]] <- capture.output(print(ls.str(simEnv(object))))
            out[[16]] <- capture.output(cat("\n"))

            ### params
            omit <- which(names(params(object))==".load" |
                            names(params(object))==".progress")

            p <- mapply(function(x, y) {
              data.frame(Module=x, Parameter=names(y), Value=unlist(y),
                         stringsAsFactors=FALSE, row.names=NULL)
            },
            x=names(params(object))[-omit], y=params(object)[-omit],
            USE.NAMES=TRUE, SIMPLIFY=FALSE)
            if (length(p)) {
              q = do.call(rbind, p)
              q = q[order(q$Module, q$Parameter),]
            } else {
              q = cbind(Module=list(), Parameter=list())
            }
            out[[17]] <- capture.output(cat(">> Parameters:\n"))
            out[[18]] <- capture.output(print(q, row.names=FALSE))
            out[[19]] <- capture.output(cat("\n"))

            ### completed events
            out[[20]] <- capture.output(cat(">> Completed Events:\n"))
            out[[21]] <- capture.output(print(completed(object)))
            out[[22]] <- capture.output(cat("\n"))

            ### scheduled events
            out[[23]] <- capture.output(cat(">> Scheduled Events:\n"))
            out[[24]] <- capture.output(print(events(object)))
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
#' @include simList-class.R
#' @docType methods
#' @rdname ls-method
ls.simList <- function(name) {
  ls(simEnv(name))
}

#' @export
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
#'
#' @param name  A \code{simList} object.
#' @param pos   A \code{simList} object, used only if \code{name} not provided.
#'
#' @export
#' @include simList-class.R
#' @docType methods
#' @rdname ls_str-method
ls.str.simList <- function(name) {
  ls.str(simEnv(name))
}

#' export
#' @rdname ls_str-method
setMethod("ls.str",
          signature(pos="missing", name="simList"),
          definition=function(name) {
            ls.str.simList(name)
})

#' @export
#' @rdname ls_str-method
setMethod("ls.str",
          signature(pos="simList", name="missing"),
          definition=function(pos) {
            ls.str.simList(pos)
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
#' @include simList-class.R
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
#' @aliases [[<-,simList,ANY,ANY,ANY-method
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
#' @include simList-class.R
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
#' @include simList-class.R
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
#' @include simList-class.R
#' @docType methods
#' @rdname simList-accessors-modules
#'
setGeneric("simModulesLoaded", function(object) {
  standardGeneric("simModulesLoaded")
})

#' @export
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
#' @include simList-class.R
#' @docType methods
#' @rdname simList-accessors-modules
#'
setGeneric("simModulesLoadOrder", function(object) {
  standardGeneric("simModulesLoadOrder")
})

#' @export
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
#' @include simList-class.R
#' @docType methods
#' @rdname simList-accessors-modules
#'
setGeneric("simDepends", function(object) {
  standardGeneric("simDepends")
})

#' @export
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
#' @export
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
#' @include simList-class.R
#' @docType methods
#' @rdname simList-accessors-modules
#'
setGeneric("simObjectsLoaded", function(object) {
  standardGeneric("simObjectsLoaded")
})

#' @export
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
#' @export
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
#' @include simList-class.R
#' @docType methods
#' @rdname simList-accessors-envir
#'
setGeneric("simObjects", function(object, ...) {
  standardGeneric("simObjects")
})

#' @export
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
#' @export
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
#'    \code{globals} \tab List of global simulation parameters.\cr
#'    \code{params} \tab Nested list of all simulation parameter.\cr
#' }
#' Advanced use
#' \tabular{lll}{
#'    Accessor method \tab Module \tab Description \cr
#'    \code{simCheckpointFile} \tab \code{.checkpoint} \tab Name of the checkpoint file. (advanced)\cr
#'    \code{simCheckpointInterval} \tab \code{.checkpoint} \tab The simulation checkpoint interval. (advanced)\cr
#'    \code{outputPath} \tab \code{NA} \tab Global simulation output path. (advanced)\cr
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
#' @include simList-class.R
#' @docType methods
#' @aliases simList-accessors-params
#' @rdname simList-accessors-params
#'
setGeneric("params", function(object) {
  standardGeneric("params")
})

#' @export
#' @rdname simList-accessors-params
setMethod("params",
          signature="simList",
          definition=function(object) {
            return(object@params)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("params<-",
           function(object, value) {
             standardGeneric("params<-")
})

#' @name params<-
#' @aliases params<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("params",
                 signature="simList",
                 function(object, value) {
                   object@params <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams params
#' @export
#' @include simList-class.R
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("simCheckpointFile", function(object) {
  standardGeneric("simCheckpointFile")
})

#' @export
#' @rdname simList-accessors-params
setMethod("simCheckpointFile",
          signature="simList",
          definition=function(object) {
            return(object@params$.checkpoint$file)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("simCheckpointFile<-",
           function(object, value) {
             standardGeneric("simCheckpointFile<-")
})

#' @name simCheckpointFile<-
#' @aliases simCheckpointFile<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("simCheckpointFile",
                 signature="simList",
                 function(object, value) {
                   object@params$.checkpoint$file <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams params
#' @export
#' @include simList-class.R
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("simCheckpointInterval", function(object) {
  standardGeneric("simCheckpointInterval")
})

#' @export
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
#' @export
setReplaceMethod("simCheckpointInterval",
                 signature="simList",
                 function(object, value) {
                   object@params$.checkpoint$interval <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams params
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("simFileList", function(object) {
  standardGeneric("simFileList")
})

#' @export
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
#' @export
setReplaceMethod("simFileList",
                 signature="simList",
                 function(object, value) {
                   object@params$.load$fileList <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams params
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("simProgressGraphical", function(object) {
  standardGeneric("simProgressGraphical")
})

#' @export
#' @rdname simList-accessors-params
setMethod("simProgressGraphical",
          signature="simList",
          definition=function(object) {
            return(object@params$.progress$graphical)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("simProgressGraphical<-",
           function(object, value) {
             standardGeneric("simProgressGraphical<-")
})

#' @name simProgressGraphical<-
#' @aliases simProgressGraphical<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("simProgressGraphical",
                 signature="simList",
                 function(object, value) {
                   object@params$.progress$graphical <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams params
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("simProgressInterval", function(object) {
  standardGeneric("simProgressInterval")
})

#' @export
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
#' @export
setReplaceMethod("simProgressInterval",
                 signature="simList",
                 function(object, value) {
                   object@params$.progress$interval <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams params
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("globals", function(object) {
  standardGeneric("globals")
})

#' @export
#' @rdname simList-accessors-params
setMethod("globals",
          signature="simList",
          definition=function(object) {
            return(object@params$.globals)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("globals<-",
           function(object, value) {
             standardGeneric("globals<-")
})

#' @name globals<-
#' @aliases globals<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("globals",
                 signature="simList",
                 function(object, value) {
                   object@params$.globals <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams params
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("outputPath", function(object) {
  standardGeneric("outputPath")
})

#' @export
#' @rdname simList-accessors-params
setMethod("outputPath",
          signature="simList",
          definition=function(object) {
            return(object@params$.globals$.outputPath)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("outputPath<-",
           function(object, value) {
             standardGeneric("outputPath<-")
})

#' @name outputPath<-
#' @aliases outputPath<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("outputPath",
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
#' of the simulation:
#' \tabular{ll}{
#'    \code{time} \tab Current simulation time.\cr
#'    \code{start} \tab Simulation start time.\cr
#'    \code{end} \tab Simulation stop time.\cr
#'    \code{timeunit} \tab Simulation timeunit.\cr
#'    \code{times} \tab List of all simulation times (current, start, stop, timeunit).\cr
#' }
#'
#' @param x      A \code{simList} simulation object.
#'
#' @param unit   Character. One of the time units used in \code{SpaDES}.
#'
#' @param value  The object to be stored at the slot.
#'
#' @param ...    Additional parameters.
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
#' @include simList-class.R
#' @include times.R
#' @docType methods
#' @aliases simList-accessors-times
#' @rdname simList-accessors-times
#'
#' @author Alex Chubaty and Eliot McIntire
#'
setGeneric("times", function(x, ...) {
  chron::times(x, ...)
})

#' @export
#' @rdname simList-accessors-times
setMethod(
  "times",
  signature="simList",
  definition=function(x) {
    return(x@simtimes)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("times<-", function(x, value) {
  standardGeneric("times<-")
})

#' @name times<-
#' @aliases times<-,simList-method
#' @export
#' @rdname simList-accessors-times
setReplaceMethod("times",
                 signature="simList",
                 function(x, value) {
                   x@simtimes <- value
                   validObject(x)
                   return(x)
})

################################################################################
#' @inheritParams times
#' @include simList-class.R
#' @include times.R
#' @export
#' @docType methods
#' @rdname simList-accessors-times
#' @importFrom stringr str_detect
#'
setGeneric("time", function(x, unit, ...) {
  stats::time(x, ...)
})

#' @export
#' @rdname simList-accessors-times
setMethod(
  "time",
  signature = c("simList", "missing"),
  definition = function(x) {
    mUnit <- .moduleTimeunit(x)
    if (is.null(mUnit)) {
      mUnit <- NA_character_
    }
    t <- time(x, mUnit)
    return(t)
})

#' @export
#' @rdname simList-accessors-times
setMethod(
  "time",
  signature = c("simList", "character"),
  definition = function(x, unit) {
    if (!is.na(unit)) {
      if (!str_detect("^seconds?$", pattern = unit)) {
        # i.e., if not in same units as simulation
        t <- convertTimeunit(x@simtimes$current, unit)
        return(t)
      }
    }
    t <- x@simtimes$current
    return(t)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("time<-",
           function(x, value) {
             standardGeneric("time<-")
})

#' @name time<-
#' @aliases time<-,simList-method
#' @export
#' @rdname simList-accessors-times
setReplaceMethod("time",
                 signature="simList",
                 function(x, value) {
                   if(is.null(attributes(value)$unit)) {
                     attributes(value)$unit <- timeunit(x)
                   }
                   value <- convertTimeunit(value, "second")
                   x@simtimes$current <- value
                   validObject(x)
                   return(x)
})

################################################################################
#' @inheritParams times
#' @importFrom stringr str_detect
#' @include times.R
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-times
#'
setGeneric("end", function(x, unit, ...) {
  stats::end(x, ...)
})

#' @export
#' @rdname simList-accessors-times
setMethod(
  "end",
  signature = c("simList", "missing"),
  definition = function(x) {
    mUnit <- .moduleTimeunit(x)
    if (is.null(mUnit)) {
      mUnit <- NA_character_
    }
    t <- end(x, mUnit)
    return(t)
})

#' @export
#' @rdname simList-accessors-times
setMethod(
  "end",
  signature=c("simList", "character"),
  definition=function(x, unit) {
    if (!is.na(unit)) {
      if (!str_detect("^seconds?$", pattern = unit)) {
        # i.e., if not in same units as simulation
        t <- convertTimeunit(x@simtimes$stop, unit)
        return(t)
      }
    }
    t <- x@simtimes$stop
    return(t)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("end<-", function(x, value) {
  standardGeneric("end<-")
})

#' @name end<-
#' @aliases end<-,simList-method
#' @export
#' @rdname simList-accessors-times
setReplaceMethod(
  "end",
  signature="simList",
  function(x, value) {
    stopifnot(is.character(attr(value, "unit")))

    # convert time units, if required
    if(is.null(attributes(value)$unit)) {
      attributes(value)$unit <- timeunit(x)
    }
    value <- convertTimeunit(value, "second")

    x@simtimes$stop <- value
    validObject(x)
    return(x)
})

################################################################################
#' @inheritParams times
#' @importFrom stringr str_detect
#' @include simList-class.R
#' @include times.R
#' @export
#' @docType methods
#' @rdname simList-accessors-times
#'
setGeneric("start", function(x, unit, ...) {
  stats::start(x, ...)
})

#' @rdname simList-accessors-times
setMethod(
  "start",
  signature=c("simList","missing"),
  definition=function(x) {
    mUnit <- .moduleTimeunit(x)
    if (is.null(mUnit)) {
      mUnit <- NA_character_
    }
    t <- start(x, mUnit)
    return(t)
})

#' @export
#' @rdname simList-accessors-times
setMethod(
  "start",
  signature=c("simList","character"),
  definition=function(x, unit) {
    if (!is.na(unit)) {
      if (!str_detect("^seconds?$", pattern = unit)) {
        # i.e., if not in same units as simulation
        t <- convertTimeunit(x@simtimes$start, unit)
        return(t)
      }
    }
    t <- x@simtimes$start
    return(t)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("start<-", function(x, value) {
  standardGeneric("start<-")
})

#' @name start<-
#' @aliases start<-,simList-method
#' @rdname simList-accessors-times
setReplaceMethod("start",
                 signature="simList",
                 function(x, value) {
                   stopifnot(is.character(attr(value, "unit")))

                   # convert time units, if required
                   if(is.null(attributes(value)$unit)) {
                     attributes(value)$unit <- timeunit(x)
                   }
                   value <- convertTimeunit(value, "second")

                   x@simtimes$start <- value
                   validObject(x)
                   return(x)
})

################################################################################
#' @inheritParams times
#' @include simList-class.R
#' @include times.R
#' @export
#' @docType methods
#' @rdname simList-accessors-times
#'
setGeneric(".moduleTimeunit", function(x) {
  standardGeneric(".moduleTimeunit")
})

#' @export
#' @docType methods
#' @rdname simList-accessors-times
setMethod(
  ".moduleTimeunit",
  signature=c("simList"),
  definition=function(x) {
    mod <- .callingModuleName(x)
    out <- if (!is.null(mod)) {
      timeunits(x)[[mod]]
    } else {
      NA_character_
    }
    return(out)
})

#' @export
#' @docType methods
#' @rdname simList-accessors-times
#'
setMethod(
  ".moduleTimeunit",
  signature=c("NULL"),
  definition=function(x) {
    return(NULL)
})

################################################################################
#' \code{.callingModuleName} returns the name of the module that is currently
#' the active module calling functions like \code{scheduleEvent}.
#' This will only return the module name if it is inside a \code{spades}
#' function call, i.e., it will return \code{NULL} if used in interactive mode.
#'
#' @inheritParams simModules
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-modules
#' @author Eliot McIntire
#'
setGeneric(".callingModuleName", function(object) {
  standardGeneric(".callingModuleName")
})

#' @export
#' @docType methods
#' @importFrom stringr str_detect
#' @rdname simList-accessors-modules
setMethod(
  ".callingModuleName",
  signature=c("simList"),
  definition=function(object) {
    # Only return module name if inside a spades call,
    #  because this only makes sense if there is an "active" module
    if (any(str_detect(as.character(sys.call(1)), pattern = "spades"))) {
      st <- str_detect(as.character(sys.calls()), pattern = "moduleCall")
      if (any(st)) {
        mod <- strsplit(
          eval(parse(text="moduleCall"), envir=sys.frame(which(st)[1]-1)),
          split="\\.")[[1]][2]
      } else {
        mod <- NULL
      }
    } else {
      mod <- NULL
    }
  return(mod)
})

################################################################################
#' Simulation event lists
#'
#' Accessor functions for the \code{events} and \code{completed} slots of a
#' \code{simList} object.
#' By default, the event lists are shown when the \code{simList} object is printed,
#' thus most users will not require direct use of these methods.
#' \tabular{ll}{
#'    \code{events} \tab Scheduled simulation events (the event queue).\cr
#'    \code{completed} \tab Completed simulation events.\cr
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
#' @include simList-class.R
#' @docType methods
#' @aliases simList-accessors-events
#' @rdname simList-accessors-events
#'
setGeneric("events", function(object) {
  standardGeneric("events")
})

#' @export
#' @rdname simList-accessors-events
setMethod("events",
          signature=c("simList","character"),
          definition=function(object, unit) {
            out <- if (!is.null(object@events$eventTime)) {
              object@events %>%
                dplyr::mutate(eventTime=convertTimeunit(eventTime, unit))
            } else {
              object@events
            }
            return(out)
})

#' @export
#' @rdname simList-accessors-events
setMethod("events",
          signature=c("simList","missing"),
          definition=function(object, unit) {
            out <- events(object, timeunit(object))
            return(out)
          })

#' @export
#' @rdname simList-accessors-events
setGeneric("events<-",
           function(object, value) {
             standardGeneric("events<-")
})

#' @name events<-
#' @aliases events<-,simList-method
#' @export
#' @rdname simList-accessors-events
setReplaceMethod("events",
                 signature="simList",
                 function(object, value) {
                   if(is.null(attributes(value$eventTime)$unit)) {
                     attributes(value$eventTime)$unit <- timeunit(object)
                   } else {
                     value[,eventTime:=convertTimeunit(eventTime, "second")]
                   }
                   object@events <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams events
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-events
#'
setGeneric("completed", function(object, unit) {
  standardGeneric("completed")
})

#' @rdname simList-accessors-events
#' @export
setMethod("completed",
          signature=c("simList","character"),
          definition=function(object, unit) {
            out <- if (!is.null(object@completed$eventTime)) {
              object@completed %>%
                dplyr::mutate(eventTime=convertTimeunit(eventTime, unit))
            } else {
              object@completed
            }
            return(out)
          })

#' @export
#' @rdname simList-accessors-events
setMethod("completed",
          signature=c("simList","missing"),
          definition=function(object, unit) {
            out <- completed(object, timeunit(object))
            return(out)
          })

#' @export
#' @rdname simList-accessors-events
setGeneric("completed<-",
           function(object, value) {
             standardGeneric("completed<-")
})

#' @name completed<-
#' @aliases completed<-,simList-method
#' @export
#' @rdname simList-accessors-events
setReplaceMethod("completed",
                 signature="simList",
                 function(object, value) {
                   object@completed <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams times
#'
#' @details \code{timeunit} will extract the current units of the time used in a
#' \code{spades} call.
#' If it is set within a \code{simInit}, e.g.,
#' \code{times=list(start=0, stop=52, timestepUnit="week")}, it will set the
#' units for that simulation.
#' By default, a \code{simInit} call will use the smallest unit contained within
#' the metadata for the modules being used.
#' If \code{NA}, \code{timestepUnit} defaults to none.
#'
#' @importFrom stringr str_detect
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-times
#' @author Eliot McIntire
#'
setGeneric("timeunit", function(x) {
  standardGeneric("timeunit")
})

#' @rdname simList-accessors-times
#' @export
setMethod("timeunit",
          signature="simList",
          definition=function(x) {
            return(x@simtimes$timestepUnit)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("timeunit<-",
           function(x, value) {
             standardGeneric("timeunit<-")
})

#' @name timeunit<-
#' @aliases timeunit<-,simList-method
#' @export
#' @rdname simList-accessors-times
setReplaceMethod(
  "timeunit",
  signature="simList",
  function(x, value) {
    if (any(str_detect(.spadesTimes, pattern = value), na.rm=TRUE)) {
      x@simtimes$timestepUnit <- value
    } else {
      x@simtimes$timestepUnit <- NA_character_
      if (!is.na(value)) {
        message("unknown timestepUnit provided: ", value)
      }
    }
    validObject(x)
    return(x)
})

################################################################################
#' @inheritParams times
#'
#' @details \code{timeunits} will extract the current units of the time of all
#' modules used in a simulation.
#' This is different from \code{timeunits} because it is not necessarily
#' associated with a \code{spades} call.
#'
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-times
#'
setGeneric("timeunits", function(x) {
  standardGeneric("timeunits")
})

#' @export
#' @rdname simList-accessors-times
setMethod(
  "timeunits",
  signature="simList",
  definition=function(x) {
    timestepUnits <- lapply(simDepends(x)@dependencies, function(y) {
      y@timestepUnit
    })
    names(timestepUnits) <- sapply(simDepends(x)@dependencies, function(y) {
      y@name
    })
    return(timestepUnits)
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
#' @include simList-class.R
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
#' @param sim A \code{simList} object.
#'
#' @return A sorted character vector of package names.
#'
#' @importFrom magrittr '%>%'
#' @export
#' @include simList-class.R
#' @docType methods
#' @rdname simReqdPkgs
#'
#' @author Alex Chubaty
#'
setGeneric("simReqdPkgs", function(sim) {
  standardGeneric("simReqdPkgs")
})

#' @export
#' @rdname simReqdPkgs
setMethod("simReqdPkgs",
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
#' @importFrom raster extent
#' @include simList-class.R
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

#' @export
#' @rdname defineModule
setMethod(
  "defineModule",
  signature(sim="simList", x="list"),
  definition=function(sim, x) {
    loadPackages(x$reqdPkgs)

    ## enforce/coerce types for the user-supplied param list
    x$name <- as.character(x$name)
    x$description <- as.character(x$description)
    x$keywords <- as.character(x$keywords)
    if (!is(x$authors, "person")) {
      stop("invalid module definition: ", x$name,
           ": authors must be a `person` class.")
      }
    if (is.character(x$version) || is.numeric(x$version)) {
      x$version <- as.numeric_version(x$version)
    }
    if (!is(x$spatialExtent, "Extent")) {
      if (is.na(x$spatialExtent)) {
        x$spatialExtent <- raster::extent(rep(NA_real_, 4))
      }
    }
    if (!is.numeric.POSIXt(x$timeframe)) {
      x$timeframe <- as.POSIXlt(x$timeframe)
      if (length(x$timeframe)==1) {
        if (is.na(x$timeframe)) {
          x$timeframe <- as.POSIXlt(c(NA,NA))
        } else {
          x$timeframe <- as.POSIXlt(c(x$timeframe[[1]],NA))
        }
      }
    }
    if (is.na(x$timestepUnit)) {
      x$timestepUnit <- NA_character_
    }
    x$reqdPkgs <- as.list(x$reqdPkgs)
    x$citation <- as.list(x$citation)
    if (!is(x$parameters, "data.frame")) {
      stop("invalid module definition: ", x$name,
           ": parameters must be a `data.frame`.")
    }
    if (!is(x$inputObjects, "data.frame")) {
      stop("invalid module definition: ", x$name,
           ": inputObjects must be a `data.frame`.")
    }
    if (!is(x$outputObjects, "data.frame")) {
      stop("invalid module definition: ", x$name,
           ": outputObjects must be a `data.frame`.")
    }

    ## create module deps object and add to sim deps
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
#' @param min       With \code{max}, used to define a suitable range of values.
#'                  Non-standard evaluation is used for the expression.
#' @param max       With \code{min}, used to define a suitable range of values.
#'                  Non-standard evaluation is used for the expression.
#' @param desc      Text string providing a brief description of the parameter.
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
#' parameters = rbind(
#'   defineParameter("lambda", "numeric", 1.23, desc="intrinsic rate of increase"),
#'   defineParameter("p", "numeric", 0.2, 0, 1, "probability of attack")
#' )
#'
setGeneric("defineParameter", function(name, class, default, min, max, desc) {
  standardGeneric("defineParameter")
})

#' @rdname defineParameter
setMethod("defineParameter",
          signature(name="character", class="character",
                    default="ANY", min="ANY", max="ANY", desc="character"),
          definition=function(name, class, default, min, max, desc) {
            # coerce `min` and `max` to same type as `default`
            min <- as(min, class)
            max <- as(max, class)

            df <- data.frame(name=name, class=class,
                             default=I(list(substitute(default))),
                             min=I(list(substitute(min))),
                             max=I(list(substitute(max))),
                             desc=desc,
                             stringsAsFactors=FALSE)
            return(df)
})

#' @rdname defineParameter
setMethod("defineParameter",
          signature(name="character", class="character",
                    default="ANY", min="missing", max="missing",
                    desc="character"),
          definition=function(name, class, default, desc) {
            # coerce `min` and `max` to same type as `default`
            min <- as(NA, class)
            max <- as(NA, class)

            df <- data.frame(name=name, class=class,
                             default=I(list(default)),
                             min=I(list(substitute(min))),
                             max=I(list(substitute(max))),
                             desc=desc,
                             stringsAsFactors=FALSE)
            return(df)
})
