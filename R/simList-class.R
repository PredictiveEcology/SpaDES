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
#' @slot inputs   The list of length 2: a \code(data.table) or \code(data.frame) of files and metadata,
#' and a list of optional arguments to pass to an import function
#'
#' @slot outputs   The list of length 2: a \code(data.table) or \code(data.frame) of files and metadata,
#' and a list of optional arguments to pass to an import function
#'
#' @slot paths   Named list of \code{modulePath}, \code{inputPath}, and \code{outputPath} paths. Partial
#' matching is performed.
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
#' @importFrom data.table data.table
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3). San Fransisco, CA: No Starch Press, Inc.. Retrieved from \url{http://www.nostarch.com/artofr.htm}
#'
#' @author Alex Chubaty and Eliot McIntire
#'
setClass(".simList",
         slots=list(modules="list", params="list",
                    events="data.table", completed="data.table",
                    depends=".simDeps", simtimes="list",
                    inputs="list", outputs="list",
                    paths="list"),
         prototype=list(modules=as.list(NULL),
                        params=list(.checkpoint=list(interval=NA_real_, file=NULL),
                                    .progress=list(type=NULL, interval=NULL)),
                        events=as.data.table(NULL), completed=as.data.table(NULL),
                        depends=new(".simDeps", dependencies=list(NULL)),
                        simtimes=list(current=0.00, start=0.00, stop=1.00, timeunit=NA_character_),
                        inputs=list(table=data.table(file=character(0), fun=character(0),
                                          package=character(0), objectName=character(0),
                                          loadTime=numeric(0), loaded=logical(0)),
                                    arg=list(NULL)),
                        outputs=list(table=as.data.table(NULL), arg=list(NULL)),
                        paths=list(modulePath="./", inputPath="./", outputPath="./")),
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
#'                  \code{ls.str}, \code{objs}. See examples.
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
  x@.list <- as.list(envir(from))
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
