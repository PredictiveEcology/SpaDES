if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(".SD", "eventTime", "savetime", "exts", "eventType"))
}

### `show` generic is already defined in the methods package
#' Show an Object
#'
#' @param object  \code{simList}
#'
#' @export
#' @include simList-class.R
#' @importFrom dplyr mutate
#' @importFrom stats na.omit
# @importFrom utils capture.output
#'
#' @docType methods
#' @rdname show-method
setMethod(
  "show",
  signature = "simList",
  definition = function(object) {
    out <- list()
    out[[1]] <- capture.output(
      cat(rep("=", getOption("width"), sep = ""), "\n", sep = "")
    )

    ### simulation dependencies
    out[[2]] <- capture.output(cat(">> Simulation dependencies:\n"))
    out[[3]] <- "use `depends(sim)` to view dependencies for each module"
    out[[4]] <- capture.output(cat("\n"))

    ### simtimes
    out[[5]] <- capture.output(cat(">> Simulation times:\n"))
    out[[6]] <- capture.output(print(rbind(times(object))))
    out[[7]] <- capture.output(cat("\n"))

    ### modules loaded
    out[[8]] <- capture.output(cat(">> Modules:\n"))
    ord <- match(unlist(modules(object)), names(timeunits(object))) %>% na.omit
    out[[9]] <- capture.output(print(
      cbind(Name = modules(object),
            #Timeunit = c(rep(NA_character_, 4), unname(timeunits(object))[ord])),
            Timeunit = unname(timeunits(object))[ord]),
      quote = FALSE, row.names = FALSE))
    out[[10]] <- capture.output(cat("\n"))

    ### objects loaded
    out[[11]] <- capture.output(cat(">> Objects Loaded:\n"))

    out[[12]] <- if (NROW(inputs(object)[na.omit(inputs(object)$loaded == TRUE), ])) {
      capture.output(print(inputs(object)[na.omit(inputs(object)$loaded == TRUE), ]))
    }
    out[[13]] <- capture.output(cat("\n"))

    ### list stored objects
    out[[14]] <- capture.output(cat(">> Objects stored:\n"))
    out[[15]] <- capture.output(print(ls.str(envir(object))))
    out[[16]] <- capture.output(cat("\n"))

    ### params
    omit <- which(names(params(object)) == ".progress")

    p <- mapply(
      function(x, y) {
        if (length(names(y)) > 0)
        data.frame(Module = x, Parameter = names(y), Value = I(as.list(y)),
                   stringsAsFactors = FALSE, row.names = NULL)
      },
      x = names(params(object))[-omit],
      y = params(object)[-omit],
      USE.NAMES = TRUE, SIMPLIFY = FALSE
    )
    if (length(p)) {
      q <- do.call(rbind, p)
      q <- q[order(q$Module, q$Parameter), ]
    } else {
      q <- cbind(Module = list(), Parameter = list())
    }
    out[[17]] <- capture.output(cat(">> Parameters:\n"))
    out[[18]] <- capture.output(print(q, row.names = FALSE))
    out[[19]] <- capture.output(cat("\n"))

    ### completed events
    out[[20]] <- capture.output(cat(">> Completed Events:\n"))
    out[[21]] <- capture.output(print(completed(object)))
    out[[22]] <- capture.output(cat("\n"))

    ### Current events
    out[[23]] <- capture.output(cat(">> Current Event:\n"))
    out[[24]] <- capture.output(print(current(object)))
    out[[25]] <- capture.output(cat("\n"))

    ### scheduled events
    out[[26]] <- capture.output(cat(">> Scheduled Events:\n"))
    out[[27]] <- capture.output(print(events(object)))
    out[[28]] <- capture.output(cat("\n"))

    ### print result
    cat(unlist(out), fill = FALSE, sep = "\n")
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
#' @family functions to access elements of a \code{simList} object
ls.simList <- function(name) {
  ls(envir(name))
}

#' @export
#' @rdname ls-method
setMethod("ls",
          signature(name = "simList"),
          definition = function(name) {
            ls.simList(name)
})

#' @rdname ls-method
objects.simList <- function(name) {
  ls(envir(name))
}

#' @export
#' @rdname ls-method
setMethod("objects",
          signature(name = "simList"),
          definition = function(name) {
            objects.simList(name)
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
#' @family functions to access elements of a \code{simList} object
#'
ls.str.simList <- function(name) {
  ls.str(name@.envir)
}

#' export
#' @rdname ls_str-method
setMethod("ls.str",
          signature(pos = "missing", name = "simList"),
          definition = function(name) {
            ls.str.simList(name)
})

#' @export
#' @rdname ls_str-method
setMethod("ls.str",
          signature(pos = "simList", name = "missing"),
          definition = function(pos) {
            ls.str.simList(pos)
})

################################################################################
#' Simulation environment
#'
#' Accessor functions for the \code{.envir} slot in a \code{simList} object.
#' These are included for advanced users.
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @inheritParams objs
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @family functions to access elements of a \code{simList} object
#' @seealso \code{\link{SpaDES}}, specifically the section 1.2.8 on simList environment.
#'
#' @export
#' @include simList-class.R
#' @docType methods
#' @aliases simList-accessors-envir
#' @rdname simList-accessors-envir
#'
#' @author Alex Chubaty
#'
setGeneric("envir", function(sim) {
  standardGeneric("envir")
})

#' @rdname simList-accessors-envir
setMethod("envir",
          signature = "simList",
          definition = function(sim) {
            return(sim@.envir)
})

#' @export
#' @rdname simList-accessors-envir
setGeneric("envir<-",
           function(sim, value) {
             standardGeneric("envir<-")
})

#' @name envir<-
#' @aliases envir<-,simList-method
#' @rdname simList-accessors-envir
setReplaceMethod("envir",
                 signature = "simList",
                 function(sim, value) {
                   if (!is.environment(value)) stop("Must be an environment")
                   sim@.envir <- value
                   return(sim)
})

################################################################################
#' Extract or replace an object from the simulation environment
#'
#' The \code{[[} and \code{$} operators provide "shortcuts" for accessing
#' objects in the simulation environment.
#' I.e., instead of using \code{envir(sim)$object} or \code{envir(sim)[["object"]]},
#' one can simply use \code{sim$object} or \code{sim[["object"]]}.
#'
#' \code{objs} can take \code{...} arguments passed to \code{ls},
#' allowing, e.g. \code{all.names=TRUE}
#' \code{objs<-} requires takes a named list of values to be assigned in
#' the simulation envirment.
#'
#' @param sim      A \code{simList} object from which to extract element(s) or
#'                 in which to replace element(s).
#' @param x        A \code{simList} object from which to extract element(s) or
#'                 in which to replace element(s).
#' @param i,j,...  Indices specifying elements to extract or replace.
#' @param name     A literal character string or a \code{\link{name}}.
#' @param drop     not implemented.
#' @param value    Any R object.
#'
#' @return Returns or sets a list of objects in the \code{simList} environment.
#'
#' @family functions to access elements of a \code{simList} object
#' @seealso \code{\link{SpaDES}}, specifically the section 1.2.1 on Simulation Parameters.
#'
#' @export
#' @include simList-class.R
#' @docType methods
#' @aliases simList-accessors-objects
#' @rdname objects
#'
setGeneric("objs", function(sim, ...) {
  standardGeneric("objs")
})

#' @export
#' @rdname objects
setMethod("objs",
          signature = "simList",
          definition = function(sim, ...) {
            w <- lapply(ls(sim@.envir, ...), function(z) {
              eval(parse(text = z), envir = sim@.envir)
            })
            names(w) <- ls(sim@.envir, ...)
            return(w)
})

#' @export
#' @rdname objects
setGeneric("objs<-",
           function(sim, value) {
             standardGeneric("objs<-")
})

#' @name objs<-
#' @aliases objs<-,simList-method
#' @rdname objects
#' @export
setReplaceMethod(
  "objs",
  signature = "simList",
  function(sim, value) {
    if (is.list(value)) {
     list2env(value, envir = sim@.envir)
     newInputs <- data.frame(
       objectName = names(value),
       loadTime = as.numeric(sim@simtimes[["current"]]),
       loaded = TRUE,
       stringsAsFactors = FALSE) %>% .fillInputRows(startTime = start(sim))
     inputs(sim) <- rbind(inputs(sim), newInputs)

    # lapply(names(value), function(z) {
    #   sim@.envir[[z]] <- value[[z]]
    # })
    } else {
     stop("must provide a named list.")
    }
    validObject(sim)
    return(sim)
})

################################################################################
#' @inheritParams objs
#' @export
#' @include simList-class.R
#' @name [[
#' @aliases [[,simList,ANY,ANY-method
#' @aliases simList-accessors-objects
#' @docType methods
#' @rdname objects
setMethod("[[", signature(x = "simList", i = "ANY", j = "ANY"),
          definition = function(x, i, j, ..., drop) {
            return(x@.envir[[i]])
})

#' @export
#' @name [[<-
#' @aliases [[<-,simList,ANY,ANY,ANY-method
#' @aliases simList-accessors-objects
#' @rdname objects
setReplaceMethod("[[", signature(x = "simList", value = "ANY"),
                 definition = function(x, i, value) {
                   assign(i, value, envir = x@.envir, inherits = FALSE)
                   return(x)
})

#' @export
#' @name $
#' @aliases $,simList-method
#' @aliases simList-accessors-objects
#' @rdname objects
setMethod("$", signature(x = "simList"),
          definition = function(x, name) {
            return(x@.envir[[name]])
})

#' @export
#' @name $<-
#' @aliases $<-,simList-method
#' @aliases simList-accessors-objects
#' @rdname objects
setReplaceMethod("$", signature(x = "simList", value = "ANY"),
                 definition = function(x, name, value) {
                   x@.envir[[name]] <- value
                   return(x)
})

################################################################################
#' Simulation modules and dependencies
#'
#' Accessor functions for the \code{depends} and \code{modules} slots in a
#' \code{simList} object.
#' These are included for advanced users.
#' \tabular{ll}{
#'    \code{\link{depends}} \tab List of simulation module dependencies. (advanced) \cr
#'    \code{\link{modules}} \tab List of simulation modules to be loaded. (advanced) \cr
#'    \code{\link{inputs}} \tab List of loaded objects used in simulation. (advanced) \cr
#' }
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @inheritParams objs
#'
#' @param value The object to be stored at the slot.
#'
#' @param hidden Logical. If TRUE, show the default core modules.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @family functions to access elements of a \code{simList} object
#' @seealso \code{\link{SpaDES}}, specifically the section 1.2.7 on Modules and dependencies.
#'
#' @export
#' @include simList-class.R
#' @docType methods
#' @aliases simList-accessors-modules
#' @rdname simList-accessors-modules
#'
#' @author Alex Chubaty
#'
setGeneric("modules", function(sim, hidden = FALSE) {
  standardGeneric("modules")
})

#' @rdname simList-accessors-modules
setMethod(
  "modules",
  signature = ".simList",
  definition = function(sim, hidden) {
    if (hidden) {
      mods <- sim@modules
    } else {
      hiddenMods <- unlist(sim@modules) %in% (.coreModules() %>% unname() %>% unlist())
      mods <- sim@modules[!hiddenMods]
    }
    return(mods)
})

#' @export
#' @rdname simList-accessors-modules
setGeneric("modules<-",
           function(sim, value) {
             standardGeneric("modules<-")
})

#' @name modules<-
#' @aliases modules<-,.simList-method
#' @rdname simList-accessors-modules
setReplaceMethod("modules",
                 signature = ".simList",
                 function(sim, value) {
                   sim@modules <- value
                   validObject(sim)
                   return(sim)
})

################################################################################
#' @inheritParams modules
#' @export
#' @include simList-class.R
#' @docType methods
#' @rdname simList-accessors-modules
#'
setGeneric("depends", function(sim) {
  standardGeneric("depends")
})

#' @export
#' @rdname simList-accessors-modules
setMethod("depends",
          signature(".simList"),
          definition = function(sim) {
            return(sim@depends)
})

#' @export
#' @rdname simList-accessors-modules
setGeneric("depends<-",
           function(sim, value) {
             standardGeneric("depends<-")
})

#' @name depends<-
#' @aliases depends<-,.simList-method
#' @rdname simList-accessors-modules
#' @export
setReplaceMethod("depends",
                 signature(".simList"),
                 function(sim, value) {
                   sim@depends <- value
                   validObject(sim)
                   return(sim)
})

################################################################################
#' Namespacing within SpaDES
#'
#' \code{.callingModuleName} returns the name of the module that is currently
#' the active module calling functions like \code{scheduleEvent}.
#' This will only return the module name if it is inside a \code{spades}
#' function call, i.e., it will return \code{NULL} if used in interactive mode.
#' The related function \code{currentModule} is simply a rapid accessor for the
#' current module name. This latter will return the module that is in the current
#' event queue, which will never be \code{NULL}
#'
#' @inheritParams modules
#' @include simList-class.R
#' @export
#' @keywords internal
#' @docType methods
#' @rdname namespacing
#' @author Eliot McIntire
#'
setGeneric(".callingModuleName", function(sim) {
  standardGeneric(".callingModuleName")
})

#' @export
#' @docType methods
#' @rdname namespacing
setMethod(
  ".callingModuleName",
  signature = c(".simList"),
  definition = function(sim) {
    # Only return module name if inside a spades call,
    #  because this only makes sense if there is an "active" module
    sc <- sys.calls()
    st <- grepl(sc, pattern = "moduleCall")
    if (any(st)) {
      mod <- strsplit(
        eval(parse(text = "moduleCall"), envir = sys.frame(which(st)[1] - 1)),
        split = "\\.")[[1]][2]
    } else {
      mod <- NULL
    }
    return(mod)
})

#' @inheritParams modules
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname namespacing
#' @author Eliot McIntire
setGeneric("currentModule", function(sim) {
  standardGeneric("currentModule")
})

#' @rdname namespacing
#' @export
setMethod(
  "currentModule",
  signature = c(".simList"),
  definition = function(sim) {
    ret <- sim@current$moduleName
    if (length(ret))
      return(ret)
    else
      return(character(0))
})

################################################################################
#' Get and set simulation parameters.
#'
#' \code{params} and \code{P} access the parameter slot in the \code{simList}.
#' \code{params} has a replace method, so can be used to update a parameter value.
#'
#' @inheritParams objs
#'
#' @param value The object to be stored at the slot.
#'
#' @param module Optional character string indicating which module params should come from.
#'
#' @param param Optional character string indicating which parameter to choose.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @note The differences between P, params and being explicit with passing arguments
#' are mostly a question of speed and code compactness.
#' The computationally fastest way to get a parameter is to specify moduleName and parameter name, as in:
#' \code{P(sim, "moduleName", "paramName")} (replacing moduleName and paramName with your
#' specific module and parameter names), but it is more verbose than P(sim)$paramName. Note: the important
#' part for speed (e.g., 2-4x faster) is specifying the moduleName.
#' Specifying the parameter name is <5% faster.
#'
#' @family functions to access elements of a \code{simList} object
#' @seealso \code{\link{SpaDES}}, specifically the section 1.2.1 on Simulation parameters.
#'
#' @export
#' @include simList-class.R
#' @docType methods
#' @aliases simList-accessors-params
#' @aliases parameters
#' @rdname params
#'
setGeneric("params", function(sim) {
  standardGeneric("params")
})

#' @export
#' @rdname params
setMethod("params",
          signature = ".simList",
          definition = function(sim) {
            return(sim@params)
})

#' @export
#' @rdname params
setGeneric("params<-",
           function(sim, value) {
             standardGeneric("params<-")
})

#' @name params<-
#' @aliases params<-,.simList-method
#' @rdname params
#' @export
setReplaceMethod("params",
                 signature = ".simList",
                 function(sim, value) {
                   sim@params <- value
                   validObject(sim)
                   return(sim)
})

#' \code{P} is a concise way to access parameters within a module. It works more like
#' a namespaced function in the sense that the module from which it is called is the
#' default place it will look for the parameter. To access a parameter from within
#' a module, you can use \code{P(sim)$paramName} instead of
#' \code{params(sim)$moduleName$paramName}
#'
#' @export
#' @note \code{P} is a function in \code{shiny} and html packages.
#' This can produce namespace clashes that are difficult to detect,
#' as the errors are not meaningful. The name of this function may
#' be changed in the future to remove this potential conflict. In the
#' mean time, use SpaDES::P(sim) if you are concerned with the potential
#' conflicts with a shiny app.
#'
#' @include simList-class.R
#' @docType methodsp
#' @aliases simList-accessors-params
#' @rdname params
#'
setGeneric("P", function(sim, module = NULL, param = NULL) {
  standardGeneric("P")
})

#' @export
#' @rdname params
setMethod("P",
          signature = ".simList",
          definition = function(sim, module, param) {
            if (is.null(module)) {
              module <- sim@current$moduleName
            }
            if (length(module) > 0) {
              if (is.null(param)) {
                return(sim@params[[module]])
              } else {
                return(sim@params[[module]][[param]])
              }
            } else {
              inSimInit <- grep(sys.calls(), pattern = ".parseModule")
              if (any(inSimInit)) {
                module <- get("m", sys.frame(grep(sys.calls(), pattern = ".parseModule")[2]))
                if (is.null(param)) {
                  return(sim@params[[module]])
                } else {
                  return(sim@params[[module]][[param]])
                }
              }

              return(sim@params)
            }
})

################################################################################
#' Get and set simulation globals.
#'
#' \code{globals} accesses or sets the "globals" slot in the \code{simList}.
#'
#' @inheritParams params
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname globals
#'
#' @family functions to access elements of a \code{simList} object
#' @seealso \code{\link{SpaDES}}, specifically the section 1.2.1 on Simulation Parameters.
#'
setGeneric("globals", function(sim) {
  standardGeneric("globals")
})

#' @export
#' @rdname globals
setMethod("globals",
          signature = ".simList",
          definition = function(sim) {
            return(sim@params$.globals)
})

#' @export
#' @rdname globals
setGeneric("globals<-",
           function(sim, value) {
             standardGeneric("globals<-")
})

#' @name globals<-
#' @aliases globals<-,.simList-method
#' @rdname globals
#' @export
setReplaceMethod("globals",
                 signature = ".simList",
                 function(sim, value) {
                   sim@params$.globals <- value
                   validObject(sim)
                   return(sim)
})

################################################################################
#' @inheritParams params
#' @param asDF Logical. For \code{parameters}, if TRUE, this will produce a single
#'                 data.frame of all model parameters. If FALSE, then it will return
#'                 a data.frame with 1 row for each parameter within nested lists,
#'                 with the same structure as \code{params}.
#'
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname params
#' @examples
#' modules = list("randomLandscapes")
#' paths = list(modulePath = system.file("sampleModules", package = "SpaDES"))
#' mySim <- simInit(modules = modules, paths = paths,
#'                  params = list(.globals = list(stackName = "landscape")))
#' parameters(mySim)
#'
setGeneric("parameters", function(sim, asDF = FALSE) {
  standardGeneric("parameters")
})

#' @export
#' @rdname params
setMethod("parameters",
          signature = ".simList",
          definition = function(sim, asDF) {
            if (any(!unlist(lapply(depends(sim)@dependencies, is.null)))) {
              if (asDF) {
                tmp <- lapply(depends(sim)@dependencies, function(x) {
                  out <- x@parameters
                })
                tmp <- do.call(rbind, tmp)
              } else {
                tmp <- lapply(depends(sim)@dependencies, function(x) {
                  out <- lapply(seq_len(NROW(x@parameters)),
                                function(y) x@parameters[y, -1])
                  names(out) <- x@parameters$paramName
                  out
                })
              }
            } else {
              tmp <- NULL
            }
            return(tmp)
})

################################################################################
#' @inheritParams params
#' @export
#' @include simList-class.R
#' @docType methods
#' @rdname checkpoint
#' @family functions to access elements of a \code{simList} object
#'
setGeneric("checkpointFile", function(sim) {
  standardGeneric("checkpointFile")
})

#' @export
#' @rdname checkpoint
setMethod("checkpointFile",
          signature = ".simList",
          definition = function(sim) {
            return(sim@params$.checkpoint$file)
})

#' @export
#' @rdname checkpoint
setGeneric("checkpointFile<-",
           function(sim, value) {
             standardGeneric("checkpointFile<-")
})

#' @name checkpointFile<-
#' @aliases checkpointFile<-,.simList-method
#' @rdname checkpoint
#' @export
setReplaceMethod("checkpointFile",
                 signature = ".simList",
                 function(sim, value) {
                   sim@params$.checkpoint$file <- value
                   validObject(sim)
                   return(sim)
})

################################################################################
#' @inheritParams params
#' @export
#' @include simList-class.R
#' @docType methods
#' @rdname checkpoint
#'
setGeneric("checkpointInterval", function(sim) {
  standardGeneric("checkpointInterval")
})

#' @export
#' @rdname checkpoint
setMethod("checkpointInterval",
          signature = ".simList",
          definition = function(sim) {
            return(sim@params$.checkpoint$interval)
})

#' @export
#' @rdname checkpoint
setGeneric("checkpointInterval<-",
           function(sim, value) {
             standardGeneric("checkpointInterval<-")
})

#' @name checkpointInterval<-
#' @aliases checkpointInterval<-,.simList-method
#' @rdname checkpoint
#' @export
setReplaceMethod("checkpointInterval",
                 signature = ".simList",
                 function(sim, value) {
                   sim@params$.checkpoint$interval <- value
                   validObject(sim)
                   return(sim)
})

################################################################################
#' Get and set simulation progress bar details
#'
#' The progress bar can be set in two ways in SpaDES. First, by setting values
#' in the .progress list element in the params list element passed to \code{\link{simInit}}.
#' Second, at the \code{\link{spades}} call itself, which can be simpler. See examples.
#'
#' @details Progress Bar:
#' Progress type can be one of  \code{"text"}, \code{"graphical"}, or \code{"shiny"}.
#' Progress interval can be a numeric.
#' These both can get set by passing a
#' \code{.progress = list(type = "graphical", interval = 1)} into the \code{simInit} call.
#' See examples.
#'
#' @inheritParams params
#' @include simList-class.R
#' @export
#' @docType methods
#' @family functions to access elements of a \code{simList} object
#' @rdname progress
#'
#' @examples
#' \dontrun{
#' mySim <- simInit(times = list(start=0.0, end=100.0),
#'                  params = list(.globals = list(stackName = "landscape"),
#'                              .progress = list(type = "text", interval = 10),
#'                              .checkpoint = list(interval = 10, file = "chkpnt.RData")),
#'                  modules = list("randomLandscapes"),
#'                  paths = list(modulePath=system.file("sampleModules", package = "SpaDES")))
#'
#' # progress bar
#' progressType(mySim) # "text"
#' progressInterval(mySim) # 10
#'
#' # parameters
#' params(mySim) # returns all parameters in all modules
#'               # including .global, .progress, .checkpoint
#' globals(mySim) # returns only global parameters
#'
#' # checkpoint
#' checkpointFile(mySim) # returns the name of the checkpoint file
#'                       # In this example, "chkpnt.RData"
#' checkpointInterval(mySim) # 10
#' }
setGeneric("progressInterval", function(sim) {
  standardGeneric("progressInterval")
})

#' @export
#' @rdname progress
setMethod("progressInterval",
          signature = ".simList",
          definition = function(sim) {
            return(sim@params$.progress$interval)
})

#' @export
#' @rdname progress
setGeneric("progressInterval<-",
           function(sim, value) {
             standardGeneric("progressInterval<-")
})

#' @name progressInterval<-
#' @aliases progressInterval<-,.simList-method
#' @rdname progress
#' @export
setReplaceMethod("progressInterval",
                 signature = ".simList",
                 function(sim, value) {
                   sim@params$.progress$interval <- value
                   validObject(sim)
                   return(sim)
})

################################################################################
#' @inheritParams params
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname progress
#'
setGeneric("progressType", function(sim) {
  standardGeneric("progressType")
})

#' @export
#' @rdname progress
setMethod("progressType",
          signature = ".simList",
          definition = function(sim) {
            return(sim@params$.progress$type)
})

#' @export
#' @rdname progress
setGeneric("progressType<-",
           function(sim, value) {
             standardGeneric("progressType<-")
})

#' @name progressType<-
#' @aliases progressType<-,.simList-method
#' @rdname progress
#' @export
setReplaceMethod("progressType",
                 signature = ".simList",
                 function(sim, value) {
                   sim@params$.progress$type <- as.character(value)
                   validObject(sim)
                   return(sim)
})

################################################################################
#' Inputs and outputs
#'
#' These functions are one of three mechanisms to add the information about which
#' input files to load in a \code{spades} call and the information about which
#' output files to save. 1) As arguments to a \code{simInit} call. Specifically, \code{inputs}
#' or \code{outputs}. See \code{?simInit}. 2) With the \code{inputs(simList)} or \code{outputs(simList)}
#' function call.
#' 3) By adding a function called \code{.inputObjects} inside a module, which will be executed
#' during the \code{simInit} call. This last way is the most "modular" way to create
#' default data sets for your model. See below for more details.
#'
#' Accessor functions for the \code{inputs} and \code{outputs} slots in a
#' \code{simList} object.
#'
#' @section inputs function or argument in \code{simInit}:
#'
#' \code{inputs} accepts a data.frame, with up to 7 columns.
#' Columns are:
#'
#' \tabular{ll}{
#' \code{file} \tab required, a character string indicating the file path. There is no
#' default.\cr
#'
#' \code{objectName} \tab optional, character string indicating the name of the object
#' that the loaded file will be assigned to in the \code{simList}. This object
#' can therefore be accessed with \code{sim$xxx} in any module, where
#' \code{objectName = "xxx"}. Defaults to the filename without file extension or
#' directory information.\cr
#'
#' \code{fun} \tab optional, a character string indicating the function to use to
#' load that file. Defaults to the known extentions in \code{SpaDES} (found by
#' examining \code{.fileExtensions()}). The \code{package} and \code{fun} can be
#' jointly specified here as \code{"packageName::functionName"}, e.g.,
#' \code{"raster::raster"}.\cr
#'
#' \code{package} \tab optional character string indicating the package in
#' which to find the \code{fun});\cr
#'
#' \code{loadTime} \tab optional numeric, indicating when in simulation time the file
#' should be loaded. The default is the highest priority at \code{start(sim)},
#' i.e., at the very start. \cr
#'
#' \code{interval} \tab optional numeric, indicating at what interval should this same
#' exact file  be reloaded from disk, e.g,. 10 would mean every 10 time units. The
#' default is NA or no interval, i.e, load the file only once as described in
#' \code{loadTime} \cr
#'
#' \code{arguments} \tab is a list of lists of named arguments, one list for each
#' \code{fun}. For example, if \code{fun="raster"}, \code{arguments = list(native = TRUE)}
#' will pass the argument "native = TRUE" to raster.  If there is only one list,
#' then it is assumed to apply to all files and will be recycled as per normal R
#' rules of recycling for each \code{fun}.\cr
#' }
#'
#' Currently, only \code{file} is required. All others will be filled with defaults
#' if not specified.
#'
#' See the modules vignette for more details (\code{browseVignettes("SpaDES")}).
#'
#' @section \code{.inputObjects} function placed inside module:
#'
#' Any code placed inside a function called \code{.inputObjects} will be run during
#' the simInit for the purpose of creating
#' any objects required by this module, i.e., objects  identified in the \code{inputObjects}
#' element of \code{defineModule}.
#' This is useful if there is something required before simulation to produce the module
#' object dependencies, including such things as downloading default datasets, e.g.,
#' \code{downloadData('LCC2005', modulePath(sim))}.
#' Nothing should be created here that does not create an named object in inputObjects.
#' Any other initiation procedures should be put in the "init" eventType of the doEvent function.
#' Note: the module developer can use 'sim$.userSuppliedObjNames' inside the function to
#' selectively skip unnecessary steps because the user has provided those inputObjects in the
#' simInit call. e.g., the following code would look to see if the user had passed \code{defaultColor}
#' into during \code{simInit}. If the user had done this, then this function would not override
#' that value with 'red'. If the user has not passed in a value for \code{defaultColor}, then
#' the module will get it here:
#'
#' \code{if (!('defaultColor' \%in\% sim$.userSuppliedObjNames)) \{
#'  sim$defaultColor <- 'red'
#' \}}
#'
#' @inheritParams objs
#'
#' @param value The object to be stored at the slot. See Details.
#'
#' @return Returns or sets the value(s) of the \code{input} or \code{output} slots
#' in the \code{simList} object.
#'
#' @family functions to access elements of a \code{simList} object
#' @seealso \code{\link{SpaDES}}, specifically the section 1.2.2 on loading and saving.
#'
#' @include simList-class.R
#' @importFrom data.table is.data.table
#' @importFrom stats na.omit
#' @export
#' @docType methods
#' @name inputs
#' @aliases simList-accessors-inout
#' @rdname simList-accessors-inout
#' @examples
#' #######################
#' # inputs
#' #######################
#'
#' # Start with a basic empty simList
#' sim <- simInit()
#'
#' test <- 1:10
#' library(igraph) # for %>%
#' tmpdir <- file.path(tempdir(), "inputs") %>% checkPath(create = TRUE)
#' tmpFile <- file.path(tmpdir, "test.rds")
#' saveRDS(test, file = tmpFile)
#' inputs(sim) <- data.frame(file = tmpFile) # using only required column, "file"
#' inputs(sim) # see that it is not yet loaded, but when it is scheduled to be loaded
#' simOut <- spades(sim)
#' inputs(simOut) # confirm it was loaded
#' simOut$test
#'
#' # can put data.frame for inputs directly inside simInit call
#' allTifs <- dir(system.file("maps", package = "SpaDES"),
#'                full.names = TRUE, pattern = "tif")
#'
#' # next: objectNames are taken from the filenames (without the extension)
#' # This will load all 5 tifs in the SpaDES sample directory, using
#' #   the raster fuction in the raster package, all at time = 0
#' if (require("rgdal", quietly = TRUE)) {
#'   sim <- simInit(
#'     inputs = data.frame(
#'       files = allTifs,
#'       functions = "raster",
#'       package = "raster",
#'       loadTime = 0,
#'       stringsAsFactors = FALSE)
#'     )
#'
#'   ##############################
#'   #A fully described inputs object, including arguments:
#'   files <- dir(system.file("maps", package = "SpaDES"),
#'                full.names = TRUE, pattern = "tif")
#'   # arguments must be a list of lists. This may require I() to keep it as a list
#'   #   once it gets coerced into the data.frame.
#'   arguments = I(rep(list(native = TRUE), length(files)))
#'   filelist = data.frame(
#'      objectName = paste0("Maps", 1:5),
#'      files = files,
#'      functions = "raster::raster",
#'      arguments = arguments,
#'      loadTime = 0,
#'      intervals = c(rep(NA, length(files) - 1), 10)
#'   )
#'   inputs(sim) <- filelist
#'   spades(sim)
#' }
#'
#'
#' # Clean up after
#' unlink(tmpdir, recursive = TRUE)
setGeneric("inputs", function(sim) {
  standardGeneric("inputs")
})

#' @export
#' @rdname simList-accessors-inout
setMethod("inputs",
          signature = ".simList",
          definition = function(sim) {

            simUnit <- sim@simtimes[["timeunit"]]
            loadTimeUnit <- attr(sim@inputs$loadTime, "unit")
            if (is.null(loadTimeUnit)) loadTimeUnit <- simUnit
            out <- if (is.na(pmatch(loadTimeUnit, simUnit)) &
                       (length(sim@inputs$loadTime) > 0)) {
              # note the above line captures empty loadTime,
              # whereas is.na does not
              if (any(!is.na(sim@inputs$loadTime))) {
                if (!is.null(sim@inputs$loadTime)) {
                  obj <- data.table::copy(sim@inputs) # don't change original sim
                  set(obj, , j = "loadTime", convertTimeunit(obj$loadTime, obj$unit, sim@.envir))
                  #obj[, loadTime := convertTimeunit(loadTime, unit, sim@.envir)]
                  obj[]
                }
              } else {
                sim@inputs
              }
            } else {
              sim@inputs
            }

            return(out)
})

#' @export
#' @rdname simList-accessors-inout
setGeneric("inputs<-",
           function(sim, value) {
             standardGeneric("inputs<-")
})

#' @name inputs<-
#' @aliases inputs<-,.simList-method
#' @rdname simList-accessors-inout
#' @export
setReplaceMethod(
  "inputs",
  signature = ".simList",
  function(sim, value) {
   if (length(value) > 0) {
     whFactors <- sapply(value, function(x) is.factor(x))
     if (any(whFactors)) {
       value[, whFactors] <- sapply(value[, whFactors], as.character)
     }

     if (!is.data.frame(value)) {
       if (!is.list(value)) {
         stop("inputs must be a list, data.frame")
       }
        value <- data.frame(value, stringsAsFactors = FALSE)
     }
#      fileTable <- .fileTableIn()
#      needRenameArgs <- grepl(names(value), pattern = "arg[s]?$")
#      if (any(needRenameArgs)) {
#        colnames(value)[needRenameArgs] <-
#          .fileTableInCols[pmatch("arg", .fileTableInCols)]
#      }
#      columns <- pmatch(names(fileTable), names(value))
#      setnames(value, old = colnames(value)[na.omit(columns)],
#                      new = colnames(fileTable)[!is.na(columns)])
#      columns2 <- pmatch(names(value), names(fileTable))
#      sim@inputs <- rbind(value[,na.omit(columns), drop = FALSE], fileTable[,columns2])
#      if (any(is.na(columns))) {
#        sim@inputs[,names(fileTable[, is.na(columns)])] <- NA
#      }
     sim@inputs <- .fillInputRows(value, start(sim))
   } else {
     sim@inputs <- value
   }
   # Deal with objects and files differently... if files (via inputs arg in simInit)...
     # Deal with file names
     # 2 things: 1. if relative, concatenate inputPath
     #           2. if absolute, don't use inputPath
   if (NROW(value) > 0) {
     sim@inputs[is.na(sim@inputs$file), "file"] <- NA

     # If a filename is provided, determine if it is absolute path, if so,
     # use that, if not, then append it to inputPath(sim)
     sim@inputs[!isAbsolutePath(sim@inputs$file) & !is.na(sim@inputs$file), "file"] <-
       file.path(inputPath(sim),
                 sim@inputs$file[!isAbsolutePath(sim@inputs$file) & !is.na(sim@inputs$file)])

     if (!all(names(sim@inputs) %in% .fileTableInCols)) {
       stop(paste("input table can only have columns named",
                  paste(.fileTableInCols, collapse = ", ")))
     }
     if (any(is.na(sim@inputs[, "loaded"]))) {
       if (!all(is.na(sim@inputs[, "loadTime"]))) {
         newTime <- sim@inputs[is.na(sim@inputs$loaded), "loadTime"]
         attributes(newTime)$unit <- sim@simtimes[["timeunit"]]
         for (nT in newTime) {
           attributes(nT)$unit <- timeunit(sim)
           sim <- scheduleEvent(sim, nT, "load", "inputs", .first())
         }
         toRemove <- duplicated(rbindlist(list(current(sim), events(sim))),
                                by = c("eventTime", "moduleName", "eventType"))
         if (any(toRemove)) {
           if (NROW(current(sim)) > 0)
             toRemove <- toRemove[-seq_len(NROW(current(sim)))]
           events(sim) <- events(sim)[!toRemove]
         }

       } else {
         sim@inputs[is.na(sim@inputs$loadTime), "loadTime"] <-
           sim@simtimes[["current"]]
         newTime <- sim@inputs[is.na(sim@inputs$loaded), "loadTime"] %>%
           min(., na.rm = TRUE)
         attributes(newTime)$unit <- "seconds"
         sim <- scheduleEvent(sim, newTime, "load", "inputs", .first())
       }
     }
   }

   return(sim)
})

################################################################################
#' @section outputs function or argument in \code{simInit}:
#'
#' \code{outputs} accepts a data.frame similar to the \code{inputs} data.frame, but
#' with up to 6 columns.
#'
#' \tabular{ll}{
#' \code{objectName} \tab required, character string indicating the name of the object
#' in the \code{simList} that will be saved to disk (without the \code{sim$} prefix).\cr
#'
#' \code{file} \tab optional, a character string indicating the file path to save to.
#' The default is to concatenate \code{objectName} with the model timeunit and
#' \code{saveTime}, separated by underscore, "_". So a default filename would be
#' "Fires_year1.rds"\cr
#'
#' \code{fun} \tab optional, a character string indicating the function to use to
#' save that file. The default is \code{\link{saveRDS}} \cr
#'
#' \code{package} \tab optional character string indicating the package in
#' which to find the \code{fun});\cr
#'
#' \code{saveTime} \tab optional numeric, indicating when in simulation time the file
#' should be saved. The default is the lowest priority at \code{end(sim)},
#' i.e., at the very end. \cr
#'
#' \code{arguments} \tab is a list of lists of named arguments, one list for each
#' \code{fun}. For example, if \code{fun = "write.csv"},
#' \code{arguments = list(row.names = TRUE)} will pass the argument
#' \code{row.names = TRUE} to \code{write.csv}  If there is only one list,
#' then it is assumed to apply to all files and will be recycled as per normal R
#' rules of recycling for each \code{fun}.\cr
#' }
#'
#' See the modules vignette for more details (\code{browseVignettes("SpaDES")}).
#'
#' @note The automatic file type handling only adds the correct extension from a given
#' \code{fun} and \code{package}. It does not do the inverse, from a given extension find the
#' correct \code{fun} and \code{package}.
#'
#' @inheritParams inputs
#' @include simList-class.R
#' @export
#' @importFrom data.table data.table ':='
#' @importFrom tools file_path_sans_ext
#' @importFrom tools file_ext
#' @importFrom dplyr inner_join
#' @importFrom R.utils isAbsolutePath
#' @importFrom stats na.omit
#' @docType methods
#' @name outputs
#' @rdname simList-accessors-inout
#' @examples
#'
#' #######################
#' # outputs
#' #######################
#'
#' library(igraph) # for %>%
#' tmpdir <- file.path(tempdir(), "outputs") %>% checkPath(create = TRUE)
#' tmpFile <- file.path(tmpdir, "temp.rds")
#' tempObj <- 1:10
#'
#' # Can add data.frame of outputs directly into simInit call
#' sim <- simInit(objects = c("tempObj"),
#'                outputs = data.frame(objectName = "tempObj"),
#'                paths = list(outputPath = tmpdir))
#' outputs(sim) # To see what will be saved, when, what filename
#' sim <- spades(sim)
#' outputs(sim) # To see that it was saved, when, what filename
#'
#' # Also can add using assignment after a simList object has been made
#' sim <- simInit(objects = c("tempObj"), paths = list(outputPath = tmpdir))
#' outputs(sim) <- data.frame(objectName = "tempObj", saveTime = 1:10)
#' sim <- spades(sim)
#' outputs(sim) # To see that it was saved, when, what filename.
#'
#' # can do highly variable saving
#' tempObj2 <- paste("val",1:10)
#' df1 <- data.frame(col1 = tempObj, col2 = tempObj2)
#' sim <- simInit(objects = c("tempObj", "tempObj2", "df1"),
#'   paths=list(outputPath = tmpdir))
#' outputs(sim) = data.frame(
#'      objectName = c(rep("tempObj", 2), rep("tempObj2", 3), "df1"),
#'      saveTime = c(c(1,4), c(2,6,7), end(sim)),
#'      fun = c(rep("saveRDS", 5), "write.csv"),
#'      package = c(rep("base", 5), "utils"),
#'      stringsAsFactors = FALSE)
#' # since write.csv has a default of adding a column, x, with rownames, must add additional
#' #   argument for 6th row in data.frame (corresponding to the write.csv function)
#' outputArgs(sim)[[6]] <- list(row.names=FALSE)
#' sim <- spades(sim)
#' outputs(sim)
#'
#' # read one back in just to test it all worked as planned
#' newObj <- read.csv(dir(tmpdir, pattern = "second10.csv", full.name = TRUE))
#' newObj
#'
#' # using saving with SpaDES-aware methods
#' # To see current ones SpaDES can do
#' .saveFileExtensions()
#'
#' library(raster)
#' if (require(rgdal)) {
#'   ras <- raster(ncol = 4, nrow = 5)
#'   ras[] <- 1:20
#'
#'   sim <- simInit(objects = c("ras"), paths = list(outputPath = tmpdir))
#'   outputs(sim) = data.frame(
#'     file = "test",
#'     fun = "writeRaster",
#'     package = "raster",
#'     objectName = "ras",
#'     stringsAsFactors = FALSE)
#'
#'   outputArgs(sim)[[1]] <- list(format = "GTiff") # see ?raster::writeFormats
#'   simOut <- spades(sim)
#'   outputs(simOut)
#'   newRas <- raster(dir(tmpdir, full.name = TRUE, pattern = ".tif"))
#'   all.equal(newRas, ras) # Should be TRUE
#' }
#' # Clean up after
#' unlink(tmpdir, recursive = TRUE)
setGeneric("outputs", function(sim) {
  standardGeneric("outputs")
})

#' @export
#' @rdname simList-accessors-inout
setMethod("outputs",
          signature = ".simList",
          definition = function(sim) {
            simUnit <- sim@simtimes[["timeunit"]]
            saveTimeUnit <- attr(sim@outputs$saveTime, "unit")
            if (is.null(saveTimeUnit)) saveTimeUnit <- simUnit

            out <- if (is.na(pmatch(saveTimeUnit, simUnit)) &
                       length(sim@outputs$saveTime) > 0) {
              #note the above line captures empty saveTime,
              # whereas is.na does not
              if (any(!is.na(sim@outputs$saveTime))) {
                if (!is.null(sim@outputs$saveTime)) {
                  obj <- data.table::copy(sim@outputs) # don't change original sim
                  obj[, saveTime := convertTimeunit(saveTime, unit, sim@.envir)]
                  obj[]
                  obj
                }
              } else {
                sim@outputs
              }
            } else {
              sim@outputs
            }
            return(out)
})

#' @export
#' @rdname simList-accessors-inout
setGeneric("outputs<-",
           function(sim, value) {
             standardGeneric("outputs<-")
})

#' @name outputs<-
#' @aliases outputs<-,.simList-method
#' @rdname simList-accessors-inout
#' @export
setReplaceMethod(
  "outputs",
  signature = ".simList",
  function(sim, value) {
    if (NROW(value)) {
       if (!is.data.frame(value)) {
         if (!is.list(value)) {
           stop("outputs must be a list or data.frame")
         }
         value <- data.frame(value, stringsAsFactors = FALSE)
       }

       sim@outputs <- .fillOutputRows(value, end(sim))

       # coerce any factors to the correct class
       for (col in which(sapply(sim@outputs, is.factor))) {
         sim@outputs[, col] <- as(sim@outputs[[col]], class(.fileTableOut()[[col]]))
       }

       # if saveTime not provided, give it end(sim)
       sim@outputs[is.na(sim@outputs$saveTime), "saveTime"] <-
         end(sim, sim@simtimes[["timeunit"]])
       attributes(sim@outputs$saveTime)$unit <- sim@simtimes[["timeunit"]]

       # Deal with file names
       # 3 things: 1. if relative, concatenate outputPath
       #           2. if absolute, don't use outputPath
       #           3. concatenate time to file name in all cases
       # If no filename provided, use the object name
       sim@outputs[is.na(sim@outputs$file), "file"] <-
         paste0(sim@outputs$objectName[is.na(sim@outputs$file)])
       # If a filename is provided, determine if it is absolute path, if so,
       # use that, if not, then append it to outputPath(sim)
       alreadyWithOutputPath <- grepl(pattern = paste0("^", outputPath(sim)), sim@outputs$file)
       if (any(!alreadyWithOutputPath)) {
         sim@outputs[!isAbsolutePath(sim@outputs$file)[!alreadyWithOutputPath], "file"] <-
           file.path(outputPath(sim), sim@outputs$file[!isAbsolutePath(sim@outputs$file)])
       }

       # If there is no function provided, then use saveRDS, from package base
       sim@outputs[is.na(sim@outputs$fun), "fun"] <- "saveRDS"
       sim@outputs[is.na(sim@outputs$package), "package"] <- "base"

       # file extension stuff
       fileExts <- .saveFileExtensions()
       fe <- suppressMessages(inner_join(sim@outputs, fileExts)$exts)
       wh <- !stri_detect_fixed(str = sim@outputs$file, pattern = ".") &
         (nzchar(fe, keepNA=TRUE))
       sim@outputs[wh, "file"] <- paste0(sim@outputs[wh, "file"], ".", fe[wh])

       # If the file name already has a time unit on it,
       # i.e., passed explicitly by user, then don't postpend again
       txtTimeA <- paste0(attr(sim@outputs[, "saveTime"], "unit"))
       txtTimeB <- paddedFloatToChar(
         sim@outputs[, "saveTime"],
         ceiling(log10(end(sim, sim@simtimes[["timeunit"]]) + 1))
       )
       # Add time unit and saveTime to filename, without stripping extension
       wh <- !stri_detect_fixed(str = sim@outputs$file, pattern = txtTimeA)
       sim@outputs[wh, "file"] <- paste0(
         file_path_sans_ext(sim@outputs[wh, "file"]),
         "_", txtTimeA, txtTimeB[wh],
         ifelse(nzchar(file_ext(sim@outputs[wh, "file"]), keepNA=TRUE) , ".", ""),
         ifelse(nzchar(file_ext(sim@outputs[wh, "file"]), keepNA=TRUE) ,
                file_ext(sim@outputs[wh, "file"]),
                "")
       )
     } else {
       sim@outputs <- value
     }

     if (!all(.fileTableOutCols %in% names(sim@outputs))) {
       stop(paste("output table must have columns named",
                  paste(.fileTableOutCols, collapse = ", ")))
     }

    return(sim)
})

################################################################################
#' \code{inputArgs} and \code{outputArgs} are ways to specify any
#' arguments that are needed for file loading and file saving. This
#' is still somewhat experimental.
#'
#' @inheritParams inputs
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-inout
#'
setGeneric("inputArgs", function(sim) {
  standardGeneric("inputArgs")
})

#' @export
#' @rdname simList-accessors-inout
setMethod("inputArgs",
          signature = ".simList",
          definition = function(sim) {
            return(sim@inputs$args)
})

#' @export
#' @rdname simList-accessors-inout
setGeneric("inputArgs<-",
           function(sim, value) {
             standardGeneric("inputArgs<-")
})

#' @name inputArgs<-
#' @aliases inputArgs<-,.simList-method
#' @rdname simList-accessors-inout
#' @export
setReplaceMethod(
  "inputArgs",
  signature = ".simList",
  function(sim, value) {
   if (is.list(value) & !is.data.frame(value)) {
     sim@inputs$args <- value
   } else if (is.null(value)) {
     sim@inputs$args <- rep(list(NULL), NROW(inputs(sim)))
   } else {
     stop("value passed to inputArgs() must be a list of named elements")
   }

   validObject(sim)
   return(sim)
})

#' @inheritParams inputs
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-inout
#'
setGeneric("outputArgs", function(sim) {
  standardGeneric("outputArgs")
})

#' @export
#' @rdname simList-accessors-inout
setMethod("outputArgs",
          signature = ".simList",
          definition = function(sim) {
            return(sim@outputs$arg)
})

#' @export
#' @rdname simList-accessors-inout
setGeneric("outputArgs<-",
           function(sim, value) {
             standardGeneric("outputArgs<-")
})

#' @name outputArgs<-
#' @aliases outputArgs<-,.simList-method
#' @rdname simList-accessors-inout
#' @export
setReplaceMethod(
  "outputArgs",
  signature = ".simList",
  function(sim, value) {
    argName <- .fileTableOutCols[pmatch("arg", .fileTableOutCols)]
   if (is.list(value) & !is.data.frame(value)) {
     sim@outputs[[argName]] <- value
   } else if (is.null(value)) {
     sim@outputs[[argName]] <- rep(list(NULL), NROW(outputs(sim)))
   } else {
     stop("value passed to outputArgs() must be a list of named elements")
   }
   validObject(sim)
   return(sim)
})

################################################################################
#' Specify paths for modules, inputs, and outputs
#'
#' Accessor functions for the \code{paths} slot in a \code{simList} object.
#'
#' These are ways to add or access the file paths used by \code{\link{spades}}.
#' There are four file paths: \code{cachePath}, \code{modulePath},
#' \code{inputPath}, and \code{outputPath}.
#' Each has a function to get or set the value in a \code{simList} object.
#' If no paths are specified, the defaults are as follows:
#'
#' \itemize{
#'   \item \code{cachePath}: \code{getOption("spades.cachePath")};
#'
#'   \item \code{inputPath}: \code{getOption("spades.modulePath")};
#'
#'   \item \code{modulePath}: \code{getOption("spades.inputPath")};
#'
#'   \item \code{inputPath}: \code{getOption("spades.outputPath")}.
#' }
#'
#' @inheritParams params
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @family functions to access elements of a \code{simList} object
#' @seealso \code{\link{SpaDES}}, specifically the section 1.2.4 on Simulation Paths.
#'
#' @include simList-class.R
#' @importFrom stats na.omit
#' @export
#' @docType methods
#' @aliases simList-accessors-paths
#' @rdname simList-accessors-paths
#'
setGeneric("paths", function(sim) {
  standardGeneric("paths")
})

#' @export
#' @rdname simList-accessors-paths
setMethod("paths",
          signature = ".simList",
          definition = function(sim) {
            return(sim@paths)
})

#' @export
#' @rdname simList-accessors-paths
setGeneric("paths<-",
           function(sim, value) {
             standardGeneric("paths<-")
})

#' @name paths<-
#' @aliases paths<-,.simList-method
#' @rdname simList-accessors-paths
#' @export
setReplaceMethod(
  "paths",
  signature = ".simList",
  function(sim, value) {
    N <- 4 # total number of named paths (cache, madule, input, output)

    # get named elements and their position in value list
    wh <- pmatch(names(sim@paths), names(value)) # always length 4, NA if no name match, number if yes
    whValueNamed <- which(!is.na(pmatch(names(value), names(sim@paths)))) # length of names of value
    whValueUnnamed <- rep(TRUE, length(value))
    if (length(whValueNamed)) whValueUnnamed[whValueNamed] <- FALSE


    # keep named elements, use unnamed in remaining order:
    #  cache, input, module, output
    # if (length(na.omit(wh)) < length(value)) {
    #   wh1 <- !(wh[1:length(value)] %in% (1:N)[1:length(value)])
    #   wh2 <- !((1:N)[1:length(value)] %in% wh[1:length(value)])
    #   if (length(wh1) < N) wh1 <- c(wh1, rep(FALSE, N - length(wh1)))
    #   if (length(wh2) < N) wh2 <- c(wh2, rep(FALSE, N - length(wh2)))
    #   wh[wh1] <- (1:N)[wh2]
    # }

    # start with .paths()
    emptyOnes <- unlist(lapply(sim@paths, is.null))
    if (sum(emptyOnes) > 0) sim@paths[emptyOnes] <- .paths()[emptyOnes]

    # override with named ones
    sim@paths[!is.na(wh)] <- value[na.omit(wh)]

    #sim@paths[is.na(wh)] <- .paths()[is.na(wh)]
    # keep named elements, use unnamed in remaining order:
    #  cache, input, module, output
    if (length(na.omit(wh)) < length(value)) {
      whichNamed <- which(!is.na(wh))
      whichUnnamed <- (1:length(sim@paths))
      if (length(whichNamed) > 0) whichUnnamed <- whichUnnamed[-whichNamed]
      sim@paths[whichUnnamed][seq_len(sum(whValueUnnamed))] <- value[whValueUnnamed]
    }

    # Don't need to create an archive in the paths directory, just have to create
    #  the directory
    #names(sim@paths) <- c("cachePath", "modulePath", "inputPath", "outputPath")
    # if (is(try(archivist::showLocalRepo(sim@paths$cachePath),
    #            silent = TRUE),
    #        "try-error")) {

    checkPath(sim@paths$cachePath, create = TRUE)
    #   archivist::createLocalRepo(sim@paths$cachePath, force = TRUE)
    # }

    validObject(sim)
    return(sim)
})

################################################################################
#' @inheritParams paths
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-paths
#'
setGeneric("cachePath", function(sim) {
  standardGeneric("cachePath")
})

#' @export
#' @rdname simList-accessors-paths
setMethod("cachePath",
          signature = ".simList",
          definition = function(sim) {
            return(sim@paths$cachePath)
})

#' @export
#' @rdname simList-accessors-paths
setGeneric("cachePath<-",
           function(sim, value) {
             standardGeneric("cachePath<-")
})

#' @name cachePath<-
#' @aliases cachePath<-,.simList-method
#' @rdname simList-accessors-paths
#' @export
setReplaceMethod(
  "cachePath",
  signature = ".simList",
  function(sim, value) {
    sim@paths$cachePath <- unname(unlist(value))
    validObject(sim)
    return(sim)
})

################################################################################
#' @inheritParams paths
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-paths
#'
setGeneric("inputPath", function(sim) {
  standardGeneric("inputPath")
})

#' @export
#' @rdname simList-accessors-paths
setMethod("inputPath",
          signature = ".simList",
          definition = function(sim) {
            return(sim@paths$inputPath)
})

#' @export
#' @rdname simList-accessors-paths
setGeneric("inputPath<-",
           function(sim, value) {
             standardGeneric("inputPath<-")
})

#' @name inputPath<-
#' @aliases inputPath<-,.simList-method
#' @rdname simList-accessors-paths
#' @export
setReplaceMethod(
  "inputPath",
  signature = ".simList",
  function(sim, value) {
    sim@paths$inputPath <- unname(unlist(value))
    validObject(sim)
    return(sim)
})

################################################################################
#' @inheritParams paths
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-paths
#'
setGeneric("outputPath", function(sim) {
  standardGeneric("outputPath")
})

#' @export
#' @rdname simList-accessors-paths
setMethod("outputPath",
          signature = ".simList",
          definition = function(sim) {
            return(sim@paths$outputPath)
})

#' @export
#' @rdname simList-accessors-paths
setGeneric("outputPath<-",
           function(sim, value) {
             standardGeneric("outputPath<-")
})

#' @name outputPath<-
#' @aliases outputPath<-,.simList-method
#' @rdname simList-accessors-paths
#' @export
setReplaceMethod("outputPath",
                 signature = ".simList",
                 function(sim, value) {
                   sim@paths$outputPath <- unname(unlist(value))
                   checkPath(sim@paths$outputPath, create = TRUE)
                   if (NROW(outputs(sim)) > 0) {
                     if ("saved" %in% colnames(outputs(sim))) {
                       notYetSaved <- !outputs(sim)$saved | is.na(outputs(sim)$saved)
                       outputs(sim)$file[notYetSaved] <-
                         file.path(sim@paths$outputPath, basename(outputs(sim)$file[notYetSaved]))
                     }
                   }
                   validObject(sim)
                   return(sim)
})

################################################################################
#' @inheritParams paths
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-paths
#'
setGeneric("modulePath", function(sim) {
  standardGeneric("modulePath")
})

#' @export
#' @rdname simList-accessors-paths
setMethod("modulePath",
          signature = ".simList",
          definition = function(sim) {
            return(sim@paths$modulePath)
})

#' @export
#' @rdname simList-accessors-paths
setGeneric("modulePath<-",
           function(sim, value) {
             standardGeneric("modulePath<-")
})

#' @name modulePath<-
#' @aliases modulePath<-,.simList-method
#' @rdname simList-accessors-paths
#' @export
setReplaceMethod(
  "modulePath",
  signature = ".simList",
  function(sim, value) {
    sim@paths$modulePath <- unname(unlist(value))
    validObject(sim)
    return(sim)
})

################################################################################
#' Time usage in \code{SpaDES}
#'
#' Functions for the \code{simtimes} slot of a \code{simList} object
#' and its elements. To maintain modularity, the behavior of these functions depends
#' on where they are used. In other words, different modules can have their own
#' timeunit. \code{SpaDES} converts these to seconds when running a simulation, but
#' shows the user time in the units of the model as shown with \code{timeunit(sim)}
#'
#' NOTE: These have default behavior that is based on the calling
#' frame timeunit. When used inside a module, then the time is in the units of the module.
#' If used in an interactive mode, then the time will be in the units of the spades
#' simulation.
#'
#' Additional methods are provided to access the current, start, and end times
#' of the simulation:
#' \tabular{ll}{
#'    \code{time} \tab Current simulation time.\cr
#'    \code{start} \tab Simulation start time.\cr
#'    \code{end} \tab Simulation end time.\cr
#'    \code{timeunit} \tab Simulation timeunit.\cr
#'    \code{timeunits} \tab Module timeunits.\cr
#'    \code{times} \tab List of all simulation times (current, start, end, timeunit).\cr
#' }
#'
#' @inheritParams objs
#'
#' @param unit   Character. One of the time units used in \code{SpaDES}.
#'
#' @param value  A time, given as a numeric, optionally with a unit attribute, but this
#'               will be deduced from the model time units or module time units (if used
#'               within a module)
#'
#' @param ...    Additional parameters.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @family functions to access elements of a \code{simList} object
#' @seealso \code{\link{SpaDES}}, specifically the section 1.2.5 on Simulation times.
#'
#' @export
#' @include simList-class.R
#' @include times.R
#' @importFrom chron times
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
  signature = ".simList",
  definition = function(x) {
    mUnit <- .callingFrameTimeunit(x)
    if (is.null(mUnit)) {
      mUnit <- NA_character_
    }
    t <- list(current = time(x, x@simtimes[["timeunit"]]), start = start(x, x@simtimes[["timeunit"]]),
           end = end(x, x@simtimes[["timeunit"]]), timeunit = x@simtimes[["timeunit"]])
    return(t)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("times<-", function(x, value) {
  standardGeneric("times<-")
})

#' @name times<-
#' @aliases times<-,.simList-method
#' @export
#' @rdname simList-accessors-times
setReplaceMethod(
  "times",
   signature = ".simList",
   function(x, value) {
     value <- as.list(value)
     if (!all(is(value$current, "numeric"),
            is(value$start, "numeric"),
            is(value$end, "numeric"),
            is(value$timeunit, "character"))) {
       stop("Please supply a named list, current, start, end, and timeunit")
     }

     if (is.null(attributes(value$current)$unit))
       attributes(value$current)$unit <- value$timeunit
     if (is.null(attributes(value$start)$unit))
       attributes(value$start)$unit <- value$timeunit
     if (is.null(attributes(value$end)$unit))
       attributes(value$end)$unit <- value$timeunit

     x@simtimes$current <- convertTimeunit(value$current, "second", x@.envir)
     x@simtimes$start <- convertTimeunit(value$start, "second", x@.envir)
     x@simtimes$end <- convertTimeunit(value$end, "second", x@.envir)
     x@simtimes$timeunit <- value$timeunit

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
setGeneric("time", function(x, unit, ...) {
  stats::time(x, ...)
})

#' @export
#' @rdname simList-accessors-times
setMethod(
  "time",
  signature = c(".simList", "missing"),
  definition = function(x) {
    mUnit <- .callingFrameTimeunit(x)
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
  signature = c(".simList", "character"),
  definition = function(x, unit) {
    if (!is.na(unit)) {
      if (is.na(pmatch("second", unit))) {
        # i.e., if not in same units as simulation
        t <- convertTimeunit(x@simtimes$current, unit, x@.envir)
        return(t)
      }
    }
    t <- x@simtimes$current
    return(t)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("time<-", function(x, value) {
  standardGeneric("time<-")
})

#' @name time<-
#' @aliases time<-,.simList-method
#' @export
#' @rdname simList-accessors-times
setReplaceMethod(
  "time",
   signature = ".simList",
   function(x, value) {
     if (is.null(attributes(value)$unit)) {
       attributes(value)$unit <- x@simtimes[["timeunit"]]
     }
     x@simtimes$current <- convertTimeunit(value, "second", x@.envir)

     if (!is.numeric(x@simtimes$current)) stop("time must be a numeric")
     if (!any(pmatch(.spadesTimes, attr(x@simtimes$current, "unit")))) {
       stop("time must be one of", paste(.spadesTimes, collapse = ", "))
     }
     return(x)
})

################################################################################
#' @inheritParams times
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
  signature = c(".simList", "missing"),
  definition = function(x) {
    mUnit <- .callingFrameTimeunit(x)
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
  signature = c(".simList", "character"),
  definition = function(x, unit) {

    if (!is.na(unit)) {
      if (is.na(pmatch("second", unit))) {
        # i.e., if not in same units as simulation
        t <- convertTimeunit(x@simtimes$end, unit, x@.envir)
        return(t)
      }
    }
    t <- x@simtimes$end
    return(t)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("end<-", function(x, value) {
  standardGeneric("end<-")
})

#' @name end<-
#' @aliases end<-,.simList-method
#' @export
#' @rdname simList-accessors-times
setReplaceMethod(
  "end",
  signature = ".simList",
  function(x, value) {
    # convert time units, if required
    if (is.null(attributes(value)$unit)) {
      attributes(value)$unit <- x@simtimes[["timeunit"]]
    }
    x@simtimes$end <- convertTimeunit(value, "second", x@.envir)
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
setGeneric("start", function(x, unit, ...) {
  stats::start(x, ...)
})

#' @rdname simList-accessors-times
setMethod(
  "start",
  signature = c(".simList", "missing"),
  definition = function(x) {
    mUnit <- .callingFrameTimeunit(x)
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
  signature = c(".simList", "character"),
  definition = function(x, unit) {
    if (!is.na(unit)) {
      if (is.na(pmatch("second", unit))) {
        # i.e., if not in same units as simulation
        t <- convertTimeunit(x@simtimes$start, unit, x@.envir)
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
#' @aliases start<-,.simList-method
#' @rdname simList-accessors-times
setReplaceMethod(
  "start",
   signature = ".simList",
   function(x, value) {
     # convert time units, if required
     if (is.null(attributes(value)$unit)) {
       attributes(value)$unit <- x@simtimes[["timeunit"]]
     }
     x@simtimes$start <- convertTimeunit(value, "second", x@.envir)
     validObject(x)
     return(x)
})

################################################################################
#' @inheritParams times
#' @include simList-class.R
#' @include times.R
#' @docType methods
#' @keywords internal
#' @rdname namespacing
#'
setGeneric(".callingFrameTimeunit", function(x) {
  standardGeneric(".callingFrameTimeunit")
})

#' @export
#' @docType methods
#' @rdname namespacing
setMethod(
  ".callingFrameTimeunit",
  signature = c(".simList"),
  definition = function(x) {
    mod <- x@current$moduleName
    out <- if (length(mod) > 0) {
      timeunits(x)[[mod]]
    } else {
      x@simtimes[["timeunit"]]
    }
    return(out)
})

#' @export
#' @docType methods
#' @rdname namespacing
#'
setMethod(
  ".callingFrameTimeunit",
  signature = c("NULL"),
  definition = function(x) {
    return(NULL)
})

################################################################################
#' @inheritParams times
#'
#' @details \code{timeunit} will extract the current units of the time used in a
#' simulation (i.e., within a \code{spades} call).
#' If it is set within a \code{simInit}, e.g.,
#' \code{times=list(start=0, end=52, timeunit = "week")}, it will set the
#' units for that simulation.
#' By default, a \code{simInit} call will use the smallest unit contained within
#' the metadata for the modules being used. If there are parent modules, then the
#' parent module timeunit will be used even if one of its children is a smaller timeunit.
#' If all modules, including parents, are set to \code{NA}, \code{timeunit} defaults to seconds.
#' If parents are set to \code{NA}, then the set of modules defined by that parent module
#' will be given the smallest units of the children.
#'
#' Currently, available units are "second", "hours", day", "week", "month", and
#' "year" can be used in the metadata of a module.
#'
#' The user can also define a new unit. The unit name can be anything, but the
#' function definition must be of the form \code{dunitName}, e.g., \code{dyear}
#' or \code{dfortnight}.
#' The unit name is the part without the \code{d} and the function name definition
#' includes the \code{d}. This new function, e.g.,
#' \code{dfortnight <- function(x) lubridate::duration(dday(14))}
#' can be placed anywhere in the search path or in a module.
#'
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-times
#'
setGeneric("timeunit", function(x) {
  standardGeneric("timeunit")
})

#' @rdname simList-accessors-times
#' @export
setMethod("timeunit",
          signature = ".simList",
          definition = function(x) {
            return(x@simtimes$timeunit)
})

#' @export
#' @rdname simList-accessors-times
setGeneric("timeunit<-",
           function(x, value) {
             standardGeneric("timeunit<-")
})

#' @name timeunit<-
#' @aliases timeunit<-,.simList-method
#' @export
#' @rdname simList-accessors-times
setReplaceMethod(
  "timeunit",
  signature = ".simList",
  function(x, value) {
    value <- as.character(value)
    if (checkTimeunit(value, envir = x@.envir)) {
        x@simtimes$timeunit <- value
    } else {
      x@simtimes$timeunit <- NA_character_
    }
    validObject(x)
    return(x)
})

################################################################################
#' @inheritParams times
#'
#' @details \code{timeunits} will extract the current units of the time of all
#' modules used in a simulation.
#' This is different from \code{timeunit} because it is not necessarily
#' associated with a \code{spades} call.
#'
#' In many cases, the "simpler" use of each of these functions may be slower
#' computationally. For instance, it is much faster to use \code{time(sim, "year")}
#' than \code{time(sim)}. So as a module developer, it is advantageous to
#' write out the longer one, minimizing the looking up that R must do.
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
  signature = ".simList",
  definition = function(x) {
    isNonParent <- !sapply(depends(x)@dependencies, function(y) {
      if (!is.null(y)) {
        length(y@childModules) > 0
      } else {
        FALSE
      }
    })
    if (all(sapply(depends(x)@dependencies[isNonParent], is.null))) {
      timestepUnits <- NULL
    } else {
      timestepUnits <- lapply(depends(x)@dependencies[isNonParent], function(y) {
        y@timeunit
      })
      names(timestepUnits) <- sapply(depends(x)@dependencies[isNonParent], function(y) {
        y@name
      })
    }
    return(timestepUnits)
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
#' @inheritParams objs
#'
#' @param unit   Character. One of the time units used in \code{SpaDES}.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @family functions to access elements of a \code{simList} object
#' @seealso \code{\link{SpaDES}}, specifically the section 1.2.6 on Simulation event queues.
#'
#' @export
#' @include simList-class.R
#' @importFrom data.table ':=' data.table copy
#' @importFrom lazyeval interp
#' @importFrom stats setNames
#' @docType methods
#' @aliases simList-accessors-events
#' @rdname simList-accessors-events
#'
setGeneric("events", function(sim, unit) {
  standardGeneric("events")
})

#' @export
#' @rdname simList-accessors-events
setMethod(
  "events",
  signature = c(".simList", "character"),
  definition = function(sim, unit) {

    out <- if (is.na(pmatch("second", unit)) &
               (length(sim@events$eventTime) > 0)) {
      #note the above line captures empty eventTime,
      # whereas is.na does not
      if (any(!is.na(sim@events$eventTime))) {
        if (!is.null(sim@events$eventTime)) {
          obj <- data.table::copy(sim@events) # don't change original sim
          obj[, eventTime := convertTimeunit(eventTime, unit, sim@.envir)]
          obj[]
          obj
        }
      } else {
        sim@events
      }
    } else {
      sim@events
    }
    return(out)
})

#' @export
#' @rdname simList-accessors-events
setMethod("events",
          signature = c(".simList", "missing"),
          definition = function(sim, unit) {
            res <- events(sim, sim@simtimes[["timeunit"]])
            return(res)
})

#' @export
#' @rdname simList-accessors-events
setGeneric("events<-",
           function(sim, value) {
             standardGeneric("events<-")
})

#' @name events<-
#' @aliases events<-,.simList-method
#' @export
#' @rdname simList-accessors-events
setReplaceMethod(
  "events",
   signature = ".simList",
   function(sim, value) {
     if (!is(value, "data.table")) stop("Event queue must be a data.table")
     if (!identical(names(value), .emptyEventListCols))
       stop("Event queue must be a data.table with columns: ",
            paste(.emptyEventListCols, collapse = ", "), ".")
     if (is.null(attributes(value$eventTime)$unit)) {
       attributes(value$eventTime)$unit <- sim@simtimes[["timeunit"]]
     }
     if (is.na(pmatch("second", attributes(value$eventTime)$unit))) {
       value[, eventTime := convertTimeunit(eventTime, "second", sim@.envir)]
     }

     sim@events <- value
     return(sim)
})

################################################################################
#' @inheritParams events
#' @include simList-class.R
#' @importFrom data.table ':=' data.table
#' @importFrom lazyeval interp
#' @importFrom stats setNames
#' @export
#' @docType methods
#' @rdname simList-accessors-events
#'
setGeneric("current", function(sim, unit) {
  standardGeneric("current")
})

#' @rdname simList-accessors-events
#' @export
setMethod(
  "current",
  signature = c(".simList", "character"),
  definition = function(sim, unit) {
    out <- if (is.na(pmatch("second", unit)) & (length(sim@current$eventTime))) {
      # note the above line captures empty eventTime, whereas `is.na` does not
      if (any(!is.na(sim@current$eventTime))) {
        if (!is.null(sim@current$eventTime)) {
          obj <- data.table::copy(sim@current) # don't change original sim
          obj[, eventTime := convertTimeunit(eventTime, unit, sim@.envir)]
          obj[]
          obj
        }
      } else {
        sim@current
      }
    } else {
      sim@current
    }
    return(out)
})

#' @export
#' @rdname simList-accessors-events
setMethod("current",
          signature = c(".simList", "missing"),
          definition = function(sim, unit) {
            out <- current(sim, sim@simtimes[["timeunit"]])
            return(out)
})

#' @export
#' @rdname simList-accessors-events
setGeneric("current<-",
           function(sim, value) {
             standardGeneric("current<-")
})

#' @name current<-
#' @aliases current<-,.simList-method
#' @export
#' @rdname simList-accessors-events
setReplaceMethod("current",
                 signature = ".simList",
                 function(sim, value) {
                   if (!is(value, "data.table")) stop("Event queue must be a data.table")
                   if (!identical(names(value), .emptyEventListCols)) {
                     stop("Event queue must be a data.table with columns: ",
                          paste(.emptyEventListCols, collapse = ", "), ".")
                   }
                   sim@current <- value
                   return(sim)
})

################################################################################
#' @inheritParams events
#' @include simList-class.R
#' @importFrom data.table ':=' data.table
#' @importFrom lazyeval interp
#' @importFrom stats setNames
#' @export
#' @docType methods
#' @rdname simList-accessors-events
#'
setGeneric("completed", function(sim, unit) {
  standardGeneric("completed")
})

#' @rdname simList-accessors-events
#' @export
setMethod(
  "completed",
  signature = c(".simList", "character"),
  definition = function(sim, unit) {
    out <- if (is.na(pmatch("second", unit)) & (length(sim@completed$eventTime))) {
      # note the above line captures empty eventTime, whereas `is.na` does not
      if (any(!is.na(sim@completed$eventTime))) {
        if (!is.null(sim@completed$eventTime)) {
          obj <- data.table::copy(sim@completed) # don't change original sim
          obj[, eventTime := convertTimeunit(eventTime, unit, sim@.envir)]
          obj[]
          obj
        }
      } else {
        sim@completed
      }
    } else {
      sim@completed
    }
    return(out)
})

#' @export
#' @rdname simList-accessors-events
setMethod("completed",
          signature = c(".simList", "missing"),
          definition = function(sim, unit) {
            out <- completed(sim, sim@simtimes[["timeunit"]])
            return(out)
})

#' @export
#' @rdname simList-accessors-events
setGeneric("completed<-",
           function(sim, value) {
             standardGeneric("completed<-")
})

#' @name completed<-
#' @aliases completed<-,.simList-method
#' @export
#' @rdname simList-accessors-events
setReplaceMethod("completed",
                 signature = ".simList",
                 function(sim, value) {
                   if (!is(value, "data.table")) stop("Completed queue must be a data.table")
                   if (!identical(names(value), .emptyEventListCols)) {
                     stop("Event queue must be a data.table with columns, ",
                          paste(.emptyEventListCols, collapse = ", "), ".")
                   }
                   sim@completed <- value
                   return(sim)
})

################################################################################
#' Add simulation dependencies
#'
#' Internal function.
#' Adds a \code{\link{.moduleDeps}} object to the simulation dependency list.
#'
#' @inheritParams objs
#'
#' @param x   A named list containing the parameters used to construct a new
#'            \code{\link{.moduleDeps}} object.
#'
#' @return A \code{simList} object.
#'
#' @include simList-class.R
#' @docType methods
#' @family functions to access elements of a \code{simList} object
#' @keywords internal
#' @rdname addDepends
#'
#' @author Alex Chubaty
#'
setGeneric(".addDepends", function(sim, x) {
  standardGeneric(".addDepends")
})

#' @rdname addDepends
setMethod(
  ".addDepends",
  signature(sim = ".simList", x = ".moduleDeps"),
  definition = function(sim, x) {
    deps <- depends(sim)
    n <- length(deps@dependencies)
    if (n == 1L) {
      if (is.null(deps@dependencies[[1L]])) n <- 0L
    }
    deps@dependencies[[n + 1L]] <- x
    dupes <- which(duplicated(deps@dependencies))
    if (length(dupes)) deps@dependencies <- deps@dependencies[-dupes]
    depends(sim) <- deps
    return(sim)
})

################################################################################
#' Get module or simulation package dependencies
#'
#' @param sim  A \code{simList} object.
#'
#' @param ...  Additional arguments.
#'             Currently only \code{module}, specifying the name of a module,
#'             and \code{filename}, specifying a module filename, are supported.
#'
#' @return A sorted character vector of package names.
#'
#' @export
#' @include simList-class.R
#' @docType methods
#' @family functions to access elements of a \code{simList} object
#' @rdname packages
#'
#' @author Alex Chubaty
#'
# igraph exports %>% from magrittr
setGeneric("packages", function(sim, ...) {
  standardGeneric("packages")
})

#' @export
#' @rdname packages
setMethod(
  "packages",
  signature(sim = ".simList"),
  definition = function(sim, ...) {
    pkgs <- lapply(depends(sim)@dependencies, function(x) {
        x@reqdPkgs
      }) %>% unlist() %>% append("SpaDES") %>% unique() %>% sort()
    return(pkgs)
})

#' @export
#' @rdname packages
setMethod(
  "packages",
  signature(sim = "missing"),
  definition = function(sim, ...) {
    args <- list(...)
    if (!is.null(args$filename)) {
      pkgs <- .parseModulePartial(filename = args$filename,
                                  defineModuleElement = "reqdPkgs") %>%
        unlist() %>% append("SpaDES") %>% unique() %>% sort()
      return(pkgs)
    } else if (!is.null(args$module)) {
      f <- file.path(getOption("spades.modulePath"), args$module, paste0(args$module, ".R"))
      pkgs <- .parseModulePartial(filename = f, defineModuleElement = "reqdPkgs") %>%
        unlist() %>% append("SpaDES") %>% unique() %>% sort()
      return(pkgs)
    } else {
      stop("one of sim, modules, or filename must be supplied.")
    }
})

################################################################################
#' Define a new module.
#'
#' Specify a new module's metadata as well as object and package dependencies.
#' Packages are loaded during this call.
#'
#' @section Required metadata elements:
#'
#' \tabular{ll}{
#'    \code{name} \tab Module name. Must match the filename (without the \code{.R} extension).
#'                     This is currently not parsed by SpaDES;
#'                         it is for human readers only. \cr
#'    \code{description} \tab Brief description of the module.
#'                            This is currently not parsed by SpaDES;
#'                            it is for human readers only. \cr
#'    \code{keywords} \tab Author-supplied keywords.
#'                         This is currently not parsed by SpaDES;
#'                         it is for human readers only. \cr
#'    \code{childModules} \tab If this contains any character vector, then it will
#'                             be treated as a parent module. If this is a parent module,
#'                             then only this list entry will be read. For normal,
#'                             i.e., 'child modules', this should be \code{character(0)} or
#'                             \code{NA}.
#'                             If a character vector is provided, then these must be the
#'                             names of the modules located in the same file path as this
#'                             parent module that will be loaded during the \code{simInit}.\cr
#'    \code{authors} \tab Module author information (as a vector of \code{\link{person}}
#'                        objects. This is currently not parsed by SpaDES;
#'                        it is for human readers only.\cr
#'    \code{version} \tab Module version number (will be coerced to \code{\link{numeric_version}}
#'                        if a character or numeric are supplied).
#'                        The module developer should update manually this with each change
#'                        that is made to the module. See \url{http://semver.org/}
#'                        for a widely accepted standard for version numering.\cr
#'    \code{spatialExtent} \tab The spatial extent of the module supplied via
#'                              \code{raster::extent}. This is currently unimplemented.
#'                              Once implemented, this should define what spatial region this
#'                              module is scientifically reasonable to be used in.\cr
#'    \code{timeframe} \tab Vector (length 2) of POSIXt dates specifying the temporal extent
#'                          of the module. Currently unimplemented.
#'                          Once implemented, this should define what time frame this
#'                          module is scientifically reasonable to be used for.\cr
#'    \code{timeunit} \tab Time scale of the module (e.g., "day", "year"). This
#'                         MUST be specified. It indicates what '1' unit of time
#'                         means for this module. \code{SpaDES} interprets this
#'                         and if modules have different \code{timeunit} values
#'                         then it will correctly schedule each module, using the
#'                         smallest (currently the default) timeunit as the
#'                         'model' timeunit in the \code{spades} call.\cr
#'    \code{citation} \tab List of character strings specifying module citation information.
#'                         Alternatively, a list of filenames of \code{.bib} or similar files.
#'                         This is currently not parsed by SpaDES;
#'                         it is for human readers only.\cr
#'    \code{documentation} \tab List of filenames referring to module documentation sources.
#'                              This is currently not parsed by SpaDES;
#'                              it is for human readers only.\cr\cr
#'    \code{reqdPkgs} \tab List of R package names required by the module. These
#'                         packages will be loaded when \code{simInit} is called. \cr
#'    \code{parameters} \tab A data.frame specifying the parameters used in the module.
#'                           Usually produced by \code{rbind}-ing the outputs of multiple
#'                           \code{\link{defineParameter}} calls. These parameters indicate
#'                           the default values that will be used unless a module user
#'                           overrides them with the \code{params} argument in the
#'                           \code{\link{simInit}} call. The minimum and maximum are
#'                           currently used
#'                           by the \code{shine} function and the \code{POM} function, and they
#'                           should indicate the range of values that are reasonable
#'                           scientifically.\cr
#'    \code{inputObjects} \tab A \code{data.frame} specifying the data objects expected as
#'                             inputs to the module,
#'                             with columns \code{objectName} (class \code{character}),
#'                             \code{objectClass} (class \code{character}),
#'                             \code{sourceURL} (class \code{character}), and \code{other}
#'                              (currently spades does nothing with this column).
#'                             This data.frame identifies the objects that are expected,
#'                             but does not do any loading of
#'                             that object into the simList. The \code{sourceURL} gives
#'                             the developer the opportunity
#'                             to identify the source of a data file that can be used
#'                             with the model. This URL will be
#'                             used if the user calls \code{downloadData} (or
#'                             \code{downloadModule(..., data = TRUE)}. If the raw data
#'                             must be modified, the developer can use create a
#'                             function called \code{.inputObjects} in their module. That
#'                             function will be run during the \code{simInit} call. The
#'                             developer should ensure that if the object is supplied
#'                             by the module user as an argument in the \code{simInit}, then
#'                             the \code{.inputObjects} should not be run, i.e., use an
#'                             \code{(is.null(sim$xxx)))}.\cr
#'    \code{outputObjects} \tab A \code{data.frame} specifying the data objects output by
#'                              the module, with columns identical to those in
#'                              \code{inputObjects}. Like \code{inputObjects} above,
#'                              this only identifies the objects that this module will output
#'                              into the \code{simList}.
#'                              The module developer must create the necessary functions
#'                              that will cause these objects to be put into the
#'                              \code{simList}.\cr
#' }
#'
#' @inheritParams objs
#'
#' @param x A list with a number of named elements, referred to as the metadata. See details.
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
#'   ## a default version of the defineModule is created with a call to newModule
#'   newModule("test", path = tempdir())
#'
#'   ## view the resulting module file
#'   #file.edit(file.path(tempdir(), "test", "test.R"))
#'
#'   # The default defineModule created by newModule is currently (SpaDES version 1.3.1.9044):
#'   defineModule(sim, list(
#'     name = "test",
#'     description = "insert module description here",
#'     keywords = c("insert key words here"),
#'     authors = c(person(c("First", "Middle"), "Last",
#'                        email = "email@example.com", role = c("aut", "cre"))),
#'     childModules = character(0),
#'     version = list(SpaDES = "1.3.1.9044", test = "0.0.1"),
#'     spatialExtent = raster::extent(rep(NA_real_, 4)),
#'     timeframe = as.POSIXlt(c(NA, NA)),
#'     timeunit = NA_character_, # e.g., "year",
#'     citation = list("citation.bib"),
#'     documentation = list("README.txt", "test.Rmd"),
#'     reqdPkgs = list(),
#'     parameters = rbind(
#'       #defineParameter("paramName", "paramClass", value, min, max,
#'       "parameter description")),
#'       defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
#'       "This describes the simulation time at which the first plot event should occur"),
#'       defineParameter(".plotInterval", "numeric", NA, NA, NA,
#'       "This describes the simulation time at which the first plot event should occur"),
#'       defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
#'       "This describes the simulation time at which the first save event should occur"),
#'       defineParameter(".saveInterval", "numeric", NA, NA, NA,
#'       "This describes the simulation time at which the first save event should occur")
#'     ),
#'     inputObjects = bind_rows(
#'       expectsInput(objectName = NA_character_, objectClass = NA_character_,
#'         sourceURL = NA_character_, desc = NA_character_, other = NA_character_)
#'     ),
#'     outputObjects = bind_rows(
#'       createsOutput(objectName = NA_character_, objectClass = NA_character_,
#'         desc = NA_character_, other = NA_character_)
#'     )
#'   ))
#'
#' }
#'
setGeneric("defineModule", function(sim, x) {
  standardGeneric("defineModule")
})

#' @export
#' @rdname defineModule
setMethod(
  "defineModule",
  signature(sim = ".simList", x = "list"),
  definition = function(sim, x) {
    # check that all metadata elements are present
    metadataRequired <- slotNames(new(".moduleDeps"))
    metadataProvided <- metadataRequired %in% names(x)
    metadataMissing <- metadataRequired[!metadataProvided]
    if (!all(metadataProvided)) {
      warning(paste0(
        "The \'", x$name, "\' module is missing the metadata for:\n",
        paste(" - ", metadataMissing, collapse = "\n"), "\n",
        "Please see ?defineModule and ?.moduleDeps for more info.\n",
        "All metadata elements must be present and valid."
      ))
    }

    # provide default values for missing metadata elements
    if (identical(x$reqdPkgs, list())) {
      x$reqdPkgs <- list()
    } else if (is.null(na.omit(x$reqdPkgs))) {
      x$reqdPkgs <- list()
    } else if (any(!nzchar(na.omit(x$reqdPkgs)))) {
      x$reqdPkgs <- list()
    } else {
      loadPackages(x$reqdPkgs)
    }

    ## enforce/coerce types for the user-supplied param list
    lapply(c("name", "description", "keywords"), function(z) {
      x[[z]] <<- if ( is.null(x[[z]]) || (length(x[[z]]) == 0) ) {
        NA_character_
      } else {
        as.character(x[[z]])
      }
    })

    x$childModules <- x$childModules %>% as.character() %>% na.omit() %>% as.character()

    x$authors <- if ( is.null(x$authors) || is.na(x$authors) ) {
      person("unknown")
    } else {
      as.person(x$authors)
    }

    ## maintain backwards compatibility with SpaDES versions prior to 1.3.1.9044
    ## where `version` was a single `numeric_version` value instead of named list
    x$version <- if (is.null(names(x$version))) {
      as.numeric_version(x$version) ## SpaDES < 1.3.1.9044
    } else {
      as.numeric_version(x$version[[x$name]]) ## SpaDES >= 1.3.1.9044
    }

    x$spatialExtent <- if (!is(x$spatialExtent, "Extent")) {
      if (is.null(x$spatialExtent)) {
        extent(rep(NA_real_, 4))
      } else {
        if (is.na(x$spatialExtent)) {
          extent(rep(NA_real_, 4))
        } else {
          extent(x$spatialExtent)
        }
      }
    }

    x$timeframe <- if ( is.null(x$timeframe) || is.na(x$timeframe) ) {
      as.POSIXlt(c(NA, NA))
    } else if (!is.numeric.POSIXt(x$timeframe)) {
      as.POSIXlt(x$timeframe)
    } %>% `[`(1:2)

    if ( is.null(x$timeunit) || is.na(x$timeunit) ) {
      x$timeunit <- NA_character_
    }

    lapply(c("citation", "documentation", "reqdPkgs"), function(z) {
      x[[z]] <<- if (is.null(x[[z]])) {
        list()
      } else {
        as.list(x[[z]])
      }
    })
    if ( is.null(x$parameters) ) {
      x$parameters <- defineParameter()
    } else {
      if ( is(x$parameters, "data.frame") ) {
        if ( !all(colnames(x$parameters) %in% colnames(defineParameter())) ||
             !all(colnames(defineParameter()) %in% colnames(x$parameters)) ) {
          stop("invalid data.frame `parameters` in module `", x$name, "`")
        }
      } else {
        x$parameters <- defineParameter()
      }
    }

    if (is.null(x$inputObjects)) {
      x$inputObjects <- .inputObjects()
    } else {
      if (is(x$inputObjects, "data.frame")) {
        if ( !all(colnames(x$inputObjects) %in% colnames(.inputObjects())) ||
             !all(colnames(.inputObjects()) %in% colnames(x$inputObjects)) ) {
          stop("invalid data.frame `inputObjects` in module `", x$name, "`:\n",
               "provided: ", paste(colnames(x$inputObjects), collapse = ", "),
               "expected: ", paste(colnames(.inputObjects()), collapse = ", "))
        }
      } else {
        x$inputObjects <- .inputObjects()
      }
    }
    if (NROW(x$inputObjects)) {
      if (is.null(x$inputObjects$sourceURL)) {
        x$inputObjects$sourceURL <- rep(NA_character_, NROW(x$inputObjects))
      }
      ids <- which(x$inputObjects$sourceURL == "")
      if (length(ids)) {
        x$inputObjects$sourceURL[ids] <- NA_character_
      }
    }

    if (is.null(x$outputObjects)) {
      x$outputObjects <- .outputObjects()
    } else {
      if (is(x$outputObjects, "data.frame")) {
        if ( !all(colnames(x$outputObjects) %in% colnames(.outputObjects())) ||
             !all(colnames(.outputObjects()) %in% colnames(x$outputObjects)) ) {
          stop("invalid data.frame `outputObjects` in module `", x$name, "`:",
               "provided: ", paste(colnames(x$outputObjects), collapse = ", "), "\n",
               "expected: ", paste(colnames(.outputObjects()), collapse = ", "))
        }
      } else {
        x$outputObjects <- .outputObjects()
      }
    }

    ## check that documentation actually exists locally
    docs <- sapply(x$documentation, na.omit) %>%
      (function(x) if (length(x)) character(0) else as.character(x))
    if (length(docs)) {
      lapply(docs, function(y) {
        if (!file.exists(file.path(modulePath(sim), y))) {
          stop("Module documentation file ", y, " not found in modulePath.")
        }
      })
    }

    ## check that children actually exist locally, and add to list of child modules
    if (length(x$childModules)) {
      lapply(x$childModules, function(y) {
        if (file.exists(file.path(modulePath(sim), y))) {
          z <- y %>% lapply(., `attributes<-`, list(type = "child"))
          modules(sim) <- append_attr(sim@modules, z)
        } else {
          stop("Module ", y, "(a child module of ", x$name, ") not found in modulePath.")
        }
      })
    }

    ## create module deps object and add to sim deps
    m <- do.call(new, c(".moduleDeps", x))
    return(.addDepends(sim, m))
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
#'   defineParameter("lambda", "numeric", 1.23, desc = "intrinsic rate of increase"),
#'   defineParameter("P", "numeric", 0.2, 0, 1, "probability of attack")
#' )
#'
setGeneric("defineParameter", function(name, class, default, min, max, desc) {
  standardGeneric("defineParameter")
})

#' @rdname defineParameter
setMethod("defineParameter",
          signature(name = "character", class = "character", default = "ANY",
                    min = "ANY", max = "ANY", desc = "character"),
          definition = function(name, class, default, min, max, desc) {

            # coerce `default`, `min`, and `max` to same specified type
            # These next lines commented out because it doesn't allow for character e.g.,
            #   start(sim)
            #default <- as(default, class)
            #min <- as(min, class)
            #max <- as(max, class)

            # previously used `substitute()` instead of `I()`,
            # but it did not allow for a vector to be passed with `c()`
            df <- data.frame(
              paramName = name, paramClass = class, default = I(list(default)),
              min = I(list(min)), max = I(list(max)), paramDesc = desc,
              stringsAsFactors = FALSE)
            return(df)
})

#' @rdname defineParameter
setMethod("defineParameter",
          signature(name = "character", class = "character",
                    default = "ANY", min = "missing", max = "missing",
                    desc = "character"),
          definition = function(name, class, default, desc) {
            NAtypes <- c("character", "complex", "integer", "logical", "numeric")
            if (class %in% NAtypes) {
              # coerce `min` and `max` to same type as `default`
              min <- as(NA, class)
              max <- as(NA, class)
            } else {
              min <- NA
              max <- NA
            }

            df <- data.frame(
              paramName = name, paramClass = class, default = I(list(default)),
              min = I(list(substitute(min))), max = I(list(substitute(max))),
              paramDesc = desc, stringsAsFactors = FALSE
            )
            return(df)
})

#' @rdname defineParameter
setMethod(
  "defineParameter",
  signature(name = "missing", class = "missing", default = "missing",
            min = "missing", max = "missing", desc = "missing"),
  definition = function() {
    df <- data.frame(
      paramName = character(0), paramClass = character(0),
      default = I(list()), min = I(list()), max = I(list()),
      paramDesc = character(0), stringsAsFactors = FALSE)
    return(df)
})

################################################################################
#' Define an input object that the module expects.
#'
#' Used to specify an input object's name, class, description, source url and
#' other specifications.
#'
#' @param objectName   Character string to define the input object's name.
#'
#' @param objectClass  Character string to specify the input object's class.
#'
#' @param desc         Text string providing a brief description of the input object.
#'
#' @param sourceURL    Character string to specify an URL to reach the input object,
#'                     default is \code{NA}.
#'
#' @param ...          Other specifications of the input object.
#'
#' @return A \code{data.frame} suitable to be passed to \code{inputObjects} in a
#' module's metadata.
#'
#' @export
#' @docType methods
#' @rdname expectsInput
#'
#' @author Yong Luo
#'
#' @examples
#' inputObjects <- dplyr::bind_rows(
#'   expectsInput(objectName = "inputObject1", objectClass = "character",
#'                desc = "this is for example", sourceURL = "not available"),
#'   expectsInput(objectName = "inputObject2", objectClass = "numeric",
#'                desc = "this is for example", sourceURL = "not available",
#'                otherInformation = "I am the second input object")
#' )
#'
setGeneric("expectsInput",
           function(objectName, objectClass, desc, sourceURL, ...) {
             standardGeneric("expectsInput")
})

#' @export
#' @rdname expectsInput
setMethod(
  "expectsInput",
  signature = signature(objectName = "ANY", objectClass = "ANY",
                        desc = "ANY", sourceURL = "ANY"),
  definition = function(objectName, objectClass, desc, sourceURL, ...) {
    return(expectsInput(as.character(objectName), as.character(objectClass),
                        as.character(desc), as.character(sourceURL), ...))
})

#' @export
#' @rdname expectsInput
setMethod(
  "expectsInput",
  signature = signature(objectName = "character", objectClass = "character",
                        desc = "character", sourceURL = "character"),
  definition = function(objectName, objectClass, desc, sourceURL, ...) {
    returnDataframe <- data.frame(cbind(objectName, objectClass, desc, sourceURL),
                                  stringsAsFactors = FALSE)
    templist <- list(...)
    if (length(templist) > 0) {
      for (i in 1:length(templist)) {
        returnDataframe <- data.frame(cbind(returnDataframe, I(list(templist[[i]])),
                                            stringsAsFactors = FALSE))
        names(returnDataframe)[ncol(returnDataframe)] <- names(templist)[i]
      }
    }
    return(returnDataframe)
})

#' @export
#' @rdname expectsInput
setMethod(
  "expectsInput",
  signature = signature(objectName = "character", objectClass = "character",
                        desc = "character", sourceURL = "missing"),
  definition = function(objectName, objectClass, desc, ...) {
    return(expectsInput(objectName, objectClass, desc, sourceURL = NA_character_, ...))
})

################################################################################
#' Define an output object of a module
#'
#' Used to specify an output object's name, class, description and other specifications.
#'
#' @param objectName   Character string to define the output object's name.
#'
#' @param objectClass  Character string to specify the output object's class.
#'
#' @param desc         Text string providing a brief description of the output object.
#'
#' @param ...          Other specifications of the output object.
#'
#' @return A \code{data.frame} suitable to be passed to \code{outputObjects} in
#' a module's metadata.
#'
#' @export
#' @docType methods
#' @rdname createsOutput
#'
#' @author Yong Luo
#'
#' @examples
#' outputObjects <- dplyr::bind_rows(
#'   createsOutput(objectName = "outputObject1", objectClass = "character",
#'                 desc = "this is for example"),
#'   createsOutput(objectName = "outputObject2", objectClass = "numeric",
#'                 desc = "this is for example",
#'                 otherInformation = "I am the second output object")
#' )
#'
setGeneric(
  "createsOutput",
  function(objectName, objectClass, desc, ...) {
    standardGeneric("createsOutput")
})

#' @export
#' @rdname createsOutput
setMethod(
  "createsOutput",
  signature = signature(objectName = "ANY", objectClass = "ANY",
                        desc = "ANY"),
  definition = function(objectName, objectClass, desc, ...) {
    return(createsOutput(as.character(objectName), as.character(objectClass),
                         as.character(desc)))
})

#' @export
#' @rdname createsOutput
setMethod(
  "createsOutput",
  signature = signature(objectName = "character", objectClass = "character",
                        desc = "character"),
  definition = function(objectName, objectClass, desc, ...) {
    returnDataframe <- data.frame(cbind(objectName, objectClass, desc),
                                  stringsAsFactors = FALSE)
    templist <- list(...)
    if (length(templist) > 0) {
      for (i in 1:length(templist)) {
        returnDataframe <- data.frame(cbind(returnDataframe, I(list(templist[[i]])),
                                            stringsAsFactors = FALSE))
        names(returnDataframe)[ncol(returnDataframe)] <- names(templist)[i]
      }
    }
    return(returnDataframe)
})

#' An internal function for coercing a data.frame to inputs()
#'
#' @param inputDF A data.frame with partial columns to pass to inputs<-
#' @param startTime Numeric time. The start(sim).
#'
#' @keywords internal
#' @rdname fillInputRows
.fillInputRows <- function(inputDF, startTime) {
  factorCols <- sapply(inputDF, is.factor)
  if (any(factorCols)) {
    inputDF[, factorCols] <- sapply(inputDF[, factorCols], as.character)
  }
  #fileTable <- .fileTableInCols ## not used??
  needRenameArgs <- grepl(names(inputDF), pattern = "arg[s]?$")
  if (any(needRenameArgs)) {
    colnames(inputDF)[needRenameArgs] <- .fileTableInCols[pmatch("arg", .fileTableInCols)]
  }
  columns <- pmatch(.fileTableInCols, names(inputDF))
  setnames(inputDF, old = colnames(inputDF)[na.omit(columns)],
           new = .fileTableInCols[!is.na(columns)])
  #columns2 <- pmatch(names(inputDF), .fileTableInCols) ## not used??
  if (any(is.na(columns))) {
    inputDF[, .fileTableInCols[is.na(columns)]] <- NA
  }

  if (any(is.na(inputDF[, "loadTime"]))) {
    inputDF[is.na(inputDF$loadTime), "loadTime"] <- startTime
  }

  if (any(is.na(inputDF[, "objectName"]))) {
    inputDF[is.na(inputDF$objectName), "objectName"] <- fileName(inputDF[is.na(inputDF$objectName), "file"])
  }

  # correct those for which a specific function is supplied in filelistDT$fun
  usesSemiColon <- grep(inputDF[, "fun"], pattern = "::")

  if (length(usesSemiColon) > 0) {
    loadFun <- inputDF$fun[usesSemiColon]
    splitPackFun <- strsplit(split = "::", loadFun)
    inputDF$package[usesSemiColon] <- sapply(splitPackFun, function(x) x[1])
    inputDF$fun[usesSemiColon] <- sapply(splitPackFun, function(x) x[2])
  }

  objectsOnly <- is.na(inputDF[, "file"])
  if (!all(objectsOnly)) {
    inputDF2 <- inputDF[!objectsOnly, ]
    if (any(is.na(inputDF2[, "fun"]))) {
      .fileExts <- .fileExtensions()
      fl <- inputDF2$file
      exts <- na.omit(match(fileExt(fl), .fileExts[, "exts"]) )
      inputDF2$fun[is.na(inputDF2$fun)] <- .fileExts[exts, "fun"]
    }

    if (any(is.na(inputDF2[, "package"]))) {
      .fileExts <- .fileExtensions()
      fl <- inputDF2$file
      exts <- match(fileExt(fl), .fileExts[, "exts"])
      inputDF2$package[is.na(inputDF2$package)]  <- .fileExts[exts, "package"]
    }
    inputDF[!objectsOnly, ] <- inputDF2
  }
  return(inputDF)
}

#' An internal function for coercing a data.frame to outputs()
#'
#' @param outputDF A data.frame with partial columns to pass to outputs<-
#' @param endTime Numeric time. The end(sim).
#'
#' @keywords internal
#' @rdname fillOutputRows
.fillOutputRows <- function(outputDF, endTime) {
  needRenameArgs <- grepl(names(outputDF), pattern = "arg[s]?$")
  if (any(needRenameArgs)) {
    colnames(outputDF)[needRenameArgs] <-
      .fileTableOutCols[pmatch("arg", .fileTableOutCols)]
  }
  columns <- pmatch(.fileTableOutCols, names(outputDF))
  setnames(outputDF, old = colnames(outputDF)[na.omit(columns)],
           new = .fileTableOutCols[!is.na(columns)])
  #columns2 <- pmatch(names(outputDF), .fileTableOutCols)
  #object@outputs <- rbind(outputDF[,na.omit(columns), drop = FALSE], .fileTableOut()[,columns2])

  if (any(is.na(columns))) {
    outputDF[, .fileTableOutCols[is.na(columns)]] <- NA
  }
  if (any(is.na(outputDF[, "saveTime"]))) {
    outputDF[is.na(outputDF$saveTime), "saveTime"] <- endTime
  }

  # correct those for which a specific function is supplied in filelistDT$fun
  usesSemiColon <- grep(outputDF[, "fun"], pattern = "::")

  if (length(usesSemiColon) > 0) {
    loadFun <- outputDF$fun[usesSemiColon]
    splitPackFun <- strsplit(split = "::", loadFun)
    outputDF$package[usesSemiColon] <- sapply(splitPackFun, function(x) x[1])
    outputDF$fun[usesSemiColon] <- sapply(splitPackFun, function(x) x[2])
  }

  if (any(is.na(outputDF[, "fun"]))) {
    .fileExts <- .saveFileExtensions()
    fl <- outputDF$file
    exts <- fileExt(fl)
    if (any(is.na(fl)) | any(!nzchar(exts, keepNA=TRUE))) {
      outputDF$fun[is.na(fl) | (!nzchar(exts, keepNA=TRUE))] <- .fileExts$fun[1]
    }
    if (any(is.na(outputDF[, "fun"]))) {
      exts <- na.omit(match(exts, .fileExts[, "exts"]) )
      outputDF$fun[is.na(outputDF$fun)] <- .fileExts[exts, "fun"]
    }
  }

  if (any(is.na(outputDF[, "package"]))) {
    .fileExts <- .saveFileExtensions()
    fl <- outputDF$file
    exts <- fileExt(fl)
    if (any(is.na(fl)) | any(!nzchar(exts, keepNA=TRUE))) {
      outputDF$package[is.na(fl) | (!nzchar(exts, keepNA=TRUE))] <- .fileExts$package[1]
    }
    if (any(is.na(outputDF[, "package"]))) {
      exts <- na.omit(match(fileExt(fl), .fileExts[, "exts"]) )
      outputDF$package[is.na(outputDF$package)] <- .fileExts[exts, "package"]
    }
  }
  return(outputDF)
}

