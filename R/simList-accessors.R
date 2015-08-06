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
# @importFrom utils capture.output
#' @importFrom stats na.omit
#' @docType methods
#' @rdname show-method
setMethod("show",
          signature="simList",
          definition=function(object) {

            out <- list()
            out[[1]] <- capture.output(cat(rep("=", getOption("width"), sep=""), "\n", sep=""))

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
            out[[9]] <- capture.output(print(cbind(ModuleName=modules(object)),
                                             quote=FALSE, row.names=FALSE))
            out[[10]] <- capture.output(cat("\n"))

            ### objects loaded
            out[[11]] <- capture.output(cat(">> Objects Loaded:\n"))

            out[[12]] <- if(NROW(inputs(object)[na.omit(inputs(object)$loaded==TRUE),])!=0) {
              capture.output(print(inputs(object)[na.omit(inputs(object)$loaded==TRUE),]))
            }
            out[[13]] <- capture.output(cat("\n"))

            ### list stored objects
            out[[14]] <- capture.output(cat(">> Objects stored:\n"))
            out[[15]] <- capture.output(print(ls.str(envir(object))))
            out[[16]] <- capture.output(cat("\n"))

            ### params
            omit <- which(names(params(object))==".progress")

            p <- mapply(
              function(x, y) {
                data.frame(Module=x, Parameter=names(y), Value=I(as.list(y)),
                           stringsAsFactors=FALSE, row.names=NULL)
              },
              x=names(params(object))[-omit],
              y=params(object)[-omit],
              USE.NAMES=TRUE, SIMPLIFY=FALSE
            )
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
  ls(envir(name))
}

#' @export
#' @rdname ls-method
setMethod("ls",
          signature(name="simList"),
          definition=function(name) {
            ls.simList(name)
})

#' @rdname ls-method
objects.simList <- function(name) {
  ls(envir(name))
}

#' @export
#' @rdname ls-method
setMethod("objects",
          signature(name="simList"),
          definition=function(name) {
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
ls.str.simList <- function(name) {
  ls.str(envir(name))
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
                   validObject(x)
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
                   validObject(x)
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
setGeneric("envir", function(object) {
  standardGeneric("envir")
})

#' @rdname simList-accessors-envir
setMethod("envir",
          signature="simList",
          definition=function(object) {
            return(object@.envir)
})

#' @export
#' @rdname simList-accessors-envir
setGeneric("envir<-",
           function(object, value) {
             standardGeneric("envir<-")
})

#' @name envir<-
#' @aliases envir<-,simList-method
#' @rdname simList-accessors-envir
setReplaceMethod("envir",
                 signature="simList",
                 function(object, value) {
                   object@.envir <- value
                   validObject(object)
                   return(object)
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
setGeneric("modules", function(object) {
  standardGeneric("modules")
})

#' @rdname simList-accessors-modules
setMethod("modules",
          signature="simList",
          definition=function(object) {
            return(object@modules)
})

#' @export
#' @rdname simList-accessors-modules
setGeneric("modules<-",
           function(object, value) {
             standardGeneric("modules<-")
})

#' @name modules<-
#' @aliases modules<-,simList-method
#' @rdname simList-accessors-modules
setReplaceMethod("modules",
                 signature="simList",
                 function(object, value) {
                   object@modules <- value
                   validObject(object)
                   return(object)
 })

################################################################################
#' @inheritParams modules
#' @export
#' @include simList-class.R
#' @docType methods
#' @rdname simList-accessors-modules
#'
setGeneric("depends", function(object) {
  standardGeneric("depends")
})

#' @export
#' @rdname simList-accessors-modules
setMethod("depends",
          signature("simList"),
          definition=function(object) {
            return(object@depends)
})

#' @export
#' @rdname simList-accessors-modules
setGeneric("depends<-",
           function(object, value) {
             standardGeneric("depends<-")
})

#' @name depends<-
#' @aliases depends<-,simList-method
#' @rdname simList-accessors-modules
#' @export
setReplaceMethod("depends",
                 signature("simList"),
                 function(object, value) {
                   object@depends <- value
                   validObject(object)
                   return(object)
})


################################################################################
#' Show/set objects referenced in the simulation environment
#'
#' \code{objs<-} requires takes a named list of values to be assigned in
#' the simulation envirment.
#'
#' @inheritParams envir
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
setGeneric("objs", function(object, ...) {
  standardGeneric("objs")
})

#' @export
#' @rdname simList-accessors-envir
setMethod("objs",
          signature="simList",
          definition=function(object, ...) {
            x <- lapply(ls(envir(object), ...), function(x) {
              eval(parse(text=x), envir=envir(object))
            })
            names(x) <- ls(envir(object), ...)
            return(x)
})

#' @export
#' @rdname simList-accessors-envir
setGeneric("objs<-",
           function(object, value) {
             standardGeneric("objs<-")
})

#' @name objs<-
#' @aliases objs<-,simList-method
#' @rdname simList-accessors-envir
#' @export
setReplaceMethod("objs",
                 signature="simList",
                 function(object, value) {
                   if (is.list(value)) {
                     lapply(names(value), function(x) {
                       object@.envir[[x]] <- value[[x]]
                     })
                   } else {
                     stop("must provide a named list.")
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
#'    \code{checkpointFile} \tab \code{.checkpoint} \tab Name of the checkpoint file. (advanced)\cr
#'    \code{checkpointInterval} \tab \code{.checkpoint} \tab The simulation checkpoint interval. (advanced)\cr
#'    \code{modulePath} \tab \code{NA} \tab Global simulation module path. (advanced)\cr
#'    \code{outputPath} \tab \code{NA} \tab Global simulation output path. (advanced)\cr
#'    \code{inputPath} \tab \code{NA} \tab Global simulation input path. (advanced)\cr
#'    \code{paths} \tab \code{NA} \tab Global simulation paths (modules, inputs, outputs). (advanced)\cr
#'    \code{inputs} \tab \code{inputs} \tab list of 2: table showing objects, files etc. & arguments (optional). (advanced)\cr
#'    \code{outputs} \tab \code{outputs} \tab list of 2: table showing objects, files etc. & arguments (optional). (advanced)\cr
#'    \code{progressType} \tab \code{.progress} \tab Type of graphical progress bar used. (advanced)\cr
#'    \code{progressInterval} \tab \code{.progress} \tab Interval for the progress bar. (advanced)\cr
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
setGeneric("checkpointFile", function(object) {
  standardGeneric("checkpointFile")
})

#' @export
#' @rdname simList-accessors-params
setMethod("checkpointFile",
          signature="simList",
          definition=function(object) {
            return(object@params$.checkpoint$file)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("checkpointFile<-",
           function(object, value) {
             standardGeneric("checkpointFile<-")
})

#' @name checkpointFile<-
#' @aliases checkpointFile<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("checkpointFile",
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
setGeneric("checkpointInterval", function(object) {
  standardGeneric("checkpointInterval")
})

#' @export
#' @rdname simList-accessors-params
setMethod("checkpointInterval",
          signature="simList",
          definition=function(object) {
            return(object@params$.checkpoint$interval)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("checkpointInterval<-",
           function(object, value) {
             standardGeneric("checkpointInterval<-")
})

#' @name checkpointInterval<-
#' @aliases checkpointInterval<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("checkpointInterval",
                 signature="simList",
                 function(object, value) {
                   object@params$.checkpoint$interval <- value
                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams params
#' @include simList-class.R
#' @importFrom data.table is.data.table
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("inputs", function(object) {
  standardGeneric("inputs")
})

#' @export
#' @rdname simList-accessors-params
setMethod("inputs",
          signature="simList",
          definition=function(object) {
            return(object@inputs)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("inputs<-",
           function(object, value) {
             standardGeneric("inputs<-")
})

#' @name inputs<-
#' @aliases inputs<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("inputs",
                 signature="simList",
                 function(object, value) {
                   if(length(value)>0) {
                     if (!is.data.frame(value)) {
                       if(!is.list(value)) {
                         stop("inputs must be a list, data.frame")
                       }
                       # pull out any "arguments" that will be passed to input functions
#                        if(any(stri_detect_fixed(pattern="arg", names(value)))) {
#                          inputArgs(object) <- rep(value$arg, length.out=length(value$files))
#                          value <- value[-pmatch("arg", names(value))]
#                        }
                        value <- data.frame(value, stringsAsFactors=FALSE)
                     }
                     browser()
                     fileTable <- data.frame(file=character(0), fun=character(0),
                                             package=character(0), objectName=character(0),
                                             loadTime=numeric(0), loaded=logical(0))
                     columns <- pmatch(names(fileTable),names(value))
                     setnames(value,old = colnames(value)[na.omit(columns)],
                                    new=colnames(fileTable)[!is.na(columns)])
                     object@inputs <- as.data.frame(bind_rows(list(value, fileTable)))
                     #object@inputs$file <- file.path(inputPath(object),object@inputs$file)

                   } else {
                     object@inputs <- value
                   }

                   # Deal with file names
                   # 3 things: 1. if relative, concatenate inputPath
                   #           2. if absolute, don't use inputPath
                   #           3. concatenate time to file name in all cases
                   # If no filename provided, use the object name
                   object@inputs[is.na(object@inputs$file),"file"] <-
                     paste0(object@inputs$objectName[is.na(object@inputs$file)])
                   # If a filename is provided, determine if it is absolute path, if so,
                   # use that, if not, then append it to inputPath(object)
                   object@inputs[!isAbsolutePath(object@inputs$file), "file"] <-
                     file.path(inputPath(object),
                               object@inputs$file[!isAbsolutePath(object@inputs$file)])

                   if (any(is.na(object@inputs[,"loaded"]))) {

                     if(!all(is.na(object@inputs[,"loadTime"]))) {
                       newTime <- min(object@inputs[is.na(object@inputs$loaded),"loadTime"], na.rm=TRUE)
                       attributes(newTime)$unit <- timeunit(object)
                       object <- scheduleEvent(object, newTime, "load", "inputs")
                     } else {
                       object@inputs[is.na(object@inputs$loadTime),"loadTime"] <- time(object, "seconds")
                       newTime <- min(object@inputs[is.na(object@inputs$loaded),"loadTime"], na.rm=TRUE)
                       attributes(newTime)$unit <- "seconds"
                       object <- scheduleEvent(object, newTime, "load", "inputs")
                       #newTime <- object@inputs[is.na(object@inputs$loaded),"loadTime"]
                       #object <- scheduleEvent(object,time(object, "seconds"), "load", "inputs")
                     }
                   }

                   validObject(object)
                   return(object)
})

################################################################################
#' @inheritParams params
#' @details \code{outputs} accepts a data.frame or data.table, with 6 columns. Currently, only one is required.
#' Columns are \code{file}, \code{fun},
#'
#' @include simList-class.R
#' @export
#' @importFrom data.table data.table ':='
#' @importFrom tools file_path_sans_ext
#' @importFrom tools file_ext
#' @importFrom dplyr inner_join
#' @importFrom R.utils isAbsolutePath
#' @importFrom stats na.omit
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("outputs", function(object) {
  standardGeneric("outputs")
})

#' @export
#' @rdname simList-accessors-params
setMethod("outputs",
          signature="simList",
          definition=function(object) {
            return(object@outputs)
          })

#' @export
#' @rdname simList-accessors-params
setGeneric("outputs<-",
           function(object, value) {
             standardGeneric("outputs<-")
})

#' @name outputs<-
#' @aliases outputs<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod(
  "outputs",
   signature="simList",
   function(object, value) {
   if(length(value)>0) {
       if (!is.data.frame(value)) {
         if(!is.list(value)) {
           stop("outputs must be a list or data.frame")
         }
         value <- data.frame(value, stringsAsFactors=FALSE)
       }

       # create a dummy data.frame with correct columns and
       fileTable <- data.frame(file=character(0), fun=character(0),
                               package=character(0), objectName=character(0),
                               saveTime=numeric(0), saved=logical(0), stringsAsFactors=FALSE)
       columns <- pmatch(names(fileTable),names(value))
       setnames(value, old=colnames(value)[na.omit(columns)],
                new=colnames(fileTable)[!is.na(columns)])
       # Merge
       object@outputs <- as.data.frame(bind_rows(list(value, fileTable)))
       #object@outputs$file <- file.path(outputPath(object),object@outputs$file)

       # coerce any factors to the correct class
       for (col in which(sapply(object@outputs, is.factor))) {
         object@outputs[,col] <- as(object@outputs[[col]], class(fileTable[[col]]))
       }

       # if saveTime not provided, give it end(object)
       object@outputs[is.na(object@outputs$saveTime), "saveTime"] <- end(object, timeunit(object))
       attributes(object@outputs$saveTime)$unit <- timeunit(object)

       # Deal with file names
       # 3 things: 1. if relative, concatenate outputPath
       #           2. if absolute, don't use outputPath
       #           3. concatenate time to file name in all cases
       # If no filename provided, use the object name
       object@outputs[is.na(object@outputs$file),"file"] <-
         paste0(object@outputs$objectName[is.na(object@outputs$file)])
       # If a filename is provided, determine if it is absolute path, if so,
       # use that, if not, then append it to outputPath(object)
       object@outputs[!isAbsolutePath(object@outputs$file), "file"] <-
         file.path(outputPath(object),
                   object@outputs$file[!isAbsolutePath(object@outputs$file)])

       # If there is no function provided, then use saveRDS, from package base
       object@outputs[is.na(object@outputs$fun),"fun"] <- "saveRDS"
       object@outputs[is.na(object@outputs$package),"package"] <- "base"

       # file extension stuff
       fileExts <- .saveFileExtensions()
       fe <- suppressMessages(inner_join(object@outputs, fileExts)$exts)
       wh <- !stri_detect_fixed(str = object@outputs$file,pattern=".") & (nchar(fe)>0)
       object@outputs[wh,"file"] <- paste0(object@outputs[wh,"file"],".",fe[wh])

       # If the file name already has a time unit on it,
       # i.e., passed explicitly by user,
       # then don't postpend again
       txtTimeA <- paste0(attr(object@outputs[,"saveTime"],"unit"))
       txtTimeB <- paddedFloatToChar(object@outputs[,"saveTime"],
                                     ceiling(log10(end(object, timeunit(object))+1)))
       wh <- !stri_detect_fixed(str = object@outputs$file,pattern=txtTimeA)
       object@outputs[wh, "file"] <- paste0(
         file_path_sans_ext(object@outputs[wh, "file"]),
         "_", txtTimeA, txtTimeB[wh],
         ifelse(nchar(file_ext(object@outputs[wh, "file"]))>0,".",""),
         ifelse(!is.null(file_ext(object@outputs[wh, "file"])),
                file_ext(object@outputs[wh, "file"]),"")
       )
     } else {
       object@outputs <- value
     }

#        if(is.null(object@outputs$arg)) {
#          object@outputs$arg <- rep(list(NULL), NROW(value))
#        } else if (NROW(value) != length(object@outputs$arg)){
#          object@outputs$arg <- rep(list(NULL), NROW(value))
#        }


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
setGeneric("inputPath", function(object) {
  standardGeneric("inputPath")
})

#' @export
#' @rdname simList-accessors-params
setMethod("inputPath",
          signature="simList",
          definition=function(object) {
            return(object@paths$inputPath)
          })

#' @export
#' @rdname simList-accessors-params
setGeneric("inputPath<-",
           function(object, value) {
             standardGeneric("inputPath<-")
           })

#' @name inputPath<-
#' @aliases inputPath<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("inputPath",
                 signature="simList",
                 function(object, value) {

                   object@paths$inputPath <- unname(unlist(value))

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
setGeneric("modulePath", function(object) {
  standardGeneric("modulePath")
})

#' @export
#' @rdname simList-accessors-params
setMethod("modulePath",
          signature="simList",
          definition=function(object) {
            return(object@paths$modulePath)
          })

#' @export
#' @rdname simList-accessors-params
setGeneric("modulePath<-",
           function(object, value) {
             standardGeneric("modulePath<-")
           })

#' @name modulePath<-
#' @aliases modulePath<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("modulePath",
                 signature="simList",
                 function(object, value) {

                   object@paths$modulePath <- unname(unlist(value))

                   validObject(object)
                   return(object)
                 })

################################################################################
#' @inheritParams params
#' @include simList-class.R
#' @importFrom stats na.omit
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("paths", function(object) {
  standardGeneric("paths")
})

#' @export
#' @rdname simList-accessors-params
setMethod("paths",
          signature="simList",
          definition=function(object) {
            return(object@paths)
          })

#' @export
#' @rdname simList-accessors-params
setGeneric("paths<-",
           function(object, value) {
             standardGeneric("paths<-")
           })

#' @name paths<-
#' @aliases paths<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("paths",
                 signature="simList",
                 function(object, value) {

                   # get named elements and their position in value list
                   wh <- pmatch(c("m","i","o"),names(value))

                   # keep named elements, use unnamed in remaining order: module, input, output
                   if(length(na.omit(wh))<length(value)) {
                     wh1 <- !(wh[1:length(value)] %in% (1:3)[1:length(value)])
                     wh2 <- !((1:3)[1:length(value)] %in% wh[1:length(value)])
                     if(length(wh1)<3) wh1 <- c(wh1, rep(FALSE, 3-length(wh1)))
                     if(length(wh2)<3) wh2 <- c(wh2, rep(FALSE, 3-length(wh2)))
                     wh[wh1] <- (1:3)[wh2]
                   }

                   object@paths[!is.na(wh)] <- value[na.omit(wh)]
                   object@paths[is.na(wh)] <- lapply(object@paths[is.na(wh)], function(x) getwd())

                   names(object@paths) <- c("modulePath", "inputPath", "outputPath")
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
setGeneric("inputArgs", function(object) {
  standardGeneric("inputArgs")
})

#' @export
#' @rdname simList-accessors-params
setMethod("inputArgs",
          signature="simList",
          definition=function(object) {
            return(object@inputs$args)
          })

#' @export
#' @rdname simList-accessors-params
setGeneric("inputArgs<-",
           function(object, value) {
             standardGeneric("inputArgs<-")
           })

#' @name inputArgs<-
#' @aliases inputArgs<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("inputArgs",
                 signature="simList",
                 function(object, value) {

                   if(is.list(value) & !is.data.frame(value)) {
                     object@inputs$args <- value
                   } else if (is.null(value)) {
                     object@inputs$args <- rep(list(NULL), NROW(inputs(object)))
                   } else {
                     stop("value passed to inputArgs() must be a list of named elements")
                   }

                   validObject(object)
                   return(object)
                 })

#' @inheritParams params
#' @include simList-class.R
#' @export
#' @docType methods
#' @rdname simList-accessors-params
#'
setGeneric("outputArgs", function(object) {
  standardGeneric("outputArgs")
})

#' @export
#' @rdname simList-accessors-params
setMethod("outputArgs",
          signature="simList",
          definition=function(object) {
            return(object@outputs$arg)
          })

#' @export
#' @rdname simList-accessors-params
setGeneric("outputArgs<-",
           function(object, value) {
             standardGeneric("outputArgs<-")
           })

#' @name outputArgs<-
#' @aliases outputArgs<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("outputArgs",
                 signature="simList",
                 function(object, value) {
                   if(is.list(value) & !is.data.frame(value)) {
                     object@outputs$arg=value
                   } else if (is.null(value)) {
                     object@outputs$arg=rep(list(NULL), NROW(outputs(object)))
                   } else {
                     stop("value passed to outputArgs() must be a list of named elements")
                   }

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
setGeneric("progressType", function(object) {
  standardGeneric("progressType")
})

#' @export
#' @rdname simList-accessors-params
setMethod("progressType",
          signature="simList",
          definition=function(object) {
            return(object@params$.progress$type)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("progressType<-",
           function(object, value) {
             standardGeneric("progressType<-")
})

#' @name progressType<-
#' @aliases progressType<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("progressType",
                 signature="simList",
                 function(object, value) {
                   object@params$.progress$type <- as.character(value)
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
setGeneric("progressInterval", function(object) {
  standardGeneric("progressInterval")
})

#' @export
#' @rdname simList-accessors-params
setMethod("progressInterval",
          signature="simList",
          definition=function(object) {
            return(object@params$.progress$interval)
})

#' @export
#' @rdname simList-accessors-params
setGeneric("progressInterval<-",
           function(object, value) {
             standardGeneric("progressInterval<-")
})

#' @name progressInterval<-
#' @aliases progressInterval<-,simList-method
#' @rdname simList-accessors-params
#' @export
setReplaceMethod("progressInterval",
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
            return(object@paths$outputPath)
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
                   object@paths$outputPath <- unname(unlist(value))
                   validObject(object)
                   return(object)
})

################################################################################
#' Get and set simulation times.
#'
#' Accessor functions for the \code{simtimes} slot of a \code{simList} object
#' and its elements. To maintain modularity, the behavior of these functions depends
#' on where they are used.
#'
#' NOTE: These have default behavior that is based on the calling
#' frame timeunit. When used inside a module, then the time is in the units of the module.
#' If used in an interactive mode, then the time will be in the units of the spades
#' simulation.
#'
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
setGeneric("time<-", function(x, value) {
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
                   x@simtimes$current <- convertTimeunit(value, "second")
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
    # convert time units, if required
    if(is.null(attributes(value)$unit)) {
      attributes(value)$unit <- timeunit(x)
    }
    x@simtimes$stop <- convertTimeunit(value, "second")
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
                   # convert time units, if required
                   if(is.null(attributes(value)$unit)) {
                     attributes(value)$unit <- timeunit(x)
                   }
                   x@simtimes$start <- convertTimeunit(value, "second")
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
setGeneric(".callingFrameTimeunit", function(x) {
  standardGeneric(".callingFrameTimeunit")
})

#' @export
#' @docType methods
#' @rdname simList-accessors-times
setMethod(
  ".callingFrameTimeunit",
  signature=c("simList"),
  definition=function(x) {
    mod <- .callingModuleName(x)
    out <- if (!is.null(mod)) {
      timeunits(x)[[mod]]
    } else {
      timeunit(x)
    }
    return(out)
})

#' @export
#' @docType methods
#' @rdname simList-accessors-times
#'
setMethod(
  ".callingFrameTimeunit",
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
#' @inheritParams modules
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
    #if (any(str_detect(as.character(sys.call(1)), pattern = "spades"))) {
      st <- str_detect(as.character(sys.calls()), pattern = "moduleCall")
      if (any(st)) {
        mod <- strsplit(
          eval(parse(text="moduleCall"), envir=sys.frame(which(st)[1]-1)),
          split="\\.")[[1]][2]
      } else {
        mod <- NULL
      }
    #} else {
    #  mod <- NULL
    #}
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
#' @param unit   Character. One of the time units used in \code{SpaDES}.
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
#' @importFrom data.table ':='
#' @importFrom dplyr mutate
#' @docType methods
#' @aliases simList-accessors-events
#' @rdname simList-accessors-events
#'
setGeneric("events", function(object, unit) {
  standardGeneric("events")
})

#' @export
#' @rdname simList-accessors-events
setMethod(
  "events",
  signature=c("simList", "character"),
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
          signature=c("simList", "missing"),
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
          signature=c("simList", "character"),
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
          signature=c("simList", "missing"),
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
#' \code{times=list(start=0, stop=52, timeunit="week")}, it will set the
#' units for that simulation.
#' By default, a \code{simInit} call will use the smallest unit contained within
#' the metadata for the modules being used.
#' If \code{NA}, \code{timeunit} defaults to none.
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
            return(x@simtimes$timeunit)
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
    value <- as.character(value)
    if (any(str_detect(.spadesTimes, pattern = value), na.rm=TRUE)) {
      x@simtimes$timeunit <- value
    } else {
      x@simtimes$timeunit <- NA_character_
      if (!is.na(value)) {
        message("unknown timeunit provided: ", value)
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
#' This is different from \code{timeunit} because it is not necessarily
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
    timestepUnits <- lapply(depends(x)@dependencies, function(y) {
      y@timeunit
    })
    names(timestepUnits) <- sapply(depends(x)@dependencies, function(y) {
      y@name
    })
    return(timestepUnits)
})

################################################################################
#' Add simulation dependencies
#'
#' Internal function.
#' Adds a \code{\link{.moduleDeps}} object to the simulation dependency list.
#'
#' @param sim A \code{simList} object.
#'
#' @param x   A named list containing the parameters used to construct a new
#'            \code{\link{.moduleDeps}} object.
#'
#' @return A \code{simList} object.
#'
#' @include simList-class.R
#' @docType methods
#' @rdname addDepends
#'
#' @author Alex Chubaty
#'
setGeneric(".addDepends", function(sim, x) {
  standardGeneric(".addDepends")
})

#' @rdname addDepends
setMethod(".addDepends",
          signature(sim="simList", x=".moduleDeps"),
          definition=function(sim, x) {
            deps <- depends(sim)
            n <- length(deps@dependencies)
            if (n==1L) {
              if (is.null(deps@dependencies[[1L]])) n <- 0L
            }
            deps@dependencies[[n+1L]] <- x
            dupes <- which(duplicated(deps@dependencies))
            if (length(dupes)) deps@dependencies <- deps@dependencies[-dupes]
            depends(sim) <- deps
            return(sim)
})

################################################################################
#' Get simulation package dependencies
#'
#' @param sim A \code{simList} object.
#'
#' @return A sorted character vector of package names.
#'
#' @export
#' @include simList-class.R
#' @docType methods
#' @rdname packages
#'
#' @author Alex Chubaty
#'
# igraph exports %>% from magrittr
setGeneric("packages", function(sim) {
  standardGeneric("packages")
})

#' @export
#' @rdname packages
setMethod("packages",
          signature(sim="simList"),
          definition=function(sim) {
            pkgs <- lapply(depends(sim)@dependencies, function(x) {
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
#' @inheritParams .addDepends
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

    # check that all metadata elements are present
    metadataRequiredNames <- c("name", "description", "keywords", "childModules",
                               "authors", "version", "spatialExtent",
                               "timeframe", "timeunit", "citation", "reqdPkgs",
                               "parameters",
                               "inputObjects", "outputObjects")

    metadataNames <- metadataRequiredNames %in% names(x)
    if(!all(metadataNames)) {
      stop(paste0("The ",x$name," module is missing the metadata for ",
                  metadataRequiredNames[!metadataNames],". Please see ?.moduleDeps ",
              "for more information on which named elements exist. ",
              " Currently, all elements must be present."))
    }

    loadPackages(x$reqdPkgs)

    ## enforce/coerce types for the user-supplied param list
    x$name <- as.character(x$name)
    x$description <- as.character(x$description)
    x$keywords <- as.character(x$keywords)
    x$childModules <- na.omit(x$childModules) %>% as.character

    if (!is(x$authors, "person")) {
      stop("invalid module definition: ", x$name,
           ": authors must be a `person` class.")
      }
    if (is.character(x$version) || is.numeric(x$version)) {
      x$version <- as.numeric_version(x$version)
    }
    if (!is(x$spatialExtent, "Extent")) {
      if (is.na(x$spatialExtent)) {
        x$spatialExtent <- extent(rep(NA_real_, 4))
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
    if (is.na(x$timeunit)) {
      x$timeunit <- NA_character_
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

    ## check that children actually exist locally, and add to list of child modules
    lapply(x$childModules, function(y) {
      if (file.exists(file.path(modulePath(sim), y))) {
        z <- y %>% lapply(., `attributes<-`, list(type="child"))
        modules(sim) <- append_attr(modules(sim), z)
      } else {
        stop("Module ", y, "(a child module of ", x$name, ") not found in modulePath.")
      }
    })

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
                             default=I(list(default)), # substitute removed because it did not allow for a vector, with a c(...)
                             min=I(list(min)), # substitute removed because it did not allow for a vector, with a c(...)
                             max=I(list(max)), # substitute removed because it did not allow for a vector, with a c(...)
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
