#' The SpaDES environment
#'
#' Environment used internally to store internal package objects and methods.
#'
#' @rdname spadesEnv
#'
.spadesEnv <- new.env(parent=emptyenv())

#' The \code{simEnv} class
#'
#' This class is simply an environment that contains a \code{simList} object.
#'
#' @aliases simEnv
#' @rdname simEnv-class
#' @exportClass simEnv
#'
#' @author Alex Chubaty
#'
setClass("simEnv", contains = "environment")

### `initialize` generic is already defined in the methods package
#' @export
#' @rdname simEnv-class
setMethod("initialize",
          signature(.Object = "simEnv"),
          definition=function(.Object) {
            .Object$.sim <- new("simList")
            return(.Object)
})

### `show` generic is already defined in the methods package
#' @export
#' @rdname simEnv-class
setMethod("show",
          signature="simEnv",
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
            out[[24]] <- capture.output(print(ls.str(object)))
            out[[25]] <- capture.output(cat("\n"))

            ### print result
            cat(unlist(out), fill=FALSE, sep="\n")
          })




#' Assign to the internal SpaDES environment.
#'
#' Internal function. Simple wrapper for \code{\link{assign}}.
#'
#' @inheritParams assignGlobal
#'
#' @docType methods
#' @rdname assignSpaDES
#'
#' @author Alex Chubaty
setGeneric(".assignSpaDES", function(x, value, ...) {
  standardGeneric(".assignSpaDES")
})

#' @rdname assignSpaDES
setMethod(".assignSpaDES",
          signature(x="character", value="ANY"),
          definition=function(x, value, ...) {
            assign(x, value, envir=.spadesEnv, ...)
})

#' @rdname assignSpaDES
setMethod(".assignSpaDES",
          signature(x="character", value="missing"),
          definition=function(x, value, ...) {
            assign(x, get(x), envir=.spadesEnv, ...)
})

#' Is an object defined in the .spades environment?
#'
#' Internal function. Simple wrapper for \code{\link{exists}}.
#'
#' @param x   An object name, given as a character string.
#'            No coercion is done, and the first element of a character vector
#'            of length greater than one will be used, with a warning.
#'
#' @param ... Additional arguments passed to \code{\link{exists}}
#'
#' @docType methods
#' @rdname existsSpaDES
#'
#' @author Alex Chubaty
#'
setGeneric(".existsSpaDES", function(x, ...) {
  standardGeneric(".existsSpaDES")
})

#' @rdname existsSpaDES
setMethod(".existsSpaDES",
          signature(x="ANY"),
          definition=function(x, ...) {
            exists(x, envir=.spadesEnv, ...)
})

#' Get objects from the internal SpaDES environment
#'
#' Internal function. Simple wrapper for \code{\link{get}}.
#'
#' @param x   For \code{getGlobal}, an object name (given as a character string).
#'            For \code{mgetGlobal}, a character vector of object names.
#'
#' @param ... Additional arguments to pass to \code{get}.
#'
#' @docType methods
#' @name .getSpaDES
#' @rdname getSpaDES
#'
#' @author Alex Chubaty
#'
setGeneric(".getSpaDES", function(x, ...) {
  standardGeneric(".getSpaDES")
})

#' @rdname getSpaDES
setMethod(".getSpaDES",
          signature(x="ANY"),
          definition=function(x, ...) {
            get(x, envir=.spadesEnv, ...)
})


#' Get all simEnv names that exist in an environment
#'
#' Internal function. Get names of simEnv environments. Primarily used within
#' Plot function.
#'
#' @param envir The environment to search for simEnvs within. Default is .GlobalEnv
#'
#' @docType methods
#' @name .getSimEnvNames
#' @rdname getSimEnvNames
#'
#' @author Eliot McIntire
#'
setGeneric(".getSimEnvNames", function(envir){
  standardGeneric(".getSimEnvNames")
})

#' @rdname getSimEnvNames
setMethod(".getSimEnvNames",
          signature = "environment",
          definition = function(envir) {
          ls(envir=envir) %>%
            sapply(., function(x) is(get(x), "simEnv")) %>%
            which %>%
            names %>%
            mget(envir=.GlobalEnv) %>%
            sapply(., function(y) attr(y, "name"))
})

#' @rdname getSimEnvNames
setMethod(".getSimEnvNames",
          signature = "missing",
          definition = function() {
            .getSimEnvNames(.GlobalEnv)
          })


#' Copy or move objects from one environment to another
#'
#' This will copy or move (if \code{rmSrc=TRUE}) objects passed as a character string to a
#' different environment. This is used with a \code{spades} call to copy or move objects to the
#' \code{simEnv} environment object.
#'
#' @param x objects passed as character string vector
#'
#' @param toEnv environment to copy or move to
#'
#' @param fromEnv environment to copy or move from
#'
#' @param rmSrc should the source copies of the objects be removed. Default is FALSE.
#'
#' @docType methods
#' @name changeObjEnv
#' @export
#' @rdname changeObjEnv
#'
#' @author Eliot Mcintire
#'
#' @examples
#'   a1 <- a2 <- a3 <- a4<- 1:1e3
#'   objs <- c("a1", "a2", "a3", "a4")
#'   e <- new.env()
#'
#'   #move objects from .GlobalEnv to e
#'   changeObjEnv(objs, e)
#'
#'   #move objects back to .GlobalEnv from e
#'   changeObjEnv(objs, .GlobalEnv, e)
#'   rm(e)
setGeneric("changeObjEnv", function(x, toEnv, fromEnv, rmSrc){
  standardGeneric("changeObjEnv")
})

#' @rdname changeObjEnv
setMethod("changeObjEnv",
          signature = c("character", "environment", "missing", "missing"),
          definition = function(x, toEnv, fromEnv, rmSrc) {
            changeObjEnv(x, toEnv, .GlobalEnv, FALSE)
})

#' @rdname changeObjEnv
setMethod("changeObjEnv",
          signature = c("character", "missing", "environment", "missing"),
          definition = function(x, toEnv, fromEnv, rmSrc) {
            changeObjEnv(x, .GlobalEnv, fromEnv, FALSE)
          })

#' @rdname changeObjEnv
setMethod("changeObjEnv",
          signature = c("character", "environment", "missing", "logical"),
          definition = function(x, toEnv, fromEnv, rmSrc) {
            changeObjEnv(x, toEnv, .GlobalEnv, rmSrc)
          })

#' @rdname changeObjEnv
setMethod("changeObjEnv",
          signature = c("character", "missing", "environment", "logical"),
          definition = function(x, toEnv, fromEnv, rmSrc) {
            changeObjEnv(x, .GlobalEnv, fromEnv, rmSrc)
          })

#' @rdname changeObjEnv
setMethod("changeObjEnv",
          signature = c("character", "environment", "environment", "missing"),
          definition = function(x, toEnv, fromEnv, rmSrc) {
            changeObjEnv(x, toEnv, fromEnv, rmSrc=FALSE)
          })

#' @rdname changeObjEnv
setMethod("changeObjEnv",
          signature = c("character", "environment", "environment", "logical"),
          definition = function(x, toEnv, fromEnv, rmSrc) {
            lapply(x, function(obj) {tryCatch(assign(`obj`, envir=toEnv,
                                            value=eval(parse(text=obj), envir=fromEnv)),
                                            error=function(x) invisible());
                                     return(invisible())})
            if(rmSrc) suppressWarnings(tryCatch(rm(list=x, envir=fromEnv), error=function(x) invisible()))
          })

