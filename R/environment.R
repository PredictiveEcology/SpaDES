#' The SpaDES environment
#'
#' Environment used internally to store internal package objects and methods.
#'
#' @rdname spadesEnv
#'
.spadesEnv <- new.env(parent=emptyenv())

#' Assign to the global environment
#'
#' Simple wrapper for \code{\link{assign}}.
#'
#' If value is omitted, then the function will get a local object with
#' name = \code{x}, via \code{get(x, envir=parent.frame())}
#'
#' @param x   a variable name, given as a character string.
#'            No coercion is done, and the first element of a character vector
#'            of length greater than one will be used, with a warning.
#'
#' @param value The object to assign. If this is missing, values will be found with
#'              \code{get(x)} in the same environment as the calling environment.
#'
#' @param ... Additional arguments to pass to \code{assign}.
#'
#' @export
#' @docType methods
#' @rdname assignGlobal
#'
#' @author Alex Chubaty
#'
setGeneric("assignGlobal", function(x, value, ...) {
  standardGeneric("assignGlobal")
})

#' @rdname assignGlobal
setMethod("assignGlobal",
          signature(x="character", value="ANY"),
          definition=function(x, value, ...) {
            assign(x, value, envir=.GlobalEnv, ...)
})

#' @rdname assignGlobal
setMethod("assignGlobal",
          signature(x="character", value="missing"),
          definition=function(x, value, ...) {
            assign(x, get(x, envir=parent.frame()), envir=.GlobalEnv, ...)
})


#' Is an object defined in the global environment?
#'
#' Simple wrapper for \code{\link{exists}}.
#'
#' @param x   An object name, given as a character string.
#'            No coercion is done, and the first element of a character vector
#'            of length greater than one will be used, with a warning.
#'
#' @param ... Additional arguments passed to \code{\link{exists}}
#'
#' @export
#' @docType methods
#' @rdname existsGlobal
#'
#' @author Alex Chubaty
#'
setGeneric("existsGlobal", function(x, ...) {
  standardGeneric("existsGlobal")
})

#' @rdname existsGlobal
setMethod("existsGlobal",
          signature(x="ANY"),
          definition=function(x, ...) {
            exists(x, envir=.GlobalEnv, ...)
})

#' Get objects from the global environment
#'
#' Simple wrapper for \code{\link{get}}.
#'
#' @param x   An object name, given as a character string.
#'            No coercion is done, and the first element of a character vector
#'            of length greater than one will be used, with a warning.
#'
#' @param ... Additional arguments passed to \code{\link{get}}
#'
#' @export
#' @docType methods
#' @rdname getGlobal
#'
#' @author Alex Chubaty
#'
setGeneric("getGlobal", function(x, ...) {
  standardGeneric("getGlobal")
})

#' @rdname getGlobal
setMethod("getGlobal",
          signature(x="ANY"),
          definition=function(x, ...) {
            get(x, envir=.GlobalEnv, ...)
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
#' @author Eliot Mcintire
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
#' @param x objects either passed as character string vector or list of objects
#'
#' @param toEnv environment to copy or move to
#'
#' @param fromEnv environment to copy or move from
#'
#' @param rmSrc should the source copies of the objects be removed. Default is TRUE to save memory.
#'
#' @docType methods
#' @name changeObjEnv
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
setGeneric("changeObjEnv", function(x, toEnv, fromEnv=.GlobalEnv, rmSrc=TRUE){
  standardGeneric("changeObjEnv")
})

#' @rdname changeObjEnv
setMethod("changeObjEnv",
          signature = c("character","environment", "missing", "missing"),
          definition = function(x, toEnv, fromEnv, rmSrc) {
            changeObjEnv(x, toEnv, .GlobalEnv, rmSrc=TRUE)
})

#' @rdname changeObjEnv
setMethod("changeObjEnv",
          signature = c("character","environment", "missing", "logical"),
          definition = function(x, toEnv, fromEnv, rmSrc) {
            changeObjEnv(x, toEnv, .GlobalEnv, rmSrc)
          })

#' @rdname changeObjEnv
setMethod("changeObjEnv",
          signature = c("character","environment", "environment", "missing"),
          definition = function(x, toEnv, fromEnv=.GlobalEnv, rmSrc=TRUE) {
            lapply(x, function(obj) {assign(obj, envir=toEnv, value=get(obj, envir=fromEnv)); return(invisible())})
            if(rmSrc) rm(list=x, envir=fromEnv)
          })

#' @rdname changeObjEnv
setMethod("changeObjEnv",
          signature = c("character","environment", "environment", "logical"),
          definition = function(x, toEnv, fromEnv, rmSrc) {
            changeObjEnv(x, toEnv, fromEnv, rmSrc)
          })
