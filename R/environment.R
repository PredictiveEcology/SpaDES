#' The SpaDES environment
#'
#' Environment used internally to store internal package objects and methods.
#'
#' @rdname spadesEnv
#'
.spadesEnv <- new.env(parent = emptyenv())

#' Assign to the internal SpaDES environment.
#'
#' Internal function. Simple wrapper for \code{\link{assign}}.
#'
#' @param x     a variable name, given as a character string.
#'              No coercion is done, and the first element of a character vector
#'              of length greater than one will be used, with a warning.
#'
#' @param value The object to assign. If this is missing, values will be found
#'              with \code{get(x)} in the same environment as the calling
#'              environment.
#'
#' @param ... Additional arguments to pass to \code{assign}.
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
          signature(x = "character", value = "ANY"),
          definition = function(x, value, ...) {
            assign(x, value, envir = .spadesEnv, ...)
})

#' @rdname assignSpaDES
setMethod(".assignSpaDES",
          signature(x = "character", value = "missing"),
          definition = function(x, value, ...) {
            assign(x, get(x), envir = .spadesEnv, ...)
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
          signature(x = "ANY"),
          definition = function(x, ...) {
            exists(x, envir = .spadesEnv, ...)
})

#' Get objects from the internal SpaDES environment
#'
#' Internal function. Simple wrapper for \code{\link{get}}.
#'
#' @param x   an object name (given as a character string).
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
          signature(x = "ANY"),
          definition = function(x, ...) {
            get(x, envir = .spadesEnv, ...)
})

#' Copy or move objects from one environment to another
#'
#' This will copy or move (if \code{rmSrc=TRUE}) objects passed as a character
#' string to a different environment. This is used with a \code{spades} call to
#' copy or move objects to the \code{envir} environment object.
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
#' e1 <- new.env()
#' e2 <- new.env()
#' assign("a1", 1:1e3, envir = e1)
#' assign("a2", 1:1e3, envir = e1)
#' objs <- c("a1", "a2")
#' # move objects between environments
#'
#' changeObjEnv(objs, fromEnv = e1, toEnv = e2)
#'
setGeneric("changeObjEnv", function(x, toEnv, fromEnv, rmSrc) {
  standardGeneric("changeObjEnv")
})

#' @rdname changeObjEnv
setMethod(
  "changeObjEnv",
  signature = c("character", "environment", "environment", "logical"),
  definition = function(x, toEnv, fromEnv, rmSrc) {

    lapply(x, function(obj) {
       tryCatch(
         assign(obj, envir = toEnv, value = get(obj, envir = fromEnv)),
         error = function(x) {
           warning(paste("object", obj, "not found and not copied"))
       })
       return(invisible())
    })
    if (rmSrc) rm(list = x, envir = fromEnv)
})

#' @rdname changeObjEnv
setMethod(
  "changeObjEnv",
  signature = c("character", "environment", "missing", "missing"),
  definition = function(x, toEnv) {
    if (is.null(getOption("spades.lowMemory"))) {
      options(spades.lowMemory = FALSE)
    }
    changeObjEnv(x, toEnv, .GlobalEnv, rmSrc = getOption("spades.lowMemory"))
})

#' @rdname changeObjEnv
setMethod(
  "changeObjEnv",
  signature = c("character", "missing", "environment", "missing"),
  definition = function(x, fromEnv) {
    if (is.null(getOption("spades.lowMemory"))) {
      options(spades.lowMemory = FALSE)
    }
    changeObjEnv(x, .GlobalEnv, fromEnv, rmSrc = getOption("spades.lowMemory"))
})

#' @rdname changeObjEnv
setMethod(
  "changeObjEnv",
  signature = c("character", "environment", "missing", "logical"),
  definition = function(x, toEnv, rmSrc) {
    changeObjEnv(x, toEnv, .GlobalEnv, rmSrc)
})

#' @rdname changeObjEnv
setMethod(
  "changeObjEnv",
  signature = c("character", "missing", "environment", "logical"),
  definition = function(x, fromEnv, rmSrc) {
    stop("Must provide a fromEnv")
})

#' @rdname changeObjEnv
setMethod(
  "changeObjEnv",
  signature = c("character", "environment", "environment", "missing"),
  definition = function(x, toEnv, fromEnv) {
    if (is.null(getOption("spades.lowMemory"))) {
      options(spades.lowMemory = FALSE)
    }
    changeObjEnv(x, toEnv, fromEnv, rmSrc = getOption("spades.lowMemory"))
})

#' @rdname changeObjEnv
setMethod(
  "changeObjEnv",
  signature = c("list", "ANY", "ANY", "ANY"),
  definition = function(x, toEnv, fromEnv, rmSrc) {
    list2env(x, envir = toEnv)

})
