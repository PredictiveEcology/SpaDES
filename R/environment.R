#' The SpaDES environment
#'
#' Environment used internally to store internal package objects and methods.
#'
#' @name .spadesEnv
#' @rdname spadesEnv
#'
.spadesEnv <- new.env(parent=emptyenv())

#' Assign to the global environment
#'
#' Simple wrapper for \code{\link{assign}}.
#'
#' @param x   a variable name, given as a character string.
#'            No coercion is done, and the first element of a character vector
#'            of length greater than one will be used, with a warning.
#'
#' @param value The object to assign. If this is missing, values will be found with
#' \code{get(x)} in the same environment as the calling environment.
#'
#'
#' @param ... Additional arguments to pass to \code{assign}.
#'
#' @export
#' @docType methods
#' @name assignGlobal
#' @rdname assignGlobal
#'
#' @author Alex Chubaty
#'
setGeneric("assignGlobal", function(x, value, ...) {
  standardGeneric("assignGlobal")
})

#' @name assignGlobal
#' @rdname assignGlobal
setMethod("assignGlobal",
          signature(x="character", value="ANY"),
          definition=function(x, value, ...) {
            assign(x, value, envir=.GlobalEnv, ...)
})

#' @name assignGlobal
#' @rdname assignGlobal
setMethod("assignGlobal",
          signature(x="character", value="missing"),
          definition=function(x, value, ...) {
            assign(x, get(x), envir=.GlobalEnv, ...)
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
#' @name existsGlobal
#' @rdname existsGlobal
#'
#' @author Alex Chubaty
#'
setGeneric("existsGlobal", function(x, ...) {
  standardGeneric("existsGlobal")
})

#' @name existsGlobal
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
#' @name getGlobal
#' @rdname getGlobal
#'
#' @author Alex Chubaty
#'
setGeneric("getGlobal", function(x, ...) {
  standardGeneric("getGlobal")
})

#' @name getGlobal
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
#' @name .assignSpaDES
#' @rdname assignSpaDES
#'
#' @author Alex Chubaty
setGeneric(".assignSpaDES", function(x, value, ...) {
  standardGeneric(".assignSpaDES")
})

#' @name .assignSpaDES
#' @rdname assignSpaDES
setMethod(".assignSpaDES",
          signature(x="character", value="ANY"),
          definition=function(x, value, ...) {
            assign(x, value, envir=.spadesEnv, ...)
})

#' @name .assignSpaDES
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
#' @name .existsSpaDES
#' @rdname existsSpaDES
#'
#' @author Alex Chubaty
#'
setGeneric(".existsSpaDES", function(x, ...) {
  standardGeneric(".existsSpaDES")
})

#' @name .existsSpaDES
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

#' @name .getSpaDES
#' @rdname getSpaDES
setMethod(".getSpaDES",
          signature(x="ANY"),
          definition=function(x, ...) {
            get(x, envir=.spadesEnv, ...)
})
