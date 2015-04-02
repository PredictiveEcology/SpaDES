#' The SpaDES environment
#'
#' needs description
#'
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
#' @rdname assignGlobal-method
#'
#' @author Alex Chubaty
#'
setGeneric("assignGlobal", function(x, value, ...) {
  standardGeneric("assignGlobal")
})

#' @rdname assignGlobal-method
#'
setMethod("assignGlobal",
          signature(x="character", value="ANY"),
          definition=function(x, value, ...) {
            assign(x, value, envir=.GlobalEnv, ...)
})

#' @rdname assignGlobal-method
#'
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
#' @rdname existsGlobal-method
#'
#' @author Alex Chubaty
#'
setGeneric("existsGlobal", function(x, ...) {
  standardGeneric("existsGlobal")
})

#' @rdname existsGlobal-method
#'
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
#' @rdname getGlobal-method
#'
#' @author Alex Chubaty
#'
setGeneric("getGlobal", function(x, ...) {
  standardGeneric("getGlobal")
})

#' @rdname getGlobal-method
#'
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
#' @rdname assignSpaDES-method
#'
#' @author Alex Chubaty
#'
setGeneric("assignSpaDES", function(x, value, ...) {
  standardGeneric("assignSpaDES")
})

#' @rdname assignSpaDES-method
#'
setMethod("assignSpaDES",
          signature(x="character", value="ANY"),
          definition=function(x, value, ...) {
            assign(x, value, envir=.spadesEnv, ...)
})

#' @rdname assignSpaDES-method
#'
setMethod("assignSpaDES",
          signature(x="character", value="missing"),
          definition=function(x, value, ...) {
            assign(x, get(x), envir=.spadesEnv, ...)
})

#' Is an object defined in the .spades environment?
#'
#' Simple wrapper for \code{\link{exists}}.
#'
#' @param x   An object name, given as a character string.
#'            No coercion is done, and the first element of a character vector
#'            of length greater than one will be used, with a warning.
#'
#' @param ... Additional arguments passed to \code{\link{exists}}
#'
#' @docType methods
#' @rdname existsSpaDES-method
#'
#' @author Alex Chubaty
#'
setGeneric("existsSpaDES", function(x, ...) {
  standardGeneric("existsSpaDES")
})

#' @rdname existsSpaDES-method
#'
setMethod("existsSpaDES",
          signature(x="ANY"),
          definition=function(x, ...) {
            exists(x, envir=.spadesEnv, ...)
})

#' Get objects from the internal SpaDES environment
#'
#' Simple wrapper for \code{\link{get}}.
#'
#' @param x   For \code{getGlobal}, an object name (given as a character string).
#'            For \code{mgetGlobal}, a character vector of object names.
#'
#' @param ... Additional arguments to pass to \code{get}.
#'
#' @docType methods
#' @rdname getSpaDES-method
#'
#' @author Alex Chubaty
#'
setGeneric("getSpaDES", function(x, ...) {
  standardGeneric("getSpaDES")
})

#' @rdname getSpaDES-method
#'
setMethod("getSpaDES",
          signature(x="ANY"),
          definition=function(x, ...) {
            get(x, envir=.spadesEnv, ...)
})
