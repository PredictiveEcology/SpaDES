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
#' @param value The object to assign.
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

#' Get objects from the global environment
#'
#' Simple wrapper for \code{\link{get}}.
#'
#' @param x   a variable name, given as a character string.
#'            No coercion is done, and the first element of a character vector
#'            of length greater than one will be used, with a warning.
#'
#' @param x   For \code{getGlobal}, an object name (given as a character string).
#'            For \code{mgetGlobal}, a character vector of object names.
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
#' Simple wrapper for \code{\link{assign}}.
#'
#' This is an internal function.
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

#' Get objects from the internal SpaDES environment
#'
#' Simple wrapper for \code{\link{get}}.
#'
#' @param x   For \code{getGlobal}, an object name (given as a character string).
#'            For \code{mgetGlobal}, a character vector of object names.
#'
#' @param ... Additional arguments to pass to \code{get}.
#'
#' @export
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

################################################################################
#' Get the arguments from a previously made function call
#'
#' @param calledFrom   Name of previounly called function.
#'
#' @return List contaning the frame called from and arguments previously passed
#'         to the function \code{calledFrom}.
#'
#' @export
#' @docType methods
#' @rdname prevArgs-method
#'
#' @author Eliot Mcintire and Alex Chubaty
#'
setGeneric("prevArgs", function(calledFrom) {
  standardGeneric("prevArgs")
})

#' @rdname prevArgs-method
#'
setMethod("prevArgs",
          signature(calledFrom="character"),
          definition=function(calledFrom) {
            out <- list(frameCalledFrom=NULL, callArgs=list())
            scalls <- sys.calls()

            # Extract from the sys.calls only the function "calledFrom"
            out$frameCalledFrom <- which(sapply(scalls, function(x) {
              grepl(x, pattern=paste0("^", calledFrom))[1]
            }))
            out$callArgs <- as.list(scalls[out$frameCalledFrom][[1]])[-1]

            return(out)
})
