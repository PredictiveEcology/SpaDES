#' The SpaDES environment
#'
#' needs description
#'
#' @rdname spadesEnv
#'
.spadesEnv <- new.env(parent=baseenv())

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
            assign(x, value, envir=.GlobalEnv)
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
            assign(x, value, envir=.spadesEnv)
})
