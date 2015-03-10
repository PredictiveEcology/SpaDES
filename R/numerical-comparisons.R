################################################################################
#' Tolerance for floating number comparisons
#'
#' See \code{\link{all.equal}} for a good idea on default tolerance values for various classes.
#'
#' @param epsilon The numerical tolerance value (default \code{.Machine$double.eps^0.5}).
#'
#' @return Invoked for its side effect of setting the tolerance value in the package environment.
#'         This value will be used for relational operations defined in this package.
#'
#' @seealso relational-operators
#'
#' @include environment.R
#' @rdname epsilon
#'
setGeneric("setTolerance", function(epsilon) {
  standardGeneric("setTolerance")
})

#' @rdname epsilon
setMethod("setTolerance",
          signature=c(epsilon="missing"),
          definition = function(epsilon) {
            assign(".epsilon", .Machine$double.eps^0.5, envir=.spadesEnv)
            return(invisible())
          }
)

#' @rdname epsilon
setMethod("setTolerance",
          signature=c(epsilon="numeric"),
          definition = function(epsilon) {
            assign(".epsilon", epsilon, envir=.spadesEnv)
            return(invisible())
          }
)

#' @export
#' @rdname epsilon
#'
setGeneric("getTolerance", function(epsilon) {
  standardGeneric("getTolerance")
})

#' @rdname epsilon
#'
setMethod("getTolerance",
          signature=c(epsilon="missing"),
          definition = function(epsilon) {
            return(get(".epsilon", envir=.spadesEnv))
          }
)

################################################################################
#' Relational operators
#'
#' Binary operators which allow the comparison of values in atomic vectors.
#'
#' These are similar to their counterparts in \code{base}, except a tolerance
#' \code{epsilon} can be specified to account for floating point errors.
#'
#' @param x An object for which methods have been written.
#' @param y An object for which methods have been written.
#'
#' @return A logical vector indicating the result of the element by element comparison.
#'         The elements of shorter vectors are recycled as necessary.
#'
#' @seealso epsilon
#'
#' @export
#' @docType methods
#' @rdname relational-operators
#'
#' @author Alex Chubaty
#'
`%>=%` <- function(x, y) { (x + getTolerance() > y) }

#' @export
#' @rdname relational-operators
`%<=%` <- function(x, y) { (x < y + getTolerance()) }

#' @export
#' @rdname relational-operators
`%==%` <- function(x, y) { (abs(x-y) < getTolerance()) }

#' @export
#' @rdname relational-operators
`%!=%` <- function(x, y) { (abs(x-y) > getTolerance()) }

################################################################################
#' Test whether a number lies within range \code{[a,b]}
#'
#' Default values of \code{a=0; b=1} allow for quick test if
#' \code{x} is a probability.
#'
#' @param x   values to be tested
#' @param a   lower bound (default 0)
#' @param b   upper bound (default 1)
#'
#' @export
#' @docType methods
#' @rdname inRange
#'
#' @author Alex Chubaty
#'
inRange <- function(x, a=0, b=1) {
  if (is.null(x)) return(NULL) # is this desired behaviour?
  if (!is.numeric(x)) stop("x must be numeric.")
  if (!is.numeric(a) || !is.numeric(b)) stop("invalid (non-numeric) bounds.")
  if (is.na(a) || is.na(b)) stop("invalid (NA) bounds.")
  if (a>=b) stop("a cannot be greater than b.")
  return((x - a)  *  (b - x) >= 0) # NAs will propagate -- is this desired?
}
