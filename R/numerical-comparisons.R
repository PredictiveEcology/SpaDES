##

###############################################################################
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
#' @examples
#' set.seed(100)
#' x <- stats::rnorm(4) # -0.50219235  0.13153117 -0.07891709  0.88678481
#' inRange(x, 0, 1)
#'
inRange <- function(x, a=0, b=1) {
  if (is.null(x)) return(NULL) # is this desired behaviour?
  if (!is.numeric(x)) {
    if (is(x, "Raster")) {
      x <- getValues(x)
    } else {
      stop("x must be numeric.")
    }
  }
  if (!is.numeric(a) || !is.numeric(b)) stop("invalid (non-numeric) bounds.")
  if (is.na(a) || is.na(b)) stop("invalid (NA) bounds.")
  if (a >= b) stop("a cannot be greater than b.")
  return((x - a) * (b - x) >= 0) # NAs will propagate -- is this desired?
}
