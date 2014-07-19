##############################################################
#' Vectorized wrapped normal density function
#'
#'  This is a modified version of \code{\link{dwrpnorm}} found in \code{CircStats}
#'  to allow for multiple angles at once (i.e., this wersion is vectorized).
#' 
#' @param theta xxx
#'
#' @param mu xxx
#' 
#' @param rho xxx
#' 
#' @param sd xxx
#' 
#' @param acc xxx
#' 
#' @param tol xxx
#' 
#' @export
#' @docType methods
#' @rdname dwrapnorm2-method
#' 
#' @author Eliot McIntire
#' 
dwrpnorm2 = function (theta, mu, rho, sd=1, acc=1e-05, tol=acc) {
  if (missing(rho)) {
    rho <- exp(-sd^2/2)
  }
  if (rho < 0 | rho > 1)
    stop("rho must be between 0 and 1")
  var <- -2 * log(rho)
  term <- function(theta, mu, var, k) {
    1/sqrt(var * 2 * pi) * exp(-((theta - mu + 2 * pi * k)^2)/(2 * var))
  }
  k <- 0
  Next <- term(theta, mu, var, k)
  Last <- Next
  delta <- rep(1, length(Last))
  while (any(delta > tol)) {
    keep = delta>tol
    k <- k + 1
    Last[keep] <- Next[keep]
    Next[keep] <- Last[keep] + term(theta[keep], mu[keep], var, k)
                  + term(theta[keep], mu[keep], var, -k)
    delta[keep] <- abs(Next[keep] - Last[keep])
  }
  Next
}

##############################################################
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
