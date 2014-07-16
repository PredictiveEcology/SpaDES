##############################################################
#' Vectorized wrapped normal density function
#'
#'  This is a modified version found in CircStats to allow for multiple angles at once
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
#' @rdname dwrapnorm2
dwrpnorm2 = function (theta, mu, rho, sd = 1, acc = 1e-05, tol = acc) {
  if (missing(rho)) {
    rho <- exp(-sd^2/2)
  }
  if (rho < 0 | rho > 1)
    stop("rho must be between 0 and 1")
  var <- -2 * log(rho)
  term <- function(theta, mu, var, k) {
    1/sqrt(var * 2 * pi) * exp(-((theta - mu + 2 * pi * k)^2)/(2 *
                                                                 var))
  }
  k <- 0
  Next <- term(theta, mu, var, k)
  Last <- Next
  delta <- rep(1, length(Last))
  while (any(delta > tol)) {
    keep = delta>tol
    k <- k + 1
    Last[keep] <- Next[keep]
    Next[keep] <- Last[keep] + term(theta[keep], mu[keep], var, k) + term(theta[keep],
                                                                          mu[keep], var, -k)
    delta[keep] <- abs(Next[keep] - Last[keep])
  }
  Next
}


##############################################################
#' Test for a number between 0 and 1
#'
#' @param x xxx
#' 
#' @export
#' @docType methods
#' @rdname is.prob
is.prob <- function(x) {
  if (!is.numeric(x)) 
    return(FALSE)
  else 
    return(!(x>1 || x<0))
}
