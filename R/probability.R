################################################################################
#' Vectorized wrapped normal density function
#'
#'  This is a modified version of \code{\link{dwrpnorm}} found in \code{CircStats}
#'  to allow for multiple angles at once (i.e., vectorized).
#'
#' @inheritParams CircStats::dwrpnorm
#'
#' @export
#' @docType methods
#' @rdname dwrpnorm2
#'
#' @author Eliot McIntire
#' @examples
#' # Values for which to evaluate density
#' theta <- c(1:500)*2*pi/500
#' # Compute wrapped normal density function
#' density <- c(1:500)
#' for(i in 1:500) density[i] <- dwrpnorm2(theta[i], pi, .75)
#' plot(theta, density)
#' # Approximate area under density curve
#' sum(density*2*pi/500)
#'
dwrpnorm2 <- function(theta, mu, rho, sd=1, acc=1e-05, tol=acc) {
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
    Next[keep] <- Last[keep] + term(theta[keep], mu[keep], var, k) +
                  term(theta[keep], mu[keep], var, -k)
    delta[keep] <- abs(Next[keep] - Last[keep])
  }
  return(Next)
}
