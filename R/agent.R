##############################################################
#' Heading between spatial points.
#'
#' Determines the heading between spatial points.
#'
#' @param from The starting position; an object of class SpatialPoints.
#'
#' @param to The ending position;  an object of class SpatialPoints.
#'
#' @return The heading between the points, in degrees.
#'
#' @importFrom CircStats deg
#' @importFrom sp SpatialPoints
#' @export
#' @docType methods
#' @rdname heading
#' @author Eliot McIntire
#'
#' @examples
#' library(sp)
#' N <- 10L                # number of agents
#' x1 <- stats::runif(N, -50, 50) # previous X location
#' y1 <- stats::runif(N, -50, 50) # previous Y location
#' x0 <- stats::rnorm(N, x1, 5)   # current X location
#' y0 <- stats::rnorm(N, y1, 5)   # current Y location
#'
#' # using SpatialPoints
#' prev <- SpatialPoints(cbind(x = x1, y = y1))
#' curr <- SpatialPoints(cbind(x = x0, y = y0))
#' heading(prev, curr)
#'
#' # using matrix
#' prev <- matrix(c(x1, y1), ncol = 2, dimnames = list(NULL, c("x","y")))
#' curr <- matrix(c(x0, y0), ncol = 2, dimnames = list(NULL, c("x","y")))
#' heading(prev, curr)
#'
#' #using both
#' prev <- SpatialPoints(cbind(x = x1, y = y1))
#' curr <- matrix(c(x0, y0), ncol = 2, dimnames = list(NULL, c("x","y")))
#' heading(prev, curr)
#'
#' prev <- matrix(c(x1, y1), ncol = 2, dimnames = list(NULL, c("x","y")))
#' curr <- SpatialPoints(cbind(x = x0, y = y0))
#' heading(prev, curr)
#'
setGeneric("heading", function(from, to) {
    standardGeneric("heading")
})

#' @export
#' @rdname heading
setMethod("heading",
          signature(from = "SpatialPoints", to = "SpatialPoints"),
          definition = function(from, to) {
            to <- coordinates(to)
            from <- coordinates(from)
            ys <- to[,2] - from[,2]
            xs <- to[,1] - from[,1]
            heading <- deg(atan((xs) / (ys)))
            ys <- (ys < 0)
            heading[(ys) & (xs) < 0] <- heading[(ys) & (xs) < 0] - 180
            heading[(ys) & (xs) > 0] <- heading[(ys) & (xs) > 0] + 180
            return(heading%%360)
})

#' @export
#' @rdname heading
setMethod("heading",
          signature(from = "matrix", to = "matrix"),
          definition = function(from, to) {
            return(heading(SpatialPoints(from), SpatialPoints(to)))
})

#' @export
#' @rdname heading
setMethod("heading",
          signature(from = "matrix", to = "SpatialPoints"),
          definition = function(from, to) {
            return(heading(SpatialPoints(from), to))
})

#' @export
#' @rdname heading
setMethod("heading",
          signature(from = "SpatialPoints", to = "matrix"),
          definition = function(from, to) {
            return(heading(from, SpatialPoints(to)))
})
