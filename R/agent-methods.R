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
#' @import CircStats sp
#' @include named-objects.R
#' @export
#' @docType methods
#' @rdname heading
#' @author Eliot McIntire
#'
# @examples
# needs examples
setGeneric("heading", function(from, to) {
    standardGeneric("heading")
})

#' @rdname heading
setMethod("heading",
          signature(from="SpatialPoints", to="SpatialPoints"),
          definition = function(from, to) {
            ys <- (to$y - from$y)
            xs <- (to$x - from$x)
            heading = deg(atan((xs) / (ys)))
            ys <- (ys < 0)
            heading[(ys) & (xs) < 0] = heading[(ys) & (xs) < 0] - 180
            heading[(ys) & (xs) > 0] = heading[(ys) & (xs) > 0] + 180
            return(heading%%360)
})

#' @rdname heading
setMethod("heading",
          signature(from="matrix", to="matrix"),
          definition = function(from, to) {
            ys <- (to[,"y"] - from[,"y"])
            xs <- (to[,"x"] - from[,"x"])
            heading = deg(atan((xs) / (ys)))
            ys <- (ys < 0)
            heading[(ys) & (xs) < 0] = heading[(ys) & (xs) < 0] - 180
            heading[(ys) & (xs) > 0] = heading[(ys) & (xs) > 0] + 180
            return(heading%%360)
          })

setMethod("heading",
          signature(from="matrix", to="SpatialPoints"),
          definition = function(from, to) {
            ys <- (to$y - from[,"y"])
            xs <- (to$x - from[,"x"])
            heading = deg(atan((xs) / (ys)))
            ys <- (ys < 0)
            heading[(ys) & (xs) < 0] = heading[(ys) & (xs) < 0] - 180
            heading[(ys) & (xs) > 0] = heading[(ys) & (xs) > 0] + 180
            return(heading%%360)
          })
