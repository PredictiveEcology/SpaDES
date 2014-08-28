#' @exportClass SpatialPointsDataFrameNamed
setClass("SpatialPointsDataFrameNamed",
         slots=list(name="character"),
         prototype=list(name=NA_character_),
         contains="SpatialPointsDataFrame",
         validity=function(object) {
           # check for valid sim times and make default list
           if (is.na(object@name)) {
             stop("name must be provided")
           }
})

#' @export
setGeneric("SpatialPointsDataFrameNamed",
           function(..., name) {
             standardGeneric("SpatialPointsDataFrameNamed")
})

#' @export
setMethod("SpatialPointsDataFrameNamed",
          signature="character",
          definition= function(..., name) {
            obj <- SpatialPointsDataFrame(...)
            name(obj) <- name
            return(obj)
})

#' @export
setMethod("show",
          signature="SpatialPointsDataFrameNamed",
          definition=function(object) {
            out = list()
            out[[1]] = capture.output(print(object,
                                            quote=FALSE, row.names=FALSE))
            out[[2]] = capture.output(cat(paste("name        :", object@name)))

            ### print result
            cat(unlist(out), fill=FALSE, sep="\n")
})

#' @exportClass SpatialPointsNamed
setClass("SpatialPointsNamed",
         slots=list(name="character"),
         prototype=list(name=NA_character_),
         contains="SpatialPoints",
         validity=function(object) {
           # check for valid sim times and make default list
           if (is.na(object@name)) {
             stop("name must be provided")
           }
})

#' @exportClass NamedSpatialPoints
setClassUnion("NamedSpatialPoints", c("SpatialPointsNamed", "SpatialPointsDataFrameNamed"))

#' @export
setGeneric("SpatialPointsNamed",
           #signature=c("..."),
           function(..., name) {
             standardGeneric("SpatialPointsNamed")
})


#' @export
setMethod("SpatialPointsNamed",
          signature="character",
          definition= function(..., name) {
            obj <- SpatialPoints(...)
            name(obj) <- name
            return(obj)
})

#' @export
setMethod("show",
          signature="SpatialPointsNamed",
          definition=function(object) {
            out = list()
            out[[1]] = capture.output(print(object,
                                            quote=FALSE, row.names=FALSE))
            out[[2]] = capture.output(cat(paste("name        :",object@name)))

            ### print result
            cat(unlist(out), fill=FALSE, sep="\n")
})

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
