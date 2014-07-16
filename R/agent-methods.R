#' initialize mobileAgent
#' 
#' @param agentlocation The initial positions of the agents
#'                      (currently only \code{RasterLayer} or
#'                      \code{SpatialPolygonsDataFrame}) accepted.
#' 
#' @param numagents The number of agents to initialize.
#' 
#' @param probinit The probability of placing an agent at a given initial position.
#' 
#' @export
setMethod("initialize", "mobileAgent", function(.Object, ..., agentlocation=NULL, numagents=NULL, probinit=NULL) {
    if (is(agentlocation, "Raster")){
        ext = extent(agentlocation)
        if (!is.null(probinit)) {
#            nonNAs = !is.na(getvalue(probinit))
            nonNAs = !is.na(getValues(probinit))
            wh.nonNAs = which(nonNAs)
#            ProbInit.v = cumsum(getvalue(probinit)[nonNAs])
            ProbInit.v = cumsum(getValues(probinit)[nonNAs])
            if (!is.null(numagents)) {
                ran = runif(numagents,0,1)
                fI = findInterval(ran, ProbInit.v)+1
                fI2 = wh.nonNAs[fI]
                last.ran = runif(numagents,0,1)
                last.fI = findInterval(last.ran, ProbInit.v)+1
                last.fI2 = wh.nonNAs[last.fI]
            } else {
#                va = getvalue(probinit)[nonNAs]
                va = getValues(probinit)[nonNAs]
                ran = runif(length(va), 0, 1)
                fI2 = wh.nonNAs[ran<va]
                
                last.ran = runif(length(fI2), 0, 1)
                last.fI = findInterval(last.ran, ProbInit.v) + 1
                last.fI2 = wh.nonNAs[last.fI]
                
                #                last.ran = runif(length(fI2),0,1)
                #                last.fI2 = wh.nonNAs[last.ran<va]
            }
            if (length(grep(pattern="Raster",class(agentlocation)))==1) {
                position = xyFromCell(agentlocation,fI2,spatial = T)
            } else if (length(grep(pattern="SpatialPoints",class(agentlocation)))==1) {
                position = coordinates(agentlocation)
            } else {
                stop("need raster layer or Spatial Points object")
            }
            numagents = length(position)
        } else {
            # probinit is NULL - start exactly the number of agents as there
            # are pixels in agentlocation
            if (!is.null(numagents)) {
                if (is(agentlocation,"Raster")) {
                    xy=matrix(runif(numagents*2,c(xmin(ext),ymin(ext)),c(xmax(ext),ymax(ext))),ncol=2,byrow=T)
                    colnames(xy)=c("x","y")
                    position = SpatialPoints(xy)
#                    position = SpatialPoints(sampleRandom(agentlocation, numagents, xy = T, sp = T))
                } else if (is(agentlocation,"SpatialPoints")) {
                    sam = sample(1:length(agentlocation),numagents)
                    position = SpatialPoints(agentlocation[sam,])
                } else {
                    stop("need raster layer or Spatial Points object")
                }
            } else { # for numagents also NULL
                if (length(grep(pattern="Raster",class(agentlocation)))==1) {
                    position = SpatialPoints(xyFromCell(agentlocation,Which(agentlocation,cells=T)))
                } else if (length(grep(pattern="SpatialPoints",class(agentlocation)))==1) {
                    position = SpatialPoints(agentlocation)
                } else {
                    stop("need raster layer or Spatial Points object")
                }
                numagents = length(position)
            }
        }
    } else if (is(agentlocation,"SpatialPolygonsDataFrame")) {
        if (!is.null(numagents)) {
            if (!is.null(pri) ) {
                position = SpatialPoints(dotsInPolys(agentlocation,as.integer(round(numagents*pri,0))))
                numagents = length(position)
            } else {stop("with SpatialPolygonsDataFrame, probinit is required")}
        } else {stop("with SpatialPolygonsDataFrame, numagents is required")}
    } else if (is.null(agentlocation)) { stop("Need to provide agentlocation, which can be a map layer")
    }
    heading1 = runif(numagents, 0, 360)
    distance = runif(numagents, 0.1, 10)
    
    .Object@ID = as.character(1:numagents)
    .Object@spatial = position
    .Object@heading = heading1
    .Object@distance = distance
    
    return(.Object)
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
#' @import sp
#' @export
#' @docType methods
#' @rdname heading
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
