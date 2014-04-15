### initialize is already defined in the methods package
#' initialize agent
#' @export
setMethod("initialize",
          signature = "agent",
          definition = function(.Object, ..., numagents=NULL) {
              # init agent IDs as integer increments by default
              #  can be re-specified by user.
              if (!is.null(numagents)) {
                  .Object@ID = as.character(1:numagents)
              }
              return(.Object)
})

# need init for spreadAgent class

#' initialize pointAgent
#' @export
setMethod("initialize",
          signature = "pointAgent",
          definition = function(.Object, ..., numagents=NULL) {
              # init agent IDs as integer increments by default
              #  unless specified by user;
              # init posistions.
              if (!is.null(numagents)) {
                  .Object@ID = as.character(1:numagents)
              }
              return(.Object)
})

#' initialize mobileAgent
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

### show is already defined in the methods package
#' show agent
#' @export
setMethod("show",
          signature = "agent",
          definition = function(object) {
              show = list()
              show[["N"]] = paste("There are", length(object@ID), "agents.")
              show[["First 5 agent IDs:"]] = head(object@ID, 5)
              if (length(object@other)>0) {
                  show[["Other agent properties:"]] = lapply(object@other, head, n=5)
              } # show other output could be cleaner
              print(show)
})

#' show spreadAgent
#' @export
setMethod("show",
          signature = "spreadAgent",
          definition = function(object) {
              show = list()
              show[["N"]] = paste("There are", length(object@ID), "agents")
              show[["First 5 agent IDs"]] = head(object@ID, 5)
              # show[["First 5 agent coordinates"]] = head(coordinates(object@spatial), 5)
              if (length(object@other)>0) {
                  show[["Other agent properties:"]] = lapply(object@other, head, n=5)
              } # show other output could be cleaner
              print(show)
})

#' show pointAgent
#' @export
setMethod("show",
          signature = "pointAgent",
          definition = function(object) {
              show = list()
              show[["N"]] = paste("There are", length(object@ID), "agents")
              show[["First 5 agent IDs"]] = head(object@ID, 5)
              show[["First 5 agent coordinates"]] = head(coordinates(object@spatial), 5)
              if (length(object@other)>0) {
                  show[["Other agent properties:"]] = lapply(object@other, head, n=5)
              } # show other output could be cleaner
              print(show)
})

#' show mobileAgent
#' @export
setMethod("show",
          signature = "mobileAgent",
          definition = function(object) {
              show = list()
              show[["N"]] = paste("There are",length(object@spatial),"agents")
              show[["First 5 agent coordinates"]] = head(coordinates(object@spatial), 5)
              show[["First 5 agent ids"]] = head(object@ID, 5)
              if (length(object@other)>0) {
                  show[["Other agent properties:"]] = lapply(object@other, head, n=5)
              } # show other output could be cleaner
              print(show)
})




### DEFINE ALL OTHER METHODS FOR ALL CLASSES

# get agent id
setGeneric("agentID", function(object) {
    standardGeneric("agentID")
})

setMethod("agentID",
          signature = "agent",
          definition = function(object) {
              object@ID = value
              return(object)
})

# set agent id
setGeneric("agentID<-",
           function(object, value) {
               standardGeneric("agentID<-")
})

setReplaceMethod("agentID",
                 signature="agent",
                 function(object, value) {
                     object@ID <- value
                     validObject(object)
                     return(object)
})

# get other (non-default) attributes of agent
setGeneric("getOther", function(object, name) {
    standardGeneric("getOther")
})

setMethod("getOther",
          signature = "agent",
          definition = function(object, name) {
              return(object@other[[as.character(name)]])
})

# set other (non-default) attributes of agent
setGeneric("setOther", function(object, name, value) {
    standardGeneric("setOther")
})

setMethod("setOther",
          signature = "agent",
          definition = function(object, name, value) {
              object@other[[as.character(name)]] = value
              return(object)
})

### length is already defined in the base package
#' length agent (this is really num.agents())
#' @export
setMethod("length",
          signature = "agent",
          definition = function(x) {
              len = length(x@ID)
              return(len)
})

### coodinates is already defined in the sp package
#' get coordinates of a pointAgent
#' @importMethodsFrom sp coordinates 
#' @export
setMethod("coordinates",
          signature = "pointAgent",
          definition = function(obj, ...) {
              coords <- coordinates(position(obj), ...)
              return(coords)
})

### points is already defined in the graphics package
#' add agents to plot
#' 
#' @param which.to.plot     Optional subset of agent IDs to plot.
#' 
#' @export
setMethod("points",
          signature = "pointAgent",
          definition = function(x, ..., which.to.plot=NULL) {
              if (is.null(which.to.plot)) { sam = 1:length(x)} else {sam = which.to.plot}
              points(x@spatial@coords[sam,], ...)
})

#' add agents to plot
#' 
#' @param which.to.plot     Optional subset of agent IDs to plot.
#' 
#' @export
setMethod("points",
          signature = "mobileAgent",
          definition = function(x, ..., which.to.plot=NULL) {
              # identical to definition in `pointAgent`
              #  should be inherited from that class already
              if (is.null(which.to.plot)) { sam = 1:length(x)} else {sam = which.to.plot}
              points(x@spatial@coords[sam,], ...)
})

# get agent positions
setGeneric("position", function(obj, ...) {
    standardGeneric("position")
})

setMethod("position",
          signature = "pointAgent",
          definition = function(obj, ...) {
              return(obj@spatial)
})



#' head method for mobileAgent class
#' @export
setMethod("head",
    signature = "mobileAgent",
    definition = function(x, ...) {
        out = head(data.table(x@spatial), ...) # why only show positions???
        print(out)
})


# get mobileAgent heading
setGeneric("agentHeading", function(object) {
    standardGeneric("agentHeading")
})

setMethod("agentHeading",
          signature = "mobileAgent",
          definition = function(object) {
              return(object@heading)
})

# set agent id
setGeneric("agentHeading<-",
           function(object, value) {
               standardGeneric("agentHeading<-")
})

setReplaceMethod("agentHeading",
                 signature="mobileAgent",
                 function(object, value) {
                     object@heading <- value
                     validObject(object)
                     return(object)
})


###
### be sure to @import sp
###
# plot arrows showing direction of mobileAgent movement
setGeneric("arrow", function(agent, ...) {
    standardGeneric("arrow")
})

setMethod("arrow",
          signature="mobileAgent",
          definition = function(agent, length = 0.1, ...) {
              co.pos = coordinates(position(agent))
              co.lpos = calculate.last.position()
              arrows(co.lpos[,"x"], co.lpos[,"y"], co.pos[,"x"], co.pos[,"y"], length = length, ...)
})



#################################################################
### generic methods (lack class-specific methods)

# calculates the distance between spatial points
setGeneric("distance", function(from, to, ...) {
    standardGeneric("distance")
})

setMethod("distance",
          signature("SpatialPoints"),
          definition = function(from, to, ...) {
              if ( (is(from)=="SpatialPoints") && (is(to)=="SpatialPoints") ) {
                  from = coordinates(from)
                  to = coordinates(to)
                  out = sqrt((from[,2] - to[,2])^2 + (from[,1] - to[,1])^2)
                  return(out)
              } else {
                  stop("Error: `from` and `to` must be SpatialPoints objects.")
              }
})

##############################################################
#' Heading between spatial points.
#'
#' Determines the heading between spatial points.
#'
#' @param from The starting position; an object of class SpatiolPoints.
#'
#' @param to The ending position;  an object of class SpatiolPoints.
#'
#' @param ... Additional arguments.
#'
#' @return The heading between the points, in RADIANS or DEGREES?.
#' 
#' #@seealso \code{\link{print}} and \code{\link{cat}}
#' 
#' @importMethodsFrom CircStats deg
#' @importClassesFrom sp SpatialPoints
#' @importMethodsFrom sp coordinates
#' @export
#' @docType methods
#' @rdname heading
#'
# @examples
# needs examples
setGeneric("heading", function(from, to, ...) {
    standardGeneric("heading")
})

#' @rdname heading
setMethod("heading",
          signature(from="SpatialPoints", to="SpatialPoints"),
          definition = function(from, to, ...) {
              lpos = coordinates(from)
              position = coordinates(to)
              heading = deg(atan((position[,1] - lpos[,1]) / (position[,2] - lpos[,2])))
              heading = ifelse((position[,2] - lpos[,2])<0,
                               ifelse((position[,1] - lpos[,1])<0,
                                      heading + 180-360, heading + 180  ),heading) %% 360
              return(heading)
})
