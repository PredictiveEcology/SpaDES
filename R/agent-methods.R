### initialize is already defined in the methods package
#' initialize agent
#' 
#' @param numagents The number of agents to initialize.
#' 
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
#' 
#' @param numagents The number of agents to initialize.
#' 
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



#' get the agent ID
#' @export
#' @rdname agent-accessor-methods
setGeneric("agentID", function(object) {
    standardGeneric("agentID")
})

#' get the agent ID
#' @rdname agent-accessor-methods
setMethod("agentID",
          signature = "agent",
          definition = function(object) {
              return(object@ID)
})

#' set the agent ID
#' @export
#' @rdname agent-accessor-methods
setGeneric("agentID<-",
           function(object, value) {
               standardGeneric("agentID<-")
})

#' set the agent ID
#' @name <-
#' @rdname agent-accessor-methods
setReplaceMethod("agentID",
                 signature="agent",
                 function(object, value) {
                     object@ID <- value
                     validObject(object)
                     return(object)
})

#' get other (non-default) attributes of agent
#' @export
#' @rdname agent-accessor-methods
setGeneric("getOther", function(object, name) {
    standardGeneric("getOther")
})

#' get other (non-default) attributes of agent
#' @rdname agent-accessor-methods
setMethod("getOther",
          signature = "agent",
          definition = function(object, name) {
              return(object@other[[as.character(name)]])
})

#' set other (non-default) attributes of agent
#' @export
#' @rdname agent-accessor-methods
setGeneric("setOther", function(object, name, value) {
    standardGeneric("setOther")
})

#' set other (non-default) attributes of agent
#' @rdname agent-accessor-methods
setMethod("setOther",
          signature = "agent",
          definition = function(object, name, value) {
              object@other[[as.character(name)]] = value
              return(object)
})

### length is already defined in the base package
#' length agent (this is really \code{num.agents()})
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
              coords <- coordinates(agentPosition(obj), ...)
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

#' head method for mobileAgent class
#' @export
setMethod("head",
    signature = "mobileAgent",
    definition = function(x, ...) {
        out = head(data.table(x@spatial), ...) # why only show positions???
        print(out)
})

##############################################################
#' Get agent position
#'
#' Currently, only get methods are defined. Set and subset methods are not.
#' 
#' @param object A \code{*Agent} object.
#' 
#' @param value The object to be stored at the slot.
#' 
#' @return Returns or sets the value of the slot from the \code{*Agent} object.
#' 
#' @seealso \code{\link{agent-class}}, \code{\link{polygonAgent-class}}, \code{\link{pointAgent-class}},
#'          \code{\link{spreadAgent-class}}, \code{\link{mobileAgent-class}}
#' 
#' @export
#' @docType methods
#' @rdname agent-accessor-methods
#' 
setGeneric("agentPosition", function(object) {
    standardGeneric("agentPosition")
})

#' get pointAgent positions
#' @rdname agent-accessor-methods
setMethod("agentPosition",
          signature = "pointAgent",
          definition = function(object) {
              return(object@spatial)
})

#' set the agent position
#' @export
#' @rdname agent-accessor-methods
setGeneric("agentPosition<-",
           function(object, value) {
               standardGeneric("agentPosition<-")
           })

#' set the pointAgent position
#' @name <-
#' @rdname agent-accessor-methods
setReplaceMethod("agentPosition",
                 signature="mobileAgent",
                 function(object, value) {
                     object@spatial <- value
                     validObject(object)
                     return(object)
                 })

#' get agent heading
#' @export
#' @rdname agent-accessor-methods
setGeneric("agentHeading", function(object) {
    standardGeneric("agentHeading")
})

#' get mobileAgent heading
#' @rdname agent-accessor-methods
setMethod("agentHeading",
          signature = "mobileAgent",
          definition = function(object) {
              return(object@heading)
})

#' set the agent heading
#' @export
#' @rdname agent-accessor-methods
setGeneric("agentHeading<-",
           function(object, value) {
               standardGeneric("agentHeading<-")
})

#' set the mobileAgent heading
#' @name <-
#' @rdname agent-accessor-methods
setReplaceMethod("agentHeading",
                 signature="mobileAgent",
                 function(object, value) {
                     object@heading <- value
                     validObject(object)
                     return(object)
})


##############################################################
#' plot arrows showing direction of mobileAgent movement
#'
#' Plots arrows showing direction of mobileAgent movement.
#' 
#' @param agent         A \code{mobileAgent} object.
#' 
#' @param ...           Additional plotting parameters.
#'
#' @return Returns the modified \code{SimList} object.
#' 
#' @importMethodsFrom sp coordinates
#' @export
#' @docType methods
#' @rdname arrow-method
#'
# @examples
# NEEDS EXAMPLES
#' 
setGeneric("arrow", function(agent, ...) {
    standardGeneric("arrow")
})

#' plot arrows showing direction of mobileAgent movement
#' 
#' @param length    The length of the arrows to draw (defaults to 0.1).
#' 
#' @rdname arrow-method
setMethod("arrow",
          signature="mobileAgent",
          definition = function(agent, ..., length = 0.1) {
              co.pos = coordinates(agentPosition(agent))
              co.lpos = calculate.last.position()
              arrows(co.lpos[,"x"], co.lpos[,"y"], co.pos[,"x"], co.pos[,"y"], length=length, ...)
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
#' @param ... Additional arguments.
#'
#' @return The heading between the points, in degrees.
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
