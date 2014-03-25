### define generic methods to load packages as required

# load a single package
setGeneric("load.package", function(package.name, ...) {
    standardGeneric("load.package")
})

setMethod("load.package",
          signature(package.name="character"),
          definition = function(package.name, ...) {
              if (!require(package.name, character.only=TRUE)) {
                  install.packages(package.name)
                  library(package.name, character.only=TRUE)
              }
})

# load a bunch of packages from a list
setGeneric("load.required.pkgs", function(package.list, ...) {
    standardGeneric("load.required.pkgs")
})

setMethod("load.required.pkgs",
          signature(package.list="list"),
          definition = function(package.list, ...) {
              lapply(package.list, load.package)
})


# check whether a module should be reloaded later
setGeneric("reload.module.later", function(depends, ...) {
    standardGeneric("reload.module.later")
})

setMethod("reload.module.later",
           signature(depends="character"),
           definition = function(depends, ...) {
               if (depends=="NONE") {
                   return(FALSE)
               } else {
                   f = all(depends %in% sim.loaded(sim))
                   return(!f)
               }
})

####################################################################################

### specify which packages need to be installed/loaded, and load them;
###  the idea here is that this function can be called from each module
###  to load packages upon initialization of the module
pkgs <- list("CircStats",
             "data.table",
             "geoR",
             "igraph",
             "plotrix",
             "raster",
             "sp")
load.required.pkgs(pkgs)

####################################################################################

### agent class (this is an aspatial agent)
setClass("agent", slots=list(ID="character", other = "list"), prototype=list(ID=NA_character_))

# define methods that extend already-prototyped functions in R
setMethod("initialize",
          signature = "agent",
          definition = function(.Object, numagents=NULL, ...) {
              # init agent IDs as integer increments by default
              #  unless specified by user.
              if (!is.null(numagents)) {
                  .Object@ID = as.character(1:numagents)
              }
              return(.Object)
})

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

setMethod("length",
          signature = "agent",
          definition = function(x) {
              len = length(x@ID)
              return(len)
})

# define our custom methods, which need to be prototyped
setGeneric("setOther", function(object, ...) {
    standardGeneric("setOther")
})

setMethod("setOther",
          signature = "agent",
          definition = function(object, name, value) {
              object@other[[as.character(name)]] = value
              return(object)
})



### rasterAgent class extends agent by making it spatial
setClass("rasterAgent", slots=list(), contains="agent") # need init etc methods




### vectorAgent class extends agent by making it spatial
setClass("vectorAgent", slots=list(), contains="agent") # need init etc methods




### polygonAgent class extends agent by making it spatial
setClass("polygonAgent", slots=list(spatial="SpatialPolygons"), contains="vectorAgent") # need init etc methods




### pointAgent class extends vectorAgent
setClass("pointAgent", slots=list(spatial="SpatialPoints"), contains="vectorAgent")

# define methods that extend already-prototyped functions in R
setMethod("initialize",
          signature = "pointAgent",
          definition = function(.Object, numagents=NULL, ...) {
              # init agent IDs as integer increments by default
              #  unless specified by user;
              # init posistions.
              if (!is.null(numagents)) {
                  .Object@ID = as.character(1:numagents)
                  # temporarily assign random positions
#                  tmp <- SpatialPoints(cbind(x=runif(n=numagents, min=-1000, max=1000),
#                                             y=runif(n=numagents, min=-1000, max=1000)))
#                  .object@spatial = tmp
              }
              return(.Object)
})

setMethod("show",
          signature = "pointAgent",
          definition = function(object) {
              show = list()
              show[["N"]] = paste("There are", length(object@ID), "agents")
              show[["First 5 agent IDs"]] = head(object@ID, 5)
              show[["First 5 agent coordinates"]] = head(coordinates(object@spatial), 5)
              # needs to print `other`
              print(show)
})

setMethod("coordinates",
          signature = "pointAgent",
          definition = function(obj, ...) {
              coords <- coordinates(position(obj), ...)
              return(coords)
})

setMethod("points",
          signature = "pointAgent",
          definition = function(x, which.to.plot=NULL, ...) {
              if (is.null(which.to.plot)) { sam = 1:length(x)} else {sam = which.to.plot}
              points(x@spatial@coords[sam,], ...)
})



# define our custom methods, which need to be prototyped
setGeneric("position", function(obj, ...) {
    standardGeneric("position")
})

setMethod("position",
          signature = "pointAgent",
          definition = function(obj, ...) {
              return(obj@spatial)
})




### spreadAgent class extends pointAgent by not only storing single position but also area
setClass("spreadAgent", slots=list(NumPixels="numeric"),
         prototype=list(NumPixels=NA_integer_), contains="rasterAgent")

# need init, show, etc methods



### mobileAgent class extends pointAgent by allowing movement
setClass("mobileAgent", slots=list(heading="numeric", distance="numeric"),
         prototype=list(heading=NA_real_, distance=NA_real_), contains="pointAgent")

# define methods that extend already-prototyped functions in R
setMethod("initialize", "mobileAgent", function(.Object, agentlocation = NULL, numagents=NULL, probinit=NULL, ...) {
    if (is(agentlocation, "Raster")){
        if (!is.null(probinit)) {
            nonNAs = !is.na(getValues(probinit))
            wh.nonNAs = which(nonNAs)
            ProbInit.v = cumsum(getValues(probinit)[nonNAs])
            if (!is.null(numagents)) {
                ran = runif(numagents,0,1)
                fI = findInterval(ran, ProbInit.v)+1
                fI2 = wh.nonNAs[fI]
                last.ran = runif(numagents,0,1)
                last.fI = findInterval(last.ran, ProbInit.v)+1
                last.fI2 = wh.nonNAs[last.fI]
            } else {
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
                if (length(grep(pattern="Raster",class(agentlocation)))==1) {
                    position = SpatialPoints(sampleRandom(agentlocation, numagents, xy = T, sp = T))
                } else if (length(grep(pattern="SpatialPoints",class(agentlocation)))==1) {
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
    }
    heading1 = runif(numagents, 0, 360) #heading(last.position, position)
    distance = runif(numagents, 0.1, 10) #distance(last.position, position)
#    nas = is.na(heading1)
#    if (sum(nas)>0) heading1[nas] = runif(sum(nas),0,360)
    .Object@ID = as.character(1:numagents)
    .Object@spatial = position
    .Object@heading = heading1
    .Object@distance = distance
    
    return(.Object)
})

setMethod("show",
    signature = "mobileAgent",
    definition = function(object) {
        show = list()
        show[["N"]] = paste("There are",length(object@spatial),"agents")
        show[["First 5 agent coordinates"]] = head(coordinates(object@spatial), 5)
        show[["First 5 agent ids"]] = head(object@ID, 5)
        print(show)
})

setMethod("head",
    signature = "mobileAgent",
    definition = function(x, ...) {
        out = head(data.table(x@spatial), ...)
        print(out)
})

setMethod("points",
          signature = "mobileAgent",
          definition = function(x, which.to.plot=NULL, ...) {
            # identical to definition in `pointAgent`
            #  should be inherited from that class already
              if (is.null(which.to.plot)) { sam = 1:length(x)} else {sam = which.to.plot}
              points(x@spatial@coords[sam,], ...)
})

# define our custom methods, which need to be prototyped

setGeneric("arrow", function(agent, ...) {
    standardGeneric("arrow")
})

setMethod("arrow",
          signature="mobileAgent",
          definition = function(agent, length = 0.1, ...) {
              co.pos = coordinates(position(agent))
              co.lpos = calculate.last.position() #coordinates(agent@last.pos)
              arrows(co.lpos[,"x"], co.lpos[,"y"], co.pos[,"x"], co.pos[,"y"], length = length, ...)
})





#######################################################
###
### the methods below all need to be reworked to:
###     - work on the appropriate agent (sub)class
###     - update the slots as per above
###
#######################################################


### generic methods (lack class-specific methods)
setGeneric("distance", function(from, to, ...) {
    standardGeneric("distance")
})

setMethod("distance",
          signature(from="SpatialPoints", to="SpatialPoints"),
          definition = function(from, to, ...) {
              from = coordinates(from)
              to = coordinates(to)
              out = sqrt((from[,2] - to[,2])^2 + (from[,1] - to[,1])^2)
              return(out)
})



setGeneric("heading", function(from, to, ...) {
    standardGeneric("heading")
})

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
