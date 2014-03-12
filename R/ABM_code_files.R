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

#setGeneric("agent", function(object) standardGeneric("agent")) # remove?


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




### spatialAgent class extends agent by making it spatial
setClass("spatialAgent", slots=list(position="SpatialPoints"), contains="agent")

# define methods that extend already-prototyped functions in R
setMethod("initialize",
          signature = "spatialAgent",
          definition = function(.Object, numagents=NULL, ...) {
              # init agent IDs as integer increments by default
              #  unless specified by user;
              # init posistions.
              if (!is.null(numagents)) {
                  .Object@ID = as.character(1:numagents)
                  # temporarily assign random positions
                  tmp <- SpatialPoints(cbind(x=runif(n=numagents, min=-1000, max=1000),
                                             y=runif(n=numagents, min=-1000, max=1000)))
                  .Object@position = tmp
              }
              return(.Object)
})

setMethod("show",
          signature = "spatialAgent",
          definition = function(object) {
              show = list()
              show[["N"]] = paste("There are", length(object@ID), "agents")
              show[["First 5 agent IDs"]] = head(object@ID, 5)
              show[["First 5 agent coordinates"]] = head(coordinates(object@position), 5)
              # needs to print `other`
              print(show)
})

setMethod("coordinates",
          signature = "spatialAgent",
          definition = function(obj, ...) {
              coords <- coordinates(position(obj), ...)
              return(coords)
})

setMethod("points",
          signature = "spatialAgent",
          definition = function(x, which.to.plot=NULL, ...) {
              if (is.null(which.to.plot)) { sam = 1:length(x)} else {sam = which.to.plot}
              points(x@position@coords[sam,], ...)
})

#setGeneric("spatialAgent", function(object) standardGeneric("spatialAgent")) # remove?


# define our custom methods, which need to be prototyped
setGeneric("position", function(obj, ...) {
    standardGeneric("position")
})

setMethod("position",
          signature = "spatialAgent",
          definition = function(obj, ...) {
              return(obj@position)
})




### spreadAgent class extends spatialAgent by not only storing single position but also area
setClass("spreadAgent", slots=list(NumPixels="numeric"), prototype=list(NumPixels=NA_integer_), contains="spatialAgent")

# define methods that extend already-prototyped functions in R
setMethod("initialize",
          signature = "spreadAgent",
          definition = function(.Object, numagents=NULL, ...) {
              # init agent IDs as integer increments by default
              #  unless specified by user;
              # init positions;
              # init NumPixels.
              if (!is.null(numagents)) {
                  .Object@ID = as.character(1:numagents)
                  # temporarily assign random positions
                  tmp <- SpatialPoints(cbind(x=runif(n=numagents, min=-1000, max=1000),
                                             y=runif(n=numagents, min=-1000, max=1000)))
                  .Object@position = tmp
                  # temporarily init random NumPixels
                  tmp <- sample(1:10, replace=TRUE)
                  .Object@NumPixels = tmp
              }
              return(.Object)
})

setMethod("show",
          signature = "spreadAgent",
          definition = function(object) {
              show = list()
              show[["N"]] = paste("There are", length(object@ID), "agents")
              show[["First 5 agent IDs"]] = head(object@ID, 5)
              show[["First 5 agent coordinates"]] = head(coordinates(object@position), 5)
              show[["First 5 agent sizes (pixels)"]] = head(object@NumPixels, 5)
              # needs to print `other`
              print(show)
})




### mobileAgent class extends spatialAgent by allowing movement
setClass("mobileAgent", slots=list(heading="numeric", distance="numeric"), prototype=list(heading=NA_real_, distance=NA_real_), contains="spatialAgent")

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
        ran = runif(length(va),0,1)
        fI2 = wh.nonNAs[ran<va]
  
        last.ran = runif(length(fI2),0,1)
        last.fI = findInterval(last.ran, ProbInit.v)+1
        last.fI2 = wh.nonNAs[last.fI]
  
  #      last.ran = runif(length(fI2),0,1)
  #      last.fI2 = wh.nonNAs[last.ran<va]
      }
      if (length(grep(pattern="Raster",class(agentlocation)))==1) {
        position = xyFromCell(agentlocation,fI2,spatial = T)
      } else if (length(grep(pattern="SpatialPoints",class(agentlocation)))==1) {
        position = coordinates(agentlocation)
      } else { stop("need raster layer or Spatial Points object") }
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
          } else { stop("need raster layer or Spatial Points object") }
      } else { # for numagents also NULL
          if (length(grep(pattern="Raster",class(agentlocation)))==1) {
            position = SpatialPoints(xyFromCell(agentlocation,Which(agentlocation,cells=T)))
          } else if (length(grep(pattern="SpatialPoints",class(agentlocation)))==1) {
            position = SpatialPoints(agentlocation)
          } else { stop("need raster layer or Spatial Points object") }
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
  heading1 = runif(numagents, 0, 360)#heading(last.position, position)
  distance = runif(numagents, 0.1, 10)#distance(last.position, position)
#  nas = is.na(heading1)
#  if (sum(nas)>0) heading1[nas] = runif(sum(nas),0,360)
#  
  .Object@ID = as.character(1:numagents)
  .Object@position = position
  .Object@heading = heading1
  .Object@distance = distance

  return(.Object)
})

setMethod("show",
    signature = "mobileAgent",
    definition = function(object) {
        show = list()
        show[["N"]] = paste("There are",length(object@position),"agents")
        show[["First 5 agent coordinates"]] = head(coordinates(object@position), 5)
        show[["First 5 agent ids"]] = head(object@ID, 5)
        print(show)
})

setMethod("head",
    signature = "mobileAgent",
    definition = function(x, ...) {
        out = head(data.table(x@position), ...)
        print(out)
})

setMethod("points",
          signature = "mobileAgent",
          definition = function(x, which.to.plot=NULL, ...) {
            # identical to definition in `spatialAgent`
            #  should be inherited from that class already
              if (is.null(which.to.plot)) { sam = 1:length(x)} else {sam = which.to.plot}
              points(x@position@coords[sam,], ...)
})



# define our custom methods, which need to be prototyped
setGeneric("arrow", function(agent, ...) {
    standardGeneric("arrow")
})

setMethod("arrow",
          signature="mobileAgent",
          definition = function(agent, length = 0.1, ...) {
              co.pos = coordinates(agent@position)
              co.lpos = calculate.last.position() #coordinates(agent@last.pos)
              arrows(co.lpos[,"x"], co.lpos[,"y"], co.pos[,"x"], co.pos[,"y"], length = length, ...)
})

#setGeneric("mobileAgent", function(object) standardGeneric("mobileAgent")) # remove?





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


### functions
ProbInit = function(map, p=NULL, absolute=FALSE) {
  if (length(p) == 1) { 
    ProbInit = raster(extent(map), nrows=nrow(map), ncols=ncol(map), crs=crs(map))
    ProbInit = setValues(ProbInit, rep(p,length(ProbInit)))
  } else if (is(p,"RasterLayer")) {
    ProbInit = p/(cellStats(p, sum)*(1-absolute)+1*(absolute))
  } else if (is(map,"SpatialPolygonsDataFrame")) {
    ProbInit = p/sum(p) 
  } else if (is(p,"NULL"))  {
    ProbInit = map/(cellStats(p,sum)*(1-absolute)+1*(absolute))
  } else {
      stop("error initializing probability map: bad inputs") # temp err msg (general)
  }
  return(ProbInit)
}

Transitions = function(p, agent) {
    agent@position@coords[which(p==0),] = NA
    return(agent)
}

NumAgents = function(N) {
    if ((length(N) == 1) && (is.numeric(N))) NumAgents = N
    else stop("N must be a single integer value, not a vector.")
    return(NumAgents)
}

move = function(hypothesis = NULL) {
    if (hypothesis == "TwoDT") move = "TwoDT"
    if (hypotehsis == "crw") move = "crw"
}

AgentLocation = function(map) {
if (length(grep(pattern = "Raster", class(map)))==1) {
    map[map==0] = NA
    } else if (length(grep(pattern = "SpatialPoints", class(map)))==1) {
    map
    } else if (!is.na(pmatch("SpatialPolygon",class(map)))) {
    map
    } else {
        stop("only raster, spatialpoints or spatialPolygon implemented")
    }
    return(map)
}

# This is a modified version found in CircStats to allow for multiple angles at once
dwrpnorm = function (theta, mu, rho, sd = 1, acc = 1e-05, tol = acc) {
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

crw = function(agent, step.len, dir.sd, hab = NULL) {
    n = length(agent)
    rand.dir = rnorm(n, agent@heading, dir.sd)
    rand.dir = ifelse(rand.dir>180, rand.dir-360, ifelse(rand.dir<(-180), 360+rand.dir, rand.dir))
    
    last.position = position(agent)
    
    # these should use `coordinates(agent) <-` or similar set methods
    agent@position@coords[,"y"] = last.position@coords[,"y"] + cos(rad(rand.dir)) * step.len
    agent@position@coords[,"x"] = last.position@coords[,"x"] + sin(rad(rand.dir)) * step.len
    
    agent@heading = heading(last.position, position(agent))
    agent@distance = distance(last.position, position(agent))
    
    return(agent)
}

ring.probs = function(agent, rings, step.len, dir.sd, hab = NULL) {
    if (!is(agent, "agent")) {
        stop("must be an agent class") # checking should be done using S4 signatures
    }
    if (!is(rings, "NextPossiblePosition")) {
        stop("rings must be an NextPossiblePosition class") # checking should be done using S4 signatures
    }
    n = length(agent)
    
    dt1 = data.table(data.frame(agent@position, ids=agent@ID, heading.rad=rad(agent@heading)))
    setkey(dt1, ids)
    setkey(rings, ids)
    fromto = rings[dt1]
    
    fromto[, headi:=heading(from=SpatialPoints(cbind(x=fromto$x.1, y=fromto$y.1)),
           to=SpatialPoints(cbind(x=fromto$x,y=fromto$y)))]
    fromto[, ProbTurn:=dwrpnorm(theta=rad(headi), mu=heading.rad, sd=dir.sd/50)] # why 50?
    
    return(fromto)
}
# identifies the xy coordinates of a circle around all live agents
#  key function is draw.circles in the plotrix package

## Results double checked
cir = function(agent, radiuses, raster_world, scale_raster){
    ### identify the pixels ("patches" in NetLogo) that are at
    ###  a buffer distance of the individual location.
    
    # create an index sequence for the number of individuals
    seq_num_ind<-seq_len(length(agent)) 
    
    # n = optimum number of points to create the circle for a given individual;
    #       gross estimation (checked that it seems to be enough so that pixels
    #       extracted are almost always duplicated, which means there is small
    #       chance that we missed some on the circle).
    n.angles<-(ceiling((radiuses/scale_raster)*2*pi)+1)
    
    ### Eliot's code to replace the createCircle of the package PlotRegionHighlighter
    positions = coordinates(agent)
    
    # create individual IDs for the number of points that will be done for their circle
    ids <- rep.int(seq_num_ind, times=n.angles)
    
    # create vector of radius for the number of points that will be done for each individual circle
    rads <- rep.int(radiuses, times=n.angles)
    
    # extract the individuals' current positions
    xs <- rep.int(positions[,1], times=n.angles)
    ys <- rep.int(positions[,2], times=n.angles)
    
    # to be used below to do calculation for angle increments
    nvs <- rep.int(c(0,n.angles[-length(n.angles)]), times=n.angles)
    
    # calculate the angle increment that each individual needs to do to complete a circle (2 pi)
    angle.inc <- rep.int(2*pi, length(n.angles)) / n.angles
    
    # repeat this angle increment the number of times it needs to be done to complete the circles
    angs<-rep.int(angle.inc,times=n.angles)
    
    ### Eliot' added's code:
    a1 = Sys.time()
    dt1 = data.table(ids, angs, xs, ys, rads)
    dt1[,angles:=cumsum(angs),by=ids]
    dt1[,x:=cos(angles)*rads+xs]
    dt1[,y:=sin(angles)*rads+ys]
    
    # put the coordinates of the points on the circles from all individuals in the same matrix
    coordinates_all_ind <- dt1[,list(x,y,ids)]    #cbind(x,y)
    
    # extract the pixel IDs under the points
    coordinates_all_ind[,pixels_under_coordinates := cellFromXY(raster_world,coordinates_all_ind)]
    coordinates_all_ind_unique =
        coordinates_all_ind[,list(pixels_under_coordinates = unique(pixels_under_coordinates)), by=ids]
    coordinates_all_ind_unique = na.omit(coordinates_all_ind_unique)
    coordinates_all_ind_unique[,unique_pixels_values := extract(raster_world,pixels_under_coordinates)]
    
    # extract the coordinates for the pixel IDs
    pixels = xyFromCell(raster_world,coordinates_all_ind_unique$pixels_under_coordinates)
    pixels_ind_ids_merged = cbind(coordinates_all_ind_unique,pixels)
    
    # put the coordinates x and y back into a list according to the individual IDs
#    coord_unique_pixels <- split(pixels_ind_ids_merged[,list(x,y)],pixels_ind_ids_merged[,ids])
    
    a2 = Sys.time()
    
    # list of df with x and y coordinates of each unique pixel of the circle of each individual
#    return(coord_unique_pixels) 
    
    # list of df with x and y coordinates of each unique pixel of the circle of each individual
    return(pixels_ind_ids_merged)
}



#cir = function (agent, radiuses, n.angles = 36)
# {
#    n = length(agent)
#    if (length(radiuses)==1) { #if radiuses is a single number
#      radiuses = rep(radiuses,n)
#    }
#
#    # Begin creating vectors of each item, which will have a length of sum(n.angles), i.e.,
#    #    n.angles values for each of the agents
#    ids = rep(1:n,times=n.angles)
#    rads = rep(radiuses,times = n.angles)
#    xs = rep(coordinates(agent)$x, times = n.angles)
#    ys = rep(coordinates(agent)$y, times = n.angles)
#    nvs = rep(c(0,n.angles[-length(n.angles)]), times = n.angles) # To be used below to do calculation for angle increments
#    if (length(n.angles)==1) { # if n.angles is given as a single number of one number for each agent
#      angle.inc <- 2 * pi/n.angles
#    } else {
#      angle.inc <- rep(2 * pi, length(n.angles))/n.angles
#    }
#    angs = rep(angle.inc, times = n.angles)
#
#    #find the angles for each of the n.angles line segments around each agent   
#    dnvs = c(0,diff(ids)) # determine the index that separates two caribous
#    nvs[dnvs==0] = 0 # make all values of the nvs = 0
#    nvs2 = cumsum(nvs)
#    cum = 1:length(ids)
#    index = cum - nvs2 - 1 # This is the series of indices for each angle increment
#    angles = angs * index
#
#    # Calculate the x and y coordinates of the points in the rings
#    x = cos(angles)*rads+xs
#    y = sin(angles)*rads+ys
#
#    coords = data.table(cbind(x,y))
#    est.circles = SpatialPointsDataFrame(coords,data.table(ids,rads))
#    return(est.circles)
#}


spread = function(maps, start.points, r=NULL, id.colour = TRUE, 
    ncells=NULL, fn=expression(!is.na(maps)), backwards = FALSE, ...) {
    
    spread = raster(maps)
    spread[] = NA
    start.cells = sort(cellFromXY(spread,start.points))
    spread[start.cells] = 1
    spread.start.id = raster(maps)
    spread.start.id[] = NA
    spread.start.id[start.cells] = start.cells
    
    for (i in 1:r) {
        spreadadj = adjacent(spread, Which(spread==i, cells=TRUE), sorted=TRUE, ...)
        
        spread[spreadadj[,"to"][eval(fn)[spreadadj[,"to"]] & 
          is.na(spread[spreadadj[,"to"]])]] = i+1
        spread.start.id[spreadadj[,"to"]] = spread.start.id[spreadadj[,"from"]]
    }
    if (id.colour) { return(spread.start.id) }
    else {return(spread)}
}

GaussMap = function(ext, cov.pars = c(5,100), speedup.index = 10) {#, fast = T, n.unique.pixels = 100) {
##   Warning message:
##    In RandomFields::GaussRF(x = xpts, y = ypts, model = get("setRF",  :
##    The function is obsolete. Use 'RFsimulate' instead.
    xmn = ext@xmin
    xmx = ext@xmax
    ymn = ext@ymin
    ymx = ext@ymax
    nr = (xmx-xmn)/speedup.index # ifelse(fast, min(n.unique.pixels,xmx-xmn),xmx-xmn)
    nc = (ymx-ymn)/speedup.index # ifelse(fast, min(ymx-ymn,n.unique.pixels),ymx-ymn)
    xfact = (xmx-xmn)/nr
    yfact = (ymx-ymn)/nc
    map <- raster(nrows=nr, ncols=nc, x=ext)
    
    sim2 <- grf(nr*nc, grid = "reg", cov.pars = cov.pars,
     xlims = c(xmn, xmx), ylims = c(ymn, ymx)) 
    xy <- cbind(x=sim2$coords[,"x"], y=sim2$coords[,"y"])
    map2 <- rasterize(xy, map, field=sim2$data+abs(min(sim2$data)))
    map3 = map2
    if(speedup.index>1) map3 <- disaggregate(map2, c(xfact, yfact))
    return(map3)
}

# To initialize with a specific number per patch, which may come from
#  data or have been derived from patch size. Options include a combination of either
#  a patchid map and a table with 2 columns, pops and num.in.pop,
#  or 2 maps, patchid and patchnumber. Returns a map with a single unique pixel
#  within each patch representing an agent to start. This means that the number
#  of pixels per patch must be greater than the number of agents per patch
spec.num.per.patch = function(patches, num.per.patch.table = NULL, num.per.patch.map=NULL) {
    patchids = as.numeric(na.omit(getValues(patches)))
    wh = Which(patches, cells = T)
    if (!is.null(num.per.patch.table)) {
        dt1 = data.table(wh, pops=patchids)
        setkey(dt1, pops)
        if (is(num.per.patch.table, "data.table")) {
            num.per.patch.table = data.table(num.per.patch.table)
        }
        setkey(num.per.patch.table, pops)
        dt2 = dt1[num.per.patch.table]
    } else if (!is.null(num.per.patch.map)) {
        num.per.patch.table = as.numeric(na.omit(getValues(num.per.patch.map)))
        dt2 = data.table(wh,pops = patchids, num.in.pop = num.per.patch.table)
    } else { stop("need num.per.patch.map or num.per.patch.table") }
    
    resample <- function(x, ...) x[sample.int(length(x), ...)]
    dt3 = dt2[, list(cells=resample(wh, unique(num.in.pop))), by=pops]
    dt3$ids = rownames(dt3)
    
    al = raster(patches)
    al[dt3$cells] = 1
    
    return(al)
}



patch.size = function(patches) {
    patch.size = freq(patches)
    return(patch.size)
}
