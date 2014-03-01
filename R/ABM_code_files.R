require(sp)
require(CircStats)
require(data.table)

### agent class (this is an aspatial agent)
setClass("agent", slots=list(ID="character", other = "list"))

# define methods that extend already-prototyped functions in R
setMethod("initialize",
          signature = "agent",
          definition = function(.Object, numagents=NULL) {
            
})

setMethod("show",
          signature = "agent",
          definition = function(object) {
              
})

setGeneric("agent", function(object) standardGeneric("agent"))







### spatialAgent class extends agent by making it spatial
setClass("spatialAgent", slots=list(position="SpatialPoints"), contains="agent")

# define methods that extend already-prototyped functions in R
setMethod("initialize",
          signature = "spatialAgent",
          definition = function(.Object, numagents=NULL) {
              
          })

setMethod("show",
          signature = "spatialAgent",
          definition = function(object) {
              
})

setMethod("coordinates", signature = "spatialAgent",
          definition = function(obj, ...) {
              obj@position
})

setMethod("length",
          signature="spatialAgent",
          definition = function(x) {
              len = length(x@position)
              return(len)
})



setGeneric("spatialAgent", function(object) standardGeneric("spatialAgent"))


### spreadAgent class extends spatialAgent by not only storing single position but also area
setClass("spreadAgent", slots=list(area="I.DONT.KNOW"), contains="spatialAgent")


# define methods that extend already-prototyped functions in R
setMethod("initialize",
          signature = "agent",
          definition = function(.Object, numagents=NULL) {
              
})


setGeneric("spreadAgent", function(object) standardGeneric("spreadAgent"))


### mobileAgent class extends spatialAgent by allowing movement
setClass("mobileAgent", slots=list(heading="numeric", distance="numeric"), contains="spatialAgent")

setGeneric("agent", function(object) standardGeneric("agent"))

# define methods that extend already-prototyped functions in R
setMethod("initialize",
          signature = "agent",
          definition = function(.Object, numagents=NULL) {
              
 })

setMethod("points",
          signature = "spatialAgent",
          definition = function(x, which.to.plot=NULL, ...) {
              if (is.null(which.to.plot)) { sam = 1:length(x)} else {sam = which.to.plot}
              points(x@position@coords[sam,],...)
})



# define our custom methods, which need to be prototyped
setGeneric("arrow", function(agent,...) {
    standardGeneric("arrow")
})

setMethod("arrow",
          signature="mobileAgent",
          definition = function(agent, length = 0.1, ...) {
              co.pos = coordinates(agent@position)
              co.lpos = calculate.last.position() #coordinates(agent@last.pos)
              arrows(co.lpos[,"x"],co.lpos[,"y"],co.pos[,"x"],co.pos[,"y"],length = length,...)
})

setGeneric("mobileAgent", function(object) standardGeneric("mobileAgent"))





#######################################################
###
### the methods below all need to be reworked to:
###     - work on the appropriate agent (sub)class
###     - update the slots as per above
###
#######################################################
setMethod("initialize",
          signature="mobileAgent",
          definition=function(.Object, agentlocation = NULL, numagents=NULL, probinit=NULL) {
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
                pos = xyFromCell(hab,fI2,spatial = T)
                last.pos = xyFromCell(hab,last.fI2,spatial = T)
                numagents = length(pos)
            
              } else {
                # probinit is NULL - start exactly the number of agents as there
                # are pixels in agentlocation
                if (!is.null(numagents)) {
                    pos = SpatialPoints(sampleRandom(agentlocation, numagents, xy = T, sp = T))
                    last.pos = SpatialPoints(sampleRandom(agentlocation, numagents, xy = T, sp = T))
                } else { # for numagents also NULL
                    pos = SpatialPoints(xyFromCell(agentlocation,Which(agentlocation,cells=T)))
                    last.pos = SpatialPoints(xyFromCell(agentlocation,Which(agentlocation,cells=T)))
                    numagents = length(pos)
                }
              }
            #  heading = deg(atan((pos@coords[,"x"] - last.pos@coords[,"x"]) / (pos@coords[,"y"] - last.pos@coords[,"y"])))
            #    heading = ifelse((pos@coords[,"y"] - last.pos@coords[,"y"])<0,
            #      ifelse((pos@coords[,"x"] - last.pos@coords[,"x"])<0,
            #        heading + 180-360,heading + 180  ),heading) %% 360
            
              no.move = coordinates(pos)==coordinates(last.pos)
              heading1 = heading(last.pos, pos)
              distance = dis(last.pos, pos)
              nas = is.na(heading1)
              if (sum(nas)>0) heading1[nas] = runif(sum(nas),0,360)
                
              ids = 1:numagents
              data = data.table(ids,heading.to.here = heading1,dist.to.here = distance)
              .Object@pos = SpatialPointsDataFrame(coordinates(pos),data)
              data = data.table(ids)
              .Object@last.pos = SpatialPointsDataFrame(coordinates(last.pos),data)
            
              return(.Object)
} )



setMethod("show",
    signature = "agent",
    definition = function(object) {
        show = list()
        show[["N"]] = paste("There are",length(object@pos),"agents")
        show[["First 5 agent coordinates"]] = head(coordinates(object)@coords,5)
        show[["First 5 agent ids"]] = head(object@pos$ids,5)
        print(show)
 })

setMethod("head",
    signature = "agent",
    definition = function(x,...) {
        out = head(data.table(x@pos,last.x = x@last.pos$x, last.y = x@last.pos$y),...)
        print(out)
 })


###########################################################################
###
###     OTHER NON-AGENT CLASS METHODS/FUNCTIONS BELOW:
###
###########################################################################
setGeneric("distance", definition = function(from,to,...) {
    standardGeneric("distance")
})

setMethod("distance",
 signature(from="SpatialPoints",to="SpatialPoints"),
 definition = function(from,to,...) {
   from = coordinates(from)
   to = coordinates(to)
   out = sqrt((from[,"y"] - to[,"y"])^2 + (from[,"x"] - to[,"x"])^2)
   return(out)
 })

setGeneric("heading", function(from,to,...) {
    standardGeneric("heading")
})

setMethod("heading",
 signature(from="SpatialPoints",to="SpatialPoints"),
 definition = function(from,to,...) {
   lpos = coordinates(from)
   pos = coordinates(to)
  heading = deg(atan((pos[,"x"] - lpos[,"x"]) / (pos[,"y"] - lpos[,"y"])))
    heading = ifelse((pos[,"y"] - lpos[,"y"])<0,
      ifelse((pos[,"x"] - lpos[,"x"])<0,
        heading + 180-360,heading + 180  ),heading) %% 360
   return(heading)
 })


 
    

  

 
ProbInit = function(map,p,absolute=F) { #
  if (length(p) == 1) { 
    ProbInit = raster(extent(map),nrows=nrow(map),ncols=ncol(map),crs = crs(map))
    ProbInit = setValues(ProbInit, rep(p,length(ProbInit)))
  }
  else if (class(p) == "RasterLayer") {
    ProbInit = p/(cellStats(p, sum)*(1-absolute)+1*(absolute))
  }
  if(absolute) print(paste("Using Absolute, Expected Number of Agents", 
    sum(na.omit(getValues(ProbInit)))))
  return(ProbInit)
}


Transitions = function(p, agent) {
  agent@pos@coords[which(p==0),]=NA
  return(agent)
}

NumAgents = function(N) {
  if (length(N) == 1)  {NumAgents = N }
  return(NumAgents)
}

move = function(hypothesis = NULL) {
  if (hypothesis == "TwoDT") move = "TwoDT"
  if (hypotehsis == "crw") move = "crw"
}

AgentLocation = function(fn=fun) {
  fn[fn==0] = NA
  return(fn)
}

# This is a modified version found in CircStats to allow for multiple angles at once
dwrpnorm = function (theta, mu, rho, sd = 1, acc = 1e-05, tol = acc)
{
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
    delta <- rep(1,length(Last))
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
  rand.dir = rnorm(n,agent@pos$heading.to.here,dir.sd)
  rand.dir = ifelse(rand.dir>180,rand.dir-360,ifelse(rand.dir<(-180),360+rand.dir,rand.dir))
  rand.len = step.len
  agent@pos@coords[,"y"] = agent@last.pos@coords[,"y"] + cos(rad(rand.dir)) * rand.len
  agent@pos@coords[,"x"] = agent@last.pos@coords[,"x"] + sin(rad(rand.dir)) * rand.len
  agent@pos$heading.to.here = heading(agent@last.pos, agent@pos)
  agent@pos$dist.to.here = dis(agent@last.pos, agent@pos)

  return(agent)
}

ring.probs = function(agent, rings, step.len, dir.sd, hab = NULL) {
  if (class(agent) != "agent") {stop("must be an agent class")}
  n = length(agent@pos)
  agent@last.pos = agent@pos

  dt1 = data.table(data.frame(wolves@pos))
  dt1$heading.rad = rad(dt1$heading.to.here)
  setkey(dt1,ids)
  setkey(rings, ids)
  fromto = rings[dt1]

  fromto[,headi:=heading(from=SpatialPoints(cbind(x=fromto$x.1,y=fromto$y.1)),
           to = SpatialPoints(cbind(x=fromto$x,y=fromto$y)))]
  fromto[,ProbTurn:=dwrpnorm(theta = rad(headi),mu= heading.rad,sd = dir.sd/50)]

#  rand.dir = rnorm(n,agent@pos$heading.to.here,dir.sd)
#  rand.dir = ifelse(rand.dir>180,rand.dir-360,ifelse(rand.dir<(-180),360+rand.dir,rand.dir))
#  rand.len = step.len
#
#  y = agent@last.pos@coords[,"y"] + cos(rad(rand.dir)) * rand.len
#  x = agent@last.pos@coords[,"x"] + sin(rad(rand.dir)) * rand.len
#
#  crw = SpatialPoints(cbind(x,y))
  return(fromto)
}
# identifies the xy coordinates of a circle around all live agents
#  key function is draw.circles in the plotrix package

## Results double checked
cir<-function(positions,radiuses,raster_world,scale_raster){ ##identify the pixels ("patches" in NetLogo) that are at a buffer distance of the individual location

  seq_num_ind<-seq_len(nrow(positions)) ##create an index sequence for the number of individuals
  n.angles<-(ceiling((radiuses/scale_raster)*2*pi)+1) ##n = optimum number of points to create the circle for a given individual
  ##n=gross estimation (checked that it seems to be enough so that pixels extracted are almost always duplicated, which means there is small chances that we missed some on the circle)

  ## Eliot's code to replace the createCircle of the package PlotRegionHighlighter

  ids<-rep.int(seq_num_ind,times=n.angles) ##create individual IDs for the number of points that will be done for their circle
  rads<-rep.int(radiuses,times=n.angles) ##create vector of radius for the number of points that will be done for each individual circle
  xs<-rep.int(positions[,1],times=n.angles) ##extract the individual current position
  ys<-rep.int(positions[,2],times=n.angles)
  nvs<-rep.int(c(0,n.angles[-length(n.angles)]),times=n.angles) ##to be used below to do calculation for angle increments
  angle.inc<-rep.int(2*pi,length(n.angles))/n.angles ##calculate the angle increment that each individual needs to do to complete a circle (2 pi)
  angs<-rep.int(angle.inc,times=n.angles) ##repeat this angle increment the number of times it needs to be done to complete the circles


  # Eliot
  a1 = Sys.time()
  dt1 = data.table(ids,angs,xs,ys,rads)
  dt1[,angles:=cumsum(angs),by=ids]
  dt1[,x:=cos(angles)*rads+xs]
  dt1[,y:=sin(angles)*rads+ys]

  coordinates_all_ind<-dt1[,list(x,y,ids)]#cbind(x,y) ##put the coordinates of the points on the circles from all individuals in the same matrix
  coordinates_all_ind[,pixels_under_coordinates:=cellFromXY(raster_world,coordinates_all_ind)] ##extract the pixel IDs under the points
  coordinates_all_ind_unique=coordinates_all_ind[,list(pixels_under_coordinates=unique(pixels_under_coordinates)),by=ids]
  coordinates_all_ind_unique=na.omit(coordinates_all_ind_unique)
  coordinates_all_ind_unique[,unique_pixels_values:=extract(raster_world,pixels_under_coordinates)]
  pixels=xyFromCell(raster_world,coordinates_all_ind_unique$pixels_under_coordinates) ##extract the coordinates for the pixel IDs
  pixels_ind_ids_merged = cbind(coordinates_all_ind_unique,pixels)
#  coord_unique_pixels<-split(pixels_ind_ids_merged[,list(x,y)],pixels_ind_ids_merged[,ids]) ##put the coordinates x and y back into a list according to the individual IDs
  a2 = Sys.time()


#  return(coord_unique_pixels) ##list of df with x and y coordinates of each unique pixel of the circle of each individual
  return(pixels_ind_ids_merged) ##list of df with x and y coordinates of each unique pixel of the circle of each individual
}
#
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

# determines value of habitat at ring cell values
cir.values = function(agent, rings, hab) {
  id = rings$ids
  ring.hab.val = list()

  coords1 = coordinates(rings) # convert x and y of agent to 1 matrix for faster computation
  ring.hab.val = split(extract(hab,coords1),id)
  return(ring.hab.val)
}


spread = function(maps, start.points, r=NULL, id.colour = T, 
  ncells=NULL, fn=expression(!is.na(maps)), backwards = F, ...) {

  spread = raster(maps)
  spread[] = NA
  start.cells = sort(cellFromXY(spread,start.points))
  spread[start.cells] = 1
  spread.start.id = raster(maps)
  spread.start.id[] = NA
  spread.start.id[start.cells] = start.cells
  
  for (i in 1:r) {
    spreadadj = adjacent(spread,Which(spread==i,cells=T),sorted=T,...)
    
    
    spread[spreadadj[,"to"][eval(fn)[spreadadj[,"to"]] & 
      is.na(spread[spreadadj[,"to"]])]] = i+1
    spread.start.id[spreadadj[,"to"]] = spread.start.id[spreadadj[,"from"]]
  }
  if (id.colour) { return(spread.start.id) }
  else {return(spread)}
}

GaussMap = function(ext, cov.pars = c(5,100), speedup.index = 10){#, fast = T, n.unique.pixels = 100) {
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
  xy <- cbind(x=sim2$coords[,"x"],y=sim2$coords[,"y"])
  map2 <- rasterize(xy, map,field=sim2$data+abs(min(sim2$data)))
  map3 = map2
  if(speedup.index>1) map3 <- disaggregate(map2,c(xfact,yfact))
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
    dt1 = data.table(wh,pops = patchids)
    setkey(dt1,pops)
    if (any(class(num.by.patch.table)!="data.table")) num.by.patch.table = data.table(num.by.patch.table)
    setkey(num.by.patch.table,pops)
    dt2 = dt1[num.per.patch.table]
  } else if (!is.null(num.per.patch.map)) {
    num.per.patch.table = as.numeric(na.omit(getValues(num.per.patch.map)))
    dt2 = data.table(wh,pops = patchids, num.in.pop = num.per.patch.table)
  }

  resample <- function(x, ...) x[sample.int(length(x), ...)]
  dt3 = dt2[,list(cells=resample(wh,unique(num.in.pop))),by=pops]
  dt3$ids = rownames(dt3)

  al = raster(patches)
  al[dt3$cells] = 1

  return(al)
}


patch.size = function(patches) {
  patch.size = freq(patches)
  return(patch.size)
}

