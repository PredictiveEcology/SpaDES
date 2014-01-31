require(sp)
require(CircStats)
require(data.table)

# ABM files

setClass("agent",slots=list(name = "character", pos="SpatialPointsDataFrame",
  last.pos="SpatialPointsDataFrame",other = "list"))#,

setMethod("initialize", "agent", function(.Object, agentlocation = NULL, numagents=NULL, probinit=NULL) {
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
    pos = SpatialPoints(sampleRandom(al, na, xy = T, sp = T))
    last.pos = SpatialPoints(sampleRandom(al, na, xy = T, sp = T))
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

setGeneric("dis", function(from,to,...) {
    standardGeneric("dis")
})

setMethod("dis",
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

setMethod("points",
  signature = "agent",
  definition = function(x,which.to.plot=NULL,...) {
    if (is.null(which.to.plot)) { sam = 1:length(x)} else {sam = which.to.plot}
    points(x@pos@coords[sam,],...)
    })
    

setGeneric("arrow", function(agent,...) {
    standardGeneric("arrow")
})



setMethod("length",
 signature="agent",
 definition = function(x) {
   len = length(x@pos)
   return(len)
 })

setMethod("arrow",
 signature="agent",
 definition = function(agent,length = 0.1, ...) {
   co.pos = coordinates(agent@pos)
   co.lpos = coordinates(agent@last.pos)
   arrows(co.lpos[,"x"],co.lpos[,"y"],co.pos[,"x"],co.pos[,"y"],length = length,...)
 })
 
setMethod("coordinates", signature = "agent",
  definition = function(obj, ...) {
    obj@pos
  })
    

  
setGeneric("agent", function(object) standardGeneric("agent"))
 
ProbInit = function(map,p,absolute=F) { # currently, there is no ability to have Absolute ProbInit
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

crw = function(agent, step.len, dir.sd, hab = NULL) {
  if (class(agent) != "agent") {stop("must be an agent class")}
  n = length(agent@pos)
  agent@last.pos = agent@pos

  rand.dir = rnorm(n,agent@pos$heading.to.here,dir.sd)
  rand.dir = ifelse(rand.dir>180,rand.dir-360,ifelse(rand.dir<(-180),360+rand.dir,rand.dir))
  rand.len = step.len

  agent@pos@coords[,"y"] = agent@last.pos@coords[,"y"] + cos(rad(rand.dir)) * rand.len
  agent@pos@coords[,"x"] = agent@last.pos@coords[,"x"] + sin(rad(rand.dir)) * rand.len


  agent@pos$heading.to.here = heading(agent@last.pos, agent@pos)

  agent@pos$dist.to.here = dis(agent@last.pos, agent@pos)

  return(agent)
}

# identifies the xy coordinates of a circle around all live agents
#  key function is draw.circles in the plotrix package

cir = function (agent, radiuses, n.angles = 36)
 {
    n = length(agent@pos)
    if (length(radiuses)==1) { #if radiuses is a single number
      radiuses = rep(radiuses,n)
    } 

    # Begin creating vectors of each item, which will have a length of sum(n.angles), i.e., 
    #    n.angles values for each of the agents
    ids = rep(1:n,times=n.angles)
    rads = rep(radiuses,times = n.angles)
    xs = rep(coordinates(agent)$x, times = n.angles)
    ys = rep(coordinates(agent)$y, times = n.angles)
    nvs = rep(c(0,n.angles[-length(n.angles)]), times = n.angles) # To be used below to do calculation for angle increments
    if (length(n.angles)==1) { # if n.angles is given as a single number of one number for each agent
      angle.inc <- 2 * pi/n.angles
    } else {
      angle.inc <- rep(2 * pi, length(n.angles))/n.angles
    }
    angs = rep(angle.inc, times = n.angles)

    #find the angles for each of the n.angles line segments around each agent
    dnvs = c(0,diff(ids)) # determine the index that separates two caribous
    nvs[dnvs==0] = 0 # make all values of the nvs = 0 
    nvs2 = cumsum(nvs)
    cum = 1:length(ids)
    index = cum - nvs2 - 1 # This is the series of indices for each angle increment
    angles = angs * index

    # Calculate the x and y coordinates of the points in the rings
    x = cos(angles)*rads+xs
    y = sin(angles)*rads+ys

    coords = data.table(cbind(x,y))
    est.circles = SpatialPointsDataFrame(coords,data.table(ids,rads))
    return(est.circles)
}

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

GaussMap = function(ext, cov.pairs = c(160,160), fast = T, n.unique.pixels = 100) {
  xmn = ext@xmin
  xmx = ext@xmax
  ymn = ext@ymin
  ymx = ext@ymax
  nr = ifelse(fast, min(n.unique.pixels,xmx-xmn),xmx-xmn)
  nc = ifelse(fast, min(ymx-ymn,n.unique.pixels),ymx-ymn)
  xfact = (xmx-xmn)/nr
  yfact = (ymx-ymn)/nc
  map <- raster(nrows=nr, ncols=nc, x=ext)

  sim2 <- grf(nr*nc, grid = "reg", cov.pars = cov.pairs,
     xlims = c(xmn, xmx), ylims = c(ymn, ymx)) 
  xy <- cbind(x=sim2$coords[,"x"],y=sim2$coords[,"y"])
  map2 <- rasterize(xy, map,field=sim2$data+abs(min(sim2$data)))
  map3 = map2
  if(fast) map3 <- disaggregate(map2,c(xfact,yfact))
  return(map3)
}
