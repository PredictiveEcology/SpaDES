
read.in.raw.maps = F
# Useful resource
#http://www.nyu.edu/projects/politicsdatalab/workshops/GISwR.pdf
#library(rbenchmark)
#library(shapefiles)
#library(maptools)
#library(maps)
#library(mapdata)
#library(data.table)
#library(rgeos)
#

######################################################################################
######################################################################################
#   LOAD MPB DATA FROM BC AND AB, BOTH POLYGON AND POINT DATA SOURCES

# To read in raw maps, and save them as individual files
if (read.in.raw.maps)  # This takes about 10 minutes
  source("C:/Eliot/Dropbox/R/import.raw.maps.r")

# Load the precollected R files instead
setwd("c:/Rwork/MPB")
objects2load= c("bc", "ab", "ab.poly", "west", "west.county", "west.r", "bc.poly")
lapply(objects2load,
  function(x) load(file = paste("mpb.",x,".rdata",sep=""),env = globalenv()))


#      # merge alberta and bc points  
#      library(snowfall)
#      sfInit(cpus = 4, parallel = T)
#      sfLibrary(sp)
#      
#      wh.ab = na.omit(pmatch(names(bc), names(ab)))
#      wh.bc = na.omit(pmatch(substr(names(ab),1,4), names(bc)))
#      sfExport("bc", "ab")
#      sfExport("wh.bc","wh.ab")
#      bcab = sfClusterApplyLB(1:length(wh.ab), function(x) {out = merge(bc[[wh.bc[x]]], ab[[wh.ab[x]]],all=T)
#                          coordinates(out) <- ~ coords.x1 + coords.x2
#                          out$ntrees = ifelse(!is.na(out$NUM_TREES), out$NUM_TREES, ifelse(!is.na(out$num_trees),out$num_trees,NA))    
#                          return(out)})
#      sfExport("west.r")
#      bcab = sfClusterApplyLB(bcab, function(x) {proj4string(x) <- proj4string(west.r); return(x)})
#      
#      
#      names(bcab) = names(bc)[wh.bc]#sapply(strsplit(bc.dir.shp,"_"),function(x) x[[3]])[wh.bc]
#      rm(bc,ab)
#      
#      
#      
#      wh.ab.poly = na.omit(pmatch(substr(names(bcab),1,4), names(ab.poly)))
#      wh.bc.poly = na.omit(pmatch(substr(names(bcab),1,4), names(bc.poly)))
#      
#      ab.polygon = ab.poly[wh.ab.poly]
#      bc.polygon = bc.poly[wh.bc.poly]
#      
#      #areas = lapply(lapply(ab.poly.ll,slot,"polygons"),function(x) sapply(x, slot,"area")/1e4)
#      #sapply(areas,range)
#      


# Convert everything to rasters @ 1km resolution.  This resolution decision is determined with the west.r rasterization above
  library(snowfall)
  sfInit(cpus= 6, parallel=T)
  sfLibrary(raster)
  
  
  sfExport("west.r")
  mpb.bc.r = stack(sfClusterApplyLB(bc, function(x) rasterize(x=x, y=west.r, field = x@data$NUM_TREES,fun="sum")))
    names(mpb.bc.r) = names(bc)
#  save(mpb.bc.r, file = "mpb.bc.r.rdata")
#  load(file = "mpb.bc.r.rdata")

  mpb.ab.r = stack(sfClusterApplyLB(ab, function(x) rasterize(x=x, y=west.r, 
    field = if(any(colnames(x@data)=="NUM_TREES")) x@data$NUM_TREES else x@data$num_trees,fun="sum")))
  names(mpb.ab.r) = sapply(names(ab), function(x) strsplit(x,"spot")[[1]])
#  save(mpb.ab.r, file = "mpb.ab.r.rdata")
#  load(file = "mpb.ab.r.rdata")

  # for the next two, I arbitrarity picked 2000 trees per 1km squared. This NEEDS to be revisited.
  mpb.ab.poly.r.stack = stack(sfClusterApplyLB(ab.poly, function(x) rasterize(x=x, y=west.r, field = 2000,fun="last")))
    names(mpb.ab.poly.r.stack) = sapply(names(ab.poly), function(x) strsplit(x,"poly")[[1]])
  # several rasters have no values because they were in southern Alberta and the west.r doesn't cover that. The next line removes NA layers
  nas = which(sapply(1:nlayers(mpb.ab.poly.r.stack), function(x) unique(!is.na(which.min(mpb.ab.poly.r.stack[[x]])))))
  mpb.ab.poly.r = mpb.ab.poly.r.stack[[nas]]
  names(mpb.ab.poly.r) = unlist(strsplit(names(ab.poly),"poly"))[nas]
#  save(mpb.ab.poly.r, file = "mpb.ab.poly.r.rdata")
#  load(file = "mpb.ab.poly.r.rdata")
  
  mpb.bc.poly.r = stack(sfClusterApplyLB(bc.poly, function(x) rasterize(x=x, y=west.r, field = 2000,fun="last")))
  names(mpb.bc.poly.r) = names(bc.poly)
#  save(mpb.bc.poly.r, file = "mpb.bc.poly.r.rdata")
#  load(file = "mpb.ab.poly.r.rdata")
  
mpb.poly.r = mpb.bc.poly.r
mpb.bc.poly.r.us = unstack(mpb.bc.poly.r)
mpb.ab.poly.r.us = unstack(mpb.ab.poly.r)
mpb.poly.r.us = mpb.bc.poly.r.us

wh.poly.bc =  na.omit(match(names(mpb.ab.poly.r),names(mpb.bc.poly.r)))
wh.poly.ab =  na.omit(match(names(mpb.bc.poly.r),names(mpb.ab.poly.r)))
mpb.poly.r.us[wh.poly.bc] = lapply(1:length(wh.poly.ab), function(x) {out = mpb.bc.poly.r.us[[wh.poly.bc[x]]]  + mpb.ab.poly.r.us[[wh.poly.ab[x]]]; return(out)})
mpb.poly.r = stack(mpb.poly.r.us)
plot(mpb.poly.r)






names(mpb.bc.r) = names(bcab)
names(mpb.ab.poly.r) = unlist(strsplit(names(ab.poly),"poly"))
names(mpb.bc.poly.r) = names(bc.poly)


wh.mpb.poly.r = na.omit(pmatch(substr(names(mpb.bc.r),2,5), names(ab.poly)))
wh.mpb.bc.r = na.omit(pmatch(substr(names(ab.poly),1,4), names(bcab)))

mpb.all = list()
inner.count = 0
for (i in 1:nlayers(mpb.bc.r)) {
  if (any(substr(names(mpb.bc.r),2,5)[i] == substr(names(mpb.poly.r),2,5))) {
    inner.count = inner.count + 1
    if (any(is.finite(cellStats(mpb.poly.r[["X2005poly"]],"range")))) {
        mpb.all[[i]] = mpb.bc.r[[wh.mpb.bc.r[inner.count]]] + mpb.poly.r[[wh.mpb.poly.r[inner.count]]]
      } else {
        mpb.all[[i]] = mpb.bc.r[[wh.mpb.bc.r[inner.count]]]
      }
  } else {
    mpb.all[[i]] = mpb.bc.r[[i]]
  }
}


mpb.all = lapply(mpb.all,function(x) {x[is.na(x)]<-0; return(x)})
mpb.all = lapply(mpb.all, function(x) {x[x>0] <- log(x[x>0])+10; return(x) })


#latlongproj = "+

mpb.stk = stack(mpb.all)
mpb.brk = brick(mpb.all)
#mpb.brk = aggregate(mpb.stk,fact = 1)
mpb.brk.ll <- projectRaster(mpb.brk,crs=latlongproj)
mpb.brk.ll = brick(lapply(1:nlayers(mpb.brk.ll), function(x) {mpb.brk.ll[[x]][is.na(mpb.brk.ll[[x]])]<- 0; return(mpb.brk.ll[[x]])}))
mpb.brk.ll@title <- "MPB intensity"

x1 = paste(2001:2011,"-10-01",sep="")
x2 = paste(2002:2012,"-10-01",sep="")
z1 = as.POSIXct(as.Date(x1))
z2 = as.POSIXct(as.Date(x2))

sps = SpatialPointsDataFrame(spTransform(spsample(west,1,type = "random"),latlongproj),
  data = data.frame(dat= 1))
mpb.ts <- new("RasterBrickTimeSeries", variable = "X", sampled = sps,
  rasters = mpb.brk.ll, TimeSpan.begin = z1,
  TimeSpan.end = z2)
dims = dim(mpb.brk.ll)
plotKML(mpb.ts, colour_scale = c(rep("black",2),heat.colors(12)[12:1]),
  pngwidth = dims[1], pngheight = dims[2], pngpointsize = 14)


names(mpb.all) = names(mpb.bc.r)
plot(mpb.all)




wind(2)
par(mfrow = c(3,3))
par(omi = c(0.01, 0.01, 0.01, 0.01))
par(mai = c(0, 0, 0.1, 0))
years = names(bcab.ll)
toplot = 2003:2011
wh = match(toplot, years)
west.county.ll = reproject(west.county)
for (x in wh) {
  plot(west.county.ll,border="light grey")
  title(years[x])
#  points(bcab.ll[[years[x]]][,match(c("coords.x1","coords.x2"),names(bcab.ll[[years[x]]]))],pch=".",col="black")
  symbols(x = coordinates(bcab.ll[[years[x]]]),#bcab.ll[[years[x]]][,match(c("coords.x1","coords.x2"),names(bcab.ll[[years[x]]]))],
    circles=bcab.ll[[years[x]]]$ntrees/1e4,col="black",add = T, inches = F)

  if (!is.na(any(pmatch(years[x],names(ab.polygon))))) 
    plot(ab.polygon[[pmatch(years[x],names(ab.polygon))]],add = T, col = x,border = x)
}

wind(4)
toplot = 2010
wh = match(toplot, years)
plot(west.county,border="light grey")
x = wh
title(years[x])
if (!is.na(any(pmatch(years[x],names(ab.polygon))))) 
  plot(ab.polygon[[pmatch(years[x],names(ab.polygon))]],add = T, col = x,border = x)
points(bcab.ll[[years[x]]][,match(c("coords.x1","coords.x2"),names(bcab.ll[[years[x]]]))],pch=".",col="black")

wind(5)
toplot = 2011
wh = match(toplot, years)
plot(west.county,border="light grey")
x = wh
title(years[x])
if (!is.na(any(pmatch(years[x],names(ab.polygon))))) 
  plot(ab.polygon[[pmatch(years[x],names(ab.polygon))]],add = T, col = x,border = x)
symbols(x = bcab.ll[[years[x]]][,match(c("coords.x1","coords.x2"),names(bcab.ll[[years[x]]]))],
  circles=bcab.ll.ntrees[[years[x]]]$ntrees,col="black",add = T)






legend("topright",legend=toplot, col = wh,pch=19,xpd=F)


  sapply(wh, function(x) points(bcab.ll[[years[x]]][,match(c("coords.x1","coords.x2"),names(bcab.ll[[years[x]]]))],pch=".",col=x))

points(bcab.ll[["2011"]][,1:2],pch=".",col="red")
points(bcab.ll[["2010"]][,1:2],pch=".",col="green")



lapply(1:length(ab.ll), function(x) plot(ab.ll[[x]], add=T,pch=".",col=x))
plot(mpb2011,add = T,pch = ".",col="red")
plot(usmpb2011,add =T, pch = ".",col="red")
plot(abmpb2011,add =T, pch = ".",col="red")



###################################################################################
###################################################################################
###################################################################################
###################################################################################
al = AgentLocation(Which(west.r==2) )



al = AgentLocation(ab.poly[[17]])
pri = ProbInit(map = ab.poly[[17]],p = sapply(ab.poly[[17]]@polygons, function(x) x@area) )
na = NumAgents(1e3)                                                    

mpb = new("agent", agentlocation=al, numagents = na, probinit = pri)


al = AgentLocation(mpb.bc.r[[5]])    
pri = ProbInit(map = ab.poly[[11]],p = 1, function(x) x@area) 
na = NumAgents(1e4)

mpb = new("agent", agentlocation=al, numagents = na)#, numagents = na) #probinit = pri, 


transitions()





plot(west)
points(bcab[["2011"]][,1:2],add = T, pch=".")

points(mpb,pch=".")


plot(west)
#ext = drawExtent()
ext = extent(x= -937658, xmax = 320751.9 , ymin = 5108872 , ymax = 6163350 )

west.empty = raster(ext)
res(west.empty) <- 1000
west.r = rasterize(west,west.empty)

plot(west.r)

west.boreal = crop(boreal,extent(west.r))
plot(boreal[boreal$HA>1e6 & boreal@data$TYPE=="BOREAL",], col = boreal@data$TYPE)



# Sparse raster
library(Matrix)

ras = Which(west.r>1)
#ras[sample(1:prod(dim(ras)[1:2]),1000,replace=T)] = sample(1:200,1000,replace=T)


ras.spm = rasterAsSparse(ras)
ras2 = rasterFromSparse(ras.spm,ras)

extract(ras


cellStats(ras2 != ras,"sum")





rasterAsSparse = function(ras) {
  ras.m = rowColFromCell(cell=Which(ras>=1,cell=T),ras)
  ras.spm = spMatrix(ncol=dim(ras)[2], nrow = dim(ras)[1], i = ras.m[,"row"], j = ras.m[,"col"],x=ras[ras.m])
  return(ras.spm)
}
  
rasterFromSparse = function(sp.ras, ras) {
  return.ras = raster(as.matrix(ras.spm))
  extent(return.ras) = extent(ras)
  crs(return.ras) = crs(ras)
  return(return.ras)
}

setwd("c:/Rwork")

ben = benchmark(replications= 1,
writeRaster(ras,"test.nc",overwrite=T),
writeRaster(ras,"test.grd",overwrite=T),
writeRaster(ras,"test.asc",overwrite=T),
writeRaster(ras,"test.sdat",overwrite=T) ,
writeRaster(ras,"test.img",overwrite=T)   ,
writeRaster(ras,"test1.tif",overwrite=T)   ,
writeRaster(ras,"test.bil",overwrite=T)     ,
writeRaster(ras,"test.envi",overwrite=T),
save(ras,file="test.rdata")     )





rm(ras)
ras1 = raster("test.img")





# Find 1 km scale
#(extent(west.r)@xmax - extent(west.r)@xmin)/1000
#(extent(west.r)@ymax - extent(west.r)@ymin)/1000


#plot(boreal,add = T)
boreal.west = intersect(boreal,west)


boreal = boreal3
plot(boreal[boreal$HA>1e6 & boreal@data$TYPE=="BOREAL",], col = boreal@data$TYPE)
plot(canada1.boreal,add = T)

rasterize(


##################Other

# if you have a data.frame with coordinates as two columns, just use function coordinates() to make it a
#  SpatialPointsDataFrame

data(meuse)
coordinates(meuse) <- c("x","y")
proj4string(meuse) <- CRS("+init=epsg:28992")




setwd("c:/Rwork/MPB/province_BC")
mpb2011.imported = readOGR(dsn=".", layer = "ibm_spot_2011")
mpb2011 = spTransform(mpb2011.imported, CRS(proj4string(boreal)))

setwd("c:/Rwork/MPB/US")
us.mpb2011.imported = readOGR(dsn=".", layer = "us_mpb2011")
usmpb2011 = spTransform(us.mpb2011.imported, CRS(proj4string(boreal)))

