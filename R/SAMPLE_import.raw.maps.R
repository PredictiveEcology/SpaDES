

  library(snowfall)
  sfInit(cpus = 3, parallel = T)
  sfLibrary(sp)
  sfLibrary(rgdal)
  #sfLibrary(plotKML)

  setwd("c:/Rwork/MPB/ab_mpb")
  ab.dir.shp = unique(sapply(strsplit(dir(pattern="spot"),"\\."),function(x) x[[1]]))
  ab = sfClusterApplyLB(ab.dir.shp, function(x) {setwd("c:/Rwork/MPB/ab_mpb");readOGR(dsn=".", layer = x)})
  names(ab) = sapply(strsplit(ab.dir.shp,"_"),function(x) x[[3]])

  setwd("c:/Rwork/MPB/ab_mpb")
  ab.poly.dir.shp = unique(sapply(strsplit(dir(pattern="poly"),"\\."),function(x) x[[1]]))
  ab.poly = sfClusterApplyLB(ab.poly.dir.shp, function(x) {setwd("c:/Rwork/MPB/ab_mpb");readOGR(dsn=".", layer = x)})
  names(ab.poly) = sapply(strsplit(ab.poly.dir.shp,"_"),function(x) x[[3]])

  setwd("c:/Rwork/MPB/province_BC")
  bc.dir.shp = unique(sapply(strsplit(dir(pattern="spot"),"\\."),function(x) x[[1]]))
  bc = sfClusterApplyLB(bc.dir.shp, function(x) {setwd("c:/Rwork/MPB/province_BC");readOGR(dsn=".", layer = x)})
  names(bc) = sapply(strsplit(bc.dir.shp,"_"),function(x) x[[3]])

  setwd("c:/Rwork/MPB/province_BC")
  bc.poly.dir.shp = unique(sapply(strsplit(dir(pattern="poly"),"\\."),function(x) x[[1]]))
  bc.poly = sfClusterApplyLB(bc.poly.dir.shp, function(x) {setwd("c:/Rwork/MPB/province_BC");readOGR(dsn=".", layer = x)})
  names(bc.poly) = sapply(strsplit(bc.poly.dir.shp,"_"),function(x) x[[3]])

######################################################################################
######################################################################################
#   LOAD OTHER MAPS, PROVINCE OUTLINES, COUNTY OUTLINES, BOREAL FOREST
    library(raster)
    library(rgdal)
    setwd("C:\\Rwork\\Maps\\boreal")
    boreal <- readOGR(dsn=".", layer="NABoreal")

    setwd("C:\\Rwork\\Maps\\")
    load("CAN_adm1.RData")
    canada1 = gadm
    canada1.boreal = spTransform(canada1, CRS(proj4string(boreal)))
    west = canada1.boreal[na.omit(match(c("Alberta", "British Columbia","Saskatchewan"),canada1.boreal$NAME_1)),]
    rm(gadm,canada1, canada1.boreal)

    load("CAN_adm2.RData")
    canada2 = gadm
    canada2.boreal = spTransform(canada2, CRS(proj4string(boreal)))
    require(data.table)
    canada2.boreal.dt = data.table(data.frame(canada2.boreal))
    setkey(canada2.boreal.dt,"NAME_1")
    west.county = canada2.boreal[match(canada2.boreal.dt[c("Alberta", "British Columbia","Saskatchewan")]$PID,canada2.boreal@data$PID),]
    rm(gadm,canada2, canada2.boreal)
    #plot(west.county)


    ext = extent(x= -1027658, xmax = 320751.9 , ymin = 5108872 , ymax = 6163350 )
    west.empty = raster(ext)
    res(west.empty) <- 1000
    west.r = rasterize(west,west.empty)

########################################################################################
########################################################################################
# reproject bc and ab so in same projection, namely the one used for the boreal dataset

    crs.boreal = CRS(proj4string(boreal))
    sfInit(cpus = 3, parallel = T)
    sfExport("crs.boreal")
      ab.bor = sfClusterApplyLB(ab, spTransform, crs.boreal)
      bc.bor = sfClusterApplyLB(bc, spTransform, crs.boreal)
    sfStop()
    sfInit(cpus = 3, parallel = T)
      bc.poly.bor = sfClusterApplyLB(bc.poly, spTransform, crs.boreal)
    sfStop()
    sfInit(cpus = 3, parallel = T)
      ab.poly.bor = sfClusterApplyLB(ab.poly, spTransform, crs.boreal)
      west.bor = spTransform(west, crs.boreal)
      west.county.bor = spTransform(west.county, crs.boreal)
      west.r.bor = projectRaster(west.r,crs=crs.boreal)
    sfStop()

    rm(bc.poly, bc, ab, ab.poly, west, west.county, west.r)
    names(bc.poly.bor) = sapply(strsplit(bc.poly.dir.shp,"_"),function(x) x[[3]])
    names(ab.poly.bor) = sapply(strsplit(ab.poly.dir.shp,"_"),function(x) x[[3]])
    names(bc.bor) = sapply(strsplit(bc.dir.shp,"_"),function(x) x[[3]])
    names(ab.bor) = sapply(strsplit(ab.dir.shp,"_"),function(x) x[[3]])

# Rename them to simpler names
    bc = bc.bor
    bc.poly=bc.poly.bor
    ab = ab.bor
    ab.poly=ab.poly.bor
    west.r = west.r.bor
    west = west.bor
    west.county = west.county.bor
    rm(bc.poly.bor,ab.poly.bor,bc.bor, ab.bor,west.bor,west.county.bor,west.r.bor)

    setwd("c:/Rwork/MPB")
    objects2save = c("bc", "bc.poly", "ab", "ab.poly", "west", "west.county", "west.r")
    lapply(objects2save,
      function(x) save(list = x,file = paste("mpb.",x,".rdata",sep="")))
#    save("bc", file="mpb.bc.rdata")
