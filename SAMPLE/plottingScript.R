library(devtools)
dev_mode(TRUE)
detach(package:SpaDES,unload=T)
install(quick = TRUE) # build_vignette currently fails
library("SpaDES", lib.loc=getOption("devtools.path"))

fileList = data.frame(files = dir(file.path(find.package("SpaDES",
                                                         lib.loc=getOption("devtools.path"),
                                                         quiet=FALSE),"maps"),
                                  full.names=TRUE, pattern= "tif"),
                      functions="raster",
                      packages="SpaDES",
                      stringsAsFactors=FALSE)
sim <- loadFiles(fileList=fileList)
landscape = stack(mget(unlist(simObjectsLoaded(sim))))

land = landscape
landscape1 <- landscape
names(landscape1) <- paste("a",names(landscape),"1",sep="")
land <- stack(landscape,landscape1)
land <- land[[-12]]
DEM = land$DEM

caribou <- data.frame(x = 1:10, y = 1:10)
coordinates(caribou) <- cbind(x=runif(10,-50,50),y=runif(10,-50,50))
name(caribou)<-"caribou"


caribou1 <- SpatialPoints(cbind(x=runif(10,-50,50),y=runif(10,-50,50)))
name(caribou1)<-"caribou1"

DEM = land$DEM
DEM1 = DEM
names(DEM1) = "DEM1"
DEM2 = DEM
names(DEM2) = "DEM2"
DEM3 = DEM
names(DEM3) = "DEM3"
DEM4 = DEM
names(DEM4) = "DEM4"
DEM5 = DEM
names(DEM5) = "DEM5"
DEM6 = DEM
names(DEM6) = "DEM6"
forestAge= land$forestAge
forestCover= land$forestCover

# toPlot<-list(DEM2, caribou, landscape, DEM1, caribou1)
# toPlot<-list(DEM3, landscape1, DEM1, caribou1)
# toPlot<-list(DEM3, caribou)
# toPlot<-list(land)

print(system.time(Plot2(land,add=F, axes=F)))
print(system.time(Plot2(stack(DEM1,forestCover),add=T,axes=F)))
print(system.time(Plot2(stack(DEM1,DEM2,DEM3),add=T,axes=F)))
print(system.time(Plot2(stack(DEM1,DEM2),add=T,axes=F)))
for(i in 1:10) {
  DEM = DEM + sample(0:100,length(DEM),replace = T)
  DEM1 = DEM1 + sample(0:10,length(DEM),replace = T)
  DEM2 = DEM2 + sample(0:10,length(DEM),replace = T)
  DEM3 = DEM3 + sample(0:10,length(DEM),replace = T)
  forestCover = forestCover + sample(0:10,length(DEM),replace = T)
  #print(system.time(Plot2(forestAge,add=T,legend = T)))
  print(system.time(Plot2(DEM, DEM1,DEM2,DEM3,add=T, axes=F, title=F,legend=F)))
#  print(system.time(Plot2(stack(DEM1,forestCover),add=T, quick = T)))
}
print(system.time(Plot2(DEM4, DEM5, DEM6, add=T, axes=F, title=F,legend=F)))

dev(4);Plot2(land)

dev(4);Plot2(landscape[[c("DEM","forestAge")]])
dev(4);Plot2(DEM)
DEM = round(DEM/100)

dev(4)
grid.newpage()
dev(4);Plot2(landscape[[c("DEM","forestAge")]],
             visualSqueeze=0.7)
dev(4);Plot2(stack(DEM,DEM1),add=T)

DEM = DEM + sample(0:10,length(DEM),replace = T)
forestAge = forestAge + sample(0:10,length(DEM),replace = T)
forestCover = forestCover + sample(0:10,length(DEM),replace = T)
obj = stack(forestCover,forestAge)
add = T; quick = T


add=F; addTo=NULL; gp=gpar(); axes="L"; speedup = 1;
size=5; cols=topo.colors(50); deletePrevious = add;
visualSqueeze=0.75; quick = FALSE; legend=!quick; draw = TRUE;
pch = 19


rm(add,addTo,gp,axes,speedup,size,cols,deletePrevious,visualSqueeze,quick,legend,draw,pch,
   arr, grobs,lay, .arr, extsKeep, extents)
rm(dev, newPlot, SpatialPointsDataFrameNeeded, SpatialPointsNamed)
toPlot<-list(caribou)
Plot2(caribou)
Plot2(landscape)



#dev(4);Plot2(land, quick = F, add = F)
dev(4);Plot2(caribou)
dev(4);print(system.time(Plot2(landscape,axes=F)))
dev(4);print(system.time(Plot2(land,add = F, axes=F)))
dev(4);print(system.time(Plot2(caribou, DEM,add = F, axes=F)))

detach(package:SpaDES,unload=T)
install(quick = TRUE) # build_vignette currently fails
library("SpaDES", lib.loc=getOption("devtools.path"))
dev(4);print(system.time(Plot2(DEM, caribou,add = F, axes=T, quick = F)))
dev(4);print(system.time(Plot2(caribou, addTo="DEM", add = T, axes=T)))

