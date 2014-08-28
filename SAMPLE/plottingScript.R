library(devtools)
dev_mode(FALSE)
detach(package:SpaDES,unload=T)
install(quick = TRUE) # build_vignette currently fails
library("SpaDES", lib.loc=getOption("devtools.path"))

fileList = data.frame(files = dir(file.path(find.package("SpaDES",
                                                         lib.loc=getOption("devtools.path"),
                                                         quiet=FALSE),"maps"),
                                  full.names=TRUE, pattern= "tif"),
                      functions="rasterToMemory",
                      packages="SpaDES",.stackNames="landscape",
                      stringsAsFactors=FALSE)
loadFiles(fileList=fileList)

#land = landscape
landscape1 <- landscape
names(landscape1) <- paste("a",names(landscape),"1",sep="")
landscape2 <- landscape
names(landscape2) <- paste("a",names(landscape),"2",sep="")
land <- stack(landscape,landscape1)
name(land) <- "land"
land <- land[[-10]]
DEM = land$DEM
forestAge = land$DEM

Plot(DEM)
Plot(forestAge, add= T)
for(i in 1:10) {
  caribou <- SpatialPoints(cbind(x=runif(1e4,-50,50),y=runif(1e4,-50,50)))
  name(caribou)<-"caribou"
  print(system.time(Plot(caribou, addTo="forestAge",size=(i+2)/2, axes=F)))
}

caribou1 <- SpatialPoints(cbind(x=runif(10,-50,50),y=runif(10,-50,50)))
name(caribou1)<-"caribou1"
caribou2 <- SpatialPoints(cbind(x=runif(10,-50,50),y=runif(10,-50,50)))
name(caribou2)<-"caribou2"

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

print(system.time(Plot(land,add=F)))
print(system.time(Plot(stack(DEM1,forestCover),add=T)))
print(system.time(Plot(stack(DEM1,DEM2,DEM3),add=T,axes=F)))
print(system.time(Plot(stack(DEM1,DEM2),add=T,axes=F)))
for(i in 1:10) {
  DEM = DEM + sample(0:100,length(DEM),replace = T)
  DEM1 = DEM1 + sample(0:10,length(DEM),replace = T)
  DEM2 = DEM2 + sample(0:10,length(DEM),replace = T)
  DEM3 = DEM3 + sample(0:10,length(DEM),replace = T)
  forestCover = forestCover + sample(0:10,length(DEM),replace = T)
  #print(system.time(Plot(forestAge,add=T,legend = T)))
  print(system.time(Plot(DEM, DEM1,DEM2,DEM3,add=T, axes=F, title=F,legend=F)))
#  print(system.time(Plot(stack(DEM1,forestCover),add=T, quick = T)))
}
print(system.time(Plot(DEM4, DEM5, DEM6, add=T, axes="L", title=T,legend=F)))

dev(4);Plot(land)

dev(4);Plot(landscape[[c("DEM","forestAge")]])
dev(4);Plot(DEM)
DEM = round(DEM/100)

dev(4);Plot(landscape[[c("DEM","forestAge")]],
             visualSqueeze=0.7)
dev(4);Plot(stack(DEM,DEM1),add=T)

DEM = DEM + sample(0:10,length(DEM),replace = T)
forestAge = forestAge + sample(0:10,length(DEM),replace = T)
forestCover = forestCover + sample(0:10,length(DEM),replace = T)
obj = stack(forestCover,forestAge)
add = T; quick = T


toPlot<-list(caribou)
dev(4);Plot(landscape)
dev(4);Plot(caribou, addTo="forestCover", add=T)



#dev(4);Plot(land, quick = F, add = F)
dev(4);Plot(caribou)
dev(4);print(system.time(Plot(landscape,axes=F)))
dev(4);print(system.time(Plot(land,add = F, axes=F)))
dev(4);print(system.time(Plot(caribou, DEM,add = T, axes=F)))

detach(package:SpaDES,unload=T)
install(quick = TRUE) # build_vignette currently fails
library("SpaDES", lib.loc=getOption("devtools.path"))
dev(4);print(system.time(Plot(DEM, caribou,add = F, axes=T)))
dev(4);print(system.time(Plot(caribou, addTo="DEM", add = T, axes=T)))

