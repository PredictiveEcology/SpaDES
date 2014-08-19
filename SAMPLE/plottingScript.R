library(devtools)
dev_mode(TRUE)
detach(package:SpaDES,unload=T)
install(build_vignettes=FALSE) # build_vignette currently fails
library("SpaDES", lib.loc=getOption("devtools.path"))

fileList = data.frame(files = dir(file.path(find.package("SpaDES",
                                                         lib.loc=getOption("devtools.path"),
                                                         quiet=FALSE),"maps"),
                                  full.names=TRUE, pattern= "tif"),
                      functions="raster",
                      packages="SpaDES",
                      stringsAsFactors=FALSE)
sim <- simLoad(fileList=fileList, stackName="landscape")
landscape = stack(mget(simObjectsLoaded(sim)))

land = landscape
landscape1 <- landscape
names(landscape1) <- paste("a",names(landscape),"1",sep="")
land <- stack(landscape,landscape1)
land <- land[[-12]]
DEM = land$DEM

obj <- stack(forestCover, DEM1)


#dev(4);Plot(land, quick = F, add = F)
dev(4);print(system.time(Plot(landscape,axes=F)))
dev(4);print(system.time(Plot(land,add = T, axes=F)))
DEM = land$DEM
DEM1 = DEM
names(DEM1) = "DEM1"
DEM2 = DEM
names(DEM2) = "DEM2"
DEM3 = DEM
names(DEM3) = "DEM3"
forestAge= land$forestAge
forestCover= land$forestCover
for(i in 1:10) {
  DEM = DEM + sample(0:100,length(DEM),replace = T)
  DEM1 = DEM1 + sample(0:10,length(DEM),replace = T)
  DEM2 = DEM2 + sample(0:10,length(DEM),replace = T)
  DEM3 = DEM3 + sample(0:10,length(DEM),replace = T)
  forestCover = forestCover + sample(0:10,length(DEM),replace = T)
  #print(system.time(Plot(forestAge,add=T,legend = T)))
  print(system.time(Plot(DEM, DEM1,DEM2,DEM3,add=T, quick = T,axes=F,speedup=2)))
#  print(system.time(Plot(stack(DEM1,forestCover),add=T, quick = T)))
}
print(system.time(Plot(land,add=F, quick = T)))
print(system.time(Plot(stack(DEM1,forestCover),add=T, quick = T)))
print(system.time(Plot(stack(DEM1,DEM2,DEM3),add=T, quick = T)))
print(system.time(Plot(stack(DEM1,DEM2),add=T, quick = T)))

dev(4);Plot(land)

dev(4);Plot(landscape[[c("DEM","forestAge")]])
dev(4);Plot(DEM)
DEM = round(DEM/100)

dev(4)
grid.newpage()
dev(4);Plot(landscape[[c("DEM","forestAge")]],
             visualSqueeze=0.7)
dev(4);Plot(stack(DEM,DEM1),add=T)

DEM = DEM + sample(0:10,length(DEM),replace = T)
forestAge = forestAge + sample(0:10,length(DEM),replace = T)
forestCover = forestCover + sample(0:10,length(DEM),replace = T)
obj = stack(forestCover,forestAge)
add = T; quick = T
