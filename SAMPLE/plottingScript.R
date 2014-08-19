library(devtools)
dev_mode(TRUE)
install(build_vignettes=FALSE) # build_vignette currently fails
library("SpaDES", lib.loc=getOption("devtools.path"))

landscape1 <- landscape
names(landscape1) <- paste("a",names(landscape),"1",sep="")
land <- stack(landscape,landscape1)
land <- land[[-12]]
DEM = land$DEM

obj <- stack(forestCover, DEM1)

#dev(4);sPlot(land, quick = F, add = F)
dev(4);sPlot(land, quick = T, add = F)
DEM = land$DEM
DEM1 = DEM
names(DEM1) = "DEM1"
DEM2 = DEM
names(DEM2) = "DEM2"
DEM3 = DEM
names(DEM3) = "DEM3"
forestAge= land$forestAge
forestCover= land$forestCover
for(i in 1:100) {
  DEM = DEM + sample(0:10,length(DEM),replace = T)
  DEM1 = DEM1 + sample(0:10,length(DEM),replace = T)
  DEM2 = DEM2 + sample(0:10,length(DEM),replace = T)
  DEM3 = DEM3 + sample(0:10,length(DEM),replace = T)
  forestCover = forestCover + sample(0:10,length(DEM),replace = T)
  #print(system.time(sPlot(forestAge,add=T,legend = T)))
  print(system.time(sPlot(stack(DEM, DEM1,DEM2,DEM3),add=T, quick = T)))
#  print(system.time(sPlot(stack(DEM1,forestCover),add=T, quick = T)))
}
print(system.time(sPlot(land,add=F, quick = T)))
print(system.time(sPlot(stack(DEM1,forestCover),add=T, quick = T)))
print(system.time(sPlot(stack(DEM1,DEM2,DEM3),add=T, quick = T)))
print(system.time(sPlot(stack(DEM1,DEM2),add=T, quick = T)))

dev(4);sPlot(land)

dev(4);sPlot(landscape[[c("DEM","forestAge")]])
dev(4);sPlot(DEM)
DEM = round(DEM/100)

dev(4)
grid.newpage()
dev(4);sPlot(landscape[[c("DEM","forestAge")]],
             visualSqueeze=0.7)
dev(4);sPlot(stack(DEM,DEM1),add=T)

DEM = DEM + sample(0:10,length(DEM),replace = T)
forestAge = forestAge + sample(0:10,length(DEM),replace = T)
forestCover = forestCover + sample(0:10,length(DEM),replace = T)
obj = stack(forestCover,forestAge)
add = T; quick = T
