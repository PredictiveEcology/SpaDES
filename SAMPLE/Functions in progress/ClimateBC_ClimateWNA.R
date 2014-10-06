library(SpaDES)
climBC <- raster("C:/Eliot/Unlinked/MAT_1961_1990.tif")
climBC <- climBC/10
if(ncell(vegMapLcc)>1e6) beginCluster(10)
climBCLcc <- projectRaster(climBC, to=vegMapLcc, method="ngb")


grid.newpage()
Plot(ageMap, add=F)
pushViewport(vp)

dev(4)

################
#vp1 <- viewport(0,0,1,1,name="top", just=c(0,0))
vp <- viewport(0.1, 0.1, width=0.5, height=0.5,
               just = c("left","bottom"),
               xscale=c(0,10),
               yscale=c(0,10))

grid.newpage()
#Plot(ageMap, add=F)
pushViewport(vp1)
grid.points(1:10/10, 1:10/10, pch=19, default.units="npc")
pushViewport(vp)
plotGrob(SpP)
popViewport()
grid.text(x = c(0.8,0.8),y=c(0.8, 0.85), label = c("hello","runing"),just=c(-0.5,0.3))
grid.points(x=c(0.8,0.8), y=c(0.8, 0.85), pch=21:22, default.units = "npc", gp=gpar(fill="red"))
#clickCoordinates()
################

grid.polygon(SpP)
grid.xaxis()


grid.raster(as.raster(ageMap))
