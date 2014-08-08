
# Google maps
library(dismo)
Alberta <- gmap("Alberta")
extAlberta <- extent(Alberta)


# Forest Cover

landCover = raster("C:/shared/data/shared/LandCoverOfCanada2005_V1_4/LCC2005_V1_4a.tif")
lambConCon <- crs(proj4string(landCover))
beginCluster(5)
Alberta.lambConCon <- projectRaster(Alberta, crs = lambConCon)
Alberta.lcc <- crop(landCover, Alberta.lambConCon)
colours <- list(Alberta.lcc@legend@colortable[1:40], Alberta@legend@colortable)
simPlot(Alberta.lcc, col = colours[1:40],speedup=0.5)
Alberta.lcc2 <- resample(Alberta.lambConCon, Alberta.lcc)
Alb.lcc <- raster(Alberta.lcc)
Alb.lcc <- setValues(Alb.lcc, getValues(Alberta.lcc))
Alb.goog <- raster(Alberta.lcc2)
Alb.goog <- setValues(Alb.goog, getValues(Alberta.lcc2))


simPlot(stack(Alb.lcc,Alb.goog),col=colours,speedup=0.5)
plot(Alb.goog)
