library(raster)
library(dplyr)
library(RColorBrewer)

# Make random forest cover map
a <- raster(extent(0,1e2,0,1e2),res=1)
hab <- gaussMap(a,speedup=1) # if raster is large (>1e6 pixels), use speedup>1
names(hab)="hab"
cells <- seedRcv <- b <- as.integer(sample(1:ncell(a),1e1))
mask <- raster(a)
mask <- setValues(mask, 0)
mask[1:5000] <- 1
numCol <- ncol(a)
numCell <- ncell(a)
directions <- 8

# Transparency involves putting 2 more hex digits on the color code, 00 is fully transparent
setColors(hab) <- paste(c("#FFFFFF",brewer.pal(8,"Greys")),c("00",rep("FF",8)),sep="")

#dev(4)
Plot(hab,new=TRUE,speedup=3) # note speedup is equivalent to making pyramids,
# so, some details are lost

# initiate 10 fires at to seedRcv
effDist = 4
maxDist = 15
b = 0.01
k = 0.95
cellSize=20
seedSrc <- hab>5
setColors(seedSrc,1) <- c("white","black")

load_all()
seedRcv <- as.integer(sample(1:ncell(hab), 3000))
rm(seeds)
system.time(seeds <- landisWardSpread(seedSrc, seedRcv=seedRcv, plot.it=FALSE))
print(length(seeds))

seeds <- landisWardSpread(seedSrc, seedRcv=c(1054,8115),spreadProbCluster=NULL,
                   spreadProbPixel=0.235, 0, NULL, 1e8, 8, 1e6, dist=TRUE)
