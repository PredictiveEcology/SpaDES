#source("C:/Eliot/GitHub/ABM/R/ABM_code_files.R")
#source("C:/Eliot/Dropbox/R/grid plotting multipanel functions.r")
devtools::load_all("c:/Eliot/GitHub/ABM")
require(raster)
require(geoR)
require(grid)
ny = 2e2#2e3#3332#1000
nx = 2e2#2e3#1964#500
speed = 3

library(snowfall)
if(!sfIsRunning()) sfInit(parallel = T, cpus=2)
par(mfrow = c(1,1))
habs.list = list()
num.maps= 10
a = function(x,y,z) (x^y - z)^2
coarseness = sample((1:num.maps)^optimize(f = a, x = num.maps, z = 500, interval = c(0,10))$minimum,num.maps)
sfExport(list=c("coarseness","ny","nx","speed"))
sfLibrary(raster)
sfLibrary(RandomFields)
sfSource("C:/Eliot/GitHub/ABM/R/movement.R")
habs.list <- sfClusterApplyLB(1:num.maps,function(i) {
#for (i in 1:num.maps) {
  map <- raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn = -ny/2, ymx = ny/2)
  map <- GaussMap(extent(map),speedup = speed, scale = coarseness[i], var = 1)
})
#sfStop()
names(habs.list) = paste("hab",1:num.maps,sep="")
habs = stack(habs.list)



caribou = new("mobileAgent", agentlocation = habs[[1]], numagents = 1e3)

x11()
simplot(habs, axes = "L", which.to.plot = "all")
sam = sample(1:length(names(habs)),4)
for(i in 1:200) {
    dev.hold()
    for (j in 1:3)
      habs[[sam[j]]]=(habs[[sam[j]]]+0.4)%%(runif(1,1.5,2.5))
    simplot(habs,sam,add= T);
    simplot(caribou,add=T,sam[4],speedup=100);
    dev.flush()
}



st1 = Sys.time(); 
a <- getValues(habs[[4]]) 
a1 = a^1.2; 
habs[[4]]=setValues(habs[[4]],a1)
st2=Sys.time();
print(st2-st1);
simplot(habs,4,speedup=10,add=T)

st1 = Sys.time(); 
habs[[4]] <- habs[[4]]^1.2
st2=Sys.time();
print(st2-st1);

st1 = Sys.time(); 
a <- habs[[4]]@data@values
a1 = a^1.2; 
habs[[4]]@data@values <- a1
st2=Sys.time();
print(st2-st1);


setwd("C:/Eliot/Dropbox/Alana/Disp_BoA_v2.0/gisData/cell/")
di = dir(pattern = ".asc")[-c(1:2)]
habs = list()
class(habs) <- "rasterList"

for (i in di) {
  habs[[i]] = raster(i)
  habs[[i]] <- setValues (habs[[i]], getValues(habs[[i]]))
}
names(habs) <- unlist(strsplit(di, ".asc"))

setwd("c:/Eliot/Dropbox/R/")
lapply(1:length(habs), function(x) {
        nam = names(habs)[x]
        assign(nam,habs[[x]])
        save(list=nam,file=paste(nam,".rdata",sep=""))})


for (i in di) {
    habs[[i]] = raster(i)
    habs[[i]] <- setValues (habs[[i]], getValues(habs[[i]]))
}


plot(habs, speedup = 1, axes = "L", which.to.plot = "cti")

habs[["cti"]][habs[["cti"]]<4]<-15

plot(habs, which.to.plot = c("cti"), add = T,speedup=2)

plot(habs, speedup = 40, axes = "L", which.to.plot = 1:4, add = T)

x11()
plot(habs, speedup = 2, axes = "L", which.to.plot = "py_ba", add = F)



x11()
st1 = system.time(plot(habs, speedup = 1, which.to.plot = "wbp_ba", axes = "L"))
st100 = system.time(plot(habs, speedup = 100, which.to.plot = 1, axes = "L"))
cti = raster("cti.asc")

speed = 2
#x11()

fun2(habs, speedup = 1, N = 2e3, timer = T, hold = F, on.which = c(11),reps = 150)

plot.all(map.list = habs, speedup = speed, which.to.plot = c(2,4,6,7))

grid.raster(as.raster(habs[[13]]*sqrt(habs[[13]]),maxpixels=4e5/(length(habs))),vp="hab13")
grid.raster(as.raster(habs[[5]],maxpixels=4e5/(cols*rows)),vp="hab4")



###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
# Benchmarking the plotting functions
test = list()
wh = (1:30)^2
for (i in wh) {
  test[[match(i,wh)]] = raster(nrows = i*10, ncols = i*10)
  test[[match(i,wh)]][] = sample(1:10, i^2*100, replace=T)
}

ben2 = benchmark(replications=1,order = "elapsed", plot(test[[1]],maxpixels = 1e5),
                 plot(test[[2]],maxpixels = 1e5),
                 plot(test[[3]],maxpixels = 1e5),
                 plot(test[[4]],maxpixels = 1e5),
                 plot(test[[5]],maxpixels = 1e5),
                 plot(test[[6]],maxpixels = 1e5),
                 plot(test[[7]],maxpixels = 1e5),
                 plot(test[[8]],maxpixels = 1e5),
                 plot(test[[9]],maxpixels = 1e5),
                 plot(test[[10]],maxpixels = 1e5),
                 plot(test[[11]],maxpixels = 1e5),
                 plot(test[[12]],maxpixels = 1e5),
                 plot(test[[13]],maxpixels = 1e5),
                 plot(test[[14]],maxpixels = 1e5),
                 plot(test[[15]],maxpixels = 1e5),
                 plot(test[[16]],maxpixels = 1e5),
                 plot(test[[17]],maxpixels = 1e5),
                 plot(test[[18]],maxpixels = 1e5),
                 plot(test[[19]],maxpixels = 1e5),
                 plot(test[[20]],maxpixels = 1e5),
                 plot(test[[21]],maxpixels = 1e5),
                 plot(test[[22]],maxpixels = 1e5),
                 plot(test[[23]],maxpixels = 1e5),
                 plot(test[[24]],maxpixels = 1e5),
                 plot(test[[25]],maxpixels = 1e5),
                 plot(test[[26]],maxpixels = 1e5),
                 plot(test[[27]],maxpixels = 1e5),
                 plot(test[[28]],maxpixels = 1e5),
                 plot(test[[29]],maxpixels = 1e5),
                 plot(test[[30]],maxpixels = 1e5))

ben2 = benchmark(replications=1,order = "elapsed", plot(test[[1]],maxpixels = 1e4),
                 plot(test[[2]],maxpixels = 1e4),
                 plot(test[[3]],maxpixels = 1e4),
                 plot(test[[4]],maxpixels = 1e4),
                 plot(test[[5]],maxpixels = 1e4),
                 plot(test[[6]],maxpixels = 1e4),
                 plot(test[[7]],maxpixels = 1e4),
                 plot(test[[8]],maxpixels = 1e4),
                 plot(test[[9]],maxpixels = 1e4),
                 plot(test[[10]],maxpixels = 1e4),
                 plot(test[[11]],maxpixels = 1e4),
                 plot(test[[12]],maxpixels = 1e4),
                 plot(test[[13]],maxpixels = 1e4),
                 plot(test[[14]],maxpixels = 1e4),
                 plot(test[[15]],maxpixels = 1e4),
                 plot(test[[16]],maxpixels = 1e4),
                 plot(test[[17]],maxpixels = 1e4),
                 plot(test[[18]],maxpixels = 1e4),
                 plot(test[[19]],maxpixels = 1e4),
                 plot(test[[20]],maxpixels = 1e4),
                 plot(test[[21]],maxpixels = 1e4),
                 plot(test[[22]],maxpixels = 1e4),
                 plot(test[[23]],maxpixels = 1e4),
                 plot(test[[24]],maxpixels = 1e4),
                 plot(test[[25]],maxpixels = 1e4),
                 plot(test[[26]],maxpixels = 1e4),
                 plot(test[[27]],maxpixels = 1e4),
                 plot(test[[28]],maxpixels = 1e4),
                 plot(test[[29]],maxpixels = 1e4),
                 plot(test[[30]],maxpixels = 1e4))

ben = benchmark(replications=1,order = "elapsed", plot(test[[1]], interpolate = F),
                plot(test[[2]], interpolate=F),
                plot(test[[3]], interpolate=F),
                plot(test[[4]],interpolate = F),
                plot(test[[5]],interpolate = F),
                plot(test[[6]],interpolate = F),
                plot(test[[7]],interpolate = F),
                plot(test[[8]],interpolate = F),
                plot(test[[9]],interpolate = F),
                plot(test[[10]],interpolate = F),
                plot(test[[11]],interpolate = F),
                plot(test[[12]],interpolate = F),
                plot(test[[13]],interpolate = F),
                plot(test[[14]],interpolate = F),
                plot(test[[15]],interpolate = F),
                plot(test[[16]],interpolate = F),
                plot(test[[17]],interpolate = F),
                plot(test[[18]],interpolate = F),
                plot(test[[19]],interpolate = F),
                plot(test[[20]],interpolate = F),
                plot(test[[21]],interpolate = F),
                plot(test[[22]],interpolate = F),
                plot(test[[23]],interpolate=F),
                plot(test[[24]],interpolate=F),
                plot(test[[25]],interpolate=F),
                plot(test[[26]],interpolate=F),
                plot(test[[27]],interpolate=F),
                plot(test[[28]],interpolate=F),
                plot(test[[29]],interpolate=F),
                plot(test[[30]],interpolate=F))

N = wh^2; 
sp = list()
for (i in N) {
  sp[[match(i,N)]] = SpatialPoints(expand.grid(x = 1:sqrt(i), y = 1:sqrt(i)))
}
ben.sp = benchmark(replications=1,
                   plot(sp[[1]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[2]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[3]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[4]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[5]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[6]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[7]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[8]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[9]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[10]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[11]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[12]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[13]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[14]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[15]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[16]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[17]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[18]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[19]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[20]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[21]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[22]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[23]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[24]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[25]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[26]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[27]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[28]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[29]],add = T, pch = 19, cex = 0.01),
                   plot(sp[[30]],add = T, pch = 19, cex = 0.01)
)
ben.sp2 = benchmark(replications=100,order = "elapsed",
                    plot(sp[[1]],add = T, pch = 19, cex = 0.01),
                    plot(sp[[2]],add = T, pch = 19, cex = 0.01),
                    plot(sp[[3]],add = T, pch = 19, cex = 0.01),
                    plot(sp[[4]],add = T, pch = 19, cex = 0.01),
                    plot(sp[[5]],add = T, pch = 19, cex = 0.01),
                    plot(sp[[6]],add = T, pch = 19, cex = 0.01),
                    plot(sp[[7]],add = T, pch = 19, cex = 0.01))


par(mfrow = c(1,1))
plot(wh^2*100, type = "l", ben$elapsed[order(as.numeric(row.names(ben)))],xlab = "Num pixels",log = "xy",axes = F,
     pch = 19,ylab = "Time in seconds to render a raster via plot", ylim = c(0.001,11))
axis(1, at = 10^(2:8))
text(4e3, 3e-4, "4000",xpd = NA)
axis(2, at = c(1e-3,1e-2,1e-1,1,10), labels = c(0.001,0.01, 0.1, 1, 10))
lines(wh^2*100, ben1$elapsed, col = "red",pch = 19)
text(x=1e7, y=0.3, "Using maxpixels = 1e5", col = "red")
lines(wh^2*100, ben2$elapsed, col = "blue",pch = 19)
text(x=1e7, y=0.06, "Using maxpixels = 1e4", col = "blue")


ben.all = c(ben.sp2$elapsed/100, ben.sp$elapsed[order(ben.sp$elapsed)][8:30])
lines(wh^2,ben.all , col = "green",pch = 19)
text(x=2e3, y=0.006, "plotting\nSpatial Points", col = "green")

abline(v=1e5,col="red", lty = "dashed")
abline(v=1e4,col="blue", lty = "dashed")
abline(v=4e3,col="green", lty = "dashed")



# End Benchmarking
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################



vp[[12]] <- viewport(x = cr[i,"cols"], y = cr[i,"rows"], w = 1/cols*0.8, h = 1/rows*0.8,
                     #   just = c(1.1, 1.1),
                     name = names(map.list)[w],
                     xscale = c(xmin(ext),xmax(ext))/col.row,yscale= c(ymin(ext),ymax(ext))/row.col)

seekViewport("hab11")


library(rbenchmark)
ben = benchmark(replications = 2, fun1(), fun2())

, layout.torture()10), x = c(0.25), y = c(0.25), w = 0.5)




grid.newpage()
grid.raster(as.raster(hab), x = c(0.25), y = c(0.25), w = 0.5, name = "Habitat")
grid.points(x = sample(-25:25,10), y = sample(-25:25, layout.torture()10), x = c(0.25), y = c(0.25), w = 0.5)


grid.rect(gp = gpar(lty = "dashed"))
vp1 <- viewport(x = 0, y = 0.5, w = 0.5, h = 0.5,
 just = c("left", "bottom"), name = "vp1")
vp2 <- viewport(x = 0.5, y = 0, w = 0.5, h = 0.5,
 just = c("left", "bottom"))
pushViewport(vp1)
grid.rect(gp = gpar(col = "grey"))
grid.text("Some drawing in graphics region 1", y = 0.8)
upViewport()
pushViewport(vp2)
grid.rect(gp = gpar(col = "grey"))
grid.text("Some drawing in graphics region 2", y = 0.8)
upViewport()
downViewport("vp1")
grid.text("MORE drawing in graphics region 1", y = 0.2)
popViewport()


grid.newpage()
hab.vp = viewport(x = 0.075, y = 0.55, w = 0.4, h = 0.4,
 just = c("left", "bottom"), name = "hab")
pushViewport(hab.vp)
grid.raster(as.raster(hab))
hab2.vp = viewport(x = 0.575, y = 0.55, w = 0.4, h = 0.4,
 just = c("left", "bottom"), name = "hab2")
upViewport()
pushViewport(hab2.vp)
grid.raster(as.raster(hab))
seekViewport("hab")
grid.text("Hab")
grid.text("Hab", y = 1, vjust = -0.5)
grid.xaxis(at = c(1,3,5,7,9)/10, label = -2:2*100)
grid.yaxis(at = c(1,3,5,7,9)/10, label = -2:2*100)

seekViewport("hab2")
grid.text("Hab2", y = 1, vjust = -0.5)
grid.text("Hab2")
grid.xaxis(at = c(1,3,5,7,9)/10, label = -2:2*100)
grid.yaxis(at = c(1,3,5,7,9)/10, label = -2:2*100)


seekViewport("hab")
grid.text("Hab")

hab = hab * sqrt(hab)
seekViewport("hab")
grid.raster(as.raster(hab))

grid.points


 