source("c:/Eliot/.Rprofile")
#source("C:/Eliot/GitHub/ABM/R/ABM_code_files.R")
#source("C:/Eliot/Dropbox/R/grid plotting multipanel functions.r")

require(raster)
#require(geoR)
require(grid)
require(Hmisc)
#devtools::install_github("lineprof")
#devtools::install_github("pryr")
#devtools::install_github("shiny-slickgrid", "wch")
library(lineprof)
library(pryr)
library(shiny)

devtools::load_all("c:/Eliot/GitHub/ABM")


#require(ABM)
st = data.frame(matrix(ncol = 3))
colnames(st)<- c("Time","NumLoci","NumPixels")
counter = 0

# if(!sfIsRunning()) sfInit(parallel = T, cpus=11)
# sfExport(list=c("counter"))
# sfLibrary(raster)
# sfLibrary(RandomFields)
# sfLibrary(ABM)
#sfSource("C:/Eliot/GitHub/ABM/R/movement.R")
#st = sfClusterApplyLB(1:2*10, function(i) {#
    
#for (i in 1:8*20) {
    ny = 1e2
    nx = 1e2
    speed = 2# 30
    
    par(mfrow = c(1,1))
    maps.list = list()
    num.maps= 50
    a = function(x,y,z) (x^y - z)^2
    coarseness = sample((1:num.maps)^optimize(f = a, x = num.maps, z = 500, interval = c(0,10))$minimum,num.maps)
    library(snowfall)
    if(!sfIsRunning()) sfInit(parallel = T, cpus=2)
    sfExport(list=c("coarseness","ny","nx","speed"))
    sfLibrary(raster)
    sfLibrary(RandomFields)
    sfSource("C:/Eliot/GitHub/ABM/R/movement.R")
    maps.list <- sfClusterApplyLB(1:num.maps,function(i) {
#    for (i in 1:num.maps) {
      map <- raster(nrows=ny, ncols=nx, xmn=-nx/2, xmx=nx/2, ymn = -ny/2, ymx = ny/2)
      map <- GaussMap(extent(map),speedup = speed, scale = coarseness[i], var = 1)
    })

    #sfStop()
    names(maps.list) = paste("map",1:num.maps,sep="")
    maps = stack(maps.list)
    
#     for (NumLoci in 1:10*40000) {
    NumLoci = 10
    caribou = new("mobileAgent", agentlocation = maps, numagents = NumLoci)
         Loci = cellFromXY(maps, coordinates(caribou))
#         
#         Landscape = map
#         counter = counter + 1
#         st[counter,1] = system.time(Potentials<-adjacent(Landscape,Loci,directions=8))[1]
#         st[counter,2] = NumLoci
#         st[counter,3] = prod(dim(map)[1:2])
#         print(paste(NumLoci,"NumLoci",prod(dim(map)[1:2]),"NumPixels"))
#    }


x11(11,8)
par(mfrow = c(1,2))
par(mai = c(1,1,1,1))
plot(st$NumPixels,st$Time,ylab="Time in seconds for 1 iteration of fn adjacent",
     xlab = "Number of Pixels",col=unique(as.factor(st$NumLoci)),pch=19)
col.vec = unique(as.factor(st$NumLoci))
legend("bottomright",xpd=T,inset=c(-0.6,0),pch=19, col=col.vec[length(col.vec):1],
       legend=col.vec[length(col.vec):1],title="Number of\nLoci",bty="n")
par(mai = c(1,1,1,0.1))
plot(st$NumLoci,st$Time,ylab="Time in seconds for 1 iteration of fn adjacent",
     xlab = "Number of Active Loci",col=as.factor(st$NumPixels/min(st$NumPixels)),pch=19)
mtext(side=3,outer = T,line=-2,paste("Fn call: \nadjacent(Landscape,Loci,directions=8)"),bty="n")
legend("bottomright",pch=19, col=unique(as.factor(st$NumPixels/min(st$NumPixels))),
       legend=unique(st$NumPixels),title="Number of\npixels",bty="n")
for (i in 1:4*1:4*40*40*100*100) lines(st$NumLoci[st$NumPixels==i],st$Time[st$NumPixels==i],
                                       col=unique(as.factor(st$NumPixels/min(st$NumPixels))[which(st$NumPixels[st$NumPixels==i]==st$NumPixels)]))

glm1 = glm(st$Time ~ st$NumPixels + st$NumLoci)
summary(glm1)


library(microbenchmark)


ext = extent(maps)
dm = dim(maps)
re = res(maps)
gt = GridTopology(c(xmin(ext),ymin(ext)),re,dm[1:2])
SP = SpatialPixels(SpatialPoints(coordinates(caribou)),grid=gt)
SP.ras =raster(coordinates(SP)[1])

set.seed(1234)
st1 = system.time(fires <- SpreadEvents(maps,Loci,spreadProb = 0.2))
set.seed(1234)
st1E = system.time(firesE <- SpreadEventsEliot(dim(maps),Loci,SpreadProb = 0.2))

# > st1
# user  system elapsed 
# 0.66    0.00    0.69 
# > length(wh);extent(maps);SpreadProb=0.2;Loci
# [1] 509
# class       : Extent 
# xmin        : -50 
# xmax        : 50 
# ymin        : -50 
# ymax        : 50 
# [1] 4957 8772 4679 2085 3006 5067 9154 9434  963 3453
# [11] 3886 7897



x = maps
cells = Loci

prof = lineprof(adja <- adjacent(maps,Loci,directions = 8))

adja <- adjacent(maps,Loci,directions = 8)

prof <- lineprof(SpreadEvents(maps,Loci,SpreadProb = 0.2),torture = FALSE)
shine(prof)


# rc.ras = rowColFromCell(maps,loci)
# adj = sort(adjacent(maps,loci,pairs=F))


adja = function(loci) {
#    rc.sp = vi2mrc(loci,dm[1],dm[2])#[,c(2,1)]
#    rc.rook = do.call(rbind,apply(rc.sp,1,function(w) rookcell(nrow=dm[1],ncol=dm[2],rowcol=w)))[,c(2,1)]
    rc.rook = rookcell.E(nrow=dm[1],ncol=dm[2],rowcol=vi2mrc(loci,dm[1],dm[2]))[,c(2,1)]
    adj.sp = cellFromRowCol(maps,rc.rook[,1],rc.rook[,2])
    adj = adjacent(maps,loci,directions=8,pairs=T)    
}





r1 <- raster(nrows=108, ncols=21, xmn=0, xmx=10)


ben = benchmark(replications = 30, adj.sp=adja(loci), adj = adjacent(maps,loci,directions=8,pairs=T))



#################################################################
x <- data.frame(matrix(runif(100 * 1e3), ncol = 1000))
medians <- vapply(x, median, numeric(1))

system.time({
    for(i in seq_along(medians)) {
        x[, i] <- x[, i] - medians[i]
    }
})

y <- as.list(x)
y = maps[[1]]

system.time({
    

#        y[[i]] <- c(y[[i]],3)
    test = function() {
        
    test = function() {
        gv = getValues(y)
        gv[40:34567] <- 3
        gv[40:34567] <- 4
        gv[40:34567] <- 5
        gv[40:34567] <- 6
        values(y)<-gv
    }
    test2 = function() {
        y[40:34567] <- 3
        y[40:34567] <- 4
        y[40:34567] <- 5
        y[40:34567] <- 6
    }
    
    ben = microbenchmark(times = 10L,
        test2(),
        test()
    )
                
                 print(c(address(y), refs(y)))  
                )
                
            }
    }

lineprof(test)

})

z <- as.matrix(x)
system.time({
    for(i in seq_along(medians)) {
        z[, i] <- z[, i] - medians[i]
    }
})



rookcell.E = function (rowcol, nrow, ncol, torus = FALSE, rmin = 1, cmin = 1) 
{
    row <- rowcol[,1]
    col <- rowcol[,2]
    if (torus) {
        y <- c(ifelse(col - 1 < cmin, ncol, col - 1), col, col, 
               ifelse(col + 1 > (ncol + (cmin - 1)), cmin, col + 
                          1))
        x <- c(row, ifelse(row - 1 < rmin, nrow, row - 1), ifelse(row + 
                                                                      1 > (nrow + (rmin - 1)), rmin, row + 1), row)
    } else {
        y <- c(ifelse(col - 1 < cmin, NA, col - 1), col, col, 
               ifelse(col + 1 > (ncol + (cmin - 1)), NA, col + 1))
        x <- c(row, ifelse(row - 1 < rmin, NA, row - 1), ifelse(row + 
                                                                    1 > (nrow + (rmin - 1)), NA, row + 1), row)
    }
    res <- as.data.frame(list(row = y, col = x))
#    res2 <- data.frame(row = y, col = x)

    
    #from=matrix(rep(c(row,col),each=4),ncol=2,byrow=F)))
    

    res <- na.omit(res)
    res <- as.matrix(res)
    rownames(res) <- NULL
    attr(res, "coords") <- c(col, row)
    res
}


rowcol = rc.sp
col=rowcol[,2]




gt = GridTopology(c(0,0),c(1,1),c(10,10))
grd = SpatialGrid(gt)
SpatialPixels

gridIndex2nb(grd)
gridIndex2nb(gt)

getGridIndex(coordinates(caribou),grid=gt)




queencell(rowcol=rc[1,],dm[2],dm[1])

library(spdep)
rowcol


SP = SpatialPixels(SpatialPoints(caribou),grid=gt)
queencell()

gridIndex2nb(SP,fullMat=F)
kml(SP)

fires = SpreadEvents(maps,SpreadProb=0.225,Loci=loci[1])

x11()
speed = 3
simPlot(maps, axes = "L", which.to.plot = "all",speedup=speed)
sam = sample(1:length(names(maps)),4)
for(i in 1:200) {
#     dev.hold()
     for (j in 1:3)
       maps[[sam[j]]]=(maps[[sam[j]]]+0.2)%%(runif(1,1.5,2.5))
    simPlot(maps,sam,add= T,speedup=speed)
#    simPlot(maps,add=F)
    simPlot(caribou,add=T,ext = extent(maps), 6,speedup=speed,gp = gpar(cex=0.2,alpha=1),pch=19,delete.previous=T);
#    dev.flush()
}




setwd("C:/Eliot/Dropbox/R/")
di = dir(pattern = ".rdata")
lapply(di,load,envir=.GlobalEnv)

maps.list = list()
#class(maps.list) <- "rasterList"

map.names = unlist(strsplit(di,".rdata"))
for (i in map.names) {
    maps.list[[i]] = get(i)
    #  maps.list[[i]] <- setValues (maps.list[[i]], getValues(maps.list[[i]]))
}

maps = stack(lapply(map.names,get))
rm(maps.list)
rm(list=map.names)
simPlot(maps,speedup=2)

cold = !is.na(maps$dd5)
pi = ProbInit(map,p=(maps$dd5>400) & (maps$dd5<440),absolute=F)
caribou = new("mobileAgent", agentlocation = cold, numagents = 1e4,probinit=pi)
simPlot(caribou,3,ext=extent(maps),pch=19,gp=gpar(cex=0.1))



st1 = Sys.time(); 
a <- getValues(habs[[4]]) 
a1 = a^1.2; 
habs[[4]]=setValues(habs[[4]],a1)
st2=Sys.time();
print(st2-st1);
simPlot(habs,4,speedup=10,add=T)

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




# Convert to .rdata
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


 