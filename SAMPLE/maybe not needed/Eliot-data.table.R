devtools::install_github("lineprof")
devtools::install_github("pryr")
devtools::install_github("shiny-slickgrid", "wch")
library(lineprof)
library(pryr)
library(shiny)

devtools::load_all("c:/Eliot/GitHub/SpaDES")
library(microbenchmark)
library(raster)
library(RColorBrewer)
#library(compiler)
#enableJIT(3)
#library(SpaDES)
a = raster(extent(0,1e2,0,1e2),res=1)
landscape = hab = GaussMap(a,speedup=10)
names(hab)="hab"
cells = loci = b = as.integer(sample(1:ncell(a),1e1))
mask = raster(a)
mask = setValues(mask, 0)
mask[1:50] <- 1
numCol <- ncol(a)
numCell <- ncell(a)
directions=8
cols = list(c("#00000000",brewer.pal(8,"RdYlGn")[8:1]),brewer.pal(9,"Greys"),brewer.pal(8,"Spectral"))
# Transparency involves putting 2 more hex digits on the color code, 00 is fully transparent

simPlot(hab)
dE = drawExtent()
dev(2)
simPlot(crop(hab,dE),col=cols[[2]])
names(hab)<-"hab"
(mb2 = microbenchmark(times = 1L,

hab = habitat[["Age"]]
fire2 <- spread(hab,loci=as.integer(sample(1:ncell(hab),10)),mapFireID=T,
                spreadProb = 0.235,0,NULL,1e8,8,1e6,mergeDuplicates = T,
                plot.it=F,col=cols[[1]],delete.previous=F,add=F)


dis <-  distanceFromPoints(hab,pts)
))

dev(4)
fires = list()
for (fir in 1:10)
fires[[fir]] <- spread(hab,loci=as.integer(sample(1:ncell(hab),10)),
                spreadProb = runif(1,0.2,0.3),0,NULL,1e8,8,1e6,mergeDuplicates = T,
                plot.it=F,col=cols[[1]],delete.previous=T,add=F,on.which.to.plot="hab")
names(fire2)<-"fire"

vp = viewport(xscale = rangex,yscale= rangey,width=0.8,height=0.8,
              name=paste(deparse(substitute(x))))

simPlot(fire2,col=cols[[1]])
upViewport()
grid.raster(as.raster(cols[[1]][9:2] ),
            x=0.94,y=0.5,height=0.5,width=0.03,
            interpolate=TRUE)
pr = unname(quantile(range(minValue(fire2),maxValue(fire2)),c(0,0.5,1)))
grid.text(pr,x=0.98, y = pr/(2*max(pr,na.rm=T))+0.25,...)



dev(4)

pts = SpatialPoints(xyFromCell(fire2,Which(fire2>0,cells=T)))
simPlot(x=pts,on.which.to.plot="fire",add=T,pch=15,gp=gpar(cex=0.5))
(mb = microbenchmark(
simPlot(fire2,col=cols[[1]]),
simPlot(x=pts,on.which.to.plot="fire",add=T,pch=15,gp=gpar(cex=0.5)),
times=10L
))

# crop
simPlot(crop(hab,dE),col=cols[[2]])
simPlot(crop(fire2,dE),add=T,on.which.to.plot="hab",delete.previous=F,col= cols[[1]])

simPlot(stack(stack(fires),hab),col=cols[c(sample(1:3,15,replace=T),2)])
#

simPlot(fire2,col=cols[[1]],speedup=10,add=T,on.which.to.plot="hab",delete.previous=F)
simPlot(fire2,col=cols[[1]],speedup=10,add=T,on.which.to.plot="fire",delete.previous=F)


newPlot()
simPlot(hab,speedup=15,col=brewer.pal(9,"Accent"))
simPlot(stack(speedup=15,fire0,fire1,hab),col=cols)

mb = list()
for (i in 3:3) {
hab = raster(extent(0,10^i,0,10^i),res=1)
b=as.integer(sample(1:ncell(hab),10))
#library(compiler)
#spread.c = cmpfun(spread)
jmax = 10
maxes = data.frame(matrix(nrow = 2, ncol=jmax))
times = data.frame(matrix(nrow = 2, ncol=jmax))
for (j in 1:jmax) {
  mb[[j]] <- microbenchmark(times = 1L,
  fire0 <- spread(hab,loci=b,1,0,NULL,1e8,8,1e6),
#  fire1 <- spread.adjacent(hab,loci=b,0.235,0,NULL,1e8,8,1e6),
  fire2 <- spread.m(hab,loci=b,1,0,NULL,1e8,8,1e6,mergeDuplicates=T)
#  fire3 <- spread.c(hab,loci=b,0.235,0,NULL,1e8,8,1e6)
  )
  maxes[,j]=c(maxValue(fire0),maxValue(fire2))
  times[,j] = summary(mb[[j]])[[4]]
  try(rm(fire0,fire2))
  gc()
  print(j)
}
}
print(rowMeans(maxes))
print(rowMeans(times))
#times = cbind(times, times1)
#maxes = cbind(maxes, maxes1)
times1 = times
maxes1 = maxes

dev(4)
r = 1
coefs = data.frame(matrix(ncol = 2))
plot(0,type = "n", xlim = c(0,200),ylim = c(1,600),
     ylab="time in seconds",xlab="num iterations",log="y")
for (r in 1:2){
coefs[r,] = coef(lm(as.numeric(times[r,])~as.numeric(maxes[r,])))
points(as.numeric(maxes[r,]), as.numeric(times[r,]),col= r,pch=19)
}
legend("topleft",col=1:2,pch=19, legend = 
         c("current spread","new spread"))


out <-sapply(mb,function(x) print(x)[[4]])
for (i in 1:3)
  out[,i]<-out[,i]/1000

par(mai=c(1, 1, 1, 1))
num.pixels = (10^(1:4))^2
fns = c("original","adj","optimized.adj","recompiled.optimized.adj")
plot(1:4,out[,4],log="y",type="l",ylim = c(0.05,25),xlab="",axes=F,ylab="Time in seconds")
lapply(1:4,function(x) {lines(1:4,out[,x],col=x)})
axis(2)
axis(1,label=fns,at=1:4)
legend("topright",inset=c(-0.1,-0.5),xpd=NA,legend=num.pixels,lty=1,col=1:4,title="num pixels")

out2 = numeric()
for(i in 1:4)
  out2[i] <- out[1,i]/out[3,i]

plot(num.pixels,out2,log="x",ylab="speedup factor")
mtext(side=3,"Speedup between spread fn in Raster, and new spread\nas a function of num.pixels in raster")

enableJIT(0)
system.time(fire2 <- spread.adj(hab,loci=b,1,0,NULL,1e3,8,1e6))
enableJIT(3)
system.time(fire3 <- spread.adj.c(hab,loci=b,1,0,NULL,1e3,8,1e6))

adj.c <- compiler::cmpfun(adj)
spread.adj.c <- compiler::cmpfun(spread.adj)

profs <- lineprof(spread(hab,loci=b,0.225,0,NULL,1e2,8,1e6))
shine(profs)

profs3 <- lineprof(adj(numCol=numCol,numCell=numCell,sort=T,as.data.table=T,
               cells=cells,directions=8,pairs=T,include=F))
shine(profs3)

library(lineprof)
prof4 <- lineprof(adj2(numCol=numCol,numCell=numCell,sort=T,as.data.table=T,
                 cells=cells,directions=8,pairs=T,include=F,match.adjacent=F))#,
shine(prof4)

#newPlot();
#dev.set(4)
simPlot(stack(fire1,fire2,fire0),speedup=1)
  
mb1 = list()
i = 0:4
#library(compiler)
#adj.cmp <- cmpfun(adj)
#enableJIT(0)
for (ind in i) {
  numCells = 10^ind
  cells = sample(numCell,numCells)
(mb1[[ind+1]] = microbenchmark(times=20L,
  adj.orig <- adjacent(a,cells,sort=T,directions=8,include=F,pairs = T),
#  adj.new4 <- adj.raw(numCol=numCol,numCell=numCell,sort=F,#,as.data.table=T,
#                   cells=cells,directions=8,pairs=T,include=F,match.adjacent=F),
  adj.new4.1 <- adj(numCol=numCol,numCell=numCell,sort=F,#,as.data.table=T,
                   cells=cells,directions=8,pairs=T,include=F,match.adjacent=F)
  #  adj.new4.1 <- adj4(numCol=numCol,numCell=numCell,sort=F,cutoff = 1e4,#,as.data.table=T,
#                   cells=cells,directions=8,pairs=T,include=F,match.adjacent=F)
#  adj.new4.2 <- adj4(numCol=numCol,numCell=numCell,sort=T,cutoff = 1e5,#,as.data.table=T,
#                   cells=cells,directions=8,pairs=T,include=F,match.adjacent=F)
#  adj.new5 <- adj4(numCol=numCol,numCell=numCell,sort=T,#,as.data.table=T,
#                   cells=cells,directions=8,pairs=T,include=F,match.adjacent=T)
  #    adj.new <- adj(numCol=numCol,numCell=numCell,sort=T,#as.data.table=T,
#               cells=cells,directions=8,pairs=T,include=F)#,
#   adj.new2 <- adj2(numCol=numCol,numCell=numCell,sort=T,as.data.table=T,
#                  cells=cells,directions=8,pairs=T,include=F,match.adjacent=F)#,
#  adj.new.m = adj.m(numCol=numCol,numCell=numCell,sort=F,as.data.table=FALSE,cells=cells,directions=8,pairs = F),
#  adj.new.m2 = adj.m(numCol=numCol,numCell=numCell,sort=T,cells=cells,directions=8,pairs = F)
  #adj.new3 <- adj3(numCol=numCol,numCell=numCell,cells=cells,directions=8),
  #adj.new4 <- adj4(numCol=numCol,numCell=numCell,cells=cells,directions=8)
))
}
print(mb1)

print(data.frame(nCells=10^(0:(length(mb1)-1)),
                 matchF=sapply(lapply(mb1,function(x) summary(x)[[4]]),function(x) x[1]/x[2]),
                 matchT=sapply(lapply(mb1,function(x) summary(x)[[4]]),function(x) x[1]/x[3])#,
                 #speedup1e5=sapply(lapply(mb1,function(x) summary(x)[[4]]),function(x) x[1]/x[4])
      ))

plot(10^(0:(length(mb1)-1)),
     sapply(lapply(mb1,function(x) summary(x)[[4]]),
            function(x) x[1]/x[3]),log="xy")

plot(mb,horiz=FALSE)
print(all.equal(adj.orig,adj.new))

###################################################################################

<<tapered-pareto, fig=FALSE, eval=FALSE>>=
  # Count number of pixels in each fire (removing the pixels with no fires)
  fireSizes = sort(unname(table(getValues(habitat[["Fires"]]))[-1]))

probEx = vector(length = length(unique(fireSizes)))
for (i in unique(fireSizes))
  probEx[match(i,unique(fireSizes))] <- length(fireSizes[fireSizes>i])

library(PtProcess)
library(parallel)
source("~/GitHub/SpaDES/SAMPLE/taperedPareto.R")
cl <- makePSOCKcluster(rep("localhost", 7))
data = data.frame(Area_ha= fireSizes)
est.tap = wrap.nlminb(data = data)
fireSizes.theor <- rtappareto(1000,lambda=est.tap$par[1],theta=est.tap$par[2],a=1)
fireSizes.theor <- sort(round(fireSizes.theor,0))
probEx.theor = vector(length = length(unique(fireSizes.theor)))
for (i in unique(fireSizes.theor))
  probEx.theor[match(i,unique(fireSizes.theor))] <- length(fireSizes.theor[fireSizes.theor>i])

dev(4); plot(unique(fireSizes.theor),probEx.theor,log="xy",type = "l", ylab = "probability of exceeding", main=paste(nFires,"fires in an",nx,"by",ny,"landscape"))
par(new = T)
dev(4); plot(unique(fireSizes),probEx, col = "red",type = "l",log="xy")
#lines(unique(fireSizes),probEx, col = "red")
@
