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
a = raster(extent(0,1e3,0,1e3),res=1)
hab = GaussMap(a,speedup=1)
names(hab)="hab"
loci = b = as.integer(sample(1:ncell(a),1e1))
mask = raster(a)
mask = setValues(mask, 0)
mask[1:5000] <- 1
numCol <- ncol(a)
numCell <- ncell(a)
cells = loci
  directions=8

# Transparency involves putting 2 more hex digits on the color code, 00 is fully transparent
cols = list(c("#00000000",brewer.pal(8,"RdYlGn")[8:1]),brewer.pal(9,"Greys"),brewer.pal(8,"Spectral"))

newPlot()
simplot(hab,col=cols[[1]],speedup=10)
#names(hab)<-"hab"
fire2 <- spread(hab,loci=as.integer(sample(1:ncell(hab),10)),
                    0.235,0,NULL,1e6,8,1e6,plot.it=T,col=cols[[1]],delete.previous=F,
                    speedup=100)
names(fire2)<-"fire"

simplot(stack(fire2,hab),col=cols[1:2],speedup=10)
simplot(fire2,col=cols[[1]],speedup=10,add=T,on.which.to.plot="hab",delete.previous=F)
simplot(fire2,col=cols[[1]],speedup=10,add=T,on.which.to.plot="fire",delete.previous=F)


newPlot()
simplot(hab,speedup=15,col=brewer.pal(9,"Accent"))
simplot(stack(speedup=15,fire0,fire1,hab),col=cols)

mb = list()
for (i in 1:4){
hab = raster(extent(0,10^i,0,10^i),res=1)
b=as.integer(sample(1:ncell(hab),1))
mb[[i]] <- microbenchmark(times = 10L,
fire0 <- spread(hab,loci=b,1,0,NULL,1e3,8,1e6),
fire1 <- spread.adjacent(hab,loci=b,1,0,NULL,1e3,8,1e6),
fire2 <- spread.adj(hab,loci=b,1,0,NULL,1e3,8,1e6),
fire3 <- spread.adj.c(hab,loci=b,1,0,NULL,1e3,8,1e6)
)
}


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

prof <- lineprof(spread.adj(hab,loci=b,0.225,0,NULL,1e2,8,1e6))
shine(prof)

#newPlot();
#dev.set(4)
simplot(stack(fire1,fire2,fire0),speedup=1)
  
(mb = microbenchmark(times=5L,
  adj.orig = adjacent(a,cells,sort=F,directions=8,pairs = F),
  adj.new = adj(numCol=numCol,numCell=numCell,as.data.table=FALSE,cells=cells,directions=8,pairs=F),
  adj.new.m = adj.m(numCol=numCol,numCell=numCell,sort=F,as.data.table=FALSE,cells=cells,directions=8,pairs = F),
  adj.new.m2 = adj.m(numCol=numCol,numCell=numCell,sort=T,cells=cells,directions=8,pairs = F)
  #adj.new3 <- adj3(numCol=numCol,numCell=numCell,cells=cells,directions=8),
  #adj.new4 <- adj4(numCol=numCol,numCell=numCell,cells=cells,directions=8)
))
plot(mb,horiz=FALSE)
print(all.equal(adj.orig,adj.new.m))

landscape = a
spreadProb = 0.225
persistance = 0
mask = NULL
maxSize = 1e6
directions = 8
iterations = 1e6
n = 1


(mb=microbenchmark(times=2L,
data.tab={potentials.dt <- adj(landscape, loci, directions,as.data.table=TRUE)
setkey(potentials.dt,to)
spreads.dt = data.table(ind=1:ncell(landscape),burned=0,key="ind")
spreads.dt[loci,burned:=n]
tmp.dt <- spreads.dt[potentials.dt,burned]},

rast={potentials = adj(landscape, loci, directions,as.data.table=FALSE)
spreads <- setValues(raster(landscape), 0)
spreads[loci] <- n
tmp <- extract(spreads, potentials[,2])
}
))

###################################################################################

install.packages("ref")
library(ref)
x = matrix(sample(1:12),ncol=1)
rd <- refdata(x) # create reference
derefdata(rd) # retrieve original data
#rd[] # get all (current) data
i = 11; j = 1
rd[i, j] # get part of data
rd[i, j, ref=TRUE] # get new reference on part of data
#rd[i, j] <- value # modify / create local copy
#rd[i, j, ref=TRUE] <- value # modify original data (respecting subsetting history)
dim(rd) # dim of (subsetted) data
dimnames(rd) # dimnames of (subsetted) data

x <- matrix(sample(1:10,replace=T,1e8),ncol=1e3) # take a matrix or data frame
rx <- refdata(x) # wrap it into an refdata object
rx # see the autoprinting
#rm(x) # delete original to save memory
#rx[] # extract all data
#rx[-1, ] # extract part of data
ras <- raster(x)
extent(ras)<-c(0,100000,0,1000)
df = data.frame(x)
dt = data.table(x)

sam = sort(sample(1:nrow(x),2e2,replace=F))
system.time(for (i in 1:1e3) {
  rx[sam,2 , ref=FALSE]<-sam
})

x <- matrix(sample(1:10,replace=T,1e5),ncol=2) # take a matrix or data frame
system.time(for (i in 1:1e4) {
  x[sam,2]<-sam+i
})


rm(x)
x <- 1:1e7 # take a matrix or data frame
sam = sample(1:length(x),8e3,replace=F)
sam.sort = sort(sam)

system.time(for (i in 1:1e1) {
  prof <- lineprof(x[sam]<-sam.sort)
#  prof <- lineprof(x[1:7e3]<-1:7e3)
  shine(prof)
})
refs(z)
address(z)

y= matrix(rep(x,10),ncol=10)
system.time(for (i in 1:1e1){
  x = numeric(length=1e7)
  x <- y[,(i-1)%%10+1]
})

<- matrix(sample(1:10,replace=T,1e7),ncol=1e3) # take a matrix or data frame
system.time(for (i in 1:1e4) {
  x[sam]<-sam
})

system.time(for (i in 1:1e4) {
  df[sam,2]<-sam
})

dt = data.table(x)
setkey(dt,x)
system.time(for (i in 1:1e4) {
  nrdt = nrow(dt)
  prof<-lineprof(dt[1:4,y:=1:4])
  shine(prof)
})

system.time(for (i in 1:1e4) {
  ras[sam,2]<-sam
}
)


system.time({  m <- matrix(getValues(ras),ncol=dim(ras)[2])
               for (i in 1:1e4) {
                 m[sam,2]<-sam
               }
               ras1<-raster(m)
               extent(ras1)<-extent(ras)
})

library(raster)
dev(4)
ras <- raster(x)
system.time(grid.raster(as.raster(ras,maxpixels=1e5),interpolate=F))

dev(4)
system.time(grid.raster(as.raster(x,max=max(x),interpolate=F)))

dt.m <- as.matrix(dt)

all.equal()















##############################################################
adj <- function(x=NULL,cells,directions=8,pairs=TRUE,include=FALSE,target=NULL,
                numCol=NULL,numCell=NULL,as.data.table=FALSE) {
  if (is.null(numCol) | is.null(numCell)) {
    if (is.null(x)) stop("must provide either numCol & numCell or a x")
    numCol = ncol(x)
    numCell = ncell(x)
  } 
  
  if (directions==8) {
    # determine the indices of the 8 surrounding cells of the cells cells
    topl=as.integer(cells-numCol-1)
    top=as.integer(cells-numCol)
    topr=as.integer(cells-numCol+1)
    lef=as.integer(cells-1)
    rig=as.integer(cells+1)
    botl=as.integer(cells+numCol-1)
    bot=as.integer(cells+numCol)
    botr=as.integer(cells+numCol+1)
    if (include)
      adj=data.table(from=rep.int(cells,times=9),
                     to=c(topl,top,topr,lef,as.integer(cells),rig,botl,bot,botr),key="from")
    else
      adj=data.table(from=rep.int(cells,times=8),
                     to=c(topl,top,topr,lef,rig,botl,bot,botr),key="from")
  } else if (directions==4) {
    # determine the indices of the 4 surrounding cells of the cells cells
    top=as.integer(cells-numCol)
    lef=as.integer(cells-1)
    rig=as.integer(cells+1)
    bot=as.integer(cells+numCol)
    if (include)
      adj=data.table(from=rep.int(cells,times=5),to=c(top,lef,as.integer(cells),rig,bot),key="from")
    else
      adj=data.table(from=rep.int(cells,times=4),to=c(top,lef,rig,bot),key="from")
  } else if (directions=="bishop") {
    topl=as.integer(cells-numCol-1)
    topr=as.integer(cells-numCol+1)
    botl=as.integer(cells+numCol-1)
    botr=as.integer(cells+numCol+1)
    if (include)
      adj=data.table(from=rep.int(cells,times=5),
                     to=c(topl,topr,as.integer(cells),botl,botr),key="from")
    else
      adj=data.table(from=rep.int(cells,times=4),
                     to=c(topl,topr,botl,botr),key="from")
  } else {stop("directions must be 4 or 8 or \'bishop\'")}
  
  # Remove all cells that are not target cells, if target is a vector of cells
  if (!is.null(target)) {
    setkey(adj,to)
    adj<-adj[J(target)] 
    setkey(adj,from)
    setcolorder(adj,c("from","to"))
  }
  
  # Remove the "from" column if pairs is FALSE
  if (!pairs) {
    from=adj$from
    adj[,from:=NULL]
  }
  
  # Good time savings if no intermediate object is created
  if (as.data.table) 
    return(adj[
      i = !((((to-1)%%numCell+1)!=to) |  #top or bottom of raster
              ((from%%numCol+to%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
      ])
  else 
    return(as.matrix(adj[
      i = !((((to-1)%%numCell+1)!=to) |  #top or bottom of raster
              ((from%%numCol+to%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
      ]))
}


#########################################################
adj.m <- function(x=NULL,cells,directions=8,sort=FALSE,pairs=TRUE,include=FALSE,target=NULL,
                numCol=NULL,numCell=NULL,as.data.table=FALSE) {
  if (length(cells)<1e4){
  if (is.null(numCol) | is.null(numCell)) {
    if (is.null(x)) stop("must provide either numCol & numCell or a x")
    numCol = ncol(x)
    numCell = ncell(x)
  } 
  
  if (directions==8) {
    # determine the indices of the 8 surrounding cells of the cells cells
    topl=as.integer(cells-numCol-1)
    top=as.integer(cells-numCol)
    topr=as.integer(cells-numCol+1)
    lef=as.integer(cells-1)
    rig=as.integer(cells+1)
    botl=as.integer(cells+numCol-1)
    bot=as.integer(cells+numCol)
    botr=as.integer(cells+numCol+1)
    if (include){
      adj=cbind(from=rep.int(cells,times=9),
                     to=c(topl,top,topr,lef,as.integer(cells),rig,botl,bot,botr))
    }else{
      adj=cbind(from=rep.int(cells,times=8),
                     to=c(topl,top,topr,lef,rig,botl,bot,botr))
    }
  } else if (directions==4) {
    # determine the indices of the 4 surrounding cells of the cells cells
    top=as.integer(cells-numCol)
    lef=as.integer(cells-1)
    rig=as.integer(cells+1)
    bot=as.integer(cells+numCol)
    if (include)
      adj=cbind(from=rep.int(cells,times=5),to=c(top,lef,as.integer(cells),rig,bot))
    else
      adj=cbind(from=rep.int(cells,times=4),to=c(top,lef,rig,bot))
  } else if (directions=="bishop") {
    topl=as.integer(cells-numCol-1)
    topr=as.integer(cells-numCol+1)
    botl=as.integer(cells+numCol-1)
    botr=as.integer(cells+numCol+1)
    if (include)
      adj=cbind(from=rep.int(cells,times=5),
                     to=c(topl,topr,as.integer(cells),botl,botr))
    else
      adj=cbind(from=rep.int(cells,times=4),
                     to=c(topl,topr,botl,botr))
  } else {stop("directions must be 4 or 8 or \'bishop\'")}
  
  # Remove all cells that are not target cells, if target is a vector of cells
  if (!is.null(target)) {
    adj<-adj[target,] 
  }
  if (sort){
    #adj <- as.matrix(data.table(adj,key="from"))
    adj<-adj[sort.list(adj[,"from"],method="quick",na.last=NA),]
  }
  
  # Remove the "from" column if pairs is FALSE
  # Good time savings if no intermediate object is created
  if (pairs) {
    return(adj[
      !((((adj[,"to"]-1)%%numCell+1)!=adj[,"to"]) |  #top or bottom of raster
              ((adj[,"from"]%%numCol+adj[,"to"]%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
      ,])
  } else {
    return(adj[
      !((((adj[,"to"]-1)%%numCell+1)!=adj[,"to"]) |  #top or bottom of raster
          ((adj[,"from"]%%numCol+adj[,"to"]%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
      ,2])
  }
  } else {
    if (is.null(numCol) | is.null(numCell)) {
      if (is.null(x)) stop("must provide either numCol & numCell or a x")
      numCol = ncol(x)
      numCell = ncell(x)
    } 
    
    if (directions==8) {
      # determine the indices of the 8 surrounding cells of the cells cells
      topl=as.integer(cells-numCol-1)
      top=as.integer(cells-numCol)
      topr=as.integer(cells-numCol+1)
      lef=as.integer(cells-1)
      rig=as.integer(cells+1)
      botl=as.integer(cells+numCol-1)
      bot=as.integer(cells+numCol)
      botr=as.integer(cells+numCol+1)
      if (include)
        adj=data.table(from=rep.int(cells,times=9),
                       to=c(topl,top,topr,lef,as.integer(cells),rig,botl,bot,botr),key="from")
      else
        adj=data.table(from=rep.int(cells,times=8),
                       to=c(topl,top,topr,lef,rig,botl,bot,botr),key="from")
    } else if (directions==4) {
      # determine the indices of the 4 surrounding cells of the cells cells
      top=as.integer(cells-numCol)
      lef=as.integer(cells-1)
      rig=as.integer(cells+1)
      bot=as.integer(cells+numCol)
      if (include)
        adj=data.table(from=rep.int(cells,times=5),to=c(top,lef,as.integer(cells),rig,bot),key="from")
      else
        adj=data.table(from=rep.int(cells,times=4),to=c(top,lef,rig,bot),key="from")
    } else if (directions=="bishop") {
      topl=as.integer(cells-numCol-1)
      topr=as.integer(cells-numCol+1)
      botl=as.integer(cells+numCol-1)
      botr=as.integer(cells+numCol+1)
      if (include)
        adj=data.table(from=rep.int(cells,times=5),
                       to=c(topl,topr,as.integer(cells),botl,botr),key="from")
      else
        adj=data.table(from=rep.int(cells,times=4),
                       to=c(topl,topr,botl,botr),key="from")
    } else {stop("directions must be 4 or 8 or \'bishop\'")}
    
    # Remove all cells that are not target cells, if target is a vector of cells
    if (!is.null(target)) {
      setkey(adj,to)
      adj<-adj[J(target)] 
      setkey(adj,from)
      setcolorder(adj,c("from","to"))
    }
    
    # Remove the "from" column if pairs is FALSE
    if (!pairs) {
      from=adj$from
      adj[,from:=NULL]
    }
    
    # Good time savings if no intermediate object is created
    if (as.data.table) 
      return(adj[
        i = !((((to-1)%%numCell+1)!=to) |  #top or bottom of raster
                ((from%%numCol+to%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
        ])
    else 
      return(as.matrix(adj[
        i = !((((to-1)%%numCell+1)!=to) |  #top or bottom of raster
                ((from%%numCol+to%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
        ]))
  }
}
