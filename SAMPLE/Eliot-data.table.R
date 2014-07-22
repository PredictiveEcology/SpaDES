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
a = raster(extent(0,1.5e3,0,1.5e3),res=1)
hab = GaussMap(a,speedup=10)
names(hab)="hab"
cells = loci = b = as.integer(sample(1:ncell(a),1e1))
mask = raster(a)
mask = setValues(mask, 0)
mask[1:5000] <- 1
numCol <- ncol(a)
numCell <- ncell(a)
directions=8

# Transparency involves putting 2 more hex digits on the color code, 00 is fully transparent
cols = list(c("#00000000",brewer.pal(8,"RdYlGn")[8:1]),brewer.pal(9,"Greys"),brewer.pal(8,"Spectral"))

newPlot()
simPlot(hab,col=cols[[1]],speedup=10)
#names(hab)<-"hab"
fire2 <- spread(hab,loci=as.integer(sample(1:ncell(hab),10)),
                0.235,0,NULL,1e6,8,1e6,
                plot.it=F,col=cols[[1]],delete.previous=F,
                speedup=10)

names(fire2)<-"fire"
simPlot(fire2,speedup=4,col=cols[[1]])
simPlot(fire2,add=T,on.which.to.plot="hab",delete.previous=F,speedup=10)

simPlot(stack(fire2,hab),col=cols[1:2],speedup=4)
simPlot(fire2,col=cols[[1]],speedup=10,add=T,on.which.to.plot="hab",delete.previous=F)
simPlot(fire2,col=cols[[1]],speedup=10,add=T,on.which.to.plot="fire",delete.previous=F)


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
simplot(stack(fire1,fire2,fire0),speedup=1)
  
mb1 = list()
i = 0:3
for (ind in i) {
  numCells = 10^ind
  cells = sample(numCell,numCells)
(mb1[[ind+1]] = microbenchmark(times=3L,
  adj.orig <- adjacent(a,cells,sort=T,directions=8,include=F,pairs = T),
  adj.new4 <- adj(numCol=numCol,numCell=numCell,sort=T,cutoff = 1e4,#,as.data.table=T,
                   cells=cells,directions=8,pairs=T,include=F,match.adjacent=F),
  adj.new4.1 <- adj(numCol=numCol,numCell=numCell,sort=T,cutoff = 1e4,#,as.data.table=T,
                   cells=cells,directions=8,pairs=T,include=F,match.adjacent=T)
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
#print(mb1)

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








##############################################################
adj4 <- function(x=NULL,cells,directions=8,sort=FALSE,pairs=TRUE,include=FALSE,target=NULL,
                numCol=NULL,numCell=NULL,match.adjacent=FALSE,cutoff.for.data.table = 1e4){
  if ((length(cells)<cutoff.for.data.table)) {
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
      if (match.adjacent){
        if (include){
          adj=cbind(from=rep.int(cells,times=9),
                    to=c(as.integer(cells),topl,lef,botl,topr,rig,botr,top,bot))
        }else{
          adj=cbind(from=rep.int(cells,times=8),
                    to=c(topl,lef,botl,topr,rig,botr,top,bot))
        }
      } else {
        if (include){
          adj=cbind(from=rep.int(cells,times=9),
                    to=c(topl,top,topr,lef,as.integer(cells),rig,botl,bot,botr))
        }else{
          adj=cbind(from=rep.int(cells,times=8),
                    to=c(topl,top,topr,lef,rig,botl,bot,botr))
        }
    }
    } else if (directions==4) {
      # determine the indices of the 4 surrounding cells of the cells cells
      top=as.integer(cells-numCol)
      lef=as.integer(cells-1)
      rig=as.integer(cells+1)
      bot=as.integer(cells+numCol)
      if (match.adjacent){
        if (include)
          adj=cbind(from=rep.int(cells,times=5),to=c(as.integer(cells),lef,rig,top,bot))
        else
          adj=cbind(from=rep.int(cells,times=4),to=c(lef,rig,top,bot))
      } else {
        if (include)
          adj=cbind(from=rep.int(cells,times=5),to=c(top,lef,as.integer(cells),rig,bot))
        else
          adj=cbind(from=rep.int(cells,times=4),to=c(top,lef,rig,bot))
      }
    } else if (directions=="bishop") {
      topl=as.integer(cells-numCol-1)
      topr=as.integer(cells-numCol+1)
      botl=as.integer(cells+numCol-1)
      botr=as.integer(cells+numCol+1)
      if (match.adjacent) {
        if (include)
          adj=cbind(from=rep.int(cells,times=5),
                    to=c(as.integer(cells),topl,botl,topr,botr))
        else
          adj=cbind(from=rep.int(cells,times=4),
                    to=c(topl,botl,topr,botr))
      } else {
        if (include)
          adj=cbind(from=rep.int(cells,times=5),
                    to=c(topl,topr,as.integer(cells),botl,botr))
        else
          adj=cbind(from=rep.int(cells,times=4),
                    to=c(topl,topr,botl,botr))
      }
    } else {stop("directions must be 4 or 8 or \'bishop\'")}
    
    # Remove all cells that are not target cells, if target is a vector of cells
    if (!is.null(target)) {
      adj<-adj[target,] 
    }
    
    if (sort){
      if (match.adjacent)
        adj<-adj[order(adj[,"from"],adj[,"to"]),]
      else 
        adj<-adj[order(adj[,"from"]),]
        #adj<-adj[sort.list(adj[,"from"],method="shell",na.last=NA),]
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
    
    
    #### THIS IS FOR SITUATIONS WHERE length(cells) is > 1e4; using data.table
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
      if (match.adjacent) {
        if (include)
          adj=data.table(from=rep.int(cells,times=9),
                         to=c(as.integer(cells),topl,lef,botl,topr,rig,botr,top,bot))
        else
          adj=data.table(from=rep.int(cells,times=8),
                         to=c(topl,lef,botl,topr,rig,botr,top,bot))
      } else {
        if (include)
          adj=data.table(from=rep.int(cells,times=9),
                         to=c(topl,top,topr,lef,as.integer(cells),rig,botl,bot,botr),key="from")
        else
          adj=data.table(from=rep.int(cells,times=8),
                         to=c(topl,top,topr,lef,rig,botl,bot,botr),key="from")
      }
    } else if (directions==4) {
      # determine the indices of the 4 surrounding cells of the cells cells
      top=as.integer(cells-numCol)
      lef=as.integer(cells-1)
      rig=as.integer(cells+1)
      bot=as.integer(cells+numCol)
      if (match.adjacent) {
        if (include)
          adj=data.table(from=rep.int(cells,times=5),to=c(as.integer(cells),lef,rig,top,bot))
        else
          adj=data.table(from=rep.int(cells,times=4),to=c(lef,rig,top,bot))
      } else {
        if (include)
          adj=data.table(from=rep.int(cells,times=5),to=c(top,lef,as.integer(cells),rig,bot),key="from")
        else
          adj=data.table(from=rep.int(cells,times=4),to=c(top,lef,rig,bot),key="from")
      }
    } else if (directions=="bishop") {
      topl=as.integer(cells-numCol-1)
      topr=as.integer(cells-numCol+1)
      botl=as.integer(cells+numCol-1)
      botr=as.integer(cells+numCol+1)
      if (match.adjacent) {
        if (include)
          adj=data.table(from=rep.int(cells,times=5),
                         to=c(as.integer(cells),topl,botl,topr,botr))
        else
          adj=data.table(from=rep.int(cells,times=4),
                         to=c(topl,botl,topr,botr))
      } else {
        if (include)
          adj=data.table(from=rep.int(cells,times=5),
                         to=c(topl,topr,as.integer(cells),botl,botr),key="from")
        else
          adj=data.table(from=rep.int(cells,times=4),
                         to=c(topl,topr,botl,botr),key="from")
      }
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
#      return(adj[
#        !((((adj[,to]-1)%%numCell+1)!=adj[,to]) |  #top or bottom of raster
#            ((adj[,from]%%numCol+adj[,to]%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
#        ,])
    
      return(as.matrix(adj[
        i = !((((to-1)%%numCell+1)!=to) |  #top or bottom of raster
                ((from%%numCol+to%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
        ]))
  }
}


###############################################################
###############################################################
###############################################################
###############################################################
# Current one in neighbourhood.R
adj2 <- function(x=NULL,cells,directions=8,sort=FALSE,pairs=TRUE,include=FALSE,target=NULL,
                numCol=NULL,numCell=NULL,as.data.table=FALSE,match.adjacent=TRUE) {

  # use data.table if requested, or if the number of cells is greater than 1e4
  use = c("data.table","cbind")[((length(cells)<1e4) & (as.data.table==FALSE))+1]

  # Calculate numCol and numCell if not provided
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
    if (match.adjacent){
      if (include){
        adj=get(use)(from=rep.int(cells,times=9),
                      to=c(as.integer(cells),topl,lef,botl,topr,rig,botr,top,bot))
      } else {
        adj=get(use)(from=rep.int(cells,times=8),
                to=c(topl,lef,botl,topr,rig,botr,top,bot))
      } 
    } else {
      if (include){
        adj=get(use)(from=rep.int(cells,times=9),
                     to=c(topl,top,topr,lef,as.integer(cells),rig,botl,bot,botr))
      } else {
        adj=get(use)(from=rep.int(cells,times=8),
                     to=c(topl,top,topr,lef,rig,botl,bot,botr))
      } 
    }
    
      
      
  } else if (directions==4) {
    # determine the indices of the 4 surrounding cells of the cells cells
    top=as.integer(cells-numCol)
    lef=as.integer(cells-1)
    rig=as.integer(cells+1)
    bot=as.integer(cells+numCol)
    if (match.adjacent) {
      if (include)
        adj=get(use)(from=rep.int(cells,times=5),
                  to=c(as.integer(cells),lef,rig,top,bot))
      else
        adj=get(use)(from=rep.int(cells,times=4),to=c(lef,rig,top,bot))
    } else {
      if (include)
        adj=get(use)(from=rep.int(cells,times=5),
                     to=c(top,lef,as.integer(cells),rig,bot))
      else
        adj=get(use)(from=rep.int(cells,times=4),to=c(top,lef,rig,bot))
    }
  } else if (directions=="bishop") {
    topl=as.integer(cells-numCol-1)
    topr=as.integer(cells-numCol+1)
    botl=as.integer(cells+numCol-1)
    botr=as.integer(cells+numCol+1)
    if (match.adjacent) {
      if (include)
        adj=get(use)(from=rep.int(cells,times=5),
                  to=c(as.integer(cells),topl,botl,topr,botr))
      else
        adj=get(use)(from=rep.int(cells,times=4),
                  to=c(topl,botl,topr,botr))
    } else {
      if (include)
        adj=get(use)(from=rep.int(cells,times=5),
                     to=c(topl,topr,as.integer(cells),botl,botr))
      else
        adj=get(use)(from=rep.int(cells,times=4),
                     to=c(topl,topr,botl,botr))
    }
  } else {stop("directions must be 4 or 8 or \'bishop\'")}
  
  # Remove all cells that are not target cells, if target is a vector of cells
  if (!is.null(target)) {
    if(use=="data.table") {
      setkey(adj,"to")
      adj<-adj[J(target),] 
    } else {
      adj<-adj[which(!is.na(match(adj[,"to"],target))),] 
    }
  }
  if (sort){
    if (use=="data.table") {
      if (match.adjacent)
        setkey(adj,from,to)
      else
        setkey(adj,from)
    } else {
      if (match.adjacent)
        adj<-adj[order(adj[,"from"],adj[,"to"]),]
      else 
        adj<-adj[order(adj[,"from"]),]
    }
  }
  
  # Remove the "from" column if pairs is FALSE
  # Good time savings if no intermediate object is created
  if (pairs) pair.cols = 1:2 else pair.cols = 2
  if (as.data.table==TRUE) {
    return(adj[
      !((((adj[,to]-1)%%numCell+1)!=adj[,to]) |  #top or bottom of raster
          ((adj[,from]%%numCol+adj[,to]%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
      ,pair.cols,with=FALSE])
  } else {
    if(is(adj,"data.table"))
      return(as.matrix(adj[
        !((((adj[,"to"]-1)%%numCell+1)!=adj[,"to"]) |  #top or bottom of raster
            ((adj[,"from"]%%numCol+adj[,"to"]%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
              ,pair.cols]))
    else 
      return(adj[
        !((((adj[,"to"]-1)%%numCell+1)!=adj[,"to"]) |  #top or bottom of raster
            ((adj[,"from"]%%numCol+adj[,"to"]%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
        ,pair.cols])
    
    
  }
}
###############################################################
###############################################################
###############################################################
###############################################################
# Current one in neighbourhood.R
adj1 <- function(x=NULL,cells,directions=8,sort=FALSE,pairs=TRUE,include=FALSE,target=NULL,
                numCol=NULL,numCell=NULL,as.data.table=FALSE,match.adjacent=TRUE) {
  
  # use data.table if requested, or if the number of cells is greater than 1e4
  use = c("data.table","cbind")[((length(cells)<1e4) & (as.data.table==FALSE))+1]
  
  # Calculate numCol and numCell if not provided
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
    if (match.adjacent){
      if (include){
        adj=get(use)(from=rep.int(cells,times=9),
                     to=c(as.integer(cells),topl,bb,lef,botl,topr,rig,botr,top,bot))
      } else {
        adj=get(use)(from=rep.int(cells,times=8),
                     to=c(topl,lef,botl,topr,bb,rig,botr,top,bot))
      } 
    } else {
      if (include){
        adj=get(use)(from=rep.int(cells,times=9),
                     to=c(topl,top,topr,lef,bb,as.integer(cells),rig,botl,bot,botr))
      } else {
        adj=data.table(from=rep.int(cells,times=8),
                     to=c(topl,top,topr,lef,rig,botl,bot,botr),key="from")
      } 
    }
    
    
    
  } else if (directions==4) {
    # determine the indices of the 4 surrounding cells of the cells cells
    top=as.integer(cells-numCol)
    lef=as.integer(cells-1)
    rig=as.integer(cells+1)
    bot=as.integer(cells+numCol)
    if (match.adjacent) {
      if (include)
        adj=get(use)(from=rep.int(cells,times=5),
                     to=c(as.integer(cells),lef,rig,top,bot))
      else
        adj=get(use)(from=rep.int(cells,times=4),to=c(lef,rig,top,bot))
    } else {
      if (include)
        adj=get(use)(from=rep.int(cells,times=5),
                     to=c(top,lef,as.integer(cells),rig,bot))
      else
        adj=get(use)(from=rep.int(cells,times=4),to=c(top,lef,rig,bot))
    }
  } else if (directions=="bishop") {
    topl=as.integer(cells-numCol-1)
    topr=as.integer(cells-numCol+1)
    botl=as.integer(cells+numCol-1)
    botr=as.integer(cells+numCol+1)
    if (match.adjacent) {
      if (include)
        adj=get(use)(from=rep.int(cells,times=5),
                     to=c(as.integer(cells),topl,botl,topr,botr))
      else
        adj=get(use)(from=rep.int(cells,times=4),
                     to=c(topl,botl,topr,botr))
    } else {
      if (include)
        adj=get(use)(from=rep.int(cells,times=5),
                     to=c(topl,topr,as.integer(cells),botl,botr))
      else
        adj=get(use)(from=rep.int(cells,times=4),
                     to=c(topl,topr,botl,botr))
    }
  } else {stop("directions must be 4 or 8 or \'bishop\'")}
  
  # Remove all cells that are not target cells, if target is a vector of cells
  if (!is.null(target)) {
    if(use=="data.table") {
      setkey(adj,"to")
      adj<-adj[J(target),] 
    } else {
      adj<-adj[which(!is.na(match(adj[,"to"],target))),] 
    }
  }
#   if (sort){
#     if (use=="data.table") {
#       if (match.adjacent)
#         setkey(adj,from,to)
#       else
#         setkey(adj,from)
#     } else {
#       if (match.adjacent)
#         adj<-adj[order(adj[,"from"],adj[,"to"]),]
#       else 
#         adj<-adj[order(adj[,"from"]),]
#     }
#   }
  
  # Remove the "from" column if pairs is FALSE
  # Good time savings if no intermediate object is created
  if (pairs) pair.cols = 1:2 else pair.cols = 2
  if (as.data.table==TRUE) {
    return(adj[
      !((((adj[,to]-1)%%numCell+1)!=adj[,to]) |  #top or bottom of raster
          ((adj[,from]%%numCol+adj[,to]%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
      ,])
  } else {
    if(is(adj,"data.table"))
      return(as.matrix(adj[
        !((((adj[,"to"]-1)%%numCell+1)!=adj[,"to"]) |  #top or bottom of raster
            ((adj[,"from"]%%numCol+adj[,"to"]%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
        ,pair.cols]))
    else 
      return(adj[
        !((((adj[,"to"]-1)%%numCell+1)!=adj[,"to"]) |  #top or bottom of raster
            ((adj[,"from"]%%numCol+adj[,"to"]%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
        ,pair.cols])
    
    
  }
}
