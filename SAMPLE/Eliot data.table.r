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
library(compiler)
enableJIT(3)
#library(SpaDES)
a = raster(extent(0,1e2,0,1e2),res=1)
hab = a#GaussMap(extent(a),speedup=1000)
loci = b = as.integer(sample(1:ncell(a),10))
mask = raster(a)
mask = setValues(mask, 0)
mask[1:5000] <- 1
numCol <- ncol(a)
numCell <- ncell(a)
#cells = 1:numCell
  directions=8

cols = list(brewer.pal(8,"RdYlGn")[8:1],brewer.pal(9,"Blues"),brewer.pal(8,"Spectral"))
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

fire2 <- spread.adj(hab,loci=b,1,0,NULL,1e3,8,1e6,mask=mask)

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

prof <- lineprof(spread.adj(hab,loci=b,0.225,0,NULL,1e6,8,1e2))
shine(prof)

#newPlot();
#dev.set(4)
simplot(stack(fire1,fire2,fire0),speedup=1)
  
(mb = microbenchmark(times=200L,
  adj.orig = adjacent(a,cells,sort=T,directions=8),
  adj.new = adj(numCol=numCol,numCell=numCell,as.data.table=TRUE,cells=cells,directions=8),
  adj.new2 = adj(numCol=numCol,numCell=numCell,as.data.table=FALSE,cells=cells,directions=8)
  #adj.new3 <- adj3(numCol=numCol,numCell=numCell,cells=cells,directions=8),
  #adj.new4 <- adj4(numCol=numCol,numCell=numCell,cells=cells,directions=8)
))
plot(mb,horiz=FALSE)
print(all.equal(adj.orig,adj.new2))

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


##############################################################
spread.adjacent <- function(landscape, loci, spreadProb, persistance,
         mask, maxSize, directions, iterations) {
  ### should sanity check map extents
  is.prob <- function(x) {
    if (!is.numeric(x)) 
      return(FALSE)
    else 
      return(!(x>1 || x<0))
  }
  
  if (is.null(loci))  {
    # start it in the centre cell
    loci <- (landscape@nrows/2 + 0.5) * landscape@ncols
  }
  
  spreads <- setValues(raster(landscape), 0)
  n <- 1
  spreads[loci] <- n
  size <- length(loci)
  
  if (is.null(iterations)) {
    iterations = Inf # this is a stupid way to do this!
  } else {
    # do nothing
  }
  
  while ( (length(loci)>0) && (iterations>=n)  ) {
    #print(paste(n, length(loci)))
    potentials <- adj(landscape, loci, directions)
    
    # drop those ineligible
    if (!is.null(mask)){
      tmp <- extract(mask, potentials[,2])
    } else {
      tmp <- extract(spreads, potentials[,2])
    }
    #print(cbind(potentials,tmp))
    potentials <- potentials[ifelse(is.na(tmp), FALSE, tmp==0),]
    
    # select which potentials actually happened
    # nrow() only works if potentials is an array
    if (is.numeric(spreadProb)) {
      if (is(potentials,"numeric")) {
        ItHappened <- runif(1) <= spreadProb
        events <- potentials[2][ItHappened]
      }
      else {
        ItHappened <- runif(nrow(potentials)) <= spreadProb
        events <- potentials[ItHappened, 2]
      }
    } else {
      stop("Unsupported type:spreadProb") # methods for raster* or function args
    }
    #print(events)
    
    if((size+length(events)) > maxSize) {
      keep<-length(events) - ((size+length(events)) - maxSize)
      events<-events[sample(length(events),keep)]
      
    }
    size <- size + length(unique(events))
    # update eligibility map
    n <- n+1
    spreads[events] <- n
    if(size >= maxSize) {
      events<-NULL
    }
    
    # drop or keep loci
    
    if (is.null(persistance)) {
      loci <- NULL
    } else {
      if (is.prob(persistance)) {
        loci <- loci[runif(length(loci))<=persistance]
      } else {
        # here is were we would handle methods for raster* or functions
        stop("Unsupported type: persistance")
      }
    }
    
    loci <- c(loci, events)
    
  }
  return(spreads)
}

spread.adj <- function(landscape, loci, spreadProb, persistance,
                       mask, maxSize, directions, iterations, plot.it=FALSE) {
  ### should sanity check map extents
  is.prob <- function(x) {
    if (!is.numeric(x)) 
      return(FALSE)
    else 
      return(!(x>1 || x<0))
  }
  
  if (is.null(loci))  {
    # start it in the centre cell
    loci <- (landscape@nrows/2 + 0.5) * landscape@ncols
  }
  
  #spreads <- setValues(raster(landscape), 0)
  spreads <- data.table(ind=1:ncell(landscape),burned=0,key="ind")
  if(!is.null(mask))
    masked<-getValues(mask)==0
  n <- 1
  spreads[loci,burned:=n]
  size <- length(loci)
  #spreads[4,burned:=NA]
  
  if (is.null(iterations)) {
    iterations = Inf # this is a stupid way to do this!
  } else {
    # do nothing
  }
  
  while ( (length(loci)>0) && (iterations>=n) ) {
    #print(paste(n, length(loci)))
    potentials <- adj(landscape, loci, directions,as.data.table=TRUE)
    setkey(potentials,to)
    
    # drop those ineligible
    if (!is.null(mask)){
      potentials <- spreads[masked][potentials][burned==0][,burned:=NULL]
    } else {
      potentials <- spreads[potentials][burned==0][,burned:=NULL]
    }
    
    # select which potentials actually happened
    # nrow() only works if potentials is an array
    if (!is.numeric(spreadProb)) {
    #  ItHappened <- runif(nrow(potentials)) <= spreadProb
    #} else {
      stop("Unsupported type:spreadProb") # methods for raster* or function args
    }

    events <- potentials[runif(nrow(potentials))<=spreadProb,ind]

    # Implement maxSize
    if((size+length(events)) > maxSize) {
      keep<-length(events) - ((size+length(events)) - maxSize)
      events<-events[sample(length(events),keep)]
    }

    size <- size + length(unique(events))

    # update eligibility map

    n <- n+1
    spreads[events,burned:=n]
        
    if(size >= maxSize) {
      events<-NULL
    }

    # drop or keep loci
    if (is.null(persistance)) {
      loci <- NULL
    } else {
      if (is.prob(persistance)) {
        loci <- loci[runif(length(loci))<=persistance]
      } else {
        # here is were we would handle methods for raster* or functions
        stop("Unsupported type: persistance")
      }
    }
    
    loci <- c(loci, events)

    if (plot.it){
      top <- raster(landscape)
      top<-setValues(top,spreads[,burned])
      plot(top)
    }

  }
  #loci = sample(1:ncell(a),20)
  spre=raster(landscape)
  spre<-setValues(spre, spreads[,burned])
  return(spre)
}

spread <- function(landscape, loci, spreadProb, persistance,
                   mask, maxSize, directions, iterations) {
  ### should sanity check map extents
  is.prob <- function(x) {
    if (!is.numeric(x)) 
      return(FALSE)
    else 
      return(!(x>1 || x<0))
  }
  
  if (is.null(loci))  {
    # start it in the centre cell
    loci <- (landscape@nrows/2 + 0.5) * landscape@ncols
  }
  
  spreads <- setValues(raster(landscape), 0)
  n <- 1
  spreads[loci] <- n
  size <- length(loci)
  
  if (is.null(iterations)) {
    iterations = Inf # this is a stupid way to do this!
  } else {
    # do nothing
  }
  
  while ( (length(loci)>0) && (iterations>=n) ) {
    #print(paste(n, length(loci)))
    potentials <- adjacent(landscape, loci, directions)
    
    # drop those ineligible
    if (!is.null(mask)){
      tmp <- extract(mask, potentials[,2])
    } else {
      tmp <- extract(spreads, potentials[,2])
    }
    #print(cbind(potentials,tmp))
    potentials <- potentials[ifelse(is.na(tmp), FALSE, tmp==0),]
    
    # select which potentials actually happened
    # nrow() only works if potentials is an array
    if (is(potentials,"numeric")) {
      ItHappened <- runif(1) <= spreadProb
      events <- potentials[2][ItHappened]
    } else if (is.numeric(spreadProb)) {
      ItHappened <- runif(nrow(potentials)) <= spreadProb
      events <- potentials[ItHappened, 2]
    } else {
      stop("Unsupported type:spreadProb") # methods for raster* or function args
    }
    #print(events)
    
    # update eligibility map
    if((size+length(events)) > maxSize) {
      keep<-length(events) - ((size+length(events)) - maxSize)
      events<-events[sample(length(events),keep)]
    }
    size <- size + length(unique(events))
    # update eligibility map

    spreads[events] <- n
    n <- n+1
    
    if(size >= maxSize) {
      events<-NULL
    }
    
    # drop or keep loci
    
    if (is.null(persistance)) {
      loci <- NULL
    } else {
      if (is.prob(persistance)) {
        loci <- loci[runif(length(loci))<=persistance]
      } else {
        # here is were we would handle methods for raster* or functions
        stop("Unsupported type: persistance")
      }
    }
    
    loci <- c(loci, events)
    
  }
  return(spreads)
}

spread.adj.c <- function(landscape, loci, spreadProb, persistance,
                       mask, maxSize, directions, iterations, plot.it=FALSE) {
  ### should sanity check map extents
  is.prob <- function(x) {
    if (!is.numeric(x)) 
      return(FALSE)
    else 
      return(!(x>1 || x<0))
  }
  
  if (is.null(loci))  {
    # start it in the centre cell
    loci <- (landscape@nrows/2 + 0.5) * landscape@ncols
  }
  
  #spreads <- setValues(raster(landscape), 0)
  spreads <- data.table(ind=1:ncell(landscape),burned=0,key="ind")
  n <- 1
  spreads[loci,burned:=n]
  size <- length(loci)
  #spreads[4,burned:=NA]
  
  if (is.null(iterations)) {
    iterations = Inf # this is a stupid way to do this!
  } else {
    # do nothing
  }
  
  while ( (length(loci)>0) && (iterations>=n) ) {
    #print(paste(n, length(loci)))
    potentials <- adj.c(landscape, loci, directions,as.data.table=TRUE)
    setkey(potentials,to)
    
    # drop those ineligible
    if (!is.null(mask)){
      tmp <- extract(mask, potentials[,to])
    } else {
      #      tmp <- extract(spreads, potentials[,to])
      potentials <- spreads[potentials][burned==0][,burned:=NULL]#,ind,keyby=burned]
      #tmp <- na.omit(tmp[burned==0])
      #setkey(tmp,ind)
      #tmp<-na.omit(tmp)
      
    }
    
    # select which potentials actually happened
    # nrow() only works if potentials is an array
    if (!is.numeric(spreadProb)) {
      #  ItHappened <- runif(nrow(potentials)) <= spreadProb
      #} else {
      stop("Unsupported type:spreadProb") # methods for raster* or function args
    }
    
    events <- potentials[runif(nrow(potentials))<=spreadProb,ind]
    
    if((size+length(events)) > maxSize) {
      keep<-length(events) - ((size+length(events)) - maxSize)
      events<-events[sample(length(events),keep)]
    }
    size <- size + length(unique(events))
    # update eligibility map
    n <- n+1
    spreads[events,burned:=n]
    
    if(size >= maxSize) {
      events<-NULL
    }
    # drop or keep loci
    
    if (is.null(persistance)) {
      loci <- NULL
    } else {
      if (is.prob(persistance)) {
        loci <- loci[runif(length(loci))<=persistance]
      } else {
        # here is were we would handle methods for raster* or functions
        stop("Unsupported type: persistance")
      }
    }
    
    loci <- c(loci, events)
    
    if (plot.it){
      top <- raster(landscape)
      top<-setValues(top,spreads[,burned])
      plot(top)
    }
    
  }
  #loci = sample(1:ncell(a),20)
  spre=raster(landscape)
  spre<-setValues(spre, spreads[,burned])
  return(spre)
}
