devtools::load_all("c:/Eliot/GitHub/SpaDES")
library(microbenchmark)
library(raster)
#library(SpaDES)
a = raster(extent(0,1e2,0,1e2),res=1)
hab = GaussMap(extent(a),speedup = 1.2)
b = sample(1:ncell(a),20)
numCol <- ncol(a)
numCell <- ncell(a)
#cells = 1:numCell
  directions=8

simplot(hab,speedup=1)

(mb <- microbenchmark(times = 300L,
fire1 = spread.adjacent(hab,loci,0.225,0,NULL,1e6,8,1e6),
fire2 = spread.adj(hab,loci,0.225,0,NULL,1e6,8,1e6)
))
simplot(stack(fire1,fire2))
  
(mb = microbenchmark(times=200L,
  adj.orig = adjacent(a,cells,sort=T,directions=8),
  adj.new = adj(numCol=numCol,numCell=numCell,as.data.table=TRUE,cells=cells,directions=8),
  adj.new2 = adj(numCol=numCol,numCell=numCell,as.data.table=FALSE,cells=cells,directions=8)
  #adj.new3 <- adj3(numCol=numCol,numCell=numCell,cells=cells,directions=8),
  #adj.new4 <- adj4(numCol=numCol,numCell=numCell,cells=cells,directions=8)
))
plot(mb,horiz=FALSE)
print(all.equal(adj.orig,adj.new2))
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
    
    # update eligibility map
    spreads[events] <- n
    n <- n+1
    
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
  
  #spreads <- setValues(raster(landscape), 0)
  spreads <- data.table(ind=1:ncell(landscape),burned=0,key="ind")
  n <- 1
  spreads[loci,burned:=n]
  
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
      tmp <- extract(mask, potentials[,to])
    } else {
      tmp <- extract(spreads, potentials[,to])
      tmp <- spreads[potentials]
    }
    #print(cbind(potentials,tmp))
    potentials <- potentials[ifelse(is.na(tmp), FALSE, tmp==0),]
    
    # select which potentials actually happened
    # nrow() only works if potentials is an array
    if (is.numeric(spreadProb)) {
      ItHappened <- runif(nrow(potentials)) <= spreadProb
    } else {
      stop("Unsupported type:spreadProb") # methods for raster* or function args
    }
    events <- potentials[ItHappened, to]
    #print(events)
    
    # update eligibility map
    spreads[events] <- n
    n <- n+1
    
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
  #simplot(spreads)
  #loci = sample(1:ncell(a),20)
  return(spreads)
}
