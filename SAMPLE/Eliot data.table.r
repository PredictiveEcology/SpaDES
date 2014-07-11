devtools::load_all("c:/Eliot/GitHub/SpaDES")
library(microbenchmark)
library(raster)
#library(SpaDES)
a = raster(extent(0,1e3,0,1e3),res=1)
cells = sample(1:ncell(a),1e3)
numCol <- ncol(a)
numCell <- ncell(a)
directions=8

  
(mb = microbenchmark(times=100L,
                     out1 = rep.int(cells,4),
                     out
                     
(mb = microbenchmark(times=100L,
  adj.orig = adjacent(a,cells,sort=T,directions=8),
  adj.new = adj(numCol=numCol,numCell=numCell,cells=cells,directions=8),
  adj.new3 <- adj3(numCol=numCol,numCell=numCell,cells=cells,directions=8)
))
print(all.equal(adj.orig,adj.new3))
##############################################################
adj3 <- function(x=NULL,cells,directions=8,pairs=TRUE,numCol=NULL,numCell=NULL) {
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
    adj=data.table(from=rep.int(cells,times=directions),
                   to=c(topl,top,topr,lef,rig,botl,bot,botr),key="from")
  } else if (directions==4) {
    # determine the indices of the 4 surrounding cells of the cells cells
    top=as.integer(cells-numCol)
    lef=as.integer(cells-1)
    rig=as.integer(cells+1)
    bot=as.integer(cells+numCol)
    adj=data.table(from=rep.int(cells,times=4),to=c(top,lef,rig,bot),key="from")
  } else if (directions=="bishop") {
    topl=as.integer(cells-numCol-1)
    topr=as.integer(cells-numCol+1)
    botl=as.integer(cells+numCol-1)
    botr=as.integer(cells+numCol+1)
    adj=data.table(from=rep.int(cells,times=directions),
                   to=c(topl,topr,botl,botr),key="from")
  } else {stop("directions must be 4 or 8 or \'bishop\'")}

  adj[,keep:=!((to%%numCell!=to) |  #top or bottom of raster
                   ((adj$from%%numCol+adj$to%%numCol)==1))]
  #setkey(adj,keep)
  adj.return<-adj[keep==TRUE]
  return(as.matrix(adj.return[,keep:=NULL]# | #right & left edge cells,with neighbours wrapped
    ))
}
