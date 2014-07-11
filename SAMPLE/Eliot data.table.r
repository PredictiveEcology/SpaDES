devtools::load_all("c:/Eliot/GitHub/SpaDES")
library(microbenchmark)
library(raster)
#library(SpaDES)
a = raster(extent(0,1e3,0,1e3),res=1)
cells = sample(1:ncell(a),1e3)
numCol <- ncol(a)
numCell <- ncell(a)
#cells = 1:numCell
  directions=8

  
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
adj4 <- function(x=NULL,cells,directions=8,pairs=TRUE,include=FALSE,target=NULL,
                  numCol=NULL,numCell=NULL,as.data.table=FALSE) {
    if (is.null(numCol) | is.null(numCell)) {
      if (is.null(x)) stop("must provide either numCol & numCell or a x")
      numCol = ncol(x)
      numCell = ncell(x)
    } 
    if (numCell < 1e3)
    cells = sort(cells,method="quick")
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
        if (numCell < 1e3) {
           adj = matrix(c(rep.int(cells,times=directions+include),topl,top,topr,lef,as.integer(cells),rig,botl,bot,botr),
                        ncol=2,byrow=FALSE,dimnames=list(NULL,c("from","to"))) 
        } else {
           adj = data.table(from=rep.int(cells,times=directions+include),
                       to=c(topl,top,topr,lef,as.integer(cells),rig,botl,bot,botr),key="from")
        }
      else
        if (numCell<1e3) {
          adj = matrix(c(rep.int(cells,times=directions),topl,top,topr,lef,rig,botl,bot,botr),
                        ncol=2,byrow=FALSE,dimnames=list(NULL,c("from","to")))
        } else {
         adj=data.table(from=rep.int(cells,times=directions+include),
                        to=c(topl,top,topr,lef,rig,botl,bot,botr),key="from")
        }
    } else if (directions==4) {
      # determine the indices of the 4 surrounding cells of the cells cells
      top=as.integer(cells-numCol)
      lef=as.integer(cells-1)
      rig=as.integer(cells+1)
      bot=as.integer(cells+numCol)
      if (include)
        if (numCell < 1e3) {
          adj = matrix(c(rep.int(cells,times=directions+1),top,lef,as.integer(cells),rig,bot),
                       ncol=2,byrow=FALSE,dimnames=list(NULL,c("from","to"))) 
        } else {
          adj=data.table(from=rep.int(cells,times=directions+1),to=c(top,lef,as.integer(cells),rig,bot),key="from")
        }
      else
        if (numCell < 1e3) {
          adj = matrix(c(rep.int(cells,times=directions),top,lef,rig,bot),
                       ncol=2,byrow=FALSE,dimnames=list(NULL,c("from","to"))) 
        } else {
          adj=data.table(from=rep.int(cells,times=directions),to=c(top,lef,rig,bot),key="from")
        }
    } else if (directions=="bishop") {
      topl=as.integer(cells-numCol-1)
      topr=as.integer(cells-numCol+1)
      botl=as.integer(cells+numCol-1)
      botr=as.integer(cells+numCol+1)
      if (include)
        if (numCell < 1e3) {
          adj = matrix(c(rep.int(cells,times=5),top,lef,as.integer(cells),rig,bot),
                       ncol=2,byrow=FALSE,dimnames=list(NULL,c("from","to"))) 
        } else {
          adj=data.table(from=rep.int(cells,times=5),
                       to=c(topl,topr,as.integer(cells),botl,botr),key="from")
        }
      else
        if (numCell < 1e3) {
          adj = matrix(c(rep.int(cells,times=4),top,lef,rig,bot),
                       ncol=2,byrow=FALSE,dimnames=list(NULL,c("from","to"))) 
        } else {
          adj=data.table(from=rep.int(cells,times=4),
                       to=c(topl,topr,botl,botr),key="from")
        }
    } else {stop("directions must be 4 or 8 or \'bishop\'")}
    
    if (numCell>=1e3) {
      if (!is.null(target)) {
        setkey(adj,to)
        adj<-adj[J(target)] 
        setkey(adj,from)
        setcolorder(adj,c("from","to"))
      }
    }

    if (!pairs) {
      from=adj$from
      adj[,from:=NULL]
    }
    if (numCell<1e3) {
      adj = adj[!((adj[,"to"]%%numCell!=adj[,"to"]) |  #top or bottom of raster
        ((adj[,"from"]%%numCol+adj[,"to"]%%numCol)==1)),]
      return(adj[order(adj.return[,"from"]),])
    } else {
     if (as.data.table) 
       return(adj[
         i = !((to%%numCell!=to) |  #top or bottom of raster
                 ((from%%numCol+to%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
         ])
     else 
       return(as.matrix(adj[
         i = !((to%%numCell!=to) |  #top or bottom of raster
                 ((from%%numCol+to%%numCol)==1))# | #right & left edge cells,with neighbours wrapped
         ]))
    }
  }
  
#   adj[,keep:=!((to%%numCell!=to) |  #top or bottom of raster
#                    ((adj$from%%numCol+adj$to%%numCol)==1))]
#   #setkey(adj,keep)
#   adj.return<-adj[keep==TRUE]
#   return(as.matrix(adj.return[,keep:=NULL]# | #right & left edge cells,with neighbours wrapped
