library(microbenchmark)
library(raster)
library(SpaDES)
a = raster(extent(0,1e3,0,1e3),res=1)
cells = sample(1:ncell(a),1e3)
numCol <- ncol(a)
numCell <- ncell(a)

  

(mb = microbenchmark(times=50L,
  adj.orig = adjacent(a,cells,sort=T,directions=8),
  adj.new = adj(numCol=numCol,numCell=numCell,cells=cells,directions=8),
#  adj.new1 = adj1(numCol=numCol,numCell=numCell,cells=cells,directions=8),
#  adj.new2 = adj2(numCol=numCol,numCell=numCell,cells=cells,directions=8),
  adj.new3 = adj3(numCol=numCol,numCell=numCell,cells=cells,directions=8)
))

##############################################################
#' Fast Adjacent function
#'
#' Faster function for determining the cells of the 4 or 8 neighbours of the \code{cells}
#' 
#' There is some speed gain if NumCol and NumCells are passed rather than a raster. 
#' This gain is on the order of 5%-10%. The fastest gain over \code{\link{adjacent}}
#' is from the use of data.table pass by reference feature.
#' 
#' @param x Raster* object for which adjacency will be calculated.
#' 
#' @param cells vector of cell numbers for which adjacent cells should be found. Cell numbers start with 1 in the upper-left corner and increase from left to right and from top to bottom
#' 
#' @param directions the number of directions in which cells should be connected: 4 (rook's case), 8 (queen's case), or 'bishop' to connect cells with one-cell diagonal moves. Or a neigborhood matrix (see Details) 
#' 
#' @param pairs logical. If TRUE, a matrix of pairs of adjacent cells is returned. If FALSE, a vector of cells adjacent to cells is returned
#' 
#' @param numCol numeric indicating number of columns in the raster. Using this with numCell is a bit faster execution time.
#' 
#' @param numCell numeric indicating number of cells in the raster. Using this with numCol is a bit faster execution time.
#' 
#' @return a matrix of one or two columns, from and to.
#' 
#' @seealso \code{\link{adjacent}}
#' 
#' @import data.table
#' @export
#' @docType methods
#' @rdname adj
#'
#' @examples
#' require(raster)
#' a <- raster(extent(0,1000,0,1000),res=1)
#' sam = sample(1:length(a),1e4)
#' numCol <- ncol(a)
#' numCell <- ncell(a)
#' adj.new <- adj(numCol=numCol,numCell=numCell,sam,directions=8)
#' print(head(adj.new))
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
    adj=data.table(from=rep.int(cells,times=directions),to=c(topl,top,topr,lef,rig,botl,bot,botr),key="to")
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
    adj=data.table(from=rep.int(cells,times=directions),to=c(topl,topr,botl,botr),key="from")
  } else {stop("directions must be 4 or 8 or \'bishop\'")}
  
  # Add 2 columns that show whether the from or to cell is one of the far
  #  right or far left cells in x
  adj[,`:=`(from.mod.numCol=from%%numCol,to.mod.numCol=to%%numCol)]
  
#  to.mod.numCol=adj$to%%numCol
#  from.mod.numCol=adj$from%%numCol
  
#   first = adj$to<1
#   second = adj$to>numCell
#   third = (from.mod.numCol==0 & to.mod.numCol==1)
#   fourth = (from.mod.numCol==1 & to.mod.numCol==0)
#   dt <- data.table(first,second,third, fourth,key=c("first","second","third","fourth"))
setkey(adj,to,from.mod.numCol,to.mod.numCol)  
  # remove any cell that is outside extent or wrapped (above, below, right or left)
#  adj.return <- adj[!(first | second | third | fourth)]
  adj.return <- adj[
    !J(unique(to),0,1)][!J(unique(to),1,0)][J(unique(to))]
#    ]#,     #above top row
#            numCell:(numCell+numCol)))#,  # below bottom row
#            !CJ(unique(to),0,1)#) #right edge cell,with neighbours wrapped
#            c(NULL,NULL,1,0))  #left edge cell,with neighbours wrapped
    #]
#setkey(adj,from.mod.numCol,to.mod.numCol)  
#adj[!J(0,1)]
  
  #remove unwanted temporary columns by reference, based on pairs
  #     if (pairs) {
  #         adj.return[,`:=`(from.mod.numCol=NULL,to.mod.numCol=NULL)]
  #     } else {
  #         adj.return[,`:=`(from=NULL,from.mod.numCol=NULL,to.mod.numCol=NULL)]
  #     }
  
  return(as.matrix(adj.return))
}
