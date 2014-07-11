##############################################################
#' Fast Adjacent function
#'
#' Faster function for determining the cells of the 4, 8 or bishop
#'  neighbours of the \code{cells}
#' 
#' About 10x speed gains over \code{adjacent} in raster package. There is some extra 
#' speed gain if NumCol and NumCells are passed rather than a raster. 
#' Efficiency gains come from 
#'  1. use data.table internally
#'   - no need to remove NAs because wrapped or outside points are
#'     just removed directly with data.table
#'   - use data.table to sort and fast select (though not fastest possible)   
#'  2. don't make intermediate objects; just put calculation into return statement
#'  
#' The steps used in the algorithm are:
#' 1. Calculate indices of neighbouring cells
#' 2. Remove "to" cells that are 
#'    - <1 or >numCells (i.e., they are above or below raster)
#'    - where the modulo of "to" cells is equal to 1 if "from" cells are 0 (wrapped right to left)
#'    - or where the modulo of the "to" cells is equal to 0 if "from" cells are 1 (wrapped left to right)
#' 
#' @param x Raster* object for which adjacency will be calculated.
#' 
#' @param cells vector of cell numbers for which adjacent cells should be found. Cell numbers start with 1 in the upper-left corner and increase from left to right and from top to bottom
#' 
#' @param directions the number of directions in which cells should be connected: 4 (rook's case), 8 (queen's case), or 'bishop' to connect cells with one-cell diagonal moves. Or a neigborhood matrix (see Details) 
#' 
#' @param pairs logical. If TRUE, a matrix of pairs of adjacent cells is returned. If FALSE, a vector of cells adjacent to cells is returned
#' 
#' @param include logical. Should the focal cells be included in the result?
#' 
#' @param numCol numeric indicating number of columns in the raster. Using this with numCell is a bit faster execution time.
#' 
#' @param numCell numeric indicating number of cells in the raster. Using this with numCol is a bit faster execution time.
#' 
#' @param as.data.table logical. Should return be a data.table (or matrix to replicate \code{adjacent}). 
#' Default is FALSE to replicate \code{adjacent} behaviour, though TRUE is faster, and has also id (as \code{adjacent})
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
          adj=data.table(from=rep.int(cells,times=directions+include),
                       to=c(topl,top,topr,lef,as.integer(cells),rig,botl,bot,botr),key="from")
        else
          adj=data.table(from=rep.int(cells,times=directions+include),
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
    
    if (!is.null(target)) {
      setkey(adj,to)
      adj<-adj[J(target)] 
      setkey(adj,from)
      setcolorder(adj,c("from","to"))
    }
    if (!pairs) {
      from=adj$from
      adj[,from:=NULL]
    }
    
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
