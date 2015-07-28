################################################################################
#' Thin method for sp objects
#'
#' This wraps the low level \code{thin} function in the fastshp
#' package for use on \code{sp} classes of spatial objects (specifically
#' \code{SpatialPolygon*} and \code{SpatialLines*}).  NOTE: THIS FUNCTION WILL
#' QUALITATIVELY ALTER THE UNDERLYING POLYGONS, i.e., THE POLYGONS WILL NO LONGER 
#' BE A PRECISE REPRESENTATION OF THE ORIGINAL DATA
#'
#' @param spGeom A sp class object.
#'
#' @param tol Numeric. Transfered to \code{tolerance} arg of \code{\link[fastshp]{thin}}
#'
#' @param method Integer. Passed to \code{method} are of  \code{\link[fastshp]{thin}}
#'
#' @param fixErrors Logical. Attempts 3 tests for common errors and tries to fix them. This
#' dramatically slows down the function. See Details.
#'
#' @param keepSmall Logical. Should "small" (fewer than 4 points after thinning) polygons 
#' or holes be removed or kept. This allows a much smaller object, if desired.
#' 
#' @param verbose Logical. Print simple messages indicating what is being done.
#'
#' @return An object of the same class as the input \code{spGeom}, but
#' with thinned points
#' 
#' @details fixErrors currently tries to catch "orphaned holes", "Self-intersection". It uses the following:
#'   - orphaned holes: maptools::checkPolygonsHoles()
#'   - self-intersection polygons: rgeos::gSimplify(sp, tol=0.0001)
#'   
#' The function internally prevents individual \code{polygon} elements of fewer than 4 points.
#' These create errors. If \code{keepSmall} is FALSE (the default), then these are removed; 
#' otherwise, they are kept, but a random selection of 4 of the original polygon points (i.e., 
#' pre-thinning) are selected.
#' 
#' Furthermore, the function keeps "first and last" points in each \code{polygon} to maintain 
#' valid polygons
#' 
#'
#' @seealso \code{\link[fastshp]{thin}}.
#' @seealso \code{\link[rgeos]{gIsValid}}.
#' @export
#' @importFrom fastshp thin
#' @importFrom data.table data.table setkey
#' @testthat expect_error
#' @testthat maptools checkPolygonsHoles
#' @docType methods
#' @rdname thin
#' @author Eliot McIntire
setGeneric("thin", function(spGeom, tol=100, method=2L, fixErrors=TRUE,
                            keepSmall=FALSE, verbose=FALSE) {
  thin(spGeom, tol, method)
})

#' @export
#' @rdname thin
setMethod(
  "thin",
  signature="SpatialPolygons",
  definition=function(spGeom, tol, method, fixErrors, keepSmall, verbose) {

    if(verbose) print("Thinning Spatial Polygons")
      
    spGeom@polygons <- lapply(spGeom@polygons, function(i) {
    i@Polygons <- lapply(i@Polygons, function(j) {
      tmp <- fastshp::thin(j@coords[,1],j@coords[,2], method=method, tol=tol)
      # if resulting polygon has 4 points or fewer, either remove it or
      #  keep exactly 4 points. Fewer than 4 points leads to imcomplete polygon
      if(sum(tmp)<4) {
        if(keepSmall) {
          tmp[sample(which(!tmp), (4-sum(tmp)))] <- TRUE
        } else {
          tmp <- NULL
        }
      } 

      # This maintains first and last points in a polygon or else it is no longer
      #  a polygon
      if(!is.null(tmp)) {
        tmp[1] <- TRUE
        tmp[length(tmp)] <- TRUE
        j@coords <- j@coords[tmp,]
      } else {
        j <- tmp
      }
      
      return(j)
    })
    i@Polygons <- i@Polygons[!sapply(i@Polygons, is.null)]
    if(length(i@Polygons)<=1){
      comment(i) <- "0"
    } else {
      comment(i) <- paste(c(0,rep(1,length(i@Polygons)-1)), collapse=" ")
    }
    if(length(i@Polygons)==0) i <- NULL
    return(i)
  })
  keep <- sapply(spGeom@polygons, length)>0
  spGeom@polygons <- spGeom@polygons[keep]
  spGeom@data <- spGeom@data[keep,]
  objOrder <- data.table(order=spGeom@plotOrder[keep],reOrder=1:sum(keep))
  setkey(objOrder, order)
  objOrder[,order:=1:sum(keep)]
  setkey(objOrder, reOrder)
  spGeom@plotOrder <- objOrder$order
  
  
  
  if(fixErrors){
    ## Get rid of "rgeos_PolyCreateComment: orphaned hole, cannot find containing polygon for hole at index"
    if(suppressWarnings(throws_error("orphaned")(gIsValid(spGeom))[[1]])) {
      if(verbose) print("Fixing orphaned holes")
      slot(spGeom, "polygons") <- lapply(slot(spGeom, "polygons"), checkPolygonsHoles)
    }
    
    # Get rid of self-intersection: gSimplify with a very small tolerance
    if(grepl(evaluate_promise(gIsValid(spGeom))$warnings, 
          pattern="Self-intersection")) {
      if(verbose) print("Fixing Self-intersections")
      data <- spGeom@data
      spGeom <- gSimplify(spGeom, tol = 0.0001)
      #gSimplify removes data
      spGeom <- SpatialPolygonsDataFrame(spGeom, data=data)
      
    }
    
    # gBuffer can fix some problems but it also generates problems, 
    #  specifically, it will delete lines randomly. So, it is not used for now.
    #    if() {
          #spGeom <- gBuffer(spGeom, width = 0, byid = TRUE)
    #    }
    
  }
  return(spGeom)
})

#' @export
#' @rdname thin
setMethod(
  "thin",
  signature="SpatialLines",
  definition=function(spGeom, tol, method) {
    spGeom@lines <- lapply(spGeom@lines, function(i) {
      i@Lines <- lapply(i@Lines, function(j) {
        tmp <- fastshp::thin(j@coords[,1],j@coords[,2], method=method, tol=tol)
        tmp[c(1,length(tmp))] <- TRUE
        j@coords <-   j@coords[tmp,]
        return(j)
      })
      return(i)
    })

    return(spGeom)
  })
