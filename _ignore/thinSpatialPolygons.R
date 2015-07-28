################################################################################
#' Thin method for sp objects
#'
#' This wraps the low level \code{thin} function in the fastshp
#' package for use on \code{sp} classes of spatial objects (specifically
#' \code{SpatialPolygon*} and \code{SpatialLines*})
#'
#' @param spGeom A sp class object.
#'
#' @param tol Numeric. Transfered to \code{tolerance} arg of \code{\link[fastshp]{thin}}
#'
#' @param method Integer. Passed to \code{method} are of  \code{\link[fastshp]{thin}}
#'
#' @param rmSelfIntersection Logical. A crude attempt using \code{gBuffer} to remove self-
#' Intersections. This often helps with creating valid GIS objects. See links below.
#'
#' @param keepSmall Logical. Should "small" (fewer than 4 points after thinning) polygons 
#' or holes be removed or kept. This allows a much smaller object, if desired.
#'
#' @return An object of the same class as the input \code{spGeom}, but
#' with thinned points
#'
#' @seealso \code{\link[fastshp]{thin}}.
#' @seealso \code{\link[rgeos]{gIsValid}}.
#' @export
#' @importFrom fastshp thin
#' @importFrom data.table data.table setkey
#' @docType methods
#' @rdname thin
#' @author Eliot McIntire
setGeneric("thin", function(spGeom, tol=100, method=2L, rmSelfIntersection=TRUE,
                            keepSmall=FALSE) {
  thin(spGeom, tol, method)
})

#' @export
#' @rdname thin
setMethod(
  "thin",
  signature="SpatialPolygons",
  definition=function(spGeom, tol, method, rmSelfIntersection, keepSmall) {
  spGeom@polygons <- lapply(spGeom@polygons, function(i) {
    i@Polygons <- lapply(i@Polygons, function(j) {
      tmp <- fastshp::thin(j@coords[,1],j@coords[,2], method=method, tol=tol)
      if(sum(tmp)<4) {
        if(keepSmall) {
          tmp[sample(which(!tmp), (4-sum(tmp)))] <- TRUE
        } else {
          tmp <- NULL
        }
      } 

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
  
  if(rmSelfIntersection) {
    #spGeom <- gBuffer(spGeom, width = 0, byid = TRUE)
    spGeom <- gSimplify(spGeom, tol=0.00001)
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
#
# hole <- lapply(1:length(sp), function(x) {
#   lapply(sp@polygons[[x]]@Polygons, function(x)
#     x@hole)
# }) %>%
#   unlist
#
# ord <- sp@plotOrder
#
# ordInner <- lapply(1:length(sp), function(x) {
#   sp@polygons[[x]]@plotOrder
# })
#
# xyOrd.l <- lapply(ord, function(i) {
#   xy[[i]][ordInner[[i]]]
# })
#
# idLength <- lapply(xyOrd.l, function(i) { lapply(i, NROW) }) %>%
#   unlist %>%
# #  `/`(., 2) %>%
#   data.table(V1 = .)
#
# idLength2 <- lapply(xyOrd.l, function(i) { sapply(i, NROW) }) %>%
#   lapply(., function(i) rep(1:length(i), i)) %>%
#   unlist %>%
#   data.table(group2=.)
#
# idLength3 <- sapply(1:length(xyOrd.l), function(i) {
#   rep(i, NROW(xyOrd.l[[i]]))}) %>% unlist %>%
#   data.table(group=.)
#
# #   lapply(., function(i) rep(1:length(i), i)) %>%
# #   unlist %>%
# #   data.table(group2=.)
#
# # lapply(xyOrd.l, function(i) sapply(i, NROW) %>% rep(1:length(i), .))
# # idL <- unlist(idLength2)
# # rep(1:NROW(xyOrd.l), ))
#
#
# # spRes <- lapply(1:length(xyOrd.l), function(i) {
# #   Polygons(lapply(xyOrd.l[[i]], Polygon), as.character(i))}) %>%
# #   SpatialPolygons(., 1:length(xyOrd.l))
#
# xyOrd <- do.call(rbind, lapply(xyOrd.l, function(i) { do.call(rbind, i) }))
#
# thinned <- data.table(
#   thin = fastshp::thin(xyOrd[, 1], xyOrd[, 2],
#                        tolerance = speedupScale * speedup,
#                        id=rep(1:length(idLength$V1), idLength$V1))
# )
# thinned[, `:=`(groups= rep(idLength3$group, idLength$V1),
#                groups2=idLength2$group2,
#                #groupd3=cumsum(idLength2$group2),
#                id=rep(1:length(idLength$V1), idLength$V1))]
# idLength <- thinned[, sum(thin),by = list(groups, groups2,id)]
# xyOrd2 <- xyOrd[thinned$thin, ]
#
#
# stopIndex <- cumsum(idLength$V1)
# startIndex <- c(1,cumsum(idLength$V1)+1)
# startIndex <- startIndex[-length(startIndex)]
# #browser()
# a = lapply(1:length(startIndex), function(x){
#   Polygon(matrix(xyOrd[startIndex[x]:stopIndex[x],],ncol=2))
# })
# b = lapply(1:max(idLength$groups), function(x)
#   Polygons(a[idLength[groups==x,id]], as.character(x))) %>%
#   SpatialPolygons(., 1:max(idLength$groups))
# return(invisible(b))
#}

