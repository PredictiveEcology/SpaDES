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
#' @return An object of the same class as the input \code{spGeom}, but
#' with thinned points
#'
#' @seealso \code{\link[fastshp]{thin}}.
#' @export
#' @importFrom fastshp thin
#' @docType methods
#' @rdname thin
#' @author Eliot McIntire
setGeneric("thin", function(spGeom, tol=100, method=2L) {
  thin(spGeom, tol, method)
})

#' @export
#' @rdname thin
setMethod(
  "thin",
  signature="SpatialPolygons",
  definition=function(spGeom, tol, method) {
  spGeom@polygons <- lapply(spGeom@polygons, function(i) {
    i@Polygons <- lapply(i@Polygons, function(j) {
      j@coords <- j@coords[fastshp::thin(j@coords[,1],j@coords[,2], method=method, tol=tol),]
      return(j)
    })
    return(i)
  })

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

