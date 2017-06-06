#' @details \code{mergeRaster} differs from \code{merge} in how overlapping tile
#' regions are handled: \code{merge} retains the values of the first raster in
#' the list. This has the consequence of retaining the values from the buffered
#' region in the first tile in place of the values from the neighbouring tile.
#' On the other hand, \code{mergeRaster} retains the values of the tile region,
#' over the values in any buffered regions. This is useful for reducing edge
#' effects when performing raster operations involving contagious processes.
#' To use the average of cell values, or do another computation, use
#' \code{\link[raster]{mosaic}}.
#'
#' @param x    A list of split raster tiles (i.e., from \code{splitRaster}).
#'
#' @return \code{mergeRaster} returns a \code{RasterLayer} object.
#'
#' @seealso \code{\link[raster]{merge}}, \code{\link[raster]{mosaic}}
#'
# igraph exports %>% from magrittr
#' @importFrom raster crop extent merge
#' @export
#' @docType methods
#' @rdname splitRaster
#'
#' @author Yong Luo and Alex Chubaty
#'
setGeneric("mergeRaster", function(x) {
  standardGeneric("mergeRaster")
})

#' @export
#' @rdname splitRaster
setMethod(
  "mergeRaster",
  signature = signature(x = "list"),
  definition = function(x) {
    xminExtent <- sapply(x, xmin) %>% unique() %>% sort()
    xmaxExtent <- sapply(x, xmax) %>% unique() %>% sort()
    yminExtent <- sapply(x, ymin) %>% unique() %>% sort()
    ymaxExtent <- sapply(x, ymax) %>% unique() %>% sort()
    xBuffer <- unique( (xmaxExtent[-length(xmaxExtent)] - xminExtent[-1]) / 2)
    yBuffer <- unique( (ymaxExtent[-length(ymaxExtent)] - yminExtent[-1]) / 2)

    for (i in seq_along(x)) {
      r <- x[[i]]
      if (xmin(r) != min(xminExtent)) {
        xminCut <- xmin(r) + xBuffer
      } else {
        xminCut <- xmin(r)
      }
      if (xmax(r) != max(xmaxExtent)) {
        xmaxCut <- xmax(r) - xBuffer
      } else {
        xmaxCut <- xmax(r)
      }
      if (ymin(r) != min(yminExtent)) {
        yminCut <- ymin(r) + yBuffer
      } else {
        yminCut <- ymin(r)
      }
      if (ymax(r) != max(ymaxExtent)) {
        ymaxCut <- ymax(r) - yBuffer
      } else {
        ymaxCut <- ymax(r)
      }
      x[[i]] <- crop(r, extent(xminCut, xmaxCut, yminCut, ymaxCut))
    }
    y <- do.call(raster::merge, x)
    names(y) <- gsub("_tile[0-9].*$", "", names(x[[1]]))
    return(y)
})
