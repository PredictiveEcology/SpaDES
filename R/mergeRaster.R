##############################################################
#' Merge the splitted tiles to one raster layer.
#'
#' Recombine the splitted tiles from \code{splitRaster} to
#' the original raster layer.
#'
#' @param x A list of splitted raster tiles with or without buffer.
#'
#'
#' @return A original raster layer before it is splitted.
#'
#' @seealso \code{\link{merge}}, \code{\link{splitRaster}}.
#'
#' @importFrom raster crop extent
#' @export
#' @docType methods
#' @rdname mergeRaster
#'
#' @author Yong Luo
#'
#' @examples
#' require(raster)
#' # an example with dimensions:
#' # nrow = 77
#' # ncol = 101
#' # nlayers = 3
#' b <- brick(system.file("external/rlogo.grd", package = "raster"))
#' r <- b[[1]] # use first layer only
#' nx <- 3
#' ny <- 4
#' y0 <- splitRaster(r, nx, ny) # without buffer
#' y1 <- splitRaster(r, nx, ny, c(3, 4)) # with 3 pixels buffer at horizontal scale
#'                                       # and 4 pixels buffer at vertical scale
#'
#'
#' # can be recombined using `raster::merge`
#' r0 <- do.call(merge, y0) # regular merge call from raster package
#' all.equal(r0, r) # TRUE
#'
#' r1 <- mergeRaster(y0) # mergeRaster function for splitted tiles without buffer
#' all.equal(r1, r) # TRUE
#'
#' r2 <- mergeRaster(y1) # mergeRaster function for splitted tiles with buffer
#' all.equal(r2, r) # TRUE
#'
# igraph exports %>% from magrittr
setGeneric("mergeRaster", function(x) {
  standardGeneric("mergeRaster")
})

#' @export
#' @rdname mergeRaster
setMethod(
  "mergeRaster",
  signature = signature(x = "list"),
  definition = function(x) {
    xminExtent <- c()
    xmaxExtent <- c()
    yminExtent <- c()
    ymaxExtent <- c()
    for(i in 1:length(x)){
      xminExtent <- c(xminExtent, xmin(x[[i]]))
      xmaxExtent <- c(xmaxExtent, xmax(x[[i]]))
      yminExtent <- c(yminExtent, ymin(x[[i]]))
      ymaxExtent <- c(ymaxExtent, ymax(x[[i]]))
    }
    xminExtent <- sort(unique(xminExtent))
    xmaxExtent <- sort(unique(xmaxExtent))
    yminExtent <- sort(unique(yminExtent))
    ymaxExtent <- sort(unique(ymaxExtent))
    nxBuffer <- unique((xmaxExtent[-length(xmaxExtent)]-xminExtent[-1])/2)
    nyBuffer <- unique((ymaxExtent[-length(ymaxExtent)]-yminExtent[-1])/2)
    rm(i)
    for(i in 1:length(x)){
      indiRaster <- x[[i]]
      if(xmin(indiRaster) != min(xminExtent)){
        xminCut <- xmin(indiRaster) + xres(indiRaster)*nxBuffer
      } else {
        xminCut <- xmin(indiRaster)
      }
      if(xmax(indiRaster) != max(xmaxExtent)){
        xmaxCut <- xmax(indiRaster) - xres(indiRaster)*nxBuffer
      } else {
        xmaxCut <- xmax(indiRaster)
      }
      if(ymin(indiRaster) != min(yminExtent)){
        yminCut <- ymin(indiRaster) + yres(indiRaster)*nyBuffer
      } else {
        yminCut <- ymin(indiRaster)
      }
      if(ymax(indiRaster) != max(ymaxExtent)){
        ymaxCut <- ymax(indiRaster) - yres(indiRaster)*nyBuffer
      } else {
        ymaxCut <- ymax(indiRaster)
      }
      indiRaster <- crop(indiRaster, extent(xminCut, xmaxCut, yminCut, ymaxCut))
      x[[i]] <- indiRaster
    }
    allRaster <- do.call(merge, x)
    return(allRaster)
  })
