##############################################################
#' Split a raster into multiple tiles.
#'
#' Divides up a raster into an arbitrary number of pieces.
#' Split rasters can be recombined using \code{do.call(merge, x)}.
#'
#' @param x   The raster to be split.
#'
#' @param nx  The number of tiles to make along the x-axis.
#'
#' @param ny  The number of tiles to make along the y-axis.
#'
#' @param bufferLength Two numeric vector, defines bufferring scale at x axis and y axis,
#'                     respectively. If these values bigger than or equal to 1,
#'                     the function will take absolute bufferLength as number
#'                     of pixels to define the buffer around the splitted tiles. If
#'                     these values are between 0 and 1, the function will use the relative
#'                     scale of the splitted tiles to define buffer.
#'                     Default is 0, which means no buffer.
#'
#' @return A list of cropped raster tiles.
#'
#' @seealso \code{\link{do.call}}, \code{\link{merge}}.
#'
#' @importFrom raster crop extent rasterize
#' @importFrom sp Polygon Polygons SpatialPolygons
#' @export
#' @docType methods
#' @rdname splitRaster
#'
#' @author Alex Chubaty
#'
#' @examples
#' library(raster)
#' # an example with dimensions:
#' # nrow = 77
#' # ncol = 101
#' # nlayers = 3
#' b <- brick(system.file("external/rlogo.grd", package = "raster"))
#' r <- b[[1]] # use first layer only
#' nx <- 3
#' ny <- 4
#' y0 <- splitRaster(r, nx, ny) # no buffer
#' y1 <- splitRaster(r, nx, ny, c(10, 10)) # with 10 pixels for x axis and
#'                                         # 10 pixels for y axis
#'                                         # this equals to splitRaster(r, nx, ny, 10)
#' y2 <- splitRaster(r, nx, ny, c(0.5, 0.5)) # with half of width and length of
#'                                           # each splitted tile
#'
#' # the original raster:
#' plot(r) # may require a call to `dev()` if using RStudio
#'
#' # the split raster:
#' layout(mat = matrix(seq_len(nx*ny), ncol = nx, nrow = ny))
#' plotOrder <- c(4,8,12,3,7,11,2,6,10,1,5,9)
#' invisible(lapply(y0[plotOrder], plot))
#'
#' # can be recombined using `raster::merge`
#' m <- do.call(merge, y0)
#' all.equal(m, r)
#'
# igraph exports %>% from magrittr
setGeneric("splitRaster", function(x, nx, ny, bufferLength) {
  standardGeneric("splitRaster")
})

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "integer",
                        ny = "integer", bufferLength = "numeric"),
  definition = function(x, nx, ny, bufferLength) {
    if(length(bufferLength) > 2){
      stop("The length of bufferLength must be a less than three.")
    } else if(length(bufferLength) == 1){
      bufferLength <- c(bufferLength, bufferLength)
    }

    if(bufferLength[1] < 1){
      bufferLength[1] <- round((bufferLength[1]*(xmax(x) - xmin(x))/nx)/xres(x))
    }
    if(bufferLength[2] < 1){
      bufferLength[2] <- round((bufferLength[2]*(ymax(x) - ymin(x))/ny)/yres(x))
    }
    ext <- extent(x)
    tiles <- vector("list", length = nx*ny)
    n <- 1L
    for (i in seq_len(nx)-1L) {
      for (j in seq_len(ny)-1L) {
        x0 <- ext@xmin + i*((ext@xmax-ext@xmin) / nx) - bufferLength[1]*xres(x)
        x1 <- ext@xmin + (i+1L)*((ext@xmax-ext@xmin) / nx) + bufferLength[1]*xres(x)
        y0 <- ext@ymin + j*((ext@ymax-ext@ymin) / ny) - bufferLength[2]*yres(x)
        y1 <- ext@ymin + (j+1L)*((ext@ymax-ext@ymin) / ny) + bufferLength[2]*yres(x)
        tiles[[n]] <- crop(x, extent(x0, x1, y0, y1))
        n <- n + 1L
      }
    }
    return(tiles)
  })

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric",
                        ny = "numeric", bufferLength = "integer"),
  definition = function(x, nx, ny, bufferLength) {
    return(splitRaster(x, as.integer(nx), as.integer(ny),
                       as.numeric(bufferLength)))
  })

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric",
                        ny = "numeric", bufferLength = "numeric"),
  definition = function(x, nx, ny, bufferLength) {
    return(splitRaster(x, as.integer(nx), as.integer(ny),
                       bufferLength))
  })

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric",
                        ny = "numeric", bufferLength = "missing"),
  definition = function(x, nx, ny) {
    return(splitRaster(x, as.integer(nx), as.integer(ny), bufferLength = c(0, 0)))
  })

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "integer",
                        ny = "integer", bufferLength = "missing"),
  definition = function(x, nx, ny) {
    return(splitRaster(x, nx, ny, bufferLength = c(0, 0)))
  })
