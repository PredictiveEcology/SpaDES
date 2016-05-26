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
#' @param nBuffer The number of pixels around the splitted files.
#'                Default is 0, which means no buffer.
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
#' require(raster)
#' # an example with dimensions:
#' # nrow = 77
#' # ncol = 101
#' # nlayers = 3
#' b <- brick(system.file("external/rlogo.grd", package = "raster"))
#' r <- b[[1]] # use first layer only
#' nx <- 3
#' ny <- 4
#' y <- splitRaster(r, nx, ny)
#'
#' # the original raster:
#' plot(r) # may require a call to `dev()` if using RStudio
#'
#' # the split raster:
#' layout(mat = matrix(seq_len(nx*ny), ncol = nx, nrow = ny))
#' plotOrder <- c(4,8,12,3,7,11,2,6,10,1,5,9)
#' invisible(lapply(y[plotOrder], plot))
#'
#' # can be recombined using `raster::merge`
#' m <- do.call(merge, y)
#' all.equal(m, r)
#'
# igraph exports %>% from magrittr
setGeneric("splitRaster", function(x, nx, ny, nBuffer) {
  standardGeneric("splitRaster")
})

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "integer",
                        ny = "integer", nBuffer = "integer"),
  definition = function(x, nx, ny, nBuffer) {
    ext <- extent(x)
    tiles <- vector("list", length = nx*ny)
    n <- 1L
    for (i in seq_len(nx)-1L) {
      for (j in seq_len(ny)-1L) {
            x0 <- ext@xmin + i*(ext@xmax / nx)-res(x)[1]*nBuffer
            x1 <- ext@xmin + (i+1L)*(ext@xmax / nx)+res(x)[1]*nBuffer
            y0 <- ext@ymin + j*(ext@ymax / ny)-res(x)[1]*nBuffer
            y1 <- ext@ymin + (j+1L)*(ext@ymax / ny)+res(x)[1]*nBuffer

            x.coords <- c(x0, x1, x1, x0, x0)
            y.coords <- c(y0, y0, y1, y1, y0)

            box <- Polygon(cbind(x.coords, y.coords)) %>%
                   list %>%
                   Polygons("box") %>%
                   list %>%
                   SpatialPolygons

            # system.time(a <- rasterize(box, x, mask = TRUE, silent = TRUE) %>%
            #               crop(box))
            tiles[[n]] <- crop(x, box)
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
                        ny = "numeric", nBuffer = "integer"),
  definition = function(x, nx, ny, nBuffer) {
    return(splitRaster(x, as.integer(nx), as.integer(ny), nBuffer))
  })

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric",
                        ny = "numeric", nBuffer = "numeric"),
  definition = function(x, nx, ny, nBuffer) {
    return(splitRaster(x, as.integer(nx), as.integer(ny),
                       as.integer(nBuffer)))
  })

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric",
                        ny = "numeric", nBuffer = "missing"),
  definition = function(x, nx, ny) {
    return(splitRaster(x, as.integer(nx), as.integer(ny), nBuffer = 0))
  })

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "integer",
                        ny = "integer", nBuffer = "missing"),
  definition = function(x, nx, ny) {
    return(splitRaster(x, nx, ny, nBuffer = 0))
  })
