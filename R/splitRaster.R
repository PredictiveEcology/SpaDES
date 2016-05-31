#' Split a RasterLayer into multiple tiles
#'
#' Divides up a raster into an arbitrary number of pieces (tiles).
#' Split rasters can be recombined using \code{do.call(merge, y)} or \code{mergeRaster(y)},
#' where \code{y <- splitRaster(x)}.
#'
#' @param x   The raster to be split.
#'
#' @param nx  The number of tiles to make along the x-axis.
#'
#' @param ny  The number of tiles to make along the y-axis.
#'
#' @param buffer  Numeric vector of length 2 giving the size of the buffer along the x and y axes.
#'                If these values less than or equal to \code{1} are used, this
#'                is interpreted as the number of pixels (cells) to use as a buffer.
#'                Values between \code{0} and \code{1} are interpreted as proportions
#'                of the number of pixels in each tile (rounded up to an integer value).
#'                Default is \code{c(0, 0)}, which means no buffer.
#'
#' @return A list (length \code{nx*ny}) of cropped raster tiles.
#'
#' @seealso \code{\link{do.call}}, \code{\link[raster]{merge}}, \code{\link{mergeRaster}}.
#'
# igraph exports %>% from magrittr
#' @importFrom raster crop extent rasterize xmax xmin xres ymax ymin yres
#' @export
#' @docType methods
#' @rdname splitRaster
#'
#' @author Alex Chubaty and Yong Luo
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
#' y1 <- splitRaster(r, nx, ny, c(10, 10)) # buffer: 10 pixels along both axes
#' y2 <- splitRaster(r, nx, ny, c(0.5, 0.5)) # buffer: half the width and length of each tile
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
#' m0 <- do.call(merge, y0)
#' all.equal(m0, r) ## TRUE
#'
#' m1 <- do.call(merge, y1)
#' all.equal(m1, r) ## TRUE
#'
#' m2 <- do.call(merge, y2)
#' all.equal(m2, r) ## TRUE
#'
#' # or recombine using SpaDES::mergeRaster
#' n0 <- mergeRaster(y0)
#' all.equal(n0, r) ## TRUE
#'
#' n1 <- mergeRaster(y1)
#' all.equal(n1, r) ## TRUE
#'
#' n2 <- mergeRaster(y2)
#' all.equal(n2, r) ## TRUE
#'
setGeneric("splitRaster", function(x, nx, ny, buffer) {
  standardGeneric("splitRaster")
})

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "integer",
                        ny = "integer", buffer = "numeric"),
  definition = function(x, nx, ny, buffer) {
    if (length(buffer) > 2) {
      warning("buffer contains more than 2 elements - only the first two will be used.")
      buffer <- buffer[1:2]
    } else if (length(buffer) == 1) {
      buffer <- c(buffer, buffer)
    }

    if (buffer[1] < 1) {
      buffer[1] <- ceiling((buffer[1]*(xmax(x) - xmin(x))/nx)/xres(x))
    }
    if (buffer[2] < 1) {
      buffer[2] <- ceiling((buffer[2]*(ymax(x) - ymin(x))/ny)/yres(x))
    }
    ext <- extent(x)
    tiles <- vector("list", length = nx*ny)
    n <- 1L
    for (i in seq_len(nx) - 1L) {
      for (j in seq_len(ny) - 1L) {
        x0 <- ext@xmin + i*((ext@xmax - ext@xmin) / nx) - buffer[1]*xres(x)
        x1 <- ext@xmin + (i + 1L)*((ext@xmax - ext@xmin) / nx) + buffer[1]*xres(x)
        y0 <- ext@ymin + j*((ext@ymax - ext@ymin) / ny) - buffer[2]*yres(x)
        y1 <- ext@ymin + (j + 1L)*((ext@ymax - ext@ymin) / ny) + buffer[2]*yres(x)
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
  signature = signature(x = "RasterLayer", nx = "numeric", ny = "numeric", buffer = "integer"),
  definition = function(x, nx, ny, buffer) {
    return(splitRaster(x, as.integer(nx), as.integer(ny), as.numeric(buffer)))
})

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric", ny = "numeric", buffer = "numeric"),
  definition = function(x, nx, ny, buffer) {
    return(splitRaster(x, as.integer(nx), as.integer(ny), buffer))
})

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric", ny = "numeric", buffer = "missing"),
  definition = function(x, nx, ny) {
    return(splitRaster(x, as.integer(nx), as.integer(ny), buffer = c(0, 0)))
})

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "integer", ny = "integer", buffer = "missing"),
  definition = function(x, nx, ny) {
    return(splitRaster(x, nx, ny, buffer = c(0, 0)))
})
