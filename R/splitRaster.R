##############################################################
#' Split a raster into multiple tiles
#'
#' Divides up a raster into an arbitrary number of pieces.
#'
#' @param x   The raster to be split.
#'
#' @param nx  The number of tiles to make along the x-axis.
#'
#' @param ny  The number of tiles to make along the y-axis.
#'
#' @param crop  Logical. Should the raster be cropped into tiles,
#'              or simply masked? Default is \code{TRUE}.
#'
#' @param ... Additional arguments.
#'
#' @return Opens a new plot device on the screen.
#'
#' @import raster
#' @export
#' @docType methods
#' @rdname splitRaster-method
#'
#' @examples
#' # an example with dimensions:
#' # nrow = 77
#' # ncol = 101
#' # nlayers = 3
#' x <- brick(system.file("external/rlogo.grd", package="raster"))
#' x1 <- x[[1]] # use first layer only
#' nx=3; ny=4
#' y = splitRaster(x1, nx, ny)
#'
#' layout(mat=matrix(seq_len(nx*ny), ncol=nx, nrow=ny))
#' lapply(y, plot)
#'
splitRaster = function(x, nx, ny, crop=TRUE, ...) {
    ext = extent(x)
    masked = vector("list", length=nx*ny)
    tiles = vector("list", length=nx*ny)

    n=1
    for (i in seq_len(nx)-1) {
        for (j in seq_len(ny)-1) {
            x0 = ext@xmin + i*(ext@xmax / nx)
            x1 = ext@xmin + (i+1)*(ext@xmax / nx)
            y0 = ext@ymin + j*(ext@ymax / ny)
            y1 = ext@ymin + (j+1)*(ext@ymax / ny)

            x.coords = c(x0, x1, x1, x0, x0)
            y.coords = c(y0, y0, y1, y1, y0)

            square = Polygon(cbind(x.coords, y.coords))
            square = Polygons(list(square), "square")
            square = SpatialPolygons(list(square))

            tiles[[n]] = rasterize(square, x[[n]], silent=TRUE)
            n = n+1
        }
    }

    # for each tile, mask or crop:
    if (crop) {
      cropped = lapply(tiles, function(y) { crop(x, y) } )
      newExtent = lapply(tiles, function(y) { extent(crop(x, y)) } )
    } else {
      masked = lapply(tiles, function(y) { mask(x, y) } )
    }
    return(masked)
}
