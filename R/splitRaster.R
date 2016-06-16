#' Split a RasterLayer into multiple tiles
#'
#' Divides up a raster into an arbitrary number of pieces (tiles).
#' Split rasters can be recombined using \code{do.call(merge, y)} or \code{mergeRaster(y)},
#' where \code{y <- splitRaster(x)}.
#' This function is parallel aware, using the same mechanism as used in the \code{raster}
#' package. Specifically, if you start a cluster using \code{\link{beginCluster}}, then
#' this function will automatically use that cluster. It is always a good
#' idea to stop the cluster when finished, using \code{\link{endCluster}}.
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
#' @param savePath character specify the directory that the split tiles will be saved,
#'                and the split tiles will be linked function's output. If missing, the function
#'                creates a folder with the x raster's name in the working directory
#'
#' @return A list (length \code{nx*ny}) of cropped raster tiles.
#'
#' @seealso \code{\link{do.call}}, \code{\link[raster]{merge}}, \code{\link{mergeRaster}}.
#'
# igraph exports %>% from magrittr
#' @importFrom raster crop extent xmax xmin xres ymax ymin yres getCluster returnCluster writeRaster
#' @importFrom parallel clusterApply
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
#' y0 <- splitRaster(r, nx, ny, savePath = file.path(tempdir(), "y0")) # no buffer
#' y1 <- splitRaster(r, nx, ny, c(10, 10), savePath = file.path(tempdir(), "y1")) # buffer: 10 pixels along both axes
#' y2 <- splitRaster(r, nx, ny, c(0.5, 0.5), savePath = file.path(tempdir(), "y2")) # buffer: half the width and length of each tile
#' # parallel cropping
#' beginCluster(2)
#' y3 <- splitRaster(r, nx, ny, c(0.7, 0.7), savePath = file.path(tempdir(), "y3"))
#' endCluster()
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
setGeneric("splitRaster", function(x, nx, ny, buffer, savePath) {
  standardGeneric("splitRaster")
})

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "integer",
                        ny = "integer", buffer = "numeric",
                        savePath = "character"),
  definition = function(x, nx, ny, buffer, savePath) {
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
    extents <- vector("list", length = nx*ny)
    if(!dir.exists(savePath)) {dir.create(savePath)}
    n <- 1L
    for (i in seq_len(nx) - 1L) {
      for (j in seq_len(ny) - 1L) {
        x0 <- ext@xmin + i*((ext@xmax - ext@xmin) / nx) - buffer[1]*xres(x)
        x1 <- ext@xmin + (i + 1L)*((ext@xmax - ext@xmin) / nx) + buffer[1]*xres(x)
        y0 <- ext@ymin + j*((ext@ymax - ext@ymin) / ny) - buffer[2]*yres(x)
        y1 <- ext@ymin + (j + 1L)*((ext@ymax - ext@ymin) / ny) + buffer[2]*yres(x)
        extents[[n]] <- extent(x0, x1, y0, y1)
        n <- n + 1L
      }
    }
    cl <- tryCatch(getCluster(), error = function(x) NULL)
    on.exit(if (!is.null(cl)) returnCluster())
    if (!is.null(cl)) {
      parFun <- "clusterApply"
      cropNew <- function(ras, x0 = x, ...) {
        crop(x = ras,y = x0)
      }
      args <- list(cl = cl, x = extents,
                   fun = cropNew, ras = x)

      # args <- list(seq = extents, fun = crop, r)
      # args1 <- list(cl = cl, y = extents, fun = crop, x = r)
      # args <- append(list(cl = cl), args)
    } else {
      parFun <- "lapply"
      args <- list(extents, FUN = crop, x = r)
    }
    tiles <- do.call(get(parFun), args)
    for(i in 1:length(tiles)){
      writeRaster(tiles[[i]], file.path(savePath, paste(names(x), "_tile", i, ".tif", sep = "")),
                  overwrite = TRUE)

      singleRaster <- raster(file.path(savePath, paste(names(x), "_tile", i, ".tif", sep = "")))
      crs(singleRaster) <- crs(x)
      tiles[[i]] <- singleRaster
    }
    return(tiles)
})

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric", ny = "numeric", buffer = "integer",
                        savePath = "character"),
  definition = function(x, nx, ny, buffer, savePath) {
    return(splitRaster(x, as.integer(nx), as.integer(ny), as.numeric(buffer), savePath))
})

setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric", ny = "numeric", buffer = "integer",
                        savePath = "missing"),
  definition = function(x, nx, ny, buffer) {
    return(splitRaster(x, as.integer(nx), as.integer(ny), as.numeric(buffer), savePath = file.path(getwd, names(x))))
  })


#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric", ny = "numeric", buffer = "numeric",
                        savePath = "character"),
  definition = function(x, nx, ny, buffer, savePath) {
    return(splitRaster(x, as.integer(nx), as.integer(ny), buffer, savePath))
})

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric", ny = "numeric", buffer = "numeric",
                        savePath = "missing"),
  definition = function(x, nx, ny, buffer) {
    return(splitRaster(x, as.integer(nx), as.integer(ny), buffer, savePath = file.path(getwd, names(x))))
  })

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric", ny = "numeric", buffer = "missing",
                        savePath = "character"),
  definition = function(x, nx, ny, savePath) {
    return(splitRaster(x, as.integer(nx), as.integer(ny), buffer = c(0, 0), savePath))
})

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "numeric", ny = "numeric", buffer = "missing",
                        savePath = "missing"),
  definition = function(x, nx, ny) {
    return(splitRaster(x, as.integer(nx), as.integer(ny), buffer = c(0, 0), savePath = file.path(getwd(), names(x))))
  })

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "integer", ny = "integer", buffer = "missing",
                        savePath = "character"),
  definition = function(x, nx, ny, savePath) {
    return(splitRaster(x, nx, ny, buffer = c(0, 0), savePath))
})

#' @export
#' @rdname splitRaster
setMethod(
  "splitRaster",
  signature = signature(x = "RasterLayer", nx = "integer", ny = "integer", buffer = "missing",
                        savePath = "missing"),
  definition = function(x, nx, ny) {
    return(splitRaster(x, nx, ny, buffer = c(0, 0), savePath = file.path(getwd(), names(x))))
  })
