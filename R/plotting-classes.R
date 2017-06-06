### Allow gg S3 class to be used with Plot, an S4 function
#' @importFrom ggplot2 ggplot
setOldClass("gg")
selectMethod("show", "gg")

### Allow histogram S3 class to be used with Plot, an S4 function
# all of `graphics` is being imported in `spades-package.R`
setOldClass("histogram")
selectMethod("show", "histogram")

### Allow igraph S3 class to be used with Plot, an S4 function
# all of `igraph` is being imported in `spades-package.R`
setOldClass("igraph")
selectMethod("show", "igraph")

### Allow gpar S3 class to be used with Plot, an S4 function
# all of `grid` is being imported in `spades-package.R`
setOldClass("gpar")

setAs(from = "list", to = "gpar", function(from) {
  if (length(from[[1]]) > 0) {
    gp1 <- gpar(from[[1]][[1]])
    if (length(from[[1]]) > 1) {
      for (i in 2:length(from[[1]])) {
        gp1 <- gpar(sapply(gp1, function(x) x), from[[1]][[i]])
      }
    }
    names(gp1) <- names(from[[1]])
    gp1
  } else {
    gpar()
  }
})

################################################################################
#' The \code{spatialObjects} class
#'
#' This class is the union of several spatial objects from \code{raster} and
#' \code{sp} packages.
#'
#' Members:
#'
#' \itemize{
#'   \item \code{RasterLayer}, \code{RasterLayerSparse}, \code{RasterStack};
#'   \item \code{SpatialLines}, \code{SpatialLinesDataFrame};
#'   \item \code{SpatialPixels}, \code{SpatialPixelsDataFrame};
#'   \item \code{SpatialPoints}, \code{SpatialPointsDataFrame};
#'   \item \code{SpatialPolygons}, \code{SpatialPolygonsDataFrame}.
#' }
#'
#' Notably missing is \code{RasterBrick}, for now.
#'
#' @seealso \code{\link{spadesClasses}}
#'
#' @aliases spatialObjects
#' @importClassesFrom raster RasterLayer
#' @importClassesFrom raster RasterLayerSparse
#' @importClassesFrom raster RasterStack
#' @importClassesFrom sp SpatialLines
#' @importClassesFrom sp SpatialLinesDataFrame
#' @importClassesFrom sp SpatialPixels
#' @importClassesFrom sp SpatialPixelsDataFrame
#' @importClassesFrom sp SpatialPoints
#' @importClassesFrom sp SpatialPointsDataFrame
#' @importClassesFrom sp SpatialPolygons
#' @importClassesFrom sp SpatialPolygonsDataFrame
#' @name spatialObjects-class
#' @rdname spatialObjects-class
#' @author Eliot McIntire
#' @exportClass spatialObjects
setClassUnion(name = "spatialObjects",
              members = c("RasterLayer", "RasterLayerSparse", "RasterStack",
                          "SpatialLines", "SpatialLinesDataFrame",
                          "SpatialPixels", "SpatialPixelsDataFrame",
                          "SpatialPoints", "SpatialPointsDataFrame",
                          "SpatialPolygons", "SpatialPolygonsDataFrame")
)

################################################################################
#' The \code{.spadesPlotObjects} class
#'
#' This class contains the union of spatialObjects and several other plot-type objects.
#' These are the object classes that the \code{\link{Plot}} function can handle.
#'
#' @seealso \code{\link{spadesClasses}}
#'
#' @slot members SpatialPoints*, SpatialPolygons*, SpatialLines*, RasterLayer, RasterStack
#' @importFrom ggplot2 ggplot
#' @aliases .spadesPlotObjects
#' @name .spadesPlotObjects-class
#' @rdname spadesPlotObjects-class
#' @author Eliot McIntire
## all of `graphics` (for histogram) is being imported in `spades-package.R`
## all of `igraph` (for igraph) has to be imported in `spades-package.R`
setClassUnion(name = ".spadesPlotObjects",
              members = c("spatialObjects", "gg"))#, "igraph", "communities"))

################################################################################
#' The \code{.spadesGrob} class
#'
#' This class contains the plotting .spadesGrob information.
#'
#' These \code{gp*} parameters will specify plot parameters that are
#' available with \code{gpar()}. \code{gp} will adjust plot parameters,
#' \code{gpText} will adjust title and legend text, \code{gpAxis} will
#' adjust the axes. \code{size} adjusts point size in a
#' \code{SpatialPoints} object. These will persist with the
#' original \code{Plot} call for each individual object. Multiple
#' entries can be used, but they must be named list elements
#' and they must match the \code{...} items to plot. This is true
#' for a RasterStack also, i.e., the list of named elements
#' must be the same length as the number of layers being
#' plotted. The naming convention used is: \code{RasterStackName$layerName},
#' i.e, \code{landscape$DEM}.
#'
#' @seealso \code{\link{spadesClasses}}
#'
#' @slot plotName  character. Name of the plot frame, which is by default a concatenation
#' of the \code{objName} and \code{layerName}
#'
#' @slot objName  character. Name of object represented by this .spadesGrob
#'
#' @slot envir environment. The environment in which to find the objName
#'
#' @slot layerName character. Name of the layer represented by this \code{.spadesGrob}.
#' Primarily used for \code{RasterStack}s.
#'
#' @slot objClass character. Class of the object represented by this \code{.spadesGrob}.
#'
#' @slot isSpatialObjects logical. TRUE if the object is one of the SpaDES recognized
#' spatialObject classes.
#'
#' @slot plotArgs list. Any parameters needed for plotting, set by \code{Plot} call.
#'
#' @aliases .spadesGrob
#' @keywords internal
#' @name .spadesGrob-class
#' @rdname spadesGrob-class
#' @author Eliot McIntire
#'
setClass(".spadesGrob",
         slots = list(plotName = "character", objName = "character",
                      envir = "environment", layerName = "character",
                      objClass = "character", isSpatialObjects = "logical",
                      plotArgs = "list"),
         prototype = list(plotName = NA_character_, objName = NA_character_,
                          layerName = NA_character_, objClass = NA_character_,
                          isSpatialObjects = NA, plotArgs = as.list(NULL)),
         validity = function(object) {
           # check for valid extents
           if (any(is.character(object@objName))) {
             stop("must supply an object name")
           }
})

###########################################################################
#' The \code{.arrangement} class
#'
#' This class contains the plotting arrangement information.
#'
#' These \code{gp*} parameters will specify plot parameters that are
#' available with \code{gpar()}. \code{gp} will adjust plot parameters,
#' \code{gpText} will adjust title and legend text, \code{gpAxis} will
#' adjust the axes. \code{size} adjusts point size in a
#' \code{SpatialPoints} object. These will persist with the
#' original \code{Plot} call for each individual object. Multiple
#' entries can be used, but they must be named list elements
#' and they must match the \code{...} items to plot. This is true
#' for a RasterStack also, i.e., the list of named elements
#' must be the same length as the number of layers being
#' plotted. The naming convention used is: \code{RasterStackName$layerName},
#' i.e, \code{landscape$DEM}.
#'
#' @seealso \code{\link{spadesClasses}}
#'
#' @slot rows    numeric. Number of rows in the arrangement.
#'
#' @slot columns numeric. Number of columns in the arrangement.
#'
#' @slot actual.ratio numeric. Ratio of columns to rows
#'
#' @slot ds.dimensionRatio numeric. Ratio of the device size to the ratio of the
#' extents
#'
#' @slot ds  numeric of length 2. The dimensions of the plotting window in inches
#'
#' @slot objects  list of length number of spatial objects. Each list has a character vector
#' of the layer names in each of those
#'
#' @slot isRaster  logical vector, indicating whether each object is a Raster* object
#'
#' @slot names  character vector. The names of the layers in the plot
#'
#' @slot extents list of class Extent objects. These are needed to calculate the
#' \code{ds.dimensionRatio}, which is used to scale the Spatial objects correctly
#'
#' @slot isSpatialObjects logical indicating whether the object(s) are \code{spatialObjects}
#' or not
#'
#' @slot layout list of length 2, with width and height measurements for layout.
#'
#' @slot gp a gpar object or list of named gpar objects. These names must
#' match the names of the \code{...} objects. Default is NULL. See details.
#'
#' @slot gpText a gpar object or a list of named gpar objects. These names must
#' match the names of the \code{...} objects. Default is NULL. See details.
#'
#' @slot gpAxis a gpar object or a list of named gpar objects. These names must
#' match the names of the \code{...} objects. Default is NULL. See details.
#'
#' @slot size a numeric or a named list of numerics, used for SpatialPoints plots.
#' Default is 5. See details.
#'
#' @aliases .arrangement
#' @keywords internal
#' @name .arrangement-class
#' @rdname arrangement-class
#' @author Eliot McIntire
#'
setClass(".arrangement",
         slots = list(rows = "numeric", columns = "numeric",
                    actual.ratio = "numeric", ds.dimensionRatio = "numeric",
                    ds = "numeric", objects = "list", isRaster = "logical", names = "character",
                    extents = "list", isSpatialObjects = "logical", layout = "list",
                    gp = "list", gpText = "list", gpAxis = "list", size = "list"),
         prototype = list(rows = 1, columns = 1,
                        actual.ratio = 1, ds.dimensionRatio = 1,
                        ds = c(7, 7), objects = as.list(NULL), isRaster = NA,
                        names = as.character(NULL),
                        extents = as.list(NULL), isSpatialObjects = NA, layout = as.list(NULL),
                        gp = as.list(NULL), gpText = as.list(NULL),
                        gpAxis = as.list(NULL), size = as.list(NULL)),
         validity = function(object) {
           # check for valid extents
           if (any(is.na(object@extents))) {
             stop("must supply a list of extents")
           }
})

###########################################################################
#' The \code{.spadesPlot} class
#'
#' This class contains all necessary information to build a Plot on a device,
#' except the actual data. Thus, this class differs notably from the grid package,
#' which keeps a copy of all data *and* information in a hidden location for further
#' access for rebuilding, erasing etc. This difference allows the Plot function to
#' be much faster than using the grid methodology directly. The cost to this speed
#' gain is that the objects *must* be available, by name, in the .GlobalEnv.
#'
#' This class contains two slots, one for the overall arrangement of the plots within
#' the device window, and the second for all the \code{\link{.spadesGrob}} objects within
#' that device window. These \code{\link{.spadesGrob}} objects are the individual
#' "smallest" plot unit.
#'
#' These \code{gp*} parameters will specify plot parameters that are
#' available with \code{gpar()}. \code{gp} will adjust plot parameters,
#' \code{gpText} will adjust title and legend text, \code{gpAxis} will
#' adjust the axes. \code{size} adjusts point size in a
#' \code{SpatialPoints} object. These will persist with the
#' original \code{Plot} call for each individual object. Multiple
#' entries can be used, but they must be named list elements
#' and they must match the \code{...} items to plot. This is true
#' for a RasterStack also, i.e., the list of named elements
#' must be the same length as the number of layers being
#' plotted. The naming convention used is: \code{RasterStackName$layerName},
#' i.e, \code{landscape$DEM}.
#'
#' @seealso \code{\link{spadesClasses}}
#'
#' @slot arr  An .arrangement object
#'
#' @slot spadesGrobList list. A list of lists of .spadesGrob objects
#'
#' @aliases .spadesPlot
#' @keywords internal
#' @name .spadesPlot-class
#' @rdname spadesPlot-class
#' @author Eliot McIntire
#'
setClass(".spadesPlot",
         slots = list(arr = ".arrangement", spadesGrobList = "list"),
         prototype = list(arr = new(".arrangement"), spadesGrobList = as.list(NULL)),
         validity = function(object) {
           # check for valid extents
           if (any(is(object@arr, ".arrangement"))) {
             stop("must supply an arrangement")
           }
})

################################################################################
#' The \code{.spadesPlottables} class
#'
#' This class is the union of all .spadesPlotObjects (e.g., \code{RasterLayer*},
#' \code{SpatialPoints*}, \code{SpatialPolygons*}, \code{ggplot}, \code{hist}, etc.)
#' and \code{\link{.spadesPlot}} class objects.
#' This allows replotting of a \code{\link{.spadesPlot}} object.
#'
#' @seealso \code{\link{spadesClasses}}
#'
#' @slot members \code{\link{.spadesPlotObjects}} and \code{\link{.spadesPlot}}
#'
#' @aliases .spadesPlottables
#' @keywords internal
#' @name .spadesPlottables-class
#' @rdname spadesPlottables-class
#' @author Eliot McIntire
#'
setClassUnion(name = ".spadesPlottables",
              members = c(".spadesPlotObjects", ".spadesPlot"))
