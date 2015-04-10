#
#  SpaDES/R/spades-package.R by Alex M Chubaty and Eliot J B McIntire
#  Copyright (C) 2015
#



#' Overview of the functions in the SpaDES package
#'
#' @description
#'
#' This package allows implementation a variety of simulation-type models, with a focus on spatially
#' explicit models. The core simulation components are built upon
#' a discrete event simulation framework that facilitates modularity, and easily
#' enables the user to include additional functionality by running user-built
#' simulation modules. Included are numerous tools to visualize various spatial data formats, as well
#' as non-spatial data.
#'
#' Bug reports: https://github.com/PredictiveEcology/SpaDES/issues
#'
#' @name spades-package
#' @aliases SpaDES spades-package
#' @docType package
#' @author Alex M. Chubaty \email{achubaty@@nrcan.gc.ca}
#' @author Eliot J. B. McIntire \email{Eliot.McIntire@@nrcan.gc.ca}
#' @keywords package
#'
#' @section Plotting:
#' There are several user-accessible plotting functions that are optimized for modularity
#' and speed of plotting:
#'
#' \tabular{ll}{
#'   \code{\link{Plot}} \tab The main function. Uses \code{\link[raster:Raster-class]{Raster*}},
#'     \code{\link[sp:SpatialPoints-class]{SpatialPoints*}}, \code{\link[sp:SpatialPolygons-class]{SpatialPolygons*}},
#'     \code{gg}, \code{\link[graphics]{hist}}, \code{igraph} objects\cr
#'   \code{\link{clickValues}} \tab Is for extracting values from a raster object at the mouse click location(s)\cr
#'   \code{\link{clickExtent}} \tab Used to zoom into a raster or polygon map that was plotted with \code{\link{Plot}}\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
NULL

################################################################################
# data documentation
#

#' Dummy maps included with SpaDES
#'
#' All maps included with SpaDES are randomly generated maps created by \code{GaussMap()}.
#' These are located within the \code{maps} folder of the package, and are used in the vignettes.
#' Use \code{system.file(package="SpaDES", "maps")} to locate the \code{maps} directory on your system.
#'
#' @details
#' \code{DEM.tif}: converted to a a small number of discrete levels (in 100m hypothetical units)
#'
#' @docType data
#' @keywords maps
#' @name SpaDES-maps
#' @aliases spades-maps
#' @rdname SpaDES-maps
#' @format raster
NULL

#' @details
#' \code{habitatQuality.tif}: made to look like a continuous habitat surface, rescaled to 0 to 1
#'
#' @docType data
#' @keywords maps
#' @name SpaDES-maps
#' @rdname SpaDES-maps
#' @format raster
NULL

#' @details
#' \code{forestAge.tif}: rescaled to possible forest ages in a boreal forest setting
#'
#' @docType data
#' @keywords maps
#' @name SpaDES-maps
#' @rdname SpaDES-maps
#' @format raster
NULL

#' @details
#' \code{forestCover.tif}: rescaled to possible forest cover in a boreal forest setting
#'
#' @docType data
#' @keywords maps
#' @name SpaDES-maps
#' @format raster
NULL

#' @details
#' \code{percentPine.tif}: rescaled to percentages.
#'
#' @docType data
#' @keywords maps
#' @name SpaDES-maps
#' @rdname SpaDES-maps
#' @format raster
NULL
