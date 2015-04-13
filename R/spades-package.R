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
#' Bug reports: \url{https://github.com/PredictiveEcology/SpaDES/issues}
#'
#' @name spades-package
#' @aliases SpaDES spades-package
#' @docType package
#' @author Alex M. Chubaty \email{achubaty@@nrcan.gc.ca}
#' @author Eliot J. B. McIntire \email{Eliot.McIntire@@nrcan.gc.ca}
#' @keywords package
#'
#'
#' @section 1. SpaDES functions:
#'
#' A collection of functions for doing discrete event simulation.
#'
#' @section 1.1 Main SpaDES functions:
#'
#' There are two workhorse functions used in SpaDES. These initialize the simulation
#' and run the simulation:
#'
#' \tabular{ll}{
#'   \code{\link{simInit}} \tab Initialize a new simulation\cr
#'   \code{\link{spades}} \tab Run a discrete event simulation\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 1.2 Discrete Event Simulation:
#'
#' \tabular{ll}{
#'   \code{\link{scheduleEvent}} \tab Schedule a simulation event\cr
#'   \code{\link{simCurrentTime}} \tab Get and set the list of simulation times\cr
#'   \code{\link{simStopTime}} \tab Get and set the stop time of a simulation \cr
#'   \code{\link{simStartTime}} \tab Get and set the start time of a simulation \cr
#'   \code{\link{simGlobals}} \tab Get and set the global simulation parameters list.\cr
#'   \code{\link{simParams}} \tab Get and set the simulation parameters list.\cr
#'   \code{\link{simEvents}} \tab Get and set the event queue\cr
#'   \code{\link{simFileList}} \tab Get and set files used in a simulation\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 1.3 Plotting:
#' There are several user-accessible plotting functions that are optimized for modularity
#' and speed of plotting:
#'
#' \tabular{ll}{
#'   \code{\link{Plot}} \tab The workhorse plotting function\cr
#'   \code{\link{rePlot}} \tab Replots all elements of device for refreshing or moving plot\cr
#'   \code{\link{clickValues}} \tab Extract values from a raster object at the mouse click location(s)\cr
#'   \code{\link{clickExtent}} \tab Zoom into a raster or polygon map that was plotted with \code{\link{Plot}}\cr
#'   \code{\link{dev}} \tab Specify which device to plot on, making a non-RStudio one as default\cr
#'   \code{\link{newPlot}} \tab Open a new default plotting device\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 1.4 File operations:
#'
#' In addition to R's file operations, we have added several here to aid in bulk
#' loading and saving of files for simulation purposes:
#'
#' \tabular{ll}{
#'   \code{\link{loadFiles}} \tab Load simulation objects according to a fileList\cr
#'   \code{\link{saveFiles}} \tab Save simulation objects according to simParams\cr
#'   \code{\link{rasterToMemory}} \tab Read a raster from file to RAM\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 1.5 Module operations:
#'
#' Modules are the basic unit of SpaDES. These are generally created and stored locally,
#' or are downloaded from remote repositories, including SpaDES-modules repository
#' on github.com:
#'
#' \tabular{ll}{
#'   \code{\link{newModule}} \tab Create new module from template\cr
#'   \code{\link{downloadModule}} \tab Open all modules nested within a base directory\cr
#'   \code{\link{openModules}} \tab Open all modules nested within a base directory\cr
#'   \code{\link{zipModule}} \tab Zip a module and its associated files\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 1.6 Object classes in SpaDES:
#'
#' \tabular{ll}{
#'   \code{\link{simList}} \tab The 'simList' class\cr
#'   \code{\link{spadesPlot}} \tab Contains the plotting spadesPlot information.\cr
#'   \code{\link{spadesGrob}} \tab Contains the plotting spadesGrob information\cr
#'   \code{\link{arrangement}} \tab The 'arrangement' class\cr
#'   \code{\link{moduleDeps}} \tab Descriptor object for specifying SpaDES module dependecies\cr
#'   \code{\link{simDeps}} \tab Defines all simulation dependencies for all modules within a SpaDES simulation\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' ------------------------------------------------------------------------------------------
#' @section 2 Module functions:
#'
#' A collection of functions that help with making modules, in addition to all the other R packages and code.
#'
#' @section 2.1 Spatial spreading:
#'
#' Spatial contagion is a key phenomenon for spatially explicit simulation models. Contagion can
#' be modelled using discrete approaches or continuous approaches. Several SpaDES functions assist
#' with these:
#'
#' \tabular{ll}{
#'   \code{\link{spread}} \tab Contagious cellular automata\cr
#'   \code{\link{adj}} \tab An optimized (i.e., faster) version of \code{\link[raster]{adjacent}}\cr
#'   \code{\link{cir}} \tab Identify pixels in a circle around a \code{\link[sp:SpatialPoints-class]{SpatialPoints*}} object\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.2 Spatial agent methods:
#'
#' Agents have several methods and functions specific to them:
#'
#' \tabular{ll}{
#'   \code{\link{crw}} \tab Simple correlated random walk function\cr
#'   \code{\link{move}} \tab A meta function that can currently only take "crw"\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.3 GIS operations:
#'
#' I addition to the vast amount of GIS operations available in R, mostly from contributed packages, like \code{sp}, \code{raster},
#' \code{maps}, \code{maptools} and many others
#' \tabular{ll}{
#'   \code{\link{equalExtent}} \tab Assess whether a list of extents are all equal\cr
#'   \code{\link{setColors}} \tab Set colours for plotting Raster* objects\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.4 Random Map Generation:
#'
#' Before all data are available, it is often useful to build dummy maps on which to build
#' simulation models. These can then be replaced later with actual data maps:
#'
#' \tabular{ll}{
#'   \code{\link{randomPolygons}} \tab Creates a random polygon with specified number of classes\cr
#'   \code{\link{GaussMap}} \tab Creates a random map upsing gaussian random fields\cr
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
