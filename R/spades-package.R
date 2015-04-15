#
#  SpaDES/R/spades-package.R by Alex M Chubaty and Eliot J B McIntire
#  Copyright (C) 2015 Her Majesty the Queen in Right of Canada, as represented by the Minister of Natural Resources Canada
#

#' Categorized view of the functions in the SpaDES package
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
#' Wiki: \url{https://github.com/PredictiveEcology/SpaDES/wiki}
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
#'   \code{\link{simStartTime}} \tab Get and set the start time of a simulation \cr
#'   \code{\link{simStopTime}} \tab Get and set the stop time of a simulation \cr
#'   \code{\link{simTimes}} \tab Get and set simulation times \cr
#'   \code{\link{simGlobals}} \tab Get and set the global simulation parameters list.\cr
#'   \code{\link{simParams}} \tab Get and set the simulation parameters list.\cr
#'   \code{\link{simCheckpointFile}} \tab Get and set the simulation parameters list.\cr
#'   \code{\link{simEvents}} \tab Get and set the event queue\cr
#'   \code{\link{simDepends}} \tab Get and set simulation dependencies\cr
#'   \code{\link{simCompleted}} \tab List of events that have been run and have completed\cr
#'   \code{\link{simObjectsLoaded}} \tab Get and set list of objects already loaded for simulation\cr
#'   \code{\link{simModules}} \tab Get and set list of modules to be loaded for simulation\cr
#'   \code{\link{simModulesLoaded}} \tab Get and set list of modules already loaded for simulation\cr
#'   \code{\link{simModulesLoadOrder}} \tab Get and set module load order for simulation\cr
#'   \code{\link{simProgressGraphical}} \tab Get and set graphical progress bar\cr
#'   \code{\link{simProgressInterval}} \tab Get and set graphical progress bar interval\cr
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
#'   \code{\link{drawArrows}} \tab Plots arrows showing direction vectors\cr
#'   \code{\link{clickValues}} \tab Extract values from a raster object at the mouse click location(s)\cr
#'   \code{\link{clickExtent}} \tab Zoom into a raster or polygon map that was plotted with \code{\link{Plot}}\cr
#'   \code{\link{clickCoordinates}} \tab Get the coordinates, in map units, under mouse click\cr
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
#'   \code{\link{getFileName}} \tab Get the name of current file\cr
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
#'   \code{\link{newModuleDocumentation}} \tab Create empty documentation for a new module\cr
#'   \code{\link{downloadModule}} \tab Open all modules nested within a base directory\cr
#'   \code{\link{openModules}} \tab Open all modules nested within a base directory\cr
#'   \code{\link{zipModule}} \tab Zip a module and its associated files\cr
#'   \code{\link{reloadModuleLater}} \tab Load modules for simulation, contingent on dependencies loaded\cr
#'   \code{\link{getModuleVersion}} \tab Get the latest module version # from module repository\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 1.6 Object classes in SpaDES:
#'
#' \tabular{ll}{
#'   \code{\link{simList}} \tab The 'simList' class\cr
#'   \code{\link{spatialObjects}} \tab Contains the union of all spatial objects classes useable\cr
#'   \code{\link{spadesPlot}} \tab Contains the plotting spadesPlot information.\cr
#'   \code{\link{spadesPlotObjects}} \tab Contains the plotting arrangement information.\cr
#'   \code{\link{spadesPlotables}} \tab Contains the union of all classes that can be plotted with \code{\link{Plot}}\cr
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
#' @section 2.1 Module metadata:
#'
#' Each module requires several items to be defined. These comprise the metadata for that module,
#' and are currently written at the top of the module R file, all within the \code{\link{defineModule}}
#' function. Functions to help with this:
#'
#' \tabular{ll}{
#'   \code{\link{defineModule}} \tab Define the module metadata\cr
#'   \code{\link{defineParameter}} \tab Specify a parameter's name, value and set a default\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.2 Module dependencies:
#'
#' Once a set of modules have been chosen, the dependency information is automatically
#' calculated once simInit is run. There are several functions to assist with dependency
#' information:
#'
#' \tabular{ll}{
#'   \code{\link{depsEdgeList}} \tab Build edge list for module dependency graph\cr
#'   \code{\link{depsGraph}} \tab Build a module dependency graph using igraph\cr
#'   \code{\link{depsLoadOrder}} \tab Determine load order required to accommodate dependencies\cr
#'   \code{\link{depsPruneEdges}} \tab Identifies cycles in dependencies and removes\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.3 Spatial spreading:
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
#' @section 2.4 Spatial agent methods:
#'
#' Agents have several methods and functions specific to them:
#'
#' \tabular{ll}{
#'   \code{\link{crw}} \tab Simple correlated random walk function\cr
#'   \code{\link{heading}} \tab Determines the heading between SpatialPoints*\cr
#'   \code{\link{move}} \tab A meta function that can currently only take "crw"\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.5 GIS operations:
#'
#' I addition to the vast amount of GIS operations available in R, mostly from contributed packages, like \code{sp}, \code{raster},
#' \code{maps}, \code{maptools} and many others
#' \tabular{ll}{
#'   \code{\link{equalExtent}} \tab Assess whether a list of extents are all equal\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.6 Map-reduce - type operations:
#'
#' These functions convert between reduced and mapped representations of the same data.
#' This allows compact representation of, say, rasters that have many individual pixels
#' that share identical information.
#' \tabular{ll}{
#'   \code{\link{rasterizeReduced}} \tab Convert reduced representation to full raster\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.7 Colors in Raster* objects:
#'
#' We likely will not want the default colours for every map.
#' Here are several helper functions to add, set and get colors to Raster* objects:

#' \tabular{ll}{
#'   \code{\link[SpaDES:setColors<-]{setColors}} \tab Set colours for plotting Raster* objects\cr
#'   \code{\link{getColors}} \tab Get colours in a Raster* objects\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.8 Random Map Generation:
#'
#' Before all data are available, it is often useful to build dummy maps on which to build
#' simulation models. These can then be replaced later with actual data maps:
#'
#' \tabular{ll}{
#'   \code{\link{randomPolygons}} \tab Creates a random polygon with specified number of classes\cr
#'   \code{\link{gaussMap}} \tab Creates a random map using gaussian random fields\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.9 Assigning and getting objects:
#'
#' SpaDES modules are groups of R functions. This means that any objects created within
#' a function needs to be returned or manually assigned. Since the structure of SpaDES
#' is to use the function calls to return only the simList object, any objects that are
#' to be used by any other function, event or module must be assigned to an environment
#' where it can be found by those other functions, events, and modules. As shorthand
#' wrappers, we have built the following to simplify this:
#'
#' \tabular{ll}{
#'   \code{\link{assignGlobal}} \tab Assign to the global environment\cr
#'   \code{\link{getGlobal}} \tab Get from the global environment\cr
#'   \code{\link{existsGlobal}} \tab Test for existence of an object in .GlobalEnv\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.10 Checking for the existence of objects:
#'
#' SpaDES modules will often require the existence of objects in the global environment.
#' These are several helpers for assessing this:
#'
#' \tabular{ll}{
#'   \code{\link{checkObject}} \tab Check for existence of a global object\cr
#'   \code{\link{checkPath}} \tab Checks the specified filepath for formatting consistencies\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.11 SELES-type approach to simulation:
#'
#' These functions are essentially skeletons and are not fully implemented.
#' They are intended to make translations from \href{http://www.lfmi.uqam.ca/seles.htm}{SELES}.
#' You must know how to use SELES for these to be useful:
#' \tabular{ll}{
#'   \code{\link{probInit}} \tab Probability of intiating an agent or event\cr
#'   \code{\link{numAgents}} \tab Number of agents\cr
#'   \code{\link{agentLocation}} \tab Agent location\cr
#'   \code{\link{transitions}} \tab Transition probability\cr
#'   \code{\link{specificNumPerPatch}} \tab Initiate a specific number of agents per patch\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.12 Miscellaneous:
#'
#' Functions that may be useful within a SpaDES context
#' \tabular{ll}{
#'   \code{\link{inRange}} \tab Test whether a number lies within range [a,b]\cr
#'   \code{\link{layerNames}} \tab Get layer names for numerous object classes\cr
#'   \code{\link{nlayers-method}} \tab Return number of layers\cr
#'   \code{\link{loadPackages}} \tab Simple wrapper for loading packages\cr
#'   \code{\link{updateList}} \tab Update values in a named list\cr
#'   \code{\link{paddedFloatToChar}} \tab Wrapper for padding (e.g., zeros) floating numbers to character\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
#'
#' @section 2.13 Data included in package:
#'
#' Several dummy data sets are included for testing of functionality
#' \tabular{ll}{
#'   \code{\link{SpaDES-maps}} \tab Help showing included maps\cr
#'   --------------------------- \tab ------------------------------------------------------------------------------------------ \cr
#' }
NULL

################################################################################
# data documentation
#

#' Dummy maps included with SpaDES
#'
#' All maps included with SpaDES are randomly generated maps created by \code{gaussMap()}.
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
