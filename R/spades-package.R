##  SpaDES/R/SpaDES-package.R by Alex M Chubaty and Eliot J B McIntire
##  Copyright (C) 2015-2022 Her Majesty the Queen in Right of Canada,
##   as represented by the Minister of Natural Resources Canada
##

#' Categorized overview of the `SpaDES` package
#'
#' @description
#'
#' \if{html}{\figure{SpaDES.png}{options: width=100 alt="SpaDES logo" style="float: right;"}}
#' \if{latex}{\figure{SpaDES.png}{options: width=0.5in}}
#'
#' Metapackage for implementing a variety of event-based models, with a focus on
#' spatially explicit models. These include raster-based, event-based, and
#' agent-based models. The core simulation components (provided by \pkg{SpaDES.core})
#' are built upon a discrete event simulation (DES; see Matloff (2011) ch 7.8.3
#' <https://nostarch.com/artofr.htm>) framework that facilitates
#' modularity, and easily enables the user to include additional functionality by
#' running user-built simulation modules (see also \pkg{SpaDES.tools}).
#' Included are numerous tools to visualize rasters and other maps (via \pkg{quickPlot}),
#' and caching methods for reproducible simulations (via \pkg{reproducible}).
#' Additional functionality is provided by the suggested \pkg{SpaDES.addins} and
#' `SpaDES.shiny` packages (see below).
#'
#' Bug reports:
#' \itemize{
#'   \item `quickPlot` package:
#'         <https://github.com/PredictiveEcology/quickPlot/issues>
#'   \item `reproducible` package:
#'         <https://github.com/PredictiveEcology/reproducible/issues>
#'   \item `SpaDES.addins` package:
#'         <https://github.com/PredictiveEcology/SpaDES.addins/issues>
#'   \item `SpaDES.core` package:
#'         <https://github.com/PredictiveEcology/SpaDES.core/issues>
#'   \item `SpaDES.shiny` package:
#'         <https://github.com/PredictiveEcology/SpaDES.shiny/issues>
#'   \item `SpaDES.tools` package:
#'         <https://github.com/PredictiveEcology/SpaDES.tools/issues>
#' }
#'
#' Module repository: <https://github.com/PredictiveEcology/SpaDES-modules>
#'
#' Wiki: <https://github.com/PredictiveEcology/SpaDES/wiki>
#'
#' @section The `SpaDES.core` package:
#'
#' The core discrete event simulation framework.
#' See \code{[SpaDES.core]{SpaDES.core-package}}, and the vignettes therein
#' (`browseVignettes()`).
#'
#' @section The `SpaDES.tools` package:
#'
#' Additional utilities for developing ecological simulation models.
#' See \code{[SpaDES.tools]{SpaDES.tools-package}}.
#'
#' @section The `SpaDES.addins` package:
#'
#' A set of RStudio addins to assist with `SpaDES` module development.
#'
#' @section The `SpaDES.shiny` package:
#'
#' Utilities for developing and running `shiny`-based app interfaces to
#' `SpaDES` simulations.
#'
#' @section The `quickPlot` package:
#'
#' The core `SpaDES` plotting engine, build upon speed and modularity.
#'
#' @section The `reproducible` package:
#'
#' Provides several aspects of reproducible simulations, including simulation caching.
#'
#' @rdname SpaDES-package
#'
"_PACKAGE"

################################################################################
# package imports
# See \url{https://r-pkgs.org/namespace.html#imports}

#' @import methods
#' @import quickPlot
#' @import reproducible
#' @import SpaDES.core
#' @import SpaDES.tools
NULL

## NOTE: SpaDES.addins moved to Suggests Feb 2021
