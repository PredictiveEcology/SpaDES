##  SpaDES/R/SpaDES-package.R by Alex M Chubaty and Eliot J B McIntire
##  Copyright (C) 2015-2022 Her Majesty the Queen in Right of Canada,
##   as represented by the Minister of Natural Resources Canada
##

#' Categorized overview of the \code{SpaDES} package
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
#' \url{https://nostarch.com/artofr.htm}) framework that facilitates
#' modularity, and easily enables the user to include additional functionality by
#' running user-built simulation modules (see also \pkg{SpaDES.tools}).
#' Included are numerous tools to visualize rasters and other maps (via \pkg{quickPlot}),
#' and caching methods for reproducible simulations (via \pkg{reproducible}).
#' Additional functionality is provided by the suggested \pkg{SpaDES.addins} and
#' \code{SpaDES.shiny} packages (see below).
#'
#' Bug reports:
#' \itemize{
#'   \item \code{quickPlot} package:
#'         \url{https://github.com/PredictiveEcology/quickPlot/issues}
#'   \item \code{reproducible} package:
#'         \url{https://github.com/PredictiveEcology/reproducible/issues}
#'   \item \code{SpaDES.addins} package:
#'         \url{https://github.com/PredictiveEcology/SpaDES.addins/issues}
#'   \item \code{SpaDES.core} package:
#'         \url{https://github.com/PredictiveEcology/SpaDES.core/issues}
#'   \item \code{SpaDES.shiny} package:
#'         \url{https://github.com/PredictiveEcology/SpaDES.shiny/issues}
#'   \item \code{SpaDES.tools} package:
#'         \url{https://github.com/PredictiveEcology/SpaDES.tools/issues}
#' }
#'
#' Module repository: \url{https://github.com/PredictiveEcology/SpaDES-modules}
#'
#' Wiki: \url{https://github.com/PredictiveEcology/SpaDES/wiki}
#'
#' @section The \code{SpaDES.core} package:
#'
#' The core discrete event simulation framework.
#' See \code{\link{SpaDES.core}{SpaDES.core-package}}, and the vignettes therein
#' (\code{browseVignettes()}).
#'
#' @section The \code{SpaDES.tools} package:
#'
#' Additional utilities for developing ecological simulation models.
#' See \code{\link{SpaDES.tools}{SpaDES.tools-package}}.
#'
#' @section The \code{SpaDES.addins} package:
#'
#' A set of RStudio addins to assist with \code{SpaDES} module development.
#'
#' @section The \code{SpaDES.shiny} package:
#'
#' Utilities for developing and running \code{shiny}-based app interfaces to
#' \code{SpaDES} simulations.
#'
#' @section The \code{quickPlot} package:
#'
#' The core \code{SpaDES} plotting engine, build upon speed and modularity.
#'
#' @section The \code{reproducible} package:
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
