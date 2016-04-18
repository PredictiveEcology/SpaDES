### deal with spurious data.table warnings
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("groups", "thin", "whGrobNamesi",
                           "xmax", "xmin", "ymax", "ymin"))
}

################################################################################
#' Make a \code{.spadesPlot} class object
#'
#' Builds a \code{.spadesPlot} object from a list of objects.
#'
#' @param plotObjects list. Any plot objects.
#'
#' @param plotArgs list. Any arguments that the the grid package can accept for
#' the specific grob types, e.g., rasterGrob, polygonGrob, etc.
#'
#' @param whichSpadesPlottables  Logical indicating which objects in the
#' \code{Plot} call can be plotted by \code{Plot}.
#'
#' @param ... additional arguments. Currently nothing.
#'
#' @return A \code{\link{.spadesPlot}} object, which has 2 slots, one for the plot arrangement
#' (i.e., layout and dimensions) and onefor all of the \code{spadesGrobs}
#' (stored as a spadesGrobList of lists \code{.spadesGrob} objects).
#'
#' @rdname makeSpadesPlot
#' @include plotting-classes.R
#' @include plotting-helpers.R
#' @export
#' @author Eliot McIntire
#' @docType methods
#'
setGeneric(".makeSpadesPlot", function(plotObjects, plotArgs, whichSpadesPlottables, ...) {
  standardGeneric(".makeSpadesPlot")
})

#' @export
#' @rdname makeSpadesPlot
setMethod(
  ".makeSpadesPlot",
  signature = c(plotObjects = "list", plotArgs = "list"),
  definition = function(plotObjects, plotArgs, ...) {
    isSpatialObjects <- sapply(plotObjects, function(x) {
      is(x, "spatialObjects")
    })

    env <- list(...)$env
    suppliedNames <- names(plotObjects)
    if (any(nchar(suppliedNames)==0)) {
      suppliedNames <- NULL
    }
    if (is.null(suppliedNames)) {
      objs <- objectNames()[whichSpadesPlottables]
    } else {
      objs <- lapply(suppliedNames, function(x) {
        list(objs = x, envs = env)
        })
    }

    names(plotObjects) <- sapply(objs,function(x)
      x$objs)

    if (!is.null(suppliedNames)) {
      if (all(sapply(suppliedNames, nchar) > 0)) {
        names(plotObjects)[!is.na(suppliedNames)] <- suppliedNames
      }
    }
    numLayers <- pmax(1, sapply(plotObjects, nlayers))

    isSpadesPlot <- sapply(plotObjects, function(x) { is(x, ".spadesPlot") })
    #isRaster <- sapply(plotObjects, function(x) { is(x, "Raster") })
    isStack <- sapply(plotObjects, function(x) { is(x, "RasterStack") })
    #isPolygon <- sapply(plotObjects, function(x) { is(x, "SpatialPolygons") })

    # Stacks are like lists in that they are a single object, with many
    # layers.  Plot must treat these as any other layers, except that
    # they are stored in single objects. The following set of objects
    # are the "long" versions of the layers, i.e,. a call to say
    # Plot(stack1, layerB) would have two objects, but maybe 5 layers,
    # if the stack had 4 layers in it.
    isSpadesPlotLong <- rep(isSpadesPlot, numLayers)
    #isRasterLong <- rep(isRaster, numLayers)
    isStackLong <- rep(isStack, numLayers)
    isSpatialObjects <- rep(isSpatialObjects, numLayers)

    lN <- rep(names(plotObjects), numLayers)
    lN[isSpadesPlotLong] <- layerNames(plotObjects[isSpadesPlot])
    objectNamesLong <- rep(names(plotObjects), numLayers)

    # Full layer names, including object name.
    # If layer name is same as object name omit it, and if layer name
    # is "layer", omit it if within a RasterLayer
    lN[isStackLong] <- paste(objectNamesLong[isStackLong],
                             layerNames(plotObjects[isStack]),
                             sep = "$")
    names(lN) <- rep(names(plotObjects), numLayers)
    names(lN)[isSpadesPlotLong] <- layerNames(plotObjects)[isSpadesPlotLong]

    # Create long version of environments
    lEnvs <- rep(sapply(objs, function(x) { x$envs }), numLayers)

    # if (any(duplicated(paste(lN, lEnvs)))) {
    #   stop(paste("Cannot plot two layers with same name from the same environment.",
    #              "Check inside RasterStacks for objects."))
    # }

    plotArgs <- .makeList(plotArgs, length(lN))

    # Make new .spadesPlot object.
    # This will be merged to existing later.
    newPlots <- new(".spadesPlot")

    newPlots@arr <- new(".arrangement")

    newPlots@spadesGrobList <- lapply(1:length(lN), function(x) {
      spadesGrobList <- list()

      if (isSpadesPlotLong[x]) {
        spadesGrobList[[lN[x]]] <-
          plotObjects[[match(
            names(isSpadesPlotLong)[x],
            names(plotObjects)
          )]]@spadesGrobList[[match(
            lN[x], layerNames(plotObjects[isSpadesPlot])
          )]][[1]]
      } else {
        spadesGrobList[[lN[x]]] <- new(".spadesGrob")
        spadesGrobList[[lN[x]]]@plotArgs <- lapply(plotArgs, function(y) y[[x]])
        spadesGrobList[[lN[x]]]@plotArgs$gpText <- plotArgs$gpText[x]
        spadesGrobList[[lN[x]]]@plotArgs$gpAxis <- plotArgs$gpAxis[x]
        spadesGrobList[[lN[x]]]@plotArgs$gp <- plotArgs$gp[x]
        spadesGrobList[[lN[x]]]@plotName <- lN[x]
        spadesGrobList[[lN[x]]]@objName <- objectNamesLong[x]
        spadesGrobList[[lN[x]]]@envir <- lEnvs[[x]]
        spadesGrobList[[lN[x]]]@layerName <- layerNames(plotObjects)[x]
        spadesGrobList[[lN[x]]]@objClass <- class(
          eval(parse(text = objectNamesLong[x]), lEnvs[[x]])
        )
        spadesGrobList[[lN[x]]]@isSpatialObjects <- isSpatialObjects[x]
      }
      return(spadesGrobList)
    })

    names(newPlots@spadesGrobList) <- lN
    return(newPlots)
  }
)

#' @rdname makeSpadesPlot
setMethod(
  ".makeSpadesPlot",
  signature = c(plotObjects = "list", plotArgs = "missing"),
  definition = function(plotObjects, ...) {
    plotArgs <- formals("Plot")[-1]
    newPlots <- .makeSpadesPlot(plotObjects, plotArgs, ...)
    return(newPlots)
  }
)

#' @rdname makeSpadesPlot
setMethod(
  ".makeSpadesPlot",
  signature = c(plotObjects = "missing", plotArgs = "missing"),
  definition = function(...) {
    newPlots <- new(".spadesPlot")
    newPlots@spadesGrobList <- lapply(1:1, function(x) {
      spadesGrobList <- list()
      spadesGrobList[[1]] <- new(".spadesGrob")
      return(spadesGrobList)
    })
    return(newPlots)
  }
)

################################################################################
#' Merge two SpaDES Plot objects
#'
#' Merges two \code{.spadesPlot} objects
#'
#' @param newSP  The "new" \code{.spadesPlot} object.
#'               I.e., the new merges and overwrites into current.
#'
#' @param curr   The "current" \code{.spadesPlot} object.
#'               I.e., the one to be merged into.
#'
#' @param ...    Additional arguments. Currently none implemented.
#'
#' @rdname updateSpadesPlot
#' @export
#' @importFrom stats na.omit
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @docType methods
setGeneric(".updateSpadesPlot", function(newSP, curr, ...) {
  standardGeneric(".updateSpadesPlot")
})

#' @export
#' @rdname updateSpadesPlot
setMethod(
  ".updateSpadesPlot",
  signature = c(newSP = ".spadesPlot", curr = ".spadesPlot"),
  definition = function(newSP, curr, ...) {
    newNames <- names(newSP@spadesGrobList)
    currNames <- names(curr@spadesGrobList)

    addToPlots <- sapply(newSP@spadesGrobList, function(x) {
      !is.null(x[[1]]@plotArgs$addTo)
    })

    addToPlotsNames <- sapply(newSP@spadesGrobList, function(x) {
      x[[1]]@plotArgs$addTo
    }) %>% unlist

    overplots <- na.omit(match(currNames, newNames))

    needNew <- -c(overplots, which(addToPlots))
    if (length(needNew) == 0) {
      needNew <- 1:length(newNames)
    }

    whichParamsChanged <- lapply(newNames[overplots], function(x) {
      sapply(names(newSP@spadesGrobList[[x]][[1]]@plotArgs), function(y) {
        if (!is.null(newSP@spadesGrobList[[x]][[1]]@plotArgs[[y]])) {
          !identical(newSP@spadesGrobList[[x]][[1]]@plotArgs[[y]],
                   curr@spadesGrobList[[x]][[1]]@plotArgs[[y]])
        } else {
          FALSE
        }
      })
    })
    names(whichParamsChanged) <- newNames[overplots]

    # Set FALSE as default for needPlotting
    needPlotting <- lapply(curr@spadesGrobList, function(x) {
      lapply(x, function(y) { FALSE })
    })

    # Set FALSE as default for isReplot
    isReplot <- lapply(curr@spadesGrobList, function(x) {
      lapply(x, function(y) { FALSE })
    })

    # Set TRUE as default for isBaseLayer
    isBaseLayer <- lapply(curr@spadesGrobList, function(x) {
      lapply(x, function(y) { TRUE })
    })

    isNewPlot <- lapply(curr@spadesGrobList, function(x) {
      lapply(x, function(y) { FALSE })
    })

    # For overplots
    for (plots in newNames[overplots]) {

      # update only those plotArgs that have changed.
      curr@spadesGrobList[[plots]][[1]]@plotArgs[whichParamsChanged[[plots]]] <-
        newSP@spadesGrobList[[plots]][[1]]@plotArgs[whichParamsChanged[[plots]]]

      needPlotting[[plots]][[plots]] <- TRUE
      isReplot[[plots]][[plots]] <- TRUE
      isBaseLayer[[plots]][[plots]] <- FALSE
      isNewPlot[[plots]][[plots]] <- FALSE
    }

    # put addTo plots into list of spadesGrobs that it will be added to
    if (!is.null(addToPlotsNames)) {
      for (plots in 1:length(addToPlotsNames)) {
        #len <- length(curr@spadesGrobList[[addToPlotsNames[plots]]])
        curr@spadesGrobList[[addToPlotsNames[plots]]][names(addToPlotsNames[plots])] <-
          newSP@spadesGrobList[[names(addToPlotsNames[plots])]]
        # change the name of the plotName to the parent object
        curr@spadesGrobList[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]]@plotName <-
          curr@spadesGrobList[[addToPlotsNames[plots]]][[1]]@plotName
        needPlotting[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]] <-
          TRUE
        isReplot[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]] <-
          FALSE
        isBaseLayer[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]] <-
          FALSE
        isNewPlot[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]] <-
          FALSE
      }
    }

    # for new plots
    for (plots in newNames[needNew]) {
      curr@spadesGrobList[[plots]] <- newSP@spadesGrobList[[plots]]
      needPlotting[[plots]] <- TRUE
      isReplot[[plots]] <- FALSE
      isBaseLayer[[plots]] <- TRUE
      isNewPlot[[plots]] <- TRUE
    }
    return(
      list(
        curr = curr, whichParamsChanged = whichParamsChanged,
        needPlotting = needPlotting, isReplot = isReplot,
        isBaseLayer = isBaseLayer, isNewPlot = isNewPlot
      )
    )
  }
)

#' @rdname updateSpadesPlot
setMethod(
  ".updateSpadesPlot",
  signature = c(newSP = ".spadesPlot", curr = NULL),
  definition = function(newSP, ...) {
    return(list(
      curr = newSP, whichParamsChanged = NULL,
      needPlotting = lapply(newSP@spadesGrobList, function(x) {
        lapply(x, function(y) { TRUE })
      }),
      isReplot = lapply(newSP@spadesGrobList, function(x) {
        lapply(x, function(y) { FALSE })
      }),
      isNewPlot = lapply(newSP@spadesGrobList, function(x) {
        lapply(x, function(y) { TRUE })
      }),
      isBaseLayer = lapply(newSP@spadesGrobList, function(x) {
        lapply(x, function(y) { TRUE })
      })
    ))
})

################################################################################
#' Determine optimal plotting arrangement of plot objects
#'
#' Internal function. Assesses the device geometry, the map geometry, and the
#' number of spatial objects to plot and builds an object that will be used by
#' the Plot functions to plot them efficiently.
#'
#' @param sPlot A \code{.spadesPlot} object.
#'
#' @rdname arrangeViewports
#' @include plotting-classes.R
#' @importFrom grDevices dev.cur dev.new dev.size
#' @importFrom sp bbox
#' @export
#' @author Eliot McIntire
#' @docType methods
# igraph exports %>% from magrittr
setGeneric(".arrangeViewports", function(sPlot) {
  standardGeneric(".arrangeViewports")
})

#' @export
#' @rdname arrangeViewports
setMethod(
  ".arrangeViewports",
  signature = c(".spadesPlot"),
  definition = function(sPlot) {
    sgl <- sPlot@spadesGrobList

    dimx <- apply(do.call(
      rbind, sapply(1:length(sgl), function(x) {
        lapply(sgl[[x]][[1]]@isSpatialObjects, function(z) {
          if (z == TRUE) {
            # for spatial objects
            apply(
              bbox(
                eval(
                  parse(text = sgl[[x]][[1]]@objName),
                  envir = sgl[[x]][[1]]@envir
                )
              ),
              1,
              function(y) {
                diff(range(y))
              }
            )
          } else {
            # for non spatial objects
            c(1,1)
          }
        })
      })), 2, max)

    nPlots <- length(sgl)
    names <- names(sgl)

    if (dev.cur() == 1) {
      dev.new(height = 8, width = 10)
    }

    ds <- dev.size()
    ds.ratio <- ds[1] / ds[2]

    dimensionRatio <- dimx[1] / dimx[2]

    ds.dimensionRatio <- ds.ratio / dimensionRatio

    col.by.row <- data.frame(matrix(ncol = 2, nrow = nPlots))

    col.by.row[, 1] <- ceiling(nPlots / (1:nPlots))
    col.by.row[, 2] <- ceiling(nPlots / col.by.row[, 1])

    # wh.best <- which.min(abs(apply(col.by.row, 1, function(x) { x[1]/x[2] }) - ds.dimensionRatio))
    # rewritten for clarity/brevity with pipes below
    wh.best <- apply(col.by.row, 1, function(x) { x[1] / x[2] }) %>%
      `-`(., ds.dimensionRatio) %>%
      abs %>%
      which.min

    columns <- col.by.row[wh.best, 1]
    rows <- col.by.row[wh.best, 2]

    actual.ratio <- columns / rows

    out <- new(
      ".arrangement", rows = rows, columns = columns,
      actual.ratio = actual.ratio,
      ds.dimensionRatio = ds.dimensionRatio,
      ds = ds
    )
    return(out)
  }
)

################################################################################
#' Plot spatial grobs (using \code{grid} package)
#'
#' Internal function. Plot a raster Grob, a points Grob, polygon Grob.
#'
#' \code{speedup} is only used for \code{SpatialPolygons}, \code{SpatialPoints},
#' and \code{SpatialLines} in this function.
#' Attempts have been made to subsample at a good level that optimizes speed of
#' plotting, without losing visible quality. Nevertheless, to force all points to
#' be plotted, use a speedup value less than 0.1.
#' From a speed perspective, there appears to be an optimal subsampling when
#' using \code{thin} from the \code{fastshp} package.
#' Presumably, too much thinning requires large distance matrices to be
#' calculated, slowing plotting down.
#' Too little thinning causes an overabundance of points to be plotted, slowing
#' plotting down.
#'
#' The suggested package \code{fastshp} can be installed with:
#' \code{install.packages("fastshp", repos = "http://rforge.net", type = "source")}.
#'
#' NOTE: you may get errors relating to not having installed the software tools
#' required for building R packages on your system.
#' For building on Windows, you'll need to install Rtools from
#' \url{https://cran.r-project.org/bin/windows/Rtools/}.
#'
#' @param grobToPlot  \code{Raster*}, \code{SpatialLines*},
#'                    \code{SpatialPoints*}, or \code{SpatialPolygons*} object.
#'
#' @param col     Currently only used for the legend of a \code{Raster*} object.
#'
#' @param size    The size of the \code{SpatialPoints}.
#'
#' @param gp      \code{grid} parameters, usually the output of a call to
#'                \code{\link{gpar}}.
#'
#' @param gpText  \code{gpar} object for legend label text.
#'
#' @param legend  Logical indicating whether a legend should be drawn.
#'                Default \code{TRUE}.
#'
#' @param legendText  Vector of values to use for legend value labels.
#'                    Defaults to \code{NULL} which results in a pretty numeric
#'                    representation. If \code{Raster*} has a Raster Attribute
#'                    Table (rat; see \code{raster} package), this will be used
#'                    by default. Currently, only a single vector is accepted.
#'
#' @param length  Numeric.
#'
#' @param minv    The minimum value on a \code{Raster*}. Required because not
#'                all Rasters have this defined internally.
#'
#' @param maxv    The maximum value on a \code{Raster*}. Required because not
#'                all Rasters have this defined internally.
#'
#' @param pch     Point character for \code{SpatialPoints}, as \code{par}.
#'
#' @param real    Logical indicating whether the data are \code{real} numbers
#'                (i.e., as opposed to \code{integer} or \code{factor}).
#'
#' @param speedup Numeric. The factor by which the number of vertices in
#'                \code{SpatialPolygons} and \code{SpatialLines*} will be
#'                subsampled. The vertices are already subsampled by default to
#'                make plotting faster.
#'
#' @param ...     Additional arguments. None currently implemented.
#'
#' @docType methods
#' @rdname plotGrob
#' @importFrom data.table data.table ':='
#' @importFrom raster extent pointDistance xmin xmax ymin ymax
#' @importFrom sp proj4string
#' @importFrom grid gpar gTree gList rasterGrob textGrob grid.draw
#' @importFrom grDevices as.raster
#'
#' @author Eliot McIntire
# package grid is imported in spade-package.R
# igraph exports %>% from magrittr
setGeneric(".plotGrob", function(grobToPlot, col = NULL, real = FALSE,
                                 size = unit(5, "points"), minv, maxv,
                                 legend = TRUE, legendText = NULL,
                                 length = NULL,
                                 gp = gpar(), gpText = gpar(), pch = 19,
                                 speedup = 1, ...) {
  standardGeneric(".plotGrob")
})

#' @rdname plotGrob
setMethod(
  ".plotGrob",
  signature = c("matrix"),
  definition = function(grobToPlot, col, real, size, minv, maxv,
                        legend, legendText, gp, gpText, pch, ...) {
    pr <- if (real) {
      pretty(range(minv, maxv))
    } else {
      if (!is.null(legendText)) {
        if (NCOL(legendText) > 1) { # means it was a factor
          if (identical(legendText$ID, 1:NROW(legendText))) {
            unique(round(pretty(range(minv, maxv), n = length(levels(legendText[, 2])))))
          } else {
            legendText$contigValue <- 1:NROW(legendText)
            legendText$contigValue
          }
        } else {
          unique(round(pretty(range(minv, maxv), n = length(legendText))))
        }
      } else {
        unique(round(pretty(range(minv, maxv))))
      }
    }

    pr <- pr[pr <= maxv & pr >= minv]
    if (length(pr) == 0) pr <- seq(minv, maxv, by = 2)
    #maxNumCols = 100
    maxcol <- length(col)
    mincol <- 2

    gpText$cex <- gpText$cex * 0.6
    if (length(gpText) == 0)
      gpText <- gpar(col = "black", cex = 0.6)

    rastGrob <- gTree(
      grobToPlot = grobToPlot, pr = pr, col = col,
      children = gList(
        rasterGrob(
          as.raster(grobToPlot),
          interpolate = FALSE,
          name = "raster"
        ),
        if (legend) {
          if (NCOL(legendText) > 1) {
            # for factors
            colForLegend <- col[rev(legendText$ID + 1)]
          } else {
            colForLegend <- col[(maxcol):mincol]
          }
          rasterGrob(
            as.raster(colForLegend),
            x = 1.04, y = 0.5,
            height = 0.5, width = 0.03,
            interpolate = FALSE,
            name = "legend"
          )
        },
        if (legend) {
          txt <- if (is.null(legendText)) {
            pr
          } else {
            legendIndex <- pr - min(pr) + 1
            if (NCOL(legendText) > 1){ # for factor legends
              legendText[legendIndex, 2]
            } else {
              legendText[legendIndex]
            }
          }
          textGrob(
            txt,
            x = 1.08,
            y = if (!real) {
              if (NCOL(legendText) > 1) { # factors
                maxv <- NROW(legendText)
              }
              ((pr - minv) / ((maxv + 1) - minv)) / 2 + 0.25 + 1 /
                (diff(range(minv, maxv)) + 1) / 4
            } else {
              ((pr - minv) / ((maxv) - minv)) / 2 + 0.25
            },
            gp = gpText,
            just = "left", check.overlap =
              TRUE,
            name = "legendText"
          )
        }
      ),
      gp = gp, cl = "plotRast"
    )
    grid.draw(rastGrob)
    return(invisible(rastGrob))
  }
)

# @rdname plotGrob
# setMethod(
#   ".plotGrob",
#   signature = c("SpatialPoints"),
#   definition = function(grobToPlot, col, size,
#                         legend, gp = gpar(), pch, ...) {
#     pntGrob <- gTree(
#       grobToPlot = grobToPlot,
#       children = gList(
#         pointsGrob(
#           x = coordinates(grobToPlot)[,1], y = coordinates(grobToPlot)[,2],
#           pch = pch, size = size
#         )
#       ),
#       gp = gp,
#       cl = "plotPoint"
#     )
#     grid.draw(pntGrob)
#     return(invisible(pntGrob))
#   }
# )

############## SpatialPoints - thin
#' @rdname plotGrob
#' @importFrom grid pointsGrob
setMethod(
  ".plotGrob",
  signature = c("SpatialPoints"),
  definition = function(grobToPlot, col, size,
                        legend, gp = gpar(), pch, speedup, ...) {
    speedupScale <- 40
    speedupScale = if (grepl(proj4string(grobToPlot), pattern = "longlat")) {
      pointDistance(
        p1 = c(xmax(extent(grobToPlot)), ymax(extent(grobToPlot))),
        p2 = c(xmin(extent(grobToPlot)), ymin(extent(grobToPlot))),
        lonlat = TRUE
      ) / (4.8e5*speedupScale)
    } else {
      max(ymax(extent(grobToPlot)) - ymin(extent(grobToPlot)),
          xmax(extent(grobToPlot)) - xmin(extent(grobToPlot))) /
        speedupScale
    }
    xyOrd <- coordinates(grobToPlot)

    if (!is.null(col)) {
      if (!is.null(gp)) {
        gp$col <- col # Accept col argument
      } else {
        gp <- gpar(col) #
      }
    }

    if (NROW(xyOrd) > 1e3) {
      # thin if greater than 1000 pts
      if (speedup > 0.1) {
        if (requireNamespace("fastshp", quietly = TRUE)) {
          thinned <- data.table(
            thin = fastshp::thin(xyOrd[, 1], xyOrd[, 2],
                                 tolerance = speedupScale * speedup)
          )
          #thinned[, groups:= rep(1:NROW(idLength), idLength$V1)]
          #idLength <- thinned[, sum(thin),by = groups]
          xyOrd <- xyOrd[thinned$thin, ]
        } else {
          message(
            paste(
              "To speed up Polygons plotting using Plot install the fastshp package:\n",
              "install.packages(\"fastshp\", repos=\"http://rforge.net\", type=\"source\")."
            )
          )
          if (Sys.info()[["sysname"]] == "Windows") {
            message(
              paste(
                "You may also need to download and install Rtools from:\n",
                " https://cran.r-project.org/bin/windows/Rtools/"
              )
            )
          }
        }
      }
    }

    pntGrob <- gTree(
      grobToPlot = grobToPlot,
      children = gList(
        pointsGrob(
          x = xyOrd[,1], y = xyOrd[,2],
          pch = pch, size = size
        )
      ),
      gp = gp,
      cl = "plotPoint"
    )
    grid.draw(pntGrob)
    return(invisible(pntGrob))
  }
)

################################################################################
#' @rdname plotGrob
#' @importFrom grid polygonGrob
setMethod(
  ".plotGrob",
  signature = c("SpatialPolygons"),
  definition = function(grobToPlot, col, size,
                        legend, gp = gpar(), pch, speedup, ...) {
    speedupScale = if (grepl(proj4string(grobToPlot), pattern = "longlat")) {
      pointDistance(
        p1 = c(xmax(extent(grobToPlot)), ymax(extent(grobToPlot))),
        p2 = c(xmin(extent(grobToPlot)), ymin(extent(grobToPlot))),
        lonlat = TRUE
      ) / 1.2e10
    } else {
      max(ymax(extent(grobToPlot)) - ymin(extent(grobToPlot)),
          xmax(extent(grobToPlot)) - xmin(extent(grobToPlot))) /
        2.4e4
    }
    # For speed of plotting
    xy <- lapply(1:length(grobToPlot), function(i) {
      lapply(grobToPlot@polygons[[i]]@Polygons, function(j) {
        j@coords
      })
    })

    hole <- lapply(1:length(grobToPlot), function(x) {
      lapply(grobToPlot@polygons[[x]]@Polygons, function(x)
        x@hole)
    }) %>%
      unlist

    ord <- grobToPlot@plotOrder

    ordInner <- lapply(1:length(grobToPlot), function(x) {
      grobToPlot@polygons[[x]]@plotOrder
    })

    xyOrd.l <- lapply(ord, function(i) {
        xy[[i]][ordInner[[i]]]
    })

    # idLength <- data.table(V1=unlist(lapply(xyOrd.l, function(i) lapply(i, length)))/2)
    idLength <- lapply(xyOrd.l, function(i) { lapply(i, length) }) %>%
      unlist %>%
      `/`(., 2) %>%
      data.table(V1 = .)

    xyOrd <- do.call(rbind, lapply(xyOrd.l, function(i) { do.call(rbind, i) }))

    if (!is.null(col)) {
      if (!is.null(gp)) {
        gp$col <- col # Accept col argument
      } else {
        gp <- gpar(col) #
      }
    }

    if (NROW(xyOrd) > 1e3) {
      # thin if fewer than 1000 pts
      if (speedup > 0.1) {

        if (requireNamespace("fastshp", quietly = TRUE)) {
          thinned <- data.table(
            thin = fastshp::thin(xyOrd[, 1], xyOrd[, 2],
                                 tolerance = speedupScale * speedup)
          )
          thinned[, groups := rep(1:NROW(idLength), idLength$V1)]
          idLength <- thinned[, sum(thin),by = groups]
          xyOrd <- xyOrd[thinned$thin, ]
        } else {
          message(
            paste(
              "To speed up Polygons plotting using Plot install the fastshp package:\n",
              "install.packages(\"fastshp\", repos=\"http://rforge.net\", type=\"source\")."
            )
          )
          if (Sys.info()[["sysname"]] == "Windows") {
            message(
              paste(
                "You may also need to download and install Rtools from:\n",
                " https://cran.r-project.org/bin/windows/Rtools/"
              )
            )
          }
        }
      }
    }

    gp$fill[hole] <- "#FFFFFF00"
    polyGrob <- gTree(children = gList(
      polygonGrob(
        x = xyOrd[, 1], y = xyOrd[, 2], id.lengths = idLength$V1,
        gp = gp, default.units = "native"
      )
    ),
    gp = gp,
    cl = "plotPoly")
    grid.draw(polyGrob)
    return(invisible(polyGrob))
})

#' @rdname plotGrob
#' @importFrom grid polylineGrob arrow
setMethod(
  ".plotGrob",
  signature = c("SpatialLines"),
  definition = function(grobToPlot, col, size,
                        legend, length, gp = gpar(), pch, speedup, ...) {
    speedupScale <- if (grepl(proj4string(grobToPlot), pattern = "longlat")) {
        pointDistance(
          p1 = c(xmax(extent(grobToPlot)), ymax(extent(grobToPlot))),
          p2 = c(xmin(extent(grobToPlot)), ymin(extent(grobToPlot))),
          lonlat = TRUE
        ) / 1.2e10
      } else {
        max(ymax(extent(grobToPlot)) - ymin(extent(grobToPlot)),
            xmax(extent(grobToPlot)) - xmin(extent(grobToPlot))) / 2.4e4
      }

    # For speed of plotting
    xy <- lapply(1:length(grobToPlot),
                 function(i) {
                   grobToPlot@lines[[i]]@Lines[[1]]@coords
                 })
    idLength <- unlist(lapply(xy, length)) / 2
    xy <- do.call(rbind,xy)

    if (NROW(xy) > 1e3) {
      # thin if fewer than 1000 pts
      if (speedup>0.1) {

        if (requireNamespace("fastshp", quietly = TRUE)) {
          thinned <- fastshp::thin(xy[, 1], xy[, 2],
                                   tolerance = speedupScale * speedup)

          # keep first and last points of every polyline,
          # if there are fewer than 10,000 vertices
          if (sum(thinned) < 1e4) {
            lastIDs <- cumsum(idLength)

            # Ensure first and last points of each line are kept:
            thinned[c(1,lastIDs + 1)[-(1 + length(lastIDs))]] <- TRUE
            thinned[lastIDs] <- TRUE
          }
          xy <- xy[thinned,]
          idLength <-
            tapply(thinned, rep(1:length(idLength), idLength), sum)
        } else {
          message(
            paste(
              "To speed up Lines plotting using Plot, install the fastshp package:\n",
              "install.packages(\"fastshp\", repos=\"http://rforge.net\", type=\"source\")"
            )
          )
          if (Sys.info()[["sysname"]] == "Windows") {
            message(
              paste(
                "You may also need to download and install Rtools from:\n",
                "  https://cran.r-project.org/bin/windows/Rtools/"
              )
            )
          }
        }
      }
    }

    if (is.null(length)) {
      lineGrob <- gTree(children = gList(
        polylineGrob(
          x = xy[, 1], y = xy[, 2], id.lengths = idLength,
          gp = gp, default.units = "native"
        )
      ),
      gp = gp,
      cl = "plotLine")
    } else {
      lineGrob <- gTree(children = gList(
        polylineGrob(
          x = xy[, 1], y = xy[, 2], id.lengths = idLength,
          gp = gp, default.units = "native",
          arrow = arrow(length = unit(length, "inches"))
        )
      ),
      gp = gp,
      cl = "plotLine")
    }

    grid.draw(lineGrob)
    return(invisible(lineGrob))
})

################################################################################
#' Make an optimal layout of plots
#'
#' Internal function. Using the size of the current device, and number and
#' dimension ratios of the plots, place them optimally in the plotting region.
#'
#' @param arr an object of class \code{.arrangement}.
#'
#' @param visualSqueeze Numeric. The proportion of the white space to be used
#'                      for plots. Default is 0.75.
#'
#' @param legend Logical indicating whether legend should be included as part of
#'               layout calculation. Default is \code{TRUE}.
#'
#' @param axes Logical indicating whether the axes should be included as part of
#'             layout calculation. Default is \code{TRUE}.
#'
#' @param title Logical indicating whether the names of each plot should be
#'              written above plots and should be included as part of layout
#'               calculation. Default is \code{TRUE}.
#'
#' @include plotting-classes.R
#' @importFrom grid unit unit.c
#' @rdname makeLayout
#' @docType methods
#' @author Eliot McIntire
#'
.makeLayout <- function(arr, visualSqueeze,
                        legend = TRUE, axes = TRUE, title = TRUE) {
    columns <- arr@columns
    rows <- arr@rows

    # Reduce by 40% of remaining space if each of the following is not wanted
    if (legend == FALSE) {
      visualSqueeze <- visualSqueeze + 0.4 * (1 - visualSqueeze)
    }
    if (axes == FALSE) {
      visualSqueeze <- visualSqueeze + 0.4 * (1 - visualSqueeze)
    }
    if (title == FALSE) {
      visualSqueeze <- visualSqueeze + 0.4 * (1 - visualSqueeze)
    }

    # calculate the visualSqueeze for the width (i.e., vS.w)
    vS.w <- min(
      visualSqueeze / columns,
      visualSqueeze / columns * arr@actual.ratio / arr@ds.dimensionRatio
    )

    wdth <- unit.c(unit(0.2, "null"),
                   unit(rep(c(0.875, vS.w, 0.875), columns),
                        rep(c("null","npc", "null"), columns)),
                   unit(0.2, "null"))

    # calculate the visualSqueeze for the height (i.e., vS.h)
    vS.h <- min(visualSqueeze / rows,
                visualSqueeze / rows * arr@ds.dimensionRatio / arr@actual.ratio)
    ht <- unit.c(unit(0.2, "null"),
                 unit(rep(c(0.875, vS.h, 0.875), rows),
                      rep(c("null", "npc", "null"), rows)),
                 unit(0.2, "null"))

    return(list(wdth = wdth, ht = ht, wdthUnits = vS.w, htUnits = vS.h,
                visualSqueeze = visualSqueeze))
}

################################################################################
#' Make viewports
#'
#' Given a set of extents, and a layout for these extents, this function will
#' output a viewport tree to allow plotting.
#'
#' This function will either create a totally new set of viewports, or simply
#' add some nested viewports to an existing arrangement, i.e., is there still
#' white space availabe to plot.
#'
#' @param sPlot An object of class \code{.spadesPlot}.
#'
#' @param newArr  Logical indicating whether this function will create a
#'                completely new viewport. Default \code{FALSE}.
#'
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @importFrom grid viewport vpTree vpList
#' @importFrom raster xmin xmax ymin ymax
#' @rdname makeViewports
#'
.makeViewports <- function(sPlot, newArr = FALSE) {
  arr <- sPlot@arr
  sgl <- sPlot@spadesGrobList

  extents <- unlist(sapply(sgl, function(x) {
    unname(lapply(x[[1]]@isSpatialObjects, function(z) {
      if (z == TRUE) {
        # for spatial objects
        if (!is.null(x[[1]]@plotArgs$zoomExtent)) {
          x[[1]]@plotArgs$zoomExtent
        } else {
          extent(eval(parse(text = x[[1]]@objName), envir = x[[1]]@envir))
        }
      } else {
        # for non spatial objects
        extent(c(xmin=0, xmax=1, ymin=0, ymax=1))
      }
    }))
  }))

  columns <- arr@columns
  rows <- arr@rows
  topVp <- viewport(
    layout = grid.layout(
      nrow = rows * 3 + 2,
      ncol = columns * 3 + 2,
      widths = arr@layout$wdth,
      heights = arr@layout$ht
    ),
    name = "top"
  )
  plotVps <- list()

  nam <- names(extents)

  # This is the biggest of the extents, and is used in .makeLayout
  #  Need to replicate it here because all plots are scaled to this
  biggestDims <-
    apply(do.call(rbind,sapply(1:length(sgl), function(x) {
      lapply(sgl[[x]][[1]]@isSpatialObjects, function(z) {
        if (z == TRUE) {
          # for spatial objects
          apply(bbox(extents[[x]]),1,function(y)
            diff(range(y)))
        } else {
          # for non spatial objects
          c(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
        }
      })
    })), 2, max)

  for (extentInd in 1:length(extents)) {
    posInd <- match(nam[extentInd], names(sgl))
    lpc <- ceiling((posInd - 1) %% columns + 1) * 3
    lpr <- ceiling(posInd / columns) * 3

    if (!sgl[[posInd]][[1]]@isSpatialObjects) {
      lpc <- c((lpc - 1):(lpc + 1))
      lpr <- c((lpr):(lpr + 1))
    }
    # makes equal scale
    yrange <- extents[[extentInd]]@ymax - extents[[extentInd]]@ymin
    if (yrange > 0) {
      if (abs((yrange /
               (extents[[extentInd]]@xmax - extents[[extentInd]]@xmin)) -
              (biggestDims[1] / biggestDims[2]))
          > (getOption("spades.tolerance"))) {
        dimensionRatio <- arr@layout$wdthUnits * arr@ds[1] /
          (arr@layout$htUnits * arr@ds[2])
        plotScaleRatio <-
          (extents[[extentInd]]@xmin - extents[[extentInd]]@xmax) /
          (extents[[extentInd]]@ymin - extents[[extentInd]]@ymax)

        vS.w <- min(1, plotScaleRatio / dimensionRatio)

        vS.h <- min(1, dimensionRatio / plotScaleRatio)

        addY <-
          abs(extents[[extentInd]]@ymax - extents[[extentInd]]@ymin -
                (extents[[extentInd]]@ymax - extents[[extentInd]]@ymin) /
                vS.h) / 2
        addX <-
          abs(extents[[extentInd]]@xmax - extents[[extentInd]]@xmin -
                (extents[[extentInd]]@xmax - extents[[extentInd]]@xmin) /
                vS.w) / 2
      } else {
        addY <- addX <- 0
      }
    } else {
      addX <- extents[[extentInd]]@xmin * 0.05
      addY <- extents[[extentInd]]@ymin * 0.05
    }
    # end equal scale
    plotVps[[extentInd]] <- viewport(
      name = nam[extentInd],
      layout.pos.col = lpc,
      layout.pos.row = lpr,
      xscale = c(extents[[extentInd]]@xmin - addX, extents[[extentInd]]@xmax +
                   addX),
      yscale = c(extents[[extentInd]]@ymin - addY, extents[[extentInd]]@ymax +
                   addY)
    )
  }

  if (newArr) {
    wholeVp <- vpTree(topVp, do.call(vpList, plotVps))
  } else {
    wholeVp <- do.call(vpList, plotVps)
  }
  return(list(wholeVp = wholeVp, extents = extents))
}

################################################################################
#' Plot: Fast, optimally arranged, multipanel plotting function with SpaDES
#'
#' The main plotting function accompanying \code{SpaDES}.
#' This can take objects of type \code{Raster*}, \code{SpatialPoints*},
#' \code{SpatialPolygons*}, and any combination of those.
#' It can also handle \code{ggplot2} objects or \code{base::histogram} objects
#' via call to \code{exHist <- hist(1:10, plot = FALSE)}.
#' Customization of the \code{ggplot2} elements can be done as a normal
#' \code{ggplot2} plot, then added with \code{Plot(ggplotObject)}.
#'
#' NOTE: Plot uses the \code{grid} package; therefore, it is NOT compatible with
#' base R graphics. Also, because it does not by default wipe the plotting device
#' before plotting, a call to \code{\link{clearPlot}} could be helpful to resolve
#' many errors.
#'
#' If \code{new = TRUE}, a new plot will be generated.
#' This is equivalent to calling \code{clearPlot(); Plot(Object)},
#' i.e,. directly before creating a new Plot.
#' When \code{new = FALSE}, any plot that already exists will be overplotted,
#' while plots that have not already been plotted will be added.
#' This function rearranges the plotting device to maximize the size of all the
#' plots, minimizing white space.
#' If using the RStudio IDE, it is recommended to make and use a new device
#' with \code{dev()}, because the built in device is not made for rapid redrawing.
#' The function is based on the grid package.
#'
#' Each panel in the multipanel plot must have a name.
#' This name is used to overplot, rearrange the plots, or overlay using
#' \code{addTo} when necessary.
#' If the \code{...} are named spatialObjects, then \code{Plot} will use
#' these names. However, this name will not persist when there is a future call
#' to \code{Plot} that forces a rearrangement of the plots.
#' A more stable way is to use the object names directly, and any layer names
#' (in the case of \code{RasterLayer} or \code{RasterStack} objects).
#' If plotting a RasterLayer and the layer name is "layer" or the same as the
#' object name, then, for simplicity, only the object name will be used.
#' In other words, only enough information is used to uniquely identify the plot.
#'
#' \code{cols} is a vector of colours that can be understood directly, or by
#' \code{colorRampePalette}, such as \code{c("orange", "blue")}, will give a
#' colour range from orange to blue, interploated.
#' If a list, it will be used, in order, for each item to be plotted.
#' It will be recycled if it is shorter than the objects to be plotted.
#' Note that when this approach to setting colours is used, any overplotting
#' will revert to the \code{colortable} slot of the object, or the default
#' for rasters, which is \code{terrain.color()}
#'
#' \code{cols} can also accept \code{RColorBrewer} colors by keyword if it is
#' character vector of length 1. i.e., this cannot be used to set many objects by keyword in
#' the same Plot call. Default \code{terrain.color()}. See Details.
#'
#' Some coloring will be automatic. If the object being plotted is a Raster, then
#' this will take the colorTable slot (can be changed via setColors() or other ways).
#' If this is a SpatialPointsDataFrame, this function will use a column called \code{colors}
#' and apply these to the symbols.
#'
#' Silently, one hidden object is made, \code{.spadesPlot} in the
#' \code{.spadesEnv} environment, which is used for arranging plots in the
#' device window, and identifying the objects to be replotted if rearranging
#' is required, subsequent to a \code{new = FALSE} additional plot.
#'
#' This function is optimized to allow modular Plotting. This means that several
#' behaviours will appear unusual.
#' For instance, if a first call to \code{Plot} is made, the legend will reflect
#' the current color scheme. If a second or subsequent call to \code{Plot} is
#' made with the same object but with different colours (e.g., with \code{cols}),
#' the legend will not update. This behaviour is made with the decision that the
#' original layer takes precedence and all subsequent plots to that same frame
#' are overplots only.
#'
#' \code{speedup} is not a precise number because it is faster to plot an
#' un-resampled raster if the new resampling is close to the original number of
#' pixels.
#' At the moment, for rasters, this is set to 1/3 of the original pixels.
#' In other words, \code{speedup} will not do anything if the factor for
#' speeding up is not high enough (i.e., >3). If no sub-sampling is desired,
#' use a speedup value less than 0.1.
#'
#' These \code{gp*} parameters will specify plot parameters that are available
#' with \code{gpar()}. \code{gp} will adjust plot parameters, \code{gpText}
#' will adjust title and legend text, \code{gpAxis} will adjust the axes.
#' \code{size} adjusts point size in a \code{SpatialPoints} object.
#' These will persist with the original \code{Plot} call for each individual object.
#' Multiple entries can be used, but they must be named list elements and they
#' must match the \code{...} items to plot.
#' This is true for a \code{RasterStack} also, i.e., the list of named elements
#' must be the same length as the number of layers being plotted.
#' The naming convention used is: \code{RasterStackName$layerName}, i.e,
#' \code{landscape$DEM}.
#'
#' @param ... A combination of \code{spatialObjects} or some non-spatial objects.
#'            See details.
#'
#' @param new Logical. If \code{TRUE}, then the previous plot is wiped and a
#'            new one made; if \code{FALSE}, then the \code{...} plots will be
#'            added to the current device, adding or rearranging the plot layout
#'            as necessary. Default is \code{FALSE}.
#'
#' @param addTo Character vector, with same length as \code{...}.
#'              This is for overplotting, when the overplot is not to occur on
#'              the plot with the same name, such as plotting a
#'              \code{SpatialPoints*} object on a \code{RasterLayer}.
#'
#' @param gp A \code{gpar} object, created by \code{\link{gpar}} function,
#'           to change plotting parameters (see \code{grid} package).
#'
#' @param gpText A \code{gpar} object for the title text.
#'               Default \code{gpar(col = "black")}.
#'
#' @param gpAxis A \code{gpar} object for the axes.
#'               Default \code{gpar(col = "black")}.
#'
#' @param axes Logical or \code{"L"}, representing the left and bottom axes,
#'             over all plots.
#'
#' @param speedup Numeric. The factor by which the number of pixels is divided
#'                by to plot rasters. See Details.
#'
#' @param size Numeric. The size, in points, for \code{SpatialPoints} symbols,
#'             if using a scalable symbol.
#'
#' @param cols (also \code{col}) Character vector or list of character vectors of colours. See details.
#'
#' @param zoomExtent An \code{Extent} object. Supplying a single extent that is
#'                   smaller than the rasters will call a crop statement before
#'                   plotting. Defaults to \code{NULL}.
#'                   This occurs after any downsampling of rasters, so it may
#'                   produce very pixelated maps.
#'
#' @param visualSqueeze Numeric. The proportion of the white space to be used
#'                      for plots. Default is 0.75.
#'
#' @param legend Logical idicating whether a legend should be drawn.
#'               Default is \code{TRUE}.
#'
#' @param legendRange Numeric vector giving values that, representing the lower
#'                    and upper bounds of a legend (i.e., \code{1:10} or
#'                    \code{c(1,10)} will give same result) that will override
#'                    the data bounds contained within the \code{grobToPlot}.
#'
#' @param legendText Character vector of legend value labels.
#'                   Defaults to \code{NULL}, which results in a pretty numeric
#'                   representation.
#'                   If \code{Raster*} has a Raster Attribute Table (rat; see
#'                   \code{\link{raster}} package), this will be used by default.
#'                   Currently, only a single vector is accepted.
#'                   The length of this must match the length of the legend, so
#'                   this is mosty useful for discrete-valued rasters.
#'
#' @param na.color Character string indicating the color for \code{NA} values.
#'                 Default transparent.
#'
#' @param zero.color Character string indicating the color for zero values,
#'                   when zero is the minimum value, otherwise, zero is
#'                   treated as any other color. Default transparent.
#'
#' @param pch see \code{?par}.
#'
#' @param title Logical indicating whether the names of each plot should be
#'              written above plots.
#'
#' @param length Numeric. Optional length, in inches, of the arrow head.
#'
#' @return Invisibly returns the \code{.spadesPlot} class object.
#' If this is assigned to an object, say \code{obj}, then this can be plotted
#' again with \code{Plot(obj)}.
#' This object is also stored in the locked \code{.spadesEnv}, so can simply be
#' replotted with \code{rePlot()} or on a new device with \code{rePlot(n)},
#' where \code{n} is the new device number.
#'
#' @seealso \code{\link{clearPlot}}, \code{\link{gpar}}, \code{\link{raster}},
#' \code{\link{par}}, \code{\link{SpatialPolygons}}, \code{\link{grid.polyline}},
#' \code{\link{ggplot}}, \code{\link{dev}}
#'
#' @rdname Plot
#' @export
#' @importFrom gridBase gridFIG
#' @importFrom ggplot2 ggplot
#' @importFrom raster crop is.factor
#' @importFrom grid upViewport pushViewport seekViewport grid.text
#' @importFrom grid grid.rect grid.xaxis grid.yaxis current.parent gpar
#' @importFrom grDevices dev.cur dev.size
#'
#' @include environment.R
#' @include plotting-classes.R
#' @include plotting-colours.R
#' @include plotting-helpers.R
#' @include plotting-other.R
#'
#' @author Eliot McIntire
#'
#' @examples
#' \dontrun{
#' library(sp)
#' library(raster)
#' library(rgdal)
#' library(igraph)
#' library(RColorBrewer)
#' # Make list of maps from package database to load, and what functions to use to load them
#' filelist <-
#'    data.frame(files =
#'      dir(file.path(
#'        find.package("SpaDES", quiet = FALSE), "maps"),
#'        full.names = TRUE, pattern= "tif"),
#'      functions = "rasterToMemory",
#'      packages = "SpaDES",
#'      stringsAsFactors = FALSE)
#'
#' # Load files to memory (using rasterToMemory)
#' mySim <- loadFiles(filelist = filelist)
#'
#' # put layers into a single stack for convenience
#' landscape <- stack(mySim$DEM, mySim$forestCover, mySim$forestAge,
#'    mySim$habitatQuality, mySim$percentPine)
#'
#' # can change color palette
#' setColors(landscape, n = 50) <- list(DEM=topo.colors(50),
#'                            forestCover = brewer.pal(9, "Set1"),
#'                            forestAge = brewer.pal("Blues", n=8),
#'                            habitatQuality = brewer.pal(9, "Spectral"),
#'                            percentPine = brewer.pal("GnBu", n=8))
#'
#' # Make a new raster derived from a previous one; must give it a unique name
#' habitatQuality2 <- landscape$habitatQuality ^ 0.3
#' names(habitatQuality2) <- "habitatQuality2"
#'
#' # make a SpatialPoints object
#' caribou <- cbind(x = stats::runif (1e2, -50, 50), y = stats::runif (1e2, -50, 50)) %>%
#'   SpatialPoints(coords = .)
#'
#' # use factor raster to give legends as character strings
#' ras <- raster(matrix(sample(1:4, size=12, replace=TRUE),
#'    ncol=4, nrow=3))
#' # needs to have a data.frame with ID as first column - see ?raster::ratify
#' levels(ras) <- data.frame(ID=1:4, Name=paste0("Level",1:4))
#' Plot(ras, new=T)
#'
#' # Arbitrary values for factors
#' levels <- c(1,2,7)
#' ras <- raster(matrix(sample(levels, size=12, replace=TRUE),
#'    ncol=4, nrow=3))
#' levels(ras) <- data.frame(ID=levels, Name=sample(LETTERS,3))
#' Plot(ras, new=T)
#' \notrun{
#' dev(2)
#' }
#' Plot(landscape, new = TRUE)
#'
#' # Can overplot, using addTo
#' Plot(caribou, addTo = "landscape$forestAge", size = 4, axes = FALSE)
#'
#' # can add a plot to the plotting window
#' Plot(caribou, new = FALSE)
#'
#' # Can add two maps with same name, if one is in a stack; they are given
#' #  unique names based on object name
#' Plot(landscape, caribou, mySim$DEM)
#'
#' # can mix stacks, rasters, SpatialPoint*
#' Plot(landscape, habitatQuality2, caribou)
#'
#' # can mix stacks, rasters, SpatialPoint*, and SpatialPolygons*
#' Plot(landscape, caribou)
#' Plot(habitatQuality2, new = FALSE)
#' Sr1 = Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2))*20-50)
#' Sr2 = Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2))*20-50)
#' Srs1 = Polygons(list(Sr1), "s1")
#' Srs2 = Polygons(list(Sr2), "s2")
#' SpP = SpatialPolygons(list(Srs1, Srs2), 1:2)
#' Plot(SpP)
#' Plot(SpP, addTo = "landscape$forestCover", gp = gpar(lwd = 2))
#'
#' }
#'
# igraph exports %>% from magrittr
setGeneric(
  "Plot",
  signature = "...",
  function(..., new = FALSE, addTo = NULL,
           gp = gpar(), gpText = gpar(), gpAxis = gpar(), axes = FALSE,
           speedup = 1, size = 5, cols = NULL, zoomExtent = NULL,
           visualSqueeze = NULL, legend = TRUE, legendRange = NULL,
           legendText = NULL, pch = 19, title = TRUE, na.color = "#FFFFFF00",
           zero.color = NULL, length = NULL) {
    standardGeneric("Plot")
})

#' @rdname Plot
#' @export
setMethod(
  "Plot",
  signature("ANY"),
  definition = function(..., new, addTo, gp, gpText, gpAxis, axes, speedup,
                        size, cols, zoomExtent, visualSqueeze, legend,
                        legendRange, legendText, pch, title, na.color,
                        zero.color, length) {
    # Section 1 - extract object names, and determine which ones need plotting,
    # which ones need replotting etc.

    if (all(sapply(new, function(x) x))) { clearPlot(dev.cur()) }

    # this covers the case where R thinks that there is nothing, but
    #  there may in fact be something.
    if (length(ls(.spadesEnv)) == 0) clearPlot(dev.cur())

    scalls <- sys.calls()
    # Section 1 # Determine object names that were passed and layer names of each
    isDoCall <- grepl("do.call", scalls) & grepl("Plot", scalls)
    dots <- list(...)
#    if(any(grepl(pattern="col", names(dots)))) {
#      usedCol <- dots$col
      #cols <- dots$col
#    }
    if (any(isDoCall)) {
      whFrame <- grep(scalls, pattern = "^do.call")
      plotFrame <- sys.frame(whFrame - 1)
      argsFrame <- sys.frame(whFrame - 2)
      dotObjs <- get(as.character(match.call(do.call, call = sys.call(whFrame))$args),
                     envir = plotFrame)
      plotArgs <- mget(names(formals("Plot")[-1]), argsFrame)
    } else {
      whFrame <- grep(scalls, pattern = "^Plot")
      dotObjs <- dots
      plotFrame <- sys.frame(whFrame)
      plotArgs <- mget(names(formals("Plot")), plotFrame)[-1]
    }

    if (any(grepl(pattern = "col", names(dots)))) {
      cols <- dots$col
      plotArgs$cols <- cols
    }

    if (!is.null(dots$env)) {
      objFrame <- dots$env
    } else {
      objFrame <- plotFrame
    }

    if (all(sapply(plotArgs$new, function(x) x))) {
      clearPlot(dev.cur())
      new <- TRUE # This is necessary in a do.call case where the arguments aren't clear
      plotArgs$new <- TRUE # for future calls
    }

    whichSpadesPlottables <- sapply(dotObjs, function(x) {
      is(x, ".spadesPlottables") })

    canPlot <- if (!is.null(names(whichSpadesPlottables))) {
      whichSpadesPlottables[names(whichSpadesPlottables) != "env"]
    } else {
      whichSpadesPlottables
    }

    if (!all(canPlot)) {
      if ((sum(canPlot) - length(grep(pattern = "col", names(canPlot)))) > 0) {
        # don't message if col is passed
        message(paste(
          "Plot can only plot objects of class .spadesPlottables.",
          "Use 'showClass(\".spadesPlottables\")' to see current available",
          "classes"
        ))
      }
    }

    plotObjs <- dotObjs[whichSpadesPlottables]

    if (length(plotObjs) == 0) {
      stop("Not a plottable object")
    }
    nonPlotArgs <- dotObjs[!whichSpadesPlottables]
    if (any(grepl(pattern = "col", names(nonPlotArgs)))){
      nonPlotArgs$col <- NULL
    }

    # intercept cases that don't make sense, and give meaningful error
    if (!is.null(addTo)) {
      if (!tryCatch(
        addTo %in% unlist(
          layerNames(get(paste0("spadesPlot", dev.cur()), envir = .spadesEnv))
          ),
        error = function(x) { FALSE }
      )) {
        message(paste(
          "Plot called with 'addTo' argument specified, but that layer does",
          "not exist. Plotting object on its own plot."
        ))
        plotArgs$addTo <- NULL
      }
    }

    # Create a .spadesPlot object from the plotObjs and plotArgs

    isSpadesPlot <- sapply(plotObjs, function(x) { is(x, ".spadesPlot") })
    newSpadesPlots <- .makeSpadesPlot(
      plotObjs, plotArgs, whichSpadesPlottables, env = objFrame
    )

    if (exists(paste0("spadesPlot", dev.cur()), envir = .spadesEnv)) {
      currSpadesPlots <- .getSpaDES(paste0("spadesPlot", dev.cur()))

      visualSqueeze <- if (is.null(visualSqueeze)) {
        currSpadesPlots@arr@layout$visualSqueeze
      } else {
        visualSqueeze
      }

      updated <- .updateSpadesPlot(newSpadesPlots, currSpadesPlots)
      newArr <- (
        length(updated$curr@spadesGrobList) >
          prod(currSpadesPlots@arr@columns, currSpadesPlots@arr@rows)
      ) | !identical(currSpadesPlots@arr@ds,dev.size())

      if (newArr) {
        updated$needPlotting <- lapply(updated$needPlotting, function(x) {
          sapply(x, function(y) { TRUE })
        })
        updated$isReplot <- lapply(updated$isReplot, function(x) {
          sapply(x, function(y) { TRUE })
        })
        updated$isNewPlot <- lapply(updated$isReplot, function(x) {
            sapply(x, function(y) { TRUE })
        })
        updated$isBaseLayer <- lapply(updated$isReplot, function(x) {
            sapply(x, function(y) { TRUE })
        })
        clearPlot(removeData = FALSE)
      }

    } else if (all(isSpadesPlot)) {
      currSpadesPlots <- .makeSpadesPlot()
      newSpadesPlots <- plotObjs[[1]]

      visualSqueeze <- if (is.null(visualSqueeze)) {
        newSpadesPlots@arr@layout$visualSqueeze
      } else {
        visualSqueeze
      }

      updated <- .updateSpadesPlot(newSpadesPlots)
      newArr <- TRUE
    } else {
      currSpadesPlots <- .makeSpadesPlot()
      updated <- .updateSpadesPlot(newSpadesPlots)
      newArr <- TRUE
    }

    # Section 2 # Optimal Layout and viewport making
    # Create optimal layout, given the objects to be plotted, whether legend and axes are to be
    #  plotted, and visualSqueeze
    if (newArr) {
      if (is.null(visualSqueeze)) {
        visualSqueeze <- 0.75
      }
      updated$curr@arr <- .arrangeViewports(updated$curr)
      updated$curr@arr@layout <- .makeLayout(
        updated$curr@arr, sapply(visualSqueeze, max), sapply(legend,any),
        sapply(axes, function(x) { !any(x == TRUE) })
      )
    }

    # Create the viewports as per the optimal layout
    if (length(newSpadesPlots@spadesGrobList) > 0) {
      vps <- .makeViewports(updated$curr, newArr = newArr)
      if (!new & !newArr & !is.null(current.parent())) {
        upViewport(1)
      }
      pushViewport(vps$wholeVp, recording = FALSE)
      upViewport(0)
    }
    updated$curr@arr@extents <- vps$extents
    updated$curr@arr@names <- names(updated$curr@spadesGrobList)
    arr <- updated$curr@arr

    spadesSubPlots <- updated$curr@spadesGrobList

    # Section 3 - the actual Plotting
    # Plot each element passed to Plot function, one at a time

    for (subPlots in names(spadesSubPlots)) {
      spadesGrobCounter <- 0

      for (sGrob in spadesSubPlots[[subPlots]]) {
        spadesGrobCounter <- spadesGrobCounter + 1
        needPlot <- updated$needPlotting[[subPlots]][[spadesGrobCounter]]

        if (needPlot) {
          isNewPlot <- updated$isNewPlot[[subPlots]][[spadesGrobCounter]]
          isReplot <- updated$isReplot[[subPlots]][[spadesGrobCounter]]
          isBaseSubPlot <- updated$isBaseLayer[[subPlots]][[spadesGrobCounter]]

          #sgl <- updated$curr@spadesGrobList

          a <- try(seekViewport(subPlots, recording = FALSE))
          if (is(a, "try-error")) {
            stop(
              paste(
                "Plot does not already exist on current device.",
                "Try new = TRUE, clearPlot(), or change device to",
                "one that has a plot named", addTo[whGrobNamesi]
              )
            )
          }

          whPlotFrame <- match(sGrob@plotName, names(spadesSubPlots))

          # Check that the extents are equal.
          # If not, then x and y axes are written where necessary.
          if (axes == "L") {
            if (arr@extents[(whPlotFrame - 1) %% arr@columns + 1][[1]] ==
                arr@extents[max(
                  which(
                    (1:length(arr@names) - 1) %% arr@columns + 1 ==
                    (whPlotFrame - 1) %% arr@columns + 1
                  )
                )][[1]]) {
              if (whPlotFrame > (length(arr@names) - arr@columns)) {
                xaxis <- TRUE
              } else {
                xaxis <- FALSE
              }
            } else {
              # not the same extent as the final one in the column
              xaxis <- TRUE
            }
          } else {
            xaxis <- axes
          }

          if (axes == "L") {
            if (arr@extents[whPlotFrame][[1]] ==
                arr@extents[(ceiling(whPlotFrame / arr@columns) - 1) *
                            arr@columns + 1][[1]]) {
              if ((whPlotFrame - 1) %% arr@columns == 0) {
                yaxis <- TRUE
              } else {
                yaxis <- FALSE
              }
            } else {
              yaxis <- TRUE
            }
          } else {
            yaxis <- axes
          }

          takeFromPlotObj <- !(sGrob@plotName %in%
                                 names(currSpadesPlots@spadesGrobList))

          grobToPlot <- .identifyGrobToPlot(sGrob, plotObjs, takeFromPlotObj)

            if (!is(sGrob@plotArgs$gpText, "gpar")) {
              sGrob@plotArgs$gpText <- as(sGrob@plotArgs$gpText, "gpar")
            }
            if (!is(sGrob@plotArgs$gpAxis, "gpar")) {
              sGrob@plotArgs$gpAxis <- as(sGrob@plotArgs$gpAxis, "gpar")
            }
            if (!is(sGrob@plotArgs$gp, "gpar")) {
              sGrob@plotArgs$gp <- as(sGrob@plotArgs$gp, "gpar")
            }

            if (is.null(sGrob@plotArgs$gpText$cex)) {
              # pipe won't work here :S
              sGrob@plotArgs$gpText$cex <- max(
                0.6,
                min(1.2, sqrt(prod(arr@ds)/prod(arr@columns, arr@rows))*0.3)
              )
            }
            if (is.null(sGrob@plotArgs$gpAxis$cex)) {
              # pipe won't work here :S
              sGrob@plotArgs$gpAxis$cex <- max(
                0.6,
                min(1.2, sqrt(prod(arr@ds)/prod(arr@columns, arr@rows))*0.3)
              )
            }


            if (is(grobToPlot, "Raster")) {
              # Rasters may be zoomed into and subsampled and have unique legend
              pR <- .prepareRaster(grobToPlot, sGrob@plotArgs$zoomExtent,
                                   sGrob@plotArgs$legendRange, takeFromPlotObj,
                                   arr, sGrob@plotArgs$speedup, newArr = newArr)
              zMat <- .makeColorMatrix(grobToPlot, pR$zoom, pR$maxpixels,
                                       pR$legendRange,
                                       na.color = sGrob@plotArgs$na.color,
                                       zero.color = sGrob@plotArgs$zero.color,
                                       cols = sGrob@plotArgs$cols,
                                       skipSample = pR$skipSample)
              # Add legendRange if not provided
              if (is.null(sGrob@plotArgs$legendRange)) {
                updated$curr@spadesGrobList[[subPlots]][[spadesGrobCounter]]@plotArgs$legendRange <-
                  c(zMat$minz, zMat$maxz)
              }

            } else if (is(grobToPlot, "SpatialPoints")) {
              if (!is.null(sGrob@plotArgs$zoomExtent)) {
                grobToPlot <- crop(grobToPlot,sGrob@plotArgs$zoomExtent)
              }
              # This handles SpatialPointsDataFrames with column "color"
              if (any(grepl(pattern = "color", names(grobToPlot))) & is.null(cols))
                sGrob@plotArgs$cols <- grobToPlot@data$color

              zMat <- list(z = grobToPlot, minz = 0, maxz = 0,
                           cols = sGrob@plotArgs$cols, real = FALSE)
            } else if (is(grobToPlot, "SpatialPolygons")) {
              if (!is.null(sGrob@plotArgs$zoomExtent)) {
                grobToPlot <- crop(grobToPlot,sGrob@plotArgs$zoomExtent)
              }
              z <- grobToPlot
              zMat <- list(z = z, minz = 0, maxz = 0,
                           cols = sGrob@plotArgs$cols, real = FALSE)

            } else if (is(grobToPlot, "SpatialLines")) {
              if (!is.null(sGrob@plotArgs$zoomExtent)) {
                grobToPlot <- crop(grobToPlot,sGrob@plotArgs$zoomExtent)
              }
              z <- grobToPlot
              zMat <- list(z = z, minz = 0, maxz = 0,
                           cols = sGrob@plotArgs$cols, real = FALSE)
            }

            if (is(grobToPlot, "gg")) {
              print(grobToPlot, vp = subPlots)
              a <- try(seekViewport(subPlots, recording = FALSE))
              if (is(a, "try-error")) {
                stop(
                  paste(
                    "Plot does not already exist on current device.",
                    "Try new = TRUE or change device to one that has a",
                    "plot named", addTo[whGrobNamesi]
                  )
                )
              }
              if (title * isBaseSubPlot * isReplot |
                  title * isBaseSubPlot * isNewPlot) {
                grid.text(
                  subPlots, name = "title", y = 1.08, vjust = 0.5,
                  gp = sGrob@plotArgs$gpText
                )
              }
            } else if (is(grobToPlot, "histogram")) {
              # Because base plotting is not set up to overplot,
              # must plot a white rectangle
              grid.rect(gp = gpar(fill = "white", col = "white"))
              par(fig = gridFIG())
              suppressWarnings(par(new = TRUE))
              plotCall <- list(grobToPlot)
              do.call(plot, args = plotCall)
              if (title * isBaseSubPlot * isReplot |
                  title * isBaseSubPlot * isNewPlot) {
                suppressWarnings(par(new = TRUE))
                mtextArgs <-
                  append(list(
                    text = subPlots, side = 3, line = 4, xpd = TRUE
                  ),
                  sGrob@plotArgs$gpText)
                do.call(mtext, args = mtextArgs)
              }
            } else if (is(grobToPlot, "igraph")) {
              # Because base plotting is not set up to overplot,
              # must plot a white rectangle
              grid.rect(gp = gpar(fill = "white", col = "white"))
              par(fig = gridFIG())
              suppressWarnings(par(new = TRUE))
              plotCall <- append(list(x = grobToPlot), nonPlotArgs)
              do.call(plot, args = plotCall)
              if (title * isBaseSubPlot * isReplot |
                  title * isBaseSubPlot * isNewPlot) {
                suppressWarnings(par(new = TRUE))
                mtextArgs <-
                  append(list(
                    text = subPlots, side = 3, line = 4, xpd = TRUE
                  ),
                  sGrob@plotArgs$gpText)
                do.call(mtext, args = mtextArgs)
              }
            } else {
              # Extract legend text if the raster is a factored raster
              if (is.null(legendText)) {
                if (is.null(sGrob@plotArgs$legendTxt)) {
                  if (raster::is.factor(grobToPlot)) {
                    sGrob@plotArgs$legendTxt <- grobToPlot@data@attributes[[1]]#[,2]
                  }
                }
              } else {
                sGrob@plotArgs$legendTxt <- legendText
                updated$curr@spadesGrobList[[subPlots]][[spadesGrobCounter]]@plotArgs$legendTxt <-
                  legendText
              }

              if (!isBaseSubPlot ) {#| isReplot) {
                sGrob@plotArgs$legendTxt <- NULL
              }

              plotGrobCall <- list(
                zMat$z, col = zMat$cols,
                size = unit(sGrob@plotArgs$size, "points"),
                real = zMat$real,
                minv = zMat$minz, maxv = zMat$maxz,
                pch = sGrob@plotArgs$pch, name = subPlots,
                legend = sGrob@plotArgs$legend * isBaseSubPlot *
                  isReplot |
                  sGrob@plotArgs$legend * isBaseSubPlot *
                  isNewPlot,
                legendText = sGrob@plotArgs$legendTxt,
                gp = sGrob@plotArgs$gp,
                gpText = sGrob@plotArgs$gpText,
                speedup = sGrob@plotArgs$speedup,
                length = sGrob@plotArgs$length
              ) %>%
                append(., nonPlotArgs)

              do.call(.plotGrob, args = plotGrobCall)
              if (sGrob@plotArgs$title * isBaseSubPlot * isReplot |
                  sGrob@plotArgs$title * isBaseSubPlot * isNewPlot) {
                grid.text(
                  subPlots, name = "title", y = 1.08, vjust = 0.5,
                  gp = sGrob@plotArgs$gpText
                )
              }

              if (xaxis * isBaseSubPlot * isReplot |
                  xaxis * isBaseSubPlot * isNewPlot) {
                grid.xaxis(name = "xaxis", gp = sGrob@plotArgs$gpAxis)
              }
              if (yaxis * isBaseSubPlot * isReplot |
                  yaxis * isBaseSubPlot * isNewPlot) {
                grid.yaxis(name = "yaxis", gp = sGrob@plotArgs$gpAxis)
              }
            } #gg vs histogram vs spatialObject
        } # needPlot
        updated$isNewPlot[[subPlots]][[spadesGrobCounter]] <- FALSE
      } # sGrob
    } # subPlots

    .assignSpaDES(paste0("spadesPlot", dev.cur()), updated$curr)
    return(invisible(updated$curr))
})


#' @rdname Plot
#' @export
setMethod(
  "Plot",
  signature("simList"),
  definition = function(..., new, addTo, gp, gpText, gpAxis, axes,
                        speedup, size, cols, zoomExtent, visualSqueeze,
                        legend, legendRange, legendText, pch, title,
                        na.color, zero.color, length) {
    # Section 1 - extract object names, and determine which ones need plotting,
    # which ones need replotting etc.
    sim <- list(...)[[1]]
    plotList <- ls(sim@.envir, all.names = TRUE)
    plotObjects = mget(plotList[sapply(plotList, function(x)
      is(get(x, envir = envir(sim)), ".spadesPlottables"))], envir(sim)) %>%
      append(., list(env = envir(sim)))
    do.call(Plot, plotObjects)
})

################################################################################
#' Re-plot to a specific device
#'
#' @param toDev    Numeric. Which device should the new rePlot be plotted to.
#'                 Default is current device.
#'
#' @param fromDev  Numeric. Which device should the replot information be taken from.
#'                 Default is current device
#'
#' @export
#' @include plotting-classes.R
#' @importFrom grDevices dev.cur
#' @rdname Plot
#' @author Eliot McIntire
#'
rePlot <- function(toDev = dev.cur(), fromDev = dev.cur(), ...) {
  if (exists(paste0("spadesPlot", fromDev),envir = .spadesEnv)) {
    currSpadesPlots <- .getSpaDES(paste0("spadesPlot", dev.cur()))
    dev(toDev)
    Plot(currSpadesPlots, new = TRUE, ...)
  } else {
    stop(
      paste(
        "Nothing to rePlot. Need to call Plot first,",
        "or change to correct active device with dev(x),",
        "where x is the active device number."
      )
    )
  }
}

################################################################################
#' Identify where to get the grob from
#'
#' Internal function.
#'
#' Because the Plot function can use the global environment as a source of
#' objects to plot, not just the call itself, this function identifies where
#' the data for the grob should come from, the current call or the global
#' environment.
#'
#' @param grobNamesi name of the object to plot
#'
#' @param toPlot list containing the objects to plot, made as a call to the
#'               \code{Plot} function
#'
#' @param takeFromPlotObj logical. If \code{TRUE}, then take from the call to
#'                        \code{Plot}; if \code{FALSE} takes from global envir.
#'
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @rdname identifyGrobToPlot
setGeneric(".identifyGrobToPlot", function(grobNamesi, toPlot, takeFromPlotObj) {
  standardGeneric(".identifyGrobToPlot")
})

#' @rdname identifyGrobToPlot
setMethod(
  ".identifyGrobToPlot",
  signature = c(".spadesGrob", "list", "logical"),
  function(grobNamesi, toPlot, takeFromPlotObj) {
    # get the object name associated with this grob
    if (length(toPlot) == 0)
      takeFromPlotObj <- FALSE
    # Does it already exist on the plot device or not
    if (nchar(grobNamesi@layerName) > 0) {
      # means it is in a raster
      grobToPlot <- eval(parse(text = grobNamesi@objName),
                         grobNamesi@envir)[[grobNamesi@layerName]]
    } else {
      grobToPlot <- eval(parse(text = grobNamesi@objName), grobNamesi@envir)
    }
    return(grobToPlot)
})

#' @rdname identifyGrobToPlot
setMethod(
  ".identifyGrobToPlot",
  signature = c(".spadesGrob", "missing", "logical"),
  function(grobNamesi, toPlot, takeFromPlotObj) {
    .identifyGrobToPlot(grobNamesi, list(), FALSE)
})

################################################################################
#' Prepare raster for plotting
#'
#' Internal function. Takes a raster .spadesGrob, and converts zoomExtent into
#' a zoom, and legendRange into a legend.
#' Then calculates the maxpixels to plot for speed.
#'
#' @param grobToPlot .spadesGrob
#' @param zoomExtent an extent object
#' @param legendRange a numeric vector of length >=2 indicating the desired legend range.
#' @param takeFromPlotObj logical. Should the object be found in the Plot call or .GlobalEnv
#' @param arr an \code{.arrangement} object
#' @param speedup numeric, greater than 1 will usually speed up plotting at the expense of resolution
#' @param newArr logical, whether this is a new arrangement or just adding to a previous one
#'
#' @include plotting-classes.R
#' @rdname prepareRaster
#' @author Eliot McIntire
# igraph exports %>% from magrittr
.prepareRaster <- function(grobToPlot, zoomExtent, legendRange,
                           takeFromPlotObj, arr, speedup, newArr) {
  if (is.null(zoomExtent)) {
    zoom <- extent(grobToPlot)
    npixels <- ncell(grobToPlot)
  } else {
    zoom <- zoomExtent
    npixels <- ncell(crop(grobToPlot,zoom))
  }
  if (is.null(legendRange)) {
#    if (is.null(legendRange) | ((takeFromPlotObj == FALSE) * !newArr)) {
      legendRange <- NA
#    }
  }

  # maxpixels <- min(5e5,3e4/(arr@columns*arr@rows)*prod(arr@ds))/speedup %>%
  #   min(., npixels)
  if (speedup > 0.1) {
    maxpixels <- min(5e5, 3e4 / (arr@columns * arr@rows) * prod(arr@ds)) %>%
      `/`(., speedup) %>%
      min(., npixels)
  } else {
    maxpixels <- npixels
  }
  skipSample <- if (is.null(zoomExtent)) {
      maxpixels >= npixels
    } else {
      FALSE
    }

  return(list(maxpixels = maxpixels, skipSample = skipSample,
              legendRange = legendRange, zoom = zoom))
}
