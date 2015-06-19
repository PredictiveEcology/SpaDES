### deal with spurious data.table warnings
if(getRversion() >= "3.1.0") {
  utils::globalVariables(c("groups", "thin", "whGrobNamesi"))
}


################################################################################
#' Find the number of layers in a Spatial Object
#'
#' There are already methods for \code{Raster*} in the raster package.
#' Adding methods for \code{list}, \code{SpatialPolygons}, \code{SpatialLines},
#' and \code{SpatialPoints}, \code{gg}, \code{histogram}, \code{igraph}.
#' These latter classes return \code{1}.
#'
#' @param x A \code{.spadesPlotObjects} object or list of these.
#'
#' @return The number of layers in the object.
#'
#' @export
#' @importFrom raster nlayers
#' @importFrom methods is
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @rdname nlayers
setMethod("nlayers",
          signature="list",
          function(x) {
            y <- sum(sapply(x, function(x) {
              if(is(x, "RasterStack")) {
                x <- nlayers(x)
              } else {
                x <- 1L
              }
              return(x)
            }))
            return(y)
          })

#' @rdname nlayers
setMethod("nlayers",
          signature="SpatialPolygons",
          definition=function(x) {
            return(1L)
          })

#' @rdname nlayers
setMethod("nlayers",
          signature="SpatialLines",
          definition=function(x) {
            return(1L)
          })

#' @rdname nlayers
setMethod("nlayers",
          signature="SpatialPoints",
          definition=function(x) {
            return(1L)
          })

#' @rdname nlayers
setMethod("nlayers",
          signature="gg",
          definition=function(x) {
            return(1L)
          })

#' @rdname nlayers
setMethod("nlayers",
          signature="histogram",
          definition=function(x) {
            return(1L)
          })

#' @rdname nlayers
setMethod("nlayers",
          signature=".spadesPlot",
          definition=function(x) {
            return(length(x@arr@extents))
            #return(sum(sapply(x@spadesGrobList, function(y) length(y))))
          })

#' @rdname nlayers
setMethod("nlayers",
          signature="igraph",
          definition=function(x) {
            return(1L)
          })

################################################################################
#' Extract the layer names of Spatial Objects
#'
#' There are already methods for \code{Raster*} objects. This adds methods for
#' \code{SpatialPoints*}, \code{SpatialLines*}, and \code{SpatialPolygons*},
#' returning an empty character vector of length 1.
#' This function was created to give consistent, meaningful results for all
#' classes of objects plotted by \code{Plot}.
#'
#' @param object  A \code{Raster*}, \code{SpatialPoints*}, \code{SpatialLines*},
#'                or \code{SpatialPolygons*} object; or list of these.
#'
#' @rdname layerNames
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @export
setGeneric("layerNames", function(object) {
  standardGeneric("layerNames")
})

#' @rdname layerNames
setMethod("layerNames",
          signature="list",
          definition=function(object) {
            unlist(lapply(object, layerNames))
          })

#' @rdname layerNames
setMethod("layerNames",
          signature="SpatialPoints",
          definition=function(object) {
            return("")
          })

#' @rdname layerNames
setMethod("layerNames",
          signature="SpatialPolygons",
          definition=function(object) {
            return("")
          })

#' @rdname layerNames
setMethod("layerNames",
          signature="SpatialLines",
          definition=function(object) {
            return("")
          })


#' @rdname layerNames
setMethod("layerNames",
          signature="Raster",
          definition=function(object) {
            names(object)
          })

#' @rdname layerNames
setMethod("layerNames",
          signature="gg",
          definition=function(object) {
            return("")
          })

#' @rdname layerNames
setMethod("layerNames",
          signature="histogram",
          definition=function(object) {
            return("")
          })

#' @rdname layerNames
setMethod("layerNames",
          signature=".spadesPlot",
          definition=function(object) {
            return(sapply(object@spadesGrobList, function(x) {
              sapply(x, function(y) y@plotName)[[1]]
            }))
          })

#' @rdname layerNames
setMethod("layerNames",
          signature="igraph",
          definition=function(object) {
            return("")
          })

################################################################################
#' Assess whether a list of extents are all equal
#'
#' @param extents list of extents objects
#' @rdname equalExtent
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @export
setGeneric("equalExtent", function(extents) {
  standardGeneric("equalExtent")
})

#' @rdname equalExtent
setMethod("equalExtent",
          signature="list",
          definition=function(extents) {
            all(c(sapply(extents, function(x) x@xmin)==extents[[1]]@xmin,
                  sapply(extents, function(x) x@xmax)==extents[[1]]@xmax,
                  sapply(extents, function(x) x@ymin)==extents[[1]]@ymin,
                  sapply(extents, function(x) x@ymax)==extents[[1]]@ymax))
          })



################################################################################
# Methods
################################################################################
#' Make a \code{.spadesPlot} class object
#'
#' Builds a \code{.spadesPlot} object from a list of objects.
#'
#' @param plotObjects list. Any plot objects.
#'
#' @param plotArgs list. Any arguments that the the grid package can accept for the specific
#' grob types, e.g., rasterGrob, polygonGrob, etc.
#'
#' @param whichSpadesPlotables A logical indicating which objects in the Plot call can be plotted by Plot
#'
#' @param ... additional arguments. Currently nothing.
#'
#' @return A \code{\link{.spadesPlot}} object, which has 2 slots, one for the plot arrangement
#' (i.e., layout and dimensions) and onefor all of the \code{spadesGrobs}
#' (stored as a spadesGrobList of lists \code{.spadesGrob} objects).
#'
#' @rdname makeSpadesPlot
#' @include plotting-classes.R
#' @export
#' @author Eliot McIntire
#' @docType methods
setGeneric(".makeSpadesPlot", function(plotObjects, plotArgs, whichSpadesPlotables, ...) {
  standardGeneric(".makeSpadesPlot")
})

#' @rdname makeSpadesPlot
setMethod(".makeSpadesPlot",
          signature=c(plotObjects="list", plotArgs="list"),
          definition= function(plotObjects, plotArgs, ...) {

            isSpatialObjects <- sapply(plotObjects, function(x) is(x, ".spatialObjects"))

            suppliedNames <- names(plotObjects)
            objs <- .objectNames()[whichSpadesPlotables]

            names(plotObjects) <- sapply(objs,function(x) x$objs)

            if(!is.null(suppliedNames)) {
              if(all(sapply(suppliedNames, nchar)>0)) {
                names(plotObjects)[!is.na(suppliedNames)] <- suppliedNames
              }
            }
            numLayers <- pmax(1, sapply(plotObjects, nlayers))


            isSpadesPlot <- sapply(plotObjects, function(x) is(x, ".spadesPlot"))
            isRaster <- sapply(plotObjects, function(x) is(x, "Raster"))
            isStack <-  sapply(plotObjects, function(x) is(x, "RasterStack"))
            isPolygon <- sapply(plotObjects, function(x) is(x, "SpatialPolygons"))

            # stacks are like lists in that they are a single object, with many layers
            # Plot must treat these as any other layers, except that they are stored in
            # single objects. The following set of objects are the "long" versions of the
            # layers, i.e,. a call to say Plot(stack1, layerB) would have two objects, but maybe 5
            # layers, if the stack had 4 layers in it.
            isSpadesPlotLong <- rep(isSpadesPlot, numLayers)
            isRasterLong <- rep(isRaster, numLayers)
            isStackLong <- rep(isStack, numLayers)
            isSpatialObjects <- rep(isSpatialObjects, numLayers)

            lN <- rep(names(plotObjects), numLayers)
            lN[isSpadesPlotLong] <- layerNames(plotObjects[isSpadesPlot])
            objectNamesLong <- rep(names(plotObjects), numLayers)

            # Full layer names, including object name. If layer name is same as object name
            #  omit it, and if layer name is "layer", omit it if within a RasterLayer
            lN[isStackLong] <- paste(objectNamesLong[isStackLong],
                                      layerNames(plotObjects[isStack]), sep="$")
            names(lN) <- rep(names(plotObjects), numLayers)
            names(lN)[isSpadesPlotLong] <- layerNames(plotObjects)[isSpadesPlotLong]

            # Create long version of environments
            lEnvs <- rep(sapply(objs, function(x) x$envs), numLayers)

#             if(any(duplicated(paste(lN,lEnvs)))) {
#               stop(paste("Cannot plot two layers with same name from the same environment. Check",
#                          "inside RasterStacks for objects"))
#             }

            plotArgs <- .makeList(plotArgs, length(lN))

            # Make new .spadesPlot object. This will be merged to existing later
            newPlots <- new(".spadesPlot")

            newPlots@arr <- new(".arrangement")

            newPlots@spadesGrobList <- lapply(1:length(lN), function(x) {

              spadesGrobList <- list()

              if(isSpadesPlotLong[x]) {
                spadesGrobList[[lN[x]]] <- plotObjects[[match(names(isSpadesPlotLong)[x],names(plotObjects))]]@
                  spadesGrobList[[match(lN[x], layerNames(plotObjects[isSpadesPlot]))]][[1]]
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
                spadesGrobList[[lN[x]]]@objClass <- class(eval(parse(text=objectNamesLong[x]), lEnvs[[x]]))
                spadesGrobList[[lN[x]]]@isSpatialObjects <- isSpatialObjects[x]
              }
              return(spadesGrobList)
            })

            names(newPlots@spadesGrobList) <- lN
            return(newPlots)
})

#' @rdname makeSpadesPlot
setMethod(".makeSpadesPlot",
          signature=c(plotObjects="list", plotArgs="missing"),
          definition= function(plotObjects, ...) {

            plotArgs <- formals("Plot")[-1]
            newPlots <- .makeSpadesPlot(plotObjects, plotArgs, ...)
            return(newPlots)
})

#' @rdname makeSpadesPlot
setMethod(".makeSpadesPlot",
          signature=c(plotObjects="missing", plotArgs="missing"),
          definition= function(...) {

            newPlots <- new(".spadesPlot")
            newPlots@spadesGrobList <- lapply(1:1, function(x) {
              spadesGrobList <- list()
              spadesGrobList[[1]] <- new(".spadesGrob")
              return(spadesGrobList)
            })
            return(newPlots)
})

setOldClass("gpar")
setAs(from="list", to="gpar", function(from) {
  if(length(from[[1]])>0) {
    gp1 <- gpar(from[[1]][[1]])
    if(length(from[[1]])>1) {
      for(i in 2:length(from[[1]])) {
        gp1 <- gpar(sapply(gp1, function(x) x), from[[1]][[i]])
      }
    }
    names(gp1) <- names(from[[1]])
    gp1
  } else {
    gpar()
  }
})

######################################################
#' Convert plotArgs to list of lists
#'
#' Take the inputs as plotArgs to the Plot function, and make them a list of length
#' \code{numSpadesPlotObjects} entries of lists. To be called internally only.
#'
#' @param plotArgs list. The arguments passed to Plot
#'
#' @param numSpadesPlotObjects numeric. The number of .spadesPlotObjects. This can't
#' easily be deduced from the plotArgs because of the RasterStacks. So passed manually.
#'
#' @rdname makeList
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @docType methods
setGeneric(".makeList", function(plotArgs, numSpadesPlotObjects) {
  standardGeneric(".makeList")
})

#' @rdname makeList
setMethod(".makeList",
          signature=c("list"),
          definition= function(plotArgs, numSpadesPlotObjects) {
            p <- plotArgs
            n <- numSpadesPlotObjects

            p$new <- if(is.list(p$new)) {if(length(p$new)!=n) {rep(p$new, length.out=n)} else {p$new}
            } else {
              if(length(p$new)==n) {as.list(p$new)} else {rep(list(p$new), length.out=n)}}

            # character or logical or numeric of length 1 per entry
            p$addTo <- if(is.list(p$addTo)) {if(length(p$addTo)!=n) {rep(p$addTo, length.out=n)} else {p$addTo}
              } else {
                if(length(p$addTo)==n) {as.list(p$addTo)} else {rep(list(p$addTo), length.out=n)}}


            p$gp <- if(is(p$gp, "gpar")) {rep(list(p$gp), n)
            } else {if(is.list(p$gp)) {rep(p$gp, n)}}
            p$gpText <- if(is(p$gpText, "gpar")) {rep(list(p$gpText), n)
            } else {if(is.list(p$gpText)) {rep(p$gpText, n)}}
            p$gpAxis <- if(is(p$gpAxis, "gpar")) {rep(list(p$gpAxis), n)
            } else {if(is.list(p$gpAxis)) {rep(p$gpAxis, n)}}
            p$axes <- if(is.list(p$axes)) {if(length(p$axes)!=n) {rep(p$axes, length.out=n)} else {p$axes}
            } else {
              if(length(p$axes)==n) {as.list(p$axes)} else {rep(list(p$axes), length.out=n)}}
            p$speedup <- if(is.list(p$speedup)) {if(length(p$speedup)!=n) {rep(p$speedup, length.out=n)} else {p$speedup}
            } else {
              if(length(p$speedup)==n) {as.list(p$speedup)} else {rep(list(p$speedup), length.out=n)}}
            p$size <- if(is.list(p$size)) {if(length(p$size)!=n) {rep(p$size, length.out=n)} else {p$size}
            } else {
              if(length(p$size)==n) {as.list(p$size)} else {rep(list(p$size), length.out=n)}}
            p$visualSqueeze <- if(is.list(p$visualSqueeze)) {if(length(p$visualSqueeze)!=n) {rep(p$visualSqueeze, length.out=n)} else {p$visualSqueeze}
            } else {
              if(length(p$visualSqueeze)==n) {as.list(p$visualSqueeze)} else {rep(list(p$visualSqueeze), length.out=n)}}
            p$legend <- if(is.list(p$legend)) {if(length(p$legend)!=n) {rep(p$legend, length.out=n)} else {p$legend}
            } else {
              if(length(p$legend)==n) {as.list(p$legend)} else {rep(list(p$legend), length.out=n)}}
            p$pch <- if(is.list(p$pch)) {if(length(p$pch)!=n) {rep(p$pch, length.out=n)} else {p$pch}
            } else {
              if(length(p$pch)==n) {as.list(p$pch)} else {rep(list(p$pch), length.out=n)}}
            p$title <- if(is.list(p$title)) {if(length(p$title)!=n) {rep(p$title, length.out=n)} else {p$title}
            } else {
              if(length(p$title)==n) {as.list(p$title)} else {rep(list(p$title), length.out=n)}}
            p$na.color <- if(is.list(p$na.color)) {if(length(p$na.color)!=n) {rep(p$na.color, length.out=n)} else {p$na.color}
            } else {
              if(length(p$na.color)==n) {as.list(p$na.color)} else {rep(list(p$na.color), length.out=n)}}
            p$zero.color <- if(is.list(p$zero.color)) {if(length(p$zero.color)!=n) {rep(p$zero.color, length.out=n)} else {p$zero.color}
            } else {
              if(length(p$zero.color)==n) {as.list(p$zero.color)} else {rep(list(p$zero.color), length.out=n)}}


            p$cols <- if(is.list(p$cols)) {p$cols} else {rep(list(p$cols), length.out=n)}
            p$zoomExtent <- if(is.list(p$zoomExtent)) {p$zoomExtent} else {rep(list(p$zoomExtent), length.out=n)}
            p$legendText <- if(is.list(p$legendText)) {p$legendText} else {rep(list(p$legendText), length.out=n)}
            p$legendRange <- if(is.list(p$legendRange)) {p$legendRange} else {rep(list(p$legendRange), length.out=n)}

            return(p)
})

######################################################
#' Merge two SpaDES Plot objects
#'
#' Merges two \code{.spadesPlot} objects
#'
#' @param newSP "new" .spadesPlot object. i.e., the new merges and overwrites into current
#'
#' @param curr "current" .spadesPlot object. i.e., the one to be merged into.
#'
#' @param ... additional arguments. Currently nothing.
#'
#' @rdname updateSpadesPlot
#' @export
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @docType methods
setGeneric(".updateSpadesPlot", function(newSP, curr, ...) {
  standardGeneric(".updateSpadesPlot")
})


#' @rdname updateSpadesPlot
setMethod(".updateSpadesPlot",
          signature=c(newSP=".spadesPlot", curr=".spadesPlot"),
          definition= function(newSP, curr, ...) {

            newNames <- names(newSP@spadesGrobList)
            currNames <- names(curr@spadesGrobList)

            addToPlots <- sapply(newSP@spadesGrobList,
                                 function(x) !is.null(x[[1]]@plotArgs$addTo))
            addToPlotsNames <- unlist(sapply(newSP@spadesGrobList,
                                             function(x) x[[1]]@plotArgs$addTo))

            overplots <- na.omit(match(currNames, newNames))

            needNew <- -c(overplots, which(addToPlots))
            if(length(needNew)==0) {needNew <- 1:length(newNames)}

            whichParamsChanged <- lapply(newNames[overplots],
                                         function(x) {
                                           sapply(names(newSP@spadesGrobList[[x]][[1]]@plotArgs),
                                                  function(y) {
                                                    changed <- !identical(newSP@spadesGrobList[[x]][[1]]@plotArgs[[y]],
                                                      curr@spadesGrobList[[x]][[1]]@plotArgs[[y]])
                                                  }
                                           )
                               })
            names(whichParamsChanged) <- newNames[overplots]


            # Set FALSE as default for needPlotting
            needPlotting <- lapply(curr@spadesGrobList, function(x) lapply(x, function(y) FALSE))

            # Set FALSE as default for isReplot
            isReplot <- lapply(curr@spadesGrobList, function(x) lapply(x, function(y) FALSE))

            # Set FALSE as default for isBaseLayer
            isBaseLayer <- lapply(curr@spadesGrobList, function(x) lapply(x, function(y) TRUE))

            isNewPlot <- lapply(curr@spadesGrobList, function(x) lapply(x, function(y) FALSE))

            # For overplots
            for(plots in newNames[overplots]) {
              curr@spadesGrobList[[plots]] <- newSP@spadesGrobList[[plots]]
              needPlotting[[plots]] <- TRUE
              isReplot[[plots]] <- TRUE
              isBaseLayer[[plots]] <- FALSE
              isNewPlot[[plots]] <- FALSE
            }

            # put addTo plots into list of spadesGrobs that it will be added to
            if(!is.null(addToPlotsNames)) {
              for(plots in 1:length(addToPlotsNames)) {
                len <- length(curr@spadesGrobList[[addToPlotsNames[plots]]])
                curr@spadesGrobList[[addToPlotsNames[plots]]][names(addToPlotsNames[plots])] <-
                  newSP@spadesGrobList[[names(addToPlotsNames[plots])]]
                # change the name of the plotName to the parent object
                curr@spadesGrobList[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]]@plotName <-
                  curr@spadesGrobList[[addToPlotsNames[plots]]][[1]]@plotName
                needPlotting[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]] <- TRUE
                isReplot[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]] <- FALSE
                isBaseLayer[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]] <- FALSE
                isNewPlot[[addToPlotsNames[plots]]][[names(addToPlotsNames[plots])]] <- FALSE
              }
            }

            # for new plots
            for(plots in newNames[needNew]) {
              curr@spadesGrobList[[plots]] <- newSP@spadesGrobList[[plots]]
              needPlotting[[plots]] <- TRUE
              isReplot[[plots]] <- FALSE
              isBaseLayer[[plots]] <- TRUE
              isNewPlot[[plots]] <- TRUE
            }

            return(list(curr=curr, whichParamsChanged=whichParamsChanged,
                        needPlotting=needPlotting, isReplot=isReplot,
                        isBaseLayer=isBaseLayer, isNewPlot=isNewPlot))

})

#' @rdname updateSpadesPlot
setMethod(".updateSpadesPlot",
          signature=c(newSP=".spadesPlot", curr=NULL),
          definition= function(newSP, ...) {

            return(list(curr=newSP, whichParamsChanged=NULL,
                        needPlotting=lapply(newSP@spadesGrobList, function(x) lapply(x, function(y) TRUE)),
                        isReplot=lapply(newSP@spadesGrobList, function(x) lapply(x, function(y) FALSE)),
                        isNewPlot=lapply(newSP@spadesGrobList, function(x) lapply(x, function(y) TRUE)),
                        isBaseLayer=lapply(newSP@spadesGrobList, function(x) lapply(x, function(y) TRUE))))
})

####################################################################
#' Determine optimal plotting arrangement of plot objects
#'
#' This assesses the device geometry, the map geometry, and the number of spatial
#' objects to plot and builds an object that will be used by the Plot functions to plot
#' them efficiently. This is meant to be used internally.
#'
#' @param sPlot A \code{.spadesPlot} object.
#'
#' @rdname arrangeViewports
#' @include plotting-classes.R
#' @export
#' @author Eliot McIntire
#' @docType methods
setGeneric(".arrangeViewports", function(sPlot) { #, name=NULL) {
  standardGeneric(".arrangeViewports")
})

#' @rdname arrangeViewports
setMethod(".arrangeViewports",
          signature=c(".spadesPlot"),
          definition= function(sPlot) {

            sgl <- sPlot@spadesGrobList

            dimx <- apply(do.call(rbind,sapply(1:length(sgl), function(x) {
              lapply(sgl[[x]][[1]]@isSpatialObjects, function(z) {
                if(z==TRUE) {
                  # for spatial objects
                  apply(bbox(eval(parse(text=sgl[[x]][[1]]@objName), envir=sgl[[x]][[1]]@envir)),1,function(y) diff(range(y)))
                } else {
                  # for non spatial objects
                  c(1,1)
                }
              })
            })), 2, max)

            nPlots <- length(sgl)
            names <- names(sgl)

            if(dev.cur()==1) {
              dev.new(height=8, width=10)
            }

            ds <- dev.size()
            ds.ratio <- ds[1]/ds[2]

            dimensionRatio <- dimx[1]/dimx[2]

            ds.dimensionRatio <- ds.ratio/dimensionRatio

            col.by.row <- data.frame(matrix(ncol=2, nrow=nPlots))

            col.by.row[, 1] <- ceiling(nPlots/(1:nPlots))
            col.by.row[, 2] <- ceiling(nPlots/col.by.row[, 1])

            wh.best <- which.min(abs(apply(col.by.row, 1, function(x) x[1]/x[2]) - ds.dimensionRatio))

            columns <- col.by.row[wh.best, 1]
            rows <- col.by.row[wh.best, 2]

            actual.ratio <- columns/rows

            out <- new(".arrangement", rows=rows, columns=columns,
                       actual.ratio=actual.ratio, ds.dimensionRatio=ds.dimensionRatio,
                       ds=ds)
            return(out)
})

######################################################
#' Plot spatial grobs (using grid package)
#'
#' Plot a raster Grob, a points Grob, polygon Grob. This should mostly be called internally.
#'
#' \code{speedup} is only used for SpatialPolygons in this function. Attempts have been made
#' to subsample at a good level that optimizes speed of plotting, without losing visible
#' quality. From a speed perspective, there appears to be an optimal subsampling
#' when using \code{thin} from the \code{fastshp} package. Presumably too much thinning requires
#' large distance matrices to be calculated, slowing plotting down. Too little thinning
#' causes an overabundance of points to be plotted, slowing plotting down.
#'
#' The suggested package \code{fastshp} can be installed with:
#'  \code{install.packages("fastshp", repos="http://rforge.net", type="source")}
#'
#' NOTE: you may get errors relating to not having installed the software tools
#' required for building R packages on your system. For development purposes on
#' a Windows machine, you'll need to install Rtools from http://cran.r-project.org/bin/windows/Rtools/.
#'
#' @param grobToPlot \code{Raster*}, \code{SpatialPoints*}, \code{SpatialPolygons*}, or
#' \code{SpatialLines*} object
#'
#' @param col Currently only used for the legend of a \code{Raster*} object.
#'
#' @param size The size of the SpatialPoints
#'
#' @param gp grid parameters, usually the output of a call to \code{\link{gpar}}
#'
#' @param gpText gpar object for legend label text
#'
#' @param legend logical, whether a legend should be drawn. Default \code{TRUE}.
#'
#' @param legendText Vector of values to use for legend value labels. Defaults to \code{NULL} which results
#' in a pretty numeric representation. If \code{Raster*} has a Raster Attribute Table (rat, see raster
#' package), this will be used by default. Currently, only a single vector is accepted.
#'
#' @param length Numeric.
#'
#' @param minv The minimum value on a Raster*. Required because not all Rasters
#' have this defined internally
#'
#' @param maxv The maximum value on a Raster*. Required because not all Rasters
#' have this defined internally
#'
#' @param pch for \code{SpatialPoints}, as \code{par}
#'
#' @param real logical. Whether the data are real numbers (vs. integer or factor).
#'
#' @param speedup Numeric. The factor by which the number of vertices in \code{SpatialPolygons}
#' and \code{SpatialLines*} will be subsampled. The vertices are already subsampled
#' by default to make plotting faster.
#'
#' @param ... additional arguments. Currently nothing.
#'
#' @rdname plotGrob
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @docType methods
setGeneric(".plotGrob", function(grobToPlot, col=NULL, real=FALSE,
                                size=unit(5, "points"),
                                minv, maxv,
                                legend=TRUE, legendText=NULL, length=NULL,
                                gp=gpar(), gpText=gpar(), pch=19,
                                speedup=1, ...) {
  standardGeneric(".plotGrob")
})


#' @rdname plotGrob
setMethod(".plotGrob",
          signature=c("matrix"),
          definition= function(grobToPlot, col, real, size, minv, maxv,
                               legend, legendText, gp, gpText, pch, ...) {

            pr <- if(real) {
              pretty(range(minv, maxv))
            } else {
              if(!is.null(legendText)) {
                unique(round(pretty(range(minv, maxv),n=length(legendText))))
              } else {
                unique(round(pretty(range(minv, maxv))))
              }
            }

            pr <- pr[pr<=maxv & pr>=minv]
            maxNumCols=100
            maxcol <- length(col)
            mincol <- 2

            gpText$cex <- gpText$cex*0.6
            if (length(gpText)==0) gpText <- gpar(col="black", cex=0.6)
            rastGrob <- gTree(grobToPlot=grobToPlot, #title=title,
                              # name=name,
                              pr=pr, col=col,
                              children=gList(
                                rasterGrob(as.raster(grobToPlot),
                                           interpolate=FALSE,
                                           name="raster"),
                                if(legend) rasterGrob(as.raster(col[(maxcol):mincol]),
                                                      x=1.04, y=0.5, height=0.5, width=0.03,
                                                      interpolate=FALSE,
                                                      name="legend"),
                                if(legend) {
                                  txt <- if(is.null(legendText)) {
                                    pr
                                  } else {
                                    legendIndex <- pr-min(pr)+1
                                    legendText[legendIndex]
                                  }
                                   textGrob(txt, x=1.08,
                                            if(maxv>=3) {
                                              y= ((pr-minv)/((maxv+1)-minv))/2+0.25+1/(diff(range(minv, maxv))+1)/4
                                            } else {
                                              y= ((pr-minv)/((maxv)-minv))/2+0.25
                                            },
                                            gp=gpText
                                            ,
                                            just="left", check.overlap=TRUE,
                                            name="legendText")
                                }
                              ),
                              gp=gp,
                              cl="plotRast")
            grid.draw(rastGrob)
            return(invisible(rastGrob))
})

#' @rdname plotGrob
setMethod(".plotGrob",
          signature=c("SpatialPoints"),
          definition= function(grobToPlot, col, size,
                               legend, gp=gpar(), pch, ...) {
            pntGrob <- gTree(grobToPlot=grobToPlot,
                             children=gList(
                               pointsGrob(x=grobToPlot$x, y=grobToPlot$y, pch=pch, size=size)
                             ),
                             gp=gp,
                             cl="plotPoint")
            grid.draw(pntGrob)
            return(invisible(pntGrob))
})

#' @rdname plotGrob
setMethod(".plotGrob",
          signature=c("SpatialPolygons"),
          definition= function(grobToPlot, col, size,
                               legend, gp=gpar(), pch, speedup, ...) {


            speedupScale = if(grepl(proj4string(grobToPlot), pattern="longlat")) {
              pointDistance(p1=c(xmax(extent(grobToPlot)), ymax(extent(grobToPlot))),
                            p2=c(xmin(extent(grobToPlot)), ymin(extent(grobToPlot))),
                            lonlat=TRUE)/1.2e10
            } else {
              max( ymax(extent(grobToPlot))-ymin(extent(grobToPlot)),
                   xmax(extent(grobToPlot))-xmin(extent(grobToPlot)))/2.4e4
            }
            # For speed of plotting
            xy <- lapply(1:length(grobToPlot),
                         function(i) lapply(grobToPlot@polygons[[i]]@Polygons,
                                            function(j) j@coords))
            hole <- unlist(lapply(1:length(grobToPlot),
                                  function(x) lapply(grobToPlot@polygons[[x]]@Polygons,
                                                     function(x) x@hole)))
            ord <- grobToPlot@plotOrder
            ordInner <- lapply(1:length(grobToPlot), function(x) grobToPlot@polygons[[x]]@plotOrder)

            xyOrd.l <- lapply(ord, function(i) xy[[i]][ordInner[[i]]])
            idLength <- data.table(V1=unlist(lapply(xyOrd.l, function(i) lapply(i, length)))/2)
            xyOrd <- do.call(rbind, lapply(xyOrd.l, function(i) do.call(rbind, i)))


            if(nrow(xyOrd) > 1e3) { # thin if fewer than 1000 pts
              if (requireNamespace("fastshp", quietly=TRUE)) {
                thinned <- data.table(thin=fastshp::thin(xyOrd[, 1], xyOrd[, 2], tolerance=speedupScale*speedup))# %>%
                #  data.table(thinned=.) %>%
                thinned[,groups:=rep(1:nrow(idLength), idLength$V1)]
                idLength <- thinned[,sum(thin),by=groups]
                xyOrd <- xyOrd[thinned$thin, ]
              } else {
                message(paste("To speed up Polygons plotting using Plot please download fastshp",
                              "install.packages(\"fastshp\", repos=\"http://rforge.net\", type=\"source\")",
                              "You may also need to download and install Rtools",
                              "at http://cran.r-project.org/bin/windows/Rtools/"))
              }
            }

            gp$fill[hole] <- "#FFFFFF00"
            polyGrob <- gTree(children=gList(
              polygonGrob(x=xyOrd[, 1], y=xyOrd[, 2], id.lengths=idLength$V1,
                          gp=gp, default.units="native")
            ),
            gp=gp,
            cl="plotPoly")
            grid.draw(polyGrob)
            return(invisible(polyGrob))
})


#' @rdname plotGrob
setMethod(".plotGrob",
          signature=c("SpatialLines"),
          definition= function(grobToPlot, col, size,
                               legend, length, gp=gpar(), pch, speedup, ...) {

            speedupScale = if(grepl(proj4string(grobToPlot), pattern="longlat")) {
              pointDistance(p1=c(xmax(extent(grobToPlot)), ymax(extent(grobToPlot))),
                            p2=c(xmin(extent(grobToPlot)), ymin(extent(grobToPlot))),
                            lonlat=TRUE)/1.2e10
            } else {
              max( ymax(extent(grobToPlot))-ymin(extent(grobToPlot)),
                   xmax(extent(grobToPlot))-xmin(extent(grobToPlot)))/2.4e4
            }

            # For speed of plotting
            xy <- lapply(1:length(grobToPlot),
                         function(i) grobToPlot@lines[[i]]@Lines[[1]]@coords)
            idLength <- unlist(lapply(xy, length))/2
            xy <- do.call(rbind,xy)

            if(nrow(xy) > 1e3) { # thin if fewer than 1000 pts
              if (requireNamespace("fastshp", quietly=TRUE)) {
                thinned <- fastshp::thin(xy[, 1], xy[, 2], tolerance=speedupScale*speedup)

                #keep first and last points of every polyline, if there are fewer than 10,000 vertices
                if(sum(thinned)<1e4) {
                  lastIDs <- cumsum(idLength)
                  thinned[c(1,lastIDs+1)[-(1+length(lastIDs))]] <- TRUE # THis ensures first point of each line is kept
                  thinned[lastIDs] <- TRUE # THis ensures final point of each line is kept
                }
                xy <- xy[thinned, ]
                idLength <- tapply(thinned, rep(1:length(idLength), idLength), sum)
              } else {
                message(paste("To speed up Lines plotting using Plot please download fastshp",
                              "install.packages(\"fastshp\", repos=\"http://rforge.net\", type=\"source\")",
                              "You may also need to download and install Rtools",
                              "at http://cran.r-project.org/bin/windows/Rtools/"))
              }
            }

            if(is.null(length)) {
              lineGrob <- gTree(children=gList(
                polylineGrob(x=xy[, 1], y=xy[, 2], id.lengths=idLength,
                             gp=gp, default.units="native")
              ),
              gp=gp,
              cl="plotLine")
            } else {
              lineGrob <- gTree(children=gList(
                polylineGrob(x=xy[, 1], y=xy[, 2], id.lengths=idLength,
                             gp=gp, default.units="native", arrow=arrow(length=unit(length, "inches")))
              ),
              gp=gp,
              cl="plotLine")
            }

            grid.draw(lineGrob)
            return(invisible(lineGrob))
})

################################################################################
#' Make an optimal layout of plots
#'
#' Using the size of the current device, and number and dimension ratios of the plots,
#' this function will place them optimally in the plotting region.
#'
#' @param arr an object of class \code{.arrangement}
#'
#' @param visualSqueeze Numeric. The proportion of the white space to be used for
#' plots. Default is 0.75.
#'
#' @param legend logical. Whether legend should be included as part of layout
#' calculation. Default is \code{TRUE}.
#'
#' @param axes Logical. Whether the axes should be included as part of layout
#' calculation. Default is \code{TRUE}.
#'
#' @param title Logical. Whether the names of each plot should be written above
#' plots and should be included as part of layout calculation.  Default is \code{TRUE}.
#'
#' @author Eliot McIntire
#' @rdname makeLayout
#' @include plotting-classes.R
#' @export
.makeLayout <- function(arr, visualSqueeze, legend=TRUE, axes=TRUE, title=TRUE) {

  columns <- arr@columns
  rows <- arr@rows

  # Reduce by 40% of remaining space if each of the following is not wanted
  if(legend==FALSE ) visualSqueeze <- visualSqueeze + 0.4*(1-visualSqueeze)
  if(axes==FALSE) visualSqueeze <- visualSqueeze + 0.4*(1-visualSqueeze)
  if(title==FALSE) visualSqueeze <- visualSqueeze + 0.4*(1-visualSqueeze)

  # calculate the visualSqueeze for the width (i.e., vS.w)
  vS.w <- min(visualSqueeze/columns,
              visualSqueeze/columns*arr@actual.ratio/arr@ds.dimensionRatio)

  wdth <- unit.c(unit(0.2, "null"), unit(rep(c(0.875, vS.w, 0.875), columns),
                                         rep(c("null","npc", "null"), columns)),
                 unit(0.2, "null"))
  # calculate the visualSqueeze for the height (i.e., vS.h)
  vS.h <- min(visualSqueeze/rows,
              visualSqueeze/rows*arr@ds.dimensionRatio/arr@actual.ratio)
  ht <- unit.c(unit(0.2, "null"), unit(rep(c(0.875, vS.h, 0.875), rows),
                                       rep(c("null", "npc", "null"), rows)),
               unit(0.2, "null"))

  return(list(wdth=wdth, ht=ht,
              wdthUnits=vS.w, htUnits=vS.h,
              visualSqueeze=visualSqueeze))
}

################################################################################
#' Make viewports
#'
#' Given a set of extents, and a layout for these extents, this function will output
#' a viewport tree to allow plotting.
#'
#' This function will either create a totally new set of viewports, or simply add
#' some nested viewports to an existing arrangement, i.e., is there still white
#' space availabe to plot.
#'
#' @param sPlot An object of class \code{.spadesPlot}
#'
#' @param newArr  Logical. Whether this function will create a completely new viewport.
#'                Default \code{FALSE}.
#'
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @rdname makeViewports
#' @export
.makeViewports <- function(sPlot, newArr=FALSE) {

  arr <- sPlot@arr
  sgl <- sPlot@spadesGrobList

  extents <- unlist(sapply(sgl, function(x) {
     unname(lapply(x[[1]]@isSpatialObjects, function(z) {

       if(z==TRUE) {
         # for spatial objects
         if(!is.null(x[[1]]@plotArgs$zoomExtent)){
           x[[1]]@plotArgs$zoomExtent
         } else {
           extent(eval(parse(text=x[[1]]@objName), envir=x[[1]]@envir))
         }
       } else {
         # for non spatial objects
         extent(c(xmin=0,xmax=1,ymin=0,ymax=1))
       }
     }))
  }))

  columns <- arr@columns
  rows <- arr@rows
  topVp <- viewport(layout=grid.layout(nrow=rows*3+2,
                                       ncol=columns*3+2,
                                       widths=arr@layout$wdth,
                                       heights=arr@layout$ht),
                    name="top")
  plotVps <- list()

  nam <- names(extents)

  # This is the biggest of the extents, and is used in .makeLayout
  #  Need to replicate it here because all plots are scaled to this
  biggestDims <- apply(do.call(rbind,sapply(1:length(sgl), function(x) {
    lapply(sgl[[x]][[1]]@isSpatialObjects, function(z) {
      if (z==TRUE) {
        # for spatial objects
        apply(bbox(extents[[x]]),1,function(y) diff(range(y)))
      } else {
        # for non spatial objects
        c(xmin=0,xmax=1,ymin=0,ymax=1)
      }
    })})),2,max)

  for(extentInd in 1:length(extents)) {
    posInd <- match(nam[extentInd], names(sgl))
    lpc <- ceiling((posInd-1)%%columns+1)*3
    lpr <- ceiling(posInd/columns)*3

    if(!sgl[[posInd]][[1]]@isSpatialObjects) {
      lpc <- c((lpc-1):(lpc+1))
      lpr <- c((lpr):(lpr+1))
    }
    # makes equal scale
    yrange <- extents[[extentInd]]@ymax - extents[[extentInd]]@ymin
    if(yrange>0) {
      if (abs((yrange /
                 (extents[[extentInd]]@xmax - extents[[extentInd]]@xmin)) -
                (biggestDims[1]/biggestDims[2]))
          > (getOption("fpCompare.tolerance"))) {

        dimensionRatio <- arr@layout$wdthUnits*arr@ds[1] /
          (arr@layout$htUnits*arr@ds[2])
        plotScaleRatio <- (extents[[extentInd]]@xmin - extents[[extentInd]]@xmax) /
          (extents[[extentInd]]@ymin - extents[[extentInd]]@ymax)

        vS.w <- min(1, plotScaleRatio/dimensionRatio)

        vS.h <- min(1, dimensionRatio/plotScaleRatio)

        addY <- abs(extents[[extentInd]]@ymax- extents[[extentInd]]@ymin -
                      (extents[[extentInd]]@ymax- extents[[extentInd]]@ymin)/vS.h)/2
        addX <- abs(extents[[extentInd]]@xmax- extents[[extentInd]]@xmin -
                      (extents[[extentInd]]@xmax- extents[[extentInd]]@xmin)/vS.w)/2
      } else {
        addY <- addX <- 0
      }
    } else {
      addX <- extents[[extentInd]]@xmin *0.05
      addY <- extents[[extentInd]]@ymin *0.05
    }
    # end equal scale
    plotVps[[extentInd]] <- viewport(
      name=nam[extentInd],
      layout.pos.col = lpc,
      layout.pos.row = lpr,
      xscale=c(extents[[extentInd]]@xmin-addX, extents[[extentInd]]@xmax+addX),
      yscale=c(extents[[extentInd]]@ymin-addY, extents[[extentInd]]@ymax+addY))
  }

  if(newArr) {
    wholeVp <- vpTree(topVp, do.call(vpList, plotVps))
  } else {
    wholeVp <- do.call(vpList, plotVps)
  }
  return(list(wholeVp=wholeVp, extents=extents))
}

################################################################################
#' Make \code{SpatialLines} object from two \code{SpatialPoints} objects
#'
#' The primary conceived usage of this is to draw arrows following the trajectories of an agent.
#'
#' @param from  Starting spatial coordinates (\code{SpatialPointsDataFrame}).
#'
#' @param to    Ending spatial coordinates (\code{SpatialPointsDataFrame}).
#'
#' @return A \code{SpatialLines} object. When this object is used within a \code{Plot} call
#' and the \code{length} argument is specified, then arrow heads will be drawn. See examples.
#'
#' @import sp
#' @export
#' @docType methods
#' @rdname makeLines
#' @include plotting-classes.R
#' @author Eliot McIntire
#' @examples
#' # Make 2 objects
#' caribou1 <- SpatialPoints(cbind(x=runif(10, -50, 50), y=runif(10, -50, 50)))
#' caribou2 <- SpatialPoints(cbind(x=runif(10, -50, 50), y=runif(10, -50, 50)))
#'
#' caribouTraj <- makeLines(caribou1, caribou2)
#' Plot(caribouTraj, new=TRUE, length=0.1)
#'
#' # or  to a previous Plot
#' \dontrun{
#' fileList <- data.table(files =
#'      dir(file.path(find.package("SpaDES",
#'                                 lib.loc=getOption("devtools.path"),
#'                                 quiet=FALSE),
#'                   "maps"),
#'         full.names=TRUE, pattern= "tif"),
#'      functions="rasterToMemory",
#'      packages="SpaDES")
#'
#' # Load files to memory (using rasterToMemory)
#' sim1 <- loadFiles(fileList=fileList)
#'
#' Plot(sim1$DEM, new=TRUE)
#' caribouTraj <- makeLines(caribou1, caribou2)
#' Plot(caribouTraj, addTo="sim1$DEM", length=0.1)
#' }
#'
setGeneric("makeLines", function(from, to) {
  standardGeneric("makeLines")
})

#' @rdname makeLines
setMethod("makeLines",
          signature=c("SpatialPoints", "SpatialPoints"),
          definition=function(from, to) {
            SpatialLines(lapply(seq_len(length(from)), function(x) {
              Lines(list(Line(coords=rbind(coordinates(from)[x,], coordinates(to)[x,]))),ID=x)
            }), proj4string=crs(from))
})

################################################################################
#' Parse arguments and find environments
#'
#' Internal function used within .objectNames.
#'
#' @param y      a character representation of the arguments passed to a function, e.g., \code{Plot}
#'
#' @param e     environment in which the function (e.g., \code{Plot}) was called
#'
#' @param eminus1   environment. The parent of e.
#'
#' @return A list of length 2, with names \code{objs} and \code{envs} giving the standardized representation (i.e., replacing [[]] with
#' $ notation for objects) of objects and their layers (if \code{RasterStacks}).
#'
#' @docType methods
#' @importFrom magrittr '%>%'
#' @include plotting-classes.R
#' @rdname parseArgs
#' @author Eliot McIntire and Alex Chubaty
.parseArgs <- function(y, e, eminus1) {
  elems <- list()
  i <- 1
  parseTxt <- parse(text=y)[[1]]
  elems[[i]] <- parseTxt
  lastOneDone <- TRUE

  while(length(parse(text=deparse(parseTxt))[[1]])!=1){
    lastOneDone <- FALSE
    if(grepl(deparse(parseTxt[[1]]), pattern="^eval")) {
      callEnv <- tryCatch(eval(match.call(definition = eval, call = parseTxt)$envir, envir=eminus1),
                          error=function(x) tryCatch(eval(match.call(definition = eval, call = parseTxt)$envir, envir=e),
                                         error=function(x) .GlobalEnv))
      parseTxt[[3]] <- match.call(definition = eval, call = parseTxt)$expr
      if(is.name(match.call(definition = parse, call = parseTxt[[3]])$text)) {
        parseTxt <- parseTxt[[3]]
        parseTxt[[3]] <- match.call(definition = parse, call = parseTxt)$text
      }
      lastOneDone <- TRUE
    }
    if(is.call(parseTxt[[3]])){
      parseTxt[[3]] <- tryCatch(eval(parseTxt[[3]], envir=e),
                                error=function(x) eval(parseTxt[[3]], envir=eminus1))
    }
    if(grepl(deparse(parseTxt[[1]]), pattern="^get")) {
      parseTxt[[3]] <- match.call(definition = get, call = parseTxt)$x
      # if the xxx of get(x = xxx) is the same as an evaluated version of xxx, then
      if(identical(eval(parse(text=deparse(match.call(definition = get, call = parseTxt)$x))), parseTxt[[3]])){
        lastOneDone=TRUE
      }
    }
    if(is.character(parseTxt[[3]])) {
      parseTxt[[3]] <- as.name(parseTxt[[3]])
    }
    elems[[i]] <- parseTxt[[3]] # This overrides previous elems entry if parseTxt is longer than 1

    # if evaluating the parsed text is a character, then this is likely then name we want to keep
    isChar <- tryCatch(is(eval(elems[[i]], envir=eminus1), "character"), error=function(x) FALSE)
    if(isChar) {
      elems[[i]] <- as.name(eval(elems[[i]], envir=eminus1))
    }
    parseTxt <- parse(text=deparse(parseTxt[[2]]))[[1]]
    i = i + 1
  }

  envs <- append(.GlobalEnv, sys.frames())[c(TRUE, sapply(sys.frames(), function(x)
    exists(deparse(parseTxt), envir=x, inherits=FALSE)))] %>%
    .[[length(.)]]

  inGlobal <- identical(envs, .GlobalEnv)
  if(is(eval(parse(text=deparse(parseTxt)), envir=envs), "environment")) {
    envs <- eval(parse(text=deparse(parseTxt)), envir=envs)
  } else {
    if(!lastOneDone)
      elems[[i]] <- parseTxt
  }
  if(exists("callEnv", inherits=FALSE)) {
    envs <- callEnv
  }

  if(!inGlobal) {
    if(!exists(paste0("dev",dev.cur()), envir=.spadesEnv)) {
      .spadesEnv[[paste0("dev",dev.cur())]] <- new.env(parent = emptyenv())
    }
    changeObjEnv(paste(sapply(rev(elems),deparse),collapse="$"),
                 fromEnv=envs, toEnv=.spadesEnv[[paste0("dev",dev.cur())]])
  }

  return(list(objs=paste(sapply(rev(elems),deparse),collapse="$"), envs=envs))
}

################################################################################
#' Extracts the object names
#'
#' This is primarily used from Plot.
#'
#' @param calledFrom character vector, length 1, indicating which function call is
#' desired. Defaults to \code{Plot}
#' @param argClass character vector, length 1, indicating which class is being
#' searched for among the arguments. Defaults to \code{.spadesPlotObjects}
#' @param argName character vector, length 1, or \code{NULL}, indicating if the
#' arguments to select have a name, no name (empty string), or do not use name (NULL).
#'
#' @return \code{NULL}. This function is invoked for its side effects.
#'
#' @importFrom methods is
#' @include plotting-classes.R
#' @docType methods
#' @author Eliot McIntire
#' @rdname objectNames
.objectNames <- function(calledFrom="Plot", argClass=".spadesPlotObjects",
                         argName="") {

  scalls <- sys.calls()
  # Extract from the sys.calls only the function "calledFrom"
  frameCalledFrom <- which(sapply(scalls, function(x) {
    grepl(x, pattern=paste0("^", calledFrom,"$"))[1]
  }))
  e <- sys.frame(frameCalledFrom)
  eminus1 <- sys.frame(frameCalledFrom-1)

  if(nchar(argName)==0){
    callNamedArgs <- as.character(substitute(list(...), env=e))[-1]
  } else {
    callNamedArgs <- as.character(substitute(parse(text=sim), env=e))[-1]
  }
  objs <- lapply(callNamedArgs, .parseArgs, e, eminus1)
  return(objs)
}



################################################################################
#' Plot: Fast, optimally arranged, multipanel plotting function with SpaDES
#'
#' The main plotting function accompanying \code{SpaDES}.
#' This can take objects of type \code{Raster*}, \code{SpatialPoints*},
#' \code{SpatialPolygons*}, and any combination of those.  It can
#' also handle \code{ggplot2} objects or base histogram objects via call to
#' \code{exHist <- hist(1:10, plot=FALSE)}, but these non-spatial objects
#' cannot be mixed among types (i.e., can't mix and match spatial and
#' non-spatial objects, or base histogram and ggplot2 types). Customization of the
#' ggplot2 elements can be done as a normal ggplot2 plot, then added with
#' \code{Plot(ggplotObject)}.
#'
#' NOTE: Plot uses the grid package; therefore, it is
#' NOT compatible with base R graphics. Also, because it does not by default wipe
#' the plotting device before plotting, a call to \code{\link{clearPlot}} could be
#' helpful to resolve many errors.
#'
#' If \code{new=TRUE}, a new plot will be generated. This is equivalent to calling
#' \code{\link{clearPlot}; Plot(Object)}, i.e,. directly before creating a new Plot.
#' When \code{new=FALSE}, any plot that already exists will be overplotted,
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
#' If the \code{...} are named .spatialObjects, then \code{Plot} will use
#' these names. However, this name will not persist when there is a future call
#' to \code{Plot} that forces a rearrangement of the plots.
#' A more stable way is to use the object names directly, and any layer names
#' (in the case of \code{RasterLayer} or \code{RasterStack} objects).
#' If plotting a RasterLayer and the layer name is "layer" or the same as the object name,
#' then, for simplicity, only the object name will be used.
#' In other words, only enough information is used to uniquely identify the plot.
#'
#' \code{cols} is a vector of colours that can be understood directly, or by
#' \code{colorRampePalette}, such as \code{c("orange", "blue")}, will give a colour range
#' from orange to blue, interploated. If a list, it will be used, in order, for each
#' item to be plotted. It will be recycled if it is shorter than the objects to be
#' plotted. Note that when this approach to setting colours is used, any overplotting
#' will revert to the \code{colortable} slot of the object, or the default for rasters,
#' which is \code{terrain.color()}
#'
#' Silently, one hidden object is made, \code{.spadesPlot} in the \code{.spadesEnv}
#' environment, which is used for arranging plots in the device window, and
#' identifying the objects to be replotted if rearranging is required, subsequent
#' to a \code{new=FALSE} additional plot.
#'
#' This function is optimized to allow modular Plotting. This means that several
#' behaviours will appear unusual. For instance, if a first call to Plot is made,
#' the legend will reflect the current color scheme. If a second or subsequent call
#' to Plot is made with the same object but with different colours (e.g., with
#' \code{cols}), the legend will not update. This behaviour is made with the decision
#' that the original layer takes precedence and all subsequent plots to that same
#' frame are overplots only.
#'
#' \code{speedup} is not a precise number because it is faster to plot an un-resampled
#' raster if the new resampling is close to the original number of pixels. At the moment,
#' this is set to 1/3 of the original pixels. In other words, \code{speedup} will not do
#' anything if the factor for speeding up is not high enough (i.e., >3).
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
#' @param ... A combination of \code{.spatialObjects} or a single \code{ggplot} object. See details.
#'
#' @param new Logical. If \code{TRUE}, then the previous plot is wiped and a new one made;
#' if \code{FALSE}, then the \code{...} plots will be added to the current device,
#' adding or rearranging the plot layout as necessary. Default is \code{FALSE}.
#'
#' @param addTo String vector, with same length as \code{...}.
#' This is for overplotting, when the overplot is not to occur on the plot with
#' the same name, such as plotting a \code{SpatialPoints*} object on a \code{RasterLayer}.
#'
#' @param gp A gpar object, created by \code{\link{gpar}} function,
#' to change plotting parameters (see \code{grid} package).
#'
#' @param gpText A gpar object for the title text. Default \code{gpar(col="black")}
#'
#' @param gpAxis A gpar object for the axes. Default \code{gpar(col="black")}
#'
#' @param axes Logical or \code{"L"}, representing the left and bottom axes, overall plots.
#'
#' @param speedup Numeric. The factor by which the number of pixels is divided by to plot rasters.
#' See Details.
#'
#' @param size Numeric. The size, in points, for \code{SpatialPoints} symbols,
#' if using a scalable symbol.
#'
#' @param cols character vector or list of character vectors. Default \code{terrain.color()}. See Details.
#'
#' @param zoomExtent extent object. Supplying a single extent object that is smaller than the
#' rasters will call a crop statement before plotting. Defaults to \code{NULL}.
#' This occurs after any downsampling of rasters, so it may produce very pixelated maps.
#'
#' @param visualSqueeze numeric. The proportion of the white space to be used for plots. Default is 0.75.
#'
#' @param legend logical. Whether legend should be drawn next to plots. Default is \code{TRUE}.
#'
#' @param legendRange numeric vector giving values that, representing the lower
#' and upper bounds of a legend (i.e., \code{1:10} or \code{c(1,10)} will give same result)
#' that will override the data bounds contained within the \code{grobToPlot}.
#'
#' @param legendText Vector of values to use for legend value labels.
#' Defaults to \code{NULL}, which results in a pretty numeric representation.
#' If \code{Raster*} has a Raster Attribute Table (rat; see \code{\link{raster}}
#' package), this will be used by default. Currently, only a single vector is accepted.
#' The length of this must match the length of the legend, so this is mosty useful for
#' discrete valued rasters.
#'
#' @param na.color string indicating the color for \code{NA} values. Default transparent.
#'
#' @param zero.color string indicating the color for zero values, when zero is
#' the minimum value, otherwise, zero is treated as any other color. Default transparent.
#'
#' @param pch see \code{?par}.
#'
#' @param title Logical. Whether the names of each plot should be written above plots.
#'
#' @param length numeric. Optional length, in inches, of the arrow head.
#'
#' @return Invisibly returns the \code{.spadesPlot} class object. If this is assigned to an
#' object, say \code{obj}, then this can be plotted again with \code{Plot(obj)}. This
#' object is also stored in the locked \code{.spadesEnv}, so can simply be replotted
#' with \code{rePlot()} or on a new device with \code{rePlot(4)}, where 4 is the new device number.
#'
#' @seealso \code{\link{clearPlot}}, \code{\link{gpar}}, \code{\link{raster}},
#' \code{\link{par}}, \code{\link{SpatialPolygons}}, \code{\link{grid.polyline}},
#' \code{\link{ggplot2}}, \code{\link{dev}}
#'
#' @rdname Plot
#' @export
#' @import grid
#' @importFrom methods is
#' @importFrom gridBase gridFIG
#' @import raster
#' @import RColorBrewer
#' @import ggplot2
#' @import rgdal
#' @import sp
#' @author Eliot McIntire
#' @include environment.R
#' @include plotting-classes.R
#' @examples
#' \dontrun{
#' library(raster)
#' library(rgdal)
#' library(magrittr)
#' library(igraph)
#' #  Make list of maps from package database to load, and what functions to use to load them
#' fileList <-
#'    data.frame(files =
#'      dir(file.path(
#'                    find.package("SpaDES",
#'                                 lib.loc=getOption("devtools.path"),
#'                                 quiet=FALSE),
#'                   "maps"),
#'         full.names=TRUE, pattern= "tif"),
#'      functions="rasterToMemory",
#'      packages="SpaDES",
#'      stringsAsFactors=FALSE)
#'
#' # Load files to memory (using rasterToMemory)
#' loadFiles(fileList=fileList)
#'
#' # put layers into a single stack for convenience
#' landscape <- stack(DEM, forestCover, forestAge, habitatQuality, percentPine)
#'
#' # can change color palette
#' setColors(landscape, n = 50)<-list(DEM=topo.colors(50),
#'                            forestCover = RColorBrewer::brewer.pal(9, "Set1"),
#'                            forestAge = RColorBrewer::brewer.pal("Blues", n=8),
#'                            habitatQuality = RColorBrewer::brewer.pal(9, "Spectral"),
#'                            percentPine = RColorBrewer::brewer.pal("GnBu", n=8))
#'
#' #Make a new raster derived from a previous one; must give it a unique name
#' habitatQuality2 <- landscape$habitatQuality ^ 0.3
#' names(habitatQuality2) <- "habitatQuality2"
#'
#' # make a SpatialPoints object
#' caribou <- SpatialPoints(coords=cbind(x=runif(1e2, -50, 50), y=runif(1e2, -50, 50)))
#'
#'
#' #Plot all maps on a new plot windows - Do not use RStudio window
#' \notrun{
#' if(is.null(dev.list())) {
#'   dev(2)
#' } else {
#'   if(any(names(dev.list())=="RStudioGD")) {
#'     dev(which(names(dev.list())=="RStudioGD")+3)
#'   } else {
#'     dev(max(dev.list()))
#'   }
#' }
#' }
#'
#' Plot(landscape, new=TRUE)
#'
#' # Can overplot, using addTo
#' Plot(caribou, addTo="landscape$forestAge", size=4, axes=FALSE)
#'
#' # can add a plot to the plotting window
#' Plot(caribou, new=FALSE)
#'
#' # Can add two maps with same name, if one is in a stack; they are given
#' #  unique names based on object name
#' Plot(landscape, caribou, DEM)
#'
#' # can mix stacks, rasters, SpatialPoint*
#' Plot(landscape, habitatQuality2, caribou)
#'
#' # can mix stacks, rasters, SpatialPoint*, and SpatialPolygons*
#' Plot(landscape, caribou)
#' Plot(habitatQuality2, new=FALSE)
#' Sr1 = Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2))*20-50)
#' Sr2 = Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2))*20-50)
#' Srs1 = Polygons(list(Sr1), "s1")
#' Srs2 = Polygons(list(Sr2), "s2")
#' SpP = SpatialPolygons(list(Srs1, Srs2), 1:2)
#' Plot(SpP)
#' Plot(SpP, addTo="landscape$forestCover", gp=gpar(lwd=2))
#'
#' }
setGeneric("Plot",
           signature="...",
           function(..., new=FALSE, addTo=NULL, gp=gpar(), gpText=gpar(), gpAxis=gpar(),
                    axes=FALSE, speedup = 1,
                    size=5, cols=NULL, zoomExtent=NULL,
                    visualSqueeze=NULL, legend=TRUE, legendRange=NULL, legendText=NULL,
                    pch = 19, title=TRUE,
                    na.color="#FFFFFF00", zero.color=NULL, length=NULL) {
             standardGeneric("Plot")
 })

#' @rdname Plot
#' @export
setMethod("Plot",
          signature("ANY"),
          definition = function(..., new, addTo, gp, gpText, gpAxis, axes, speedup, size,
                                cols, zoomExtent, visualSqueeze,
                                legend, legendRange, legendText, pch, title, na.color,
                                zero.color, length) {
      # Section 1 - extract object names, and determine which ones need plotting,
      # which ones need replotting etc.

      if (all(sapply(new, function(x) x))) clearPlot(dev.cur())

      dotObjs <- list(...)
      # Section 1 # Determine object names that were passed and layer names of each
      plotArgs <- mget(names(formals("Plot")),
                       sys.frame(grep(sys.calls(),pattern="^Plot")))[-1]

      whichSpadesPlotables <- as.logical(sapply(dotObjs, function(x) is(x, ".spadesPlottables")))

      if(!all(whichSpadesPlotables)) {
        message(paste("Plot can only plot objects of class .spadesPlottables. ",
                      "Type 'showClass(\".spadesPlottables\")' to see current available",
                      "classes"))
      }

      plotObjs <- dotObjs[whichSpadesPlotables]

      if(length(plotObjs)==0){
        stop("Nothing to Plot")
      }
      nonPlotArgs <- dotObjs[!whichSpadesPlotables]

      # intercept cases that don't make sense, and give meaningful error
      if(!is.null(addTo)) {
        if( !tryCatch(addTo %in% unlist(layerNames(get(paste0("spadesPlot", dev.cur()),envir=.spadesEnv))) ,
                 error=function(x) FALSE)) {
          message(paste("Plot called with 'addTo' argument specified, but that layer does not exist.",
                        "Plotting object on its own plot"))
          plotArgs$addTo <- NULL
        }
      }

      # Create a .spadesPlot object from the plotObjs and plotArgs

      isSpadesPlot <- sapply(plotObjs, function(x) is(x,".spadesPlot"))
      #if(any(!isSpadesPlot)) {
        newSpadesPlots <- .makeSpadesPlot(plotObjs, plotArgs, whichSpadesPlotables)
      #}

      if(exists(paste0("spadesPlot", dev.cur()),envir=.spadesEnv)) {
        currSpadesPlots <- .getSpaDES(paste0("spadesPlot", dev.cur()))
        visualSqueeze <- if(is.null(visualSqueeze)) {currSpadesPlots@arr@layout$visualSqueeze} else {visualSqueeze}
        updated <- .updateSpadesPlot(newSpadesPlots, currSpadesPlots)
        newArr <- (length(updated$curr@spadesGrobList) >
                     prod(currSpadesPlots@arr@columns,
                          currSpadesPlots@arr@rows)) |
          !identical(currSpadesPlots@arr@ds,dev.size())

        if(newArr) {
          updated$needPlotting <-
            lapply(updated$needPlotting, function(x) sapply(x, function(y) TRUE))
          updated$isReplot <-
            lapply(updated$isReplot, function(x) sapply(x, function(y) TRUE))
          updated$isNewPlot <-
            lapply(updated$isReplot, function(x) sapply(x, function(y) TRUE))
          updated$isBaseLayer <-
            lapply(updated$isReplot, function(x) sapply(x, function(y) TRUE))
          clearPlot(removeData=FALSE)
        }

      } else if(all(isSpadesPlot)) {
        currSpadesPlots <- .makeSpadesPlot()
        newSpadesPlots <- plotObjs[[1]]
        visualSqueeze <- if(is.null(visualSqueeze)) {newSpadesPlots@arr@layout$visualSqueeze} else {visualSqueeze}
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
      if(newArr) {
        if(is.null(visualSqueeze)) {visualSqueeze <- 0.75}
        updated$curr@arr <-
          .arrangeViewports(updated$curr)
        updated$curr@arr@layout <-
          .makeLayout(updated$curr@arr,
                     sapply(visualSqueeze,max), sapply(legend,any),
                     sapply(axes, function(x) !any(x==TRUE)))
      }

      # Create the viewports as per the optimal layout
      if(length(newSpadesPlots@spadesGrobList)>0) {
         vps <- .makeViewports(updated$curr,
                              newArr=newArr)
         if(!new & !newArr & !is.null(current.parent()))
           upViewport(1)
         pushViewport(vps$wholeVp, recording = FALSE)
         upViewport(2)
       }
      updated$curr@arr@extents <- vps$extents
      updated$curr@arr@names <- names(updated$curr@spadesGrobList)
      arr <- updated$curr@arr

      spadesSubPlots <- updated$curr@spadesGrobList

      # Section 3 - the actual Plotting
      # Plot each element passed to Plot function, one at a time

      for(subPlots in names(spadesSubPlots)) {

        spadesGrobCounter <- 0

        for(sGrob in spadesSubPlots[[subPlots]]) {

          spadesGrobCounter <- spadesGrobCounter+1
          needPlot <- updated$needPlotting[[subPlots]][[spadesGrobCounter]]

          if (needPlot) {
            isNewPlot <- updated$isNewPlot[[subPlots]][[spadesGrobCounter]]
            isReplot <- updated$isReplot[[subPlots]][[spadesGrobCounter]]
            isBaseSubPlot <- updated$isBaseLayer[[subPlots]][[spadesGrobCounter]]

            sgl <- updated$curr@spadesGrobList

            a <- try(seekViewport(subPlots, recording=FALSE))
            if(is(a, "try-error")) stop(paste("Plot does not already exist on current device.",
                                             "Try new=TRUE, clearPlot(), or change device to",
                                             "one that has a plot named", addTo[whGrobNamesi]))

            whPlotFrame <- match(sGrob@plotName, names(spadesSubPlots))

            # This checks that the extents are equal. If not, then x and y axes are written where
            # necessary
            if(axes=="L") {
              if (arr@extents[(whPlotFrame-1)%%arr@columns+1][[1]]==
                    arr@extents[max(which((1:length(arr@names)-1)%%arr@columns+1==
                                            (whPlotFrame-1)%%arr@columns+1))][[1]]) {
                if(whPlotFrame>(length(arr@names)-arr@columns)) {
                  xaxis <- TRUE } else { xaxis <- FALSE}
              } else { #not the same extent as the final one in the column
                xaxis <- TRUE
              }
            } else {
              xaxis <- axes
            }

            if(axes=="L") {
              if(arr@extents[whPlotFrame][[1]]==
                 arr@extents[(ceiling(whPlotFrame/arr@columns)-1)*arr@columns+1][[1]]) {
                if((whPlotFrame-1)%%arr@columns==0) {
                  yaxis <- TRUE } else { yaxis <- FALSE}
              } else {
                  yaxis <- TRUE
              }
            } else {
              yaxis <- axes
            }

            takeFromPlotObj=!(sGrob@plotName %in% names(currSpadesPlots@spadesGrobList))

            grobToPlot <- .identifyGrobToPlot(sGrob, plotObjs, takeFromPlotObj)

            if(!is(sGrob@plotArgs$gpText, "gpar")) {
              sGrob@plotArgs$gpText <- as(sGrob@plotArgs$gpText, "gpar")
            }
            if(!is(sGrob@plotArgs$gpAxis, "gpar")) {
              sGrob@plotArgs$gpAxis <- as(sGrob@plotArgs$gpAxis, "gpar")
            }
            if(!is(sGrob@plotArgs$gp, "gpar")) {
              sGrob@plotArgs$gp <- as(sGrob@plotArgs$gp, "gpar")
            }

            if(is.null(sGrob@plotArgs$gpText$cex)) {

              sGrob@plotArgs$gpText$cex <- cex <- max(0.6, min(1.2, sqrt(prod(arr@ds)/prod(arr@columns, arr@rows))*0.3))
            }
            if(is.null(sGrob@plotArgs$gpAxis$cex)) {
              sGrob@plotArgs$gpAxis$cex <- cex <- max(0.6, min(1.2, sqrt(prod(arr@ds)/prod(arr@columns, arr@rows))*0.3))
            }

            if(is(grobToPlot, "Raster")) {

              # Rasters may be zoomed into and subsampled and have unique legend
             pR <- .prepareRaster(grobToPlot, sGrob@plotArgs$zoomExtent, sGrob@plotArgs$legendRange,
                                  takeFromPlotObj,
                                  arr,
                                  sGrob@plotArgs$speedup, newArr=newArr)

             zMat <- .makeColorMatrix(grobToPlot, pR$zoom, pR$maxpixels,
                                     pR$legendRange,
                                     na.color=sGrob@plotArgs$na.color,
                                     zero.color=sGrob@plotArgs$zero.color,
                                     cols=sGrob@plotArgs$cols,
                                     skipSample=pR$skipSample)
           } else if (is(grobToPlot, "SpatialPoints")) { # it is a SpatialPoints object

            if(!is.null(sGrob@plotArgs$zoomExtent)) {
              grobToPlot <- crop(grobToPlot,sGrob@plotArgs$zoomExtent)
            }

            len <- length(grobToPlot)
            if(len<(1e4/sGrob@plotArgs$speedup)) {
              z <- grobToPlot
            } else {
              z <- sample(grobToPlot, 1e4/sGrob@plotArgs$speedup)
            }
            zMat <- list(z=z, minz=0, maxz=0, cols=NULL, real=FALSE)

          } else if (is(grobToPlot, "SpatialPolygons")) { # it is a SpatialPolygons object
            if(!is.null(sGrob@plotArgs$zoomExtent)) {
              grobToPlot <- crop(grobToPlot,sGrob@plotArgs$zoomExtent)
              }
            z <- grobToPlot
            zMat <- list(z=z, minz=0, maxz=0, cols=NULL, real=FALSE)

          } else if (is(grobToPlot, "SpatialLines")) { # it is a SpatialPolygons object

            if(!is.null(sGrob@plotArgs$zoomExtent)) {
              grobToPlot <- crop(grobToPlot,sGrob@plotArgs$zoomExtent)
            }
            z <- grobToPlot
            zMat <- list(z=z, minz=0, maxz=0, cols=NULL, real=FALSE)
          }

          if (is(grobToPlot, "gg")) {

            print(grobToPlot, vp=subPlots)
            a <- try(seekViewport(subPlots, recording=FALSE))
            if(is(a, "try-error")) stop(paste("Plot does not already exist on current device.",
                                               "Try new=TRUE or change device to",
                                               "one that has a plot named", addTo[whGrobNamesi]))
            if(title*isBaseSubPlot*isReplot | title*isBaseSubPlot*isNewPlot) grid.text(subPlots,
                                name="title", y=1.08, vjust=0.5, gp = sGrob@plotArgs$gpText)

          } else if(is(grobToPlot, "histogram")) {
            # Because base plotting is not set up to overplot, must plot a white rectangle
            grid.rect(gp=gpar(fill="white", col="white"))
            par(fig=gridFIG())
            suppressWarnings(par(new=TRUE))
            plotCall <- list(grobToPlot)#, nonPlotArgs)
            #names(plotCall)[1] <- "x"
            do.call(plot, args=plotCall)
            if(title*isBaseSubPlot*isReplot | title*isBaseSubPlot*isNewPlot) {
              suppressWarnings(par(new=TRUE))
              mtextArgs <- append(list(text=subPlots, side=3, line=4, xpd=TRUE), sGrob@plotArgs$gpText)
              do.call(mtext, args=mtextArgs)
            }
          } else if(is(grobToPlot, "igraph")) {
            # Because base plotting is not set up to overplot, must plot a white rectangle
            grid.rect(gp=gpar(fill="white", col="white"))
            par(fig=gridFIG())
            suppressWarnings(par(new=TRUE))
            plotCall <- append(list(x=grobToPlot), nonPlotArgs)
            #names(plotCall)[1] <- "x"
            do.call(plot, args=plotCall)
            if(title*isBaseSubPlot*isReplot | title*isBaseSubPlot*isNewPlot) {
              suppressWarnings(par(new=TRUE))
              mtextArgs <- append(list(text=subPlots, side=3, line=4, xpd=TRUE), sGrob@plotArgs$gpText)
              do.call(mtext, args=mtextArgs)
            }

          } else {
            # Extract legend text if the raster is a factored raster
             if(is.factor(grobToPlot) & is.null(legendText)) {
               legendTxt <- levels(grobToPlot)[[1]][,2]
             } else {
               legendTxt <- legendText
             }
            legendTxt <- if(!isBaseSubPlot | !isReplot) {NULL}
            plotGrobCall <- append(list(zMat$z, col = zMat$cols,
                     size=unit(sGrob@plotArgs$size, "points"),
                     real=zMat$real, minv=zMat$minz, maxv=zMat$maxz,
                     pch=sGrob@plotArgs$pch, name = subPlots,
                     legend = sGrob@plotArgs$legend*isBaseSubPlot*isReplot |
                       sGrob@plotArgs$legend*isBaseSubPlot*isNewPlot,
                     legendText=sGrob@plotArgs$legendTxt,
                     gp = sGrob@plotArgs$gp,
                     gpText = sGrob@plotArgs$gpText,
                     speedup=sGrob@plotArgs$speedup,
                     length=sGrob@plotArgs$length), nonPlotArgs)
            do.call(.plotGrob, args=plotGrobCall)
            if(sGrob@plotArgs$title*isBaseSubPlot*isReplot |
                 sGrob@plotArgs$title*isBaseSubPlot*isNewPlot) {
              grid.text(subPlots, name="title", y=1.08, vjust=0.5,
                        gp = sGrob@plotArgs$gpText)
            }


            if(xaxis*isBaseSubPlot*isReplot | xaxis*isBaseSubPlot*isNewPlot) grid.xaxis(name="xaxis", gp = sGrob@plotArgs$gpAxis)
            if(yaxis*isBaseSubPlot*isReplot | yaxis*isBaseSubPlot*isNewPlot) grid.yaxis(name="yaxis", gp = sGrob@plotArgs$gpAxis)

          } #gg vs histogram vs spatialObject
        } # needPlot
        updated$isNewPlot[[subPlots]][[spadesGrobCounter]] <- FALSE
      } # sGrob
    } # subPlots

    .assignSpaDES(paste0("spadesPlot", dev.cur()), updated$curr)
    return(invisible(updated$curr))
})

#' @param toDev numeric. Which device should the new rePlot be plotted to. Default is current device.
#'
#' @param fromDev numeric. Which device should the replot information be taken from. Default
#' is current device
#'
#' @export
#' @include plotting-classes.R
#' @rdname Plot
#' @author Eliot McIntire
rePlot <- function(toDev=dev.cur(), fromDev=dev.cur(), ...) {
              if(exists(paste0("spadesPlot", fromDev),envir=.spadesEnv)) {
                currSpadesPlots <- .getSpaDES(paste0("spadesPlot", dev.cur()))
                dev(toDev)
                Plot(currSpadesPlots, new=TRUE, ...)
              } else {
                stop(paste("Nothing to rePlot. Need to call Plot first, or change to",
                     "correct active device with dev(x), where x is the active device number"))
              }
}

#' Convert Raster to color matrix useable by raster function for plotting
#'
#' @param grobToPlot a SpatialObject
#'
#' @param zoomExtent an extent object for zooming to. Defaults to whole extent of grobToPlot
#'
#' @param maxpixels numeric. Number of cells to subsample the complete grobToPlot
#'
#' @param legendRange numeric vector giving values that, representing the lower
#' and upper bounds of a legend (i.e., \code{1:10} or \code{c(1,10)} will give same result)
#' that will override the data bounds contained within the \code{grobToPlot}.
#'
#' @param cols colours specified in a way that can be understood directly or by
#'  \code{\link{colorRampPalette}}.
#'
#' @param na.color string indicating the color for \code{NA} values. Default transparent.
#'
#' @param zero.color string indicating the color for zero values, when zero is
#' the minimum value. Otherwise, it is treated as any other color. Default transparent.
#' Use \code{NULL} if zero should be the value given to it by the colortable associated with the Raster.
#'
#' @param skipSample logical. If no downsampling is necessary, skip. Default \code{TRUE}.
#'
#' @rdname makeColorMatrix
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @docType methods
setGeneric(".makeColorMatrix", function(grobToPlot, zoomExtent, maxpixels, legendRange,
                                       cols=NULL, na.color="#FFFFFF00",
                                       zero.color=NULL, skipSample=TRUE) {
  standardGeneric(".makeColorMatrix")
})


#' @rdname makeColorMatrix
setMethod(".makeColorMatrix",
          signature=c("Raster", "Extent", "numeric", "ANY"),
          definition=function(grobToPlot, zoomExtent, maxpixels, legendRange,
                              cols, na.color, zero.color, skipSample=TRUE) {
            zoom <- zoomExtent
            # It is 5x faster to access the min and max from the Raster than to calculate it,
            #  but it is also often wrong... it is only metadata on the raster, so it
            #  is possible that it is incorrect
            if(!skipSample) {
              colorTable <- getColors(grobToPlot)[[1]]
              if(!is(try(minValue(grobToPlot)),"try-error")) {
                minz <- minValue(grobToPlot)
              }
              grobToPlot <- sampleRegular(x=grobToPlot, size=maxpixels,
                                          ext=zoom, asRaster=TRUE, useGDAL=TRUE)
              if(length(colorTable)>0) cols<-colorTable
            }
            z <- getValues(grobToPlot)

            # If minValue is defined, then use it, otherwise, calculate them.
            #  This is different than maxz because of the sampleRegular. If the low
            #  values in the raster are missed in the sampleRegular, then the legend
            #  will be off by as many as are missing at the bottom; so, use the
            #  metadata version of minValue, but use the max(z) to accomodate cases
            #  where there are too many legend values for the number of raster values
            if(!exists("minz")) {
              minz <- min(z, na.rm=TRUE)
            }
            if (is.na(minz)) {
              minz <- min(z, na.rm=TRUE)
            }
            #
            maxz <- max(z, na.rm=TRUE)
            real <- any(na.omit(z) %% 1 != 0) # Test for real values or not

            # Deal with colors - This gets all combinations, real vs. integers,
            #  with zero, with no zero, with NA, with no NA, not enough numbers,
            #  too many numbers
            maxNumCols = 100

            nValues <- ifelse(real,maxNumCols+1, maxz-minz+1)
            colTable <- NULL

            if(is.null(cols)) { #i.e., contained within raster or nothing
              if(length(getColors(grobToPlot)[[1]])>0) {
                colTable <- getColors(grobToPlot)[[1]]
                lenColTable <- length(colTable)

                cols <- if(nValues>lenColTable) { # not enough colors, use colorRamp
                  colorRampPalette(colTable)(nValues)
                } else if (nValues<=(lenColTable)) { # one more color than needed:
                  #   assume bottom is NA
                  colTable
                } else if (nValues<=(lenColTable-1)) { # one more color than needed:
                  #   assume bottom is NA
                  na.color <- colTable[1]
                  colTable[minz:maxz - minz + 2]
                  #colTable[minz:maxz+max(0,1-minz)+1]
                } else if (nValues<=(lenColTable-2)) { # 2 more colors than needed,
                  #  assume bottom is NA, second is white
                  na.color <- colTable[1]
                  zero.color <- colTable[2]
                  colTable[minz:maxz - minz + 3]
                  #colTable[minz:maxz+max(0,1-minz)+1]
                } else { colTable }
              } else {
                cols <- rev(terrain.colors(nValues)) # default color if nothing specified
              }
            } else {
              cols <- if(nValues>length(cols)) {colorRampPalette(cols)(nValues)
              } else if (nValues<length(cols)) {cols[minz:maxz+max(0,1-minz)]
              } else {cols}
            }


            # colors are indexed from 1, as with all objects in R, but there are generally
            #  zero values on the rasters, so shift according to the minValue value, if
            # it is below 1. Shift it by 2, 1 to make the zeros into two, the other for the NAs
            #  to be ones

            # If object is real numbers, the default above is to discretize. This is
            #  particularly bad for numbers below 10. Here, numbers below maxNumCols
            #  that are reals will be rescaled to max = 100. These are, of course,
            #  only used for the color matrix, not the values on the Raster
            if((maxz <= maxNumCols) & real) {
              z <- maxNumCols/maxz*z
              # rescales so the minimum is 1, not <1
              z <- z + (((maxNumCols/maxz*minz)<1) * (-(maxNumCols/maxz*minz) + 1)) # for the values if below 1
            } else { # rescales so that the minimum is 1, not <1
              z <- z + ((minz<1) * (-minz + 1)) # for the values if below 1
            }


            if(any(!is.na(legendRange))) {
              if((max(legendRange)-min(legendRange)+1)<length(cols)) {
                message(paste0("legendRange is not wide enough, scaling to min and max",
                               " raster values"))
              } else {
                minz <- min(legendRange)
                maxz <- max(legendRange)
                if(is.null(colTable)) {
                  cols <- colorRampPalette(cols)(maxz-minz+1)
                } else {
                  if(length(getColors(grobToPlot)[[1]])>0) {
                    cols <- colorRampPalette(colTable)(maxz-minz+1)
                  } else {
                    cols <- rev(terrain.colors(maxz-minz+1)) # default color if nothing specified
                  }
                }
              }
            }


            # here, the default color (transparent) for zero, if it is the minimum
            # value, can be overridden
            if(!is.null(zero.color)) {
              if(minz==0) {
                cols[1] <- zero.color
              }
            }
            z <- z + 1 # for the NAs
            z[is.na(z)] <- max(1, minz)

            #if there are no zeros in the data, then don't shift colors

            #if(minz!=0) cols <- cols[-1]

            cols<-c(na.color, cols) # make first index of colors be transparent

            if((minz>1) | (minz<0)) {
              z <- matrix(cols[z-minz+1], nrow=nrow(grobToPlot), ncol=ncol(grobToPlot), byrow=TRUE)
            } else {
              z <- matrix(cols[z], nrow=nrow(grobToPlot), ncol=ncol(grobToPlot), byrow=TRUE)
            }
            list(z=z, minz=minz, maxz=maxz, cols=cols, real=real)
})


################################################################################
#' Identify where to get the grob from
#'
#' Because the Plot function can use the global environment as a source of
#' objects to plot, not just the call itself, this function identifies where
#' the data for the grob should come from, the current call or the global
#' environment. This should be called internally only.
#'
#' @param grobNamesi name of the object to plot
#'
#' @param toPlot list containing the objects to plot, made as a call to the Plot function
#'
#' @param takeFromPlotObj logical. If TRUE, then take from the call to Plot, FALSE takes
#' from global environment
#'
#' @author Eliot McIntire
#' @include plotting-classes.R
#' @rdname identifyGrobToPlot
setGeneric(".identifyGrobToPlot", function(grobNamesi, toPlot, takeFromPlotObj) {
  standardGeneric(".identifyGrobToPlot")
})

#' @include plotting-classes.R
#' @rdname identifyGrobToPlot
setMethod(".identifyGrobToPlot",
          signature=c(".spadesGrob", "list", "logical"),
          function(grobNamesi, toPlot, takeFromPlotObj) {

  # get the object name associated with this grob
  if (length(toPlot)==0) takeFromPlotObj <- FALSE
  # Does it already exist on the plot device or not
  #if(!takeFromPlotObj) { # Is this a replot
    if(nchar(grobNamesi@layerName)>0) {# means it is in a raster
      grobToPlot <- eval(parse(text=grobNamesi@objName), grobNamesi@envir)[[grobNamesi@layerName]]
    } else {
      grobToPlot <- eval(parse(text=grobNamesi@objName), grobNamesi@envir)
    }
})

#' @rdname identifyGrobToPlot
setMethod(".identifyGrobToPlot",
          signature=c(".spadesGrob", "missing", "logical"),
          function(grobNamesi, toPlot, takeFromPlotObj) {
            .identifyGrobToPlot(grobNamesi, list(), FALSE)
})

#' Prepare raster for plotting
#'
#' This is to be used internally. This takes a raster .spadesGrob, and converts zoomExtent
#' into a zoom, legendRange into a legend, and calculates the maxpixels to plot for speed.
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
.prepareRaster <- function(grobToPlot, zoomExtent, legendRange,
                           takeFromPlotObj, arr, speedup, newArr) {

  if(is.null(zoomExtent)) {
    zoom <- extent(grobToPlot)
    npixels <- ncell(grobToPlot)
  } else {
    zoom <- zoomExtent
    npixels <- ncell(crop(grobToPlot,zoom))
  }
  if(is.null(legendRange) | ((takeFromPlotObj==FALSE)*!newArr)) legendRange <- NA

  maxpixels <- min(5e5,3e4/(arr@columns*arr@rows)*prod(arr@ds))/speedup %>%
    min(., npixels)
  skipSample <- if(is.null(zoomExtent)) {maxpixels>=npixels} else {FALSE}

  return(list(maxpixels=maxpixels, skipSample=skipSample,
              legendRange=legendRange, zoom=zoom))

}

