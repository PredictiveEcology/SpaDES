### deal with spurious data.table warnings
if(getRversion() >= "3.1.0") {
  utils::globalVariables(c("groups", "thin", "whGrobNamesi"))
}

### Allow gg S3 class to be used with Plot, an S4 function
#' @import ggplot2
setOldClass("gg")
selectMethod("show", "gg")

### Allow histogram S3 class to be used with Plot, an S4 function
#' @import graphics
setOldClass("histogram")
selectMethod("show", "histogram")

### Allow histogram S3 class to be used with Plot, an S4 function
#' @import igraph
setOldClass("igraph")
selectMethod("show", "igraph")

################################################
#' The \code{.spatialObjects} class
#'
#' This class is the union of several spatial objects from raster and sp packages. Notably
#' missing is RasterBrick, for now.
#'
#' @seealso \code{\link{spadesClasses}}
#'
#' @slot members  SpatialPoints*, SpatialPolygons*, SpatialLines*,
#'                RasterLayer, RasterStack
#'
#' @aliases .spatialObjects
#' @name .spatialObjects-class
#' @rdname spatialObjects-class
#' @author Eliot McIntire
setClassUnion(name=".spatialObjects",
              members=c("SpatialPoints", "SpatialPolygons", "SpatialLines",
                        "RasterLayer", "RasterStack")
)

################################################
#' The \code{.spadesPlotObjects} class
#'
#' This class contains the union of .spatialObjects and several other plot-type objects.
#' These are the object classes that the \code{\link{Plot}} function can handle.
#'
#' @seealso \code{\link{spadesClasses}}
#'
#' @slot members SpatialPoints*, SpatialPolygons*, SpatialLines*, RasterLayer, RasterStack
#' @import ggplot2
#' @import graphics

#' @aliases .spadesPlotObjects
#' @name .spadesPlotObjects-class
#' @rdname spadesPlotObjects-class
#' @author Eliot McIntire
setClassUnion(name=".spadesPlotObjects",
              members=c(".spatialObjects", "gg", "histogram", "igraph"))

###########################################################################
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
#' @slot layerName character. Name of the layer represented by this .spadesGrob. Primarily
#' used for RasterStacks
#'
#' @slot objClass character. Class of the object represented by this .spadesGrob
#'
#' @slot isSpatialObjects logical. TRUE if the object is one of the SpaDES recognized
#' spatialObject classes
#'
#' @slot plotArgs list. Any parameters needed for plotting, set by Plot call.
#'
#' @aliases .spadesGrob
#' @name .spadesGrob-class
#' @rdname spadesGrob-class
#' @author Eliot McIntire
setClass(".spadesGrob",
         slots=list(plotName="character", objName="character", envir="environment",
                    layerName="character",
                    objClass="character", isSpatialObjects="logical",
                    plotArgs="list"),
         prototype=list(plotName=NA_character_, objName=NA_character_, layerName=NA_character_,
                        objClass=NA_character_, isSpatialObjects=NA,
                        plotArgs=as.list(NULL)),
         validity=function(object) {
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
#' @slot columns numeric. Number of columns in the arragnement.
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
#' @slot isSpatialObjects logical indicating whether the object(s) are \code{.spatialObjects}
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
#' @name .arrangement-class
#' @rdname arrangement-class
#' @author Eliot McIntire
setClass(".arrangement",
         slots=list(rows="numeric", columns="numeric",
                    actual.ratio="numeric", ds.dimensionRatio="numeric",
                    ds="numeric", objects="list", isRaster="logical", names="character",
                    extents="list", isSpatialObjects="logical", layout="list",
                    gp="list", gpText="list", gpAxis="list", size="list"),
         prototype=list(rows=1, columns=1,
                        actual.ratio=1, ds.dimensionRatio=1,
                        ds=c(7, 7), objects=as.list(NULL), isRaster=NA,
                        names=as.character(NULL),
                        extents=as.list(NULL), isSpatialObjects=NA, layout=as.list(NULL),
                        gp=as.list(NULL), gpText=as.list(NULL),
                        gpAxis=as.list(NULL), size=as.list(NULL)),
         validity=function(object) {
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
#' @name .spadesPlot-class
#' @rdname spadesPlot-class
#' @author Eliot McIntire
setClass(".spadesPlot",
         slots=list(arr=".arrangement",
                    spadesGrobList="list"),
         prototype=list(arr=new(".arrangement"),
                        spadesGrobList=as.list(NULL)),
         validity=function(object) {
           # check for valid extents
           if (any(is(object@arr, ".arrangement"))) {
             stop("must supply an arrangement")
           }
})

################################################################################
#' The \code{.spadesPlottables} class
#'
#' This class is the union of all .spadesPlotObjects (e.g., RasterLayer*,
#' SpatialPoints*, SpatialPolygons*, ggplot, hist etc.) and \code{\link{.spadesPlot}}
#' class objects.  This allows replotting of a \code{\link{.spadesPlot}} object
#'
#' @seealso \code{\link{spadesClasses}}
#'
#' @slot members \code{\link{.spadesPlotObjects}} and \code{\link{.spadesPlot}}
#' @import ggplot2
#' @import graphics
#'
#' @aliases .spadesPlottables
#' @name .spadesPlottables-class
#' @rdname spadesPlottables-class
#' @author Eliot McIntire
#'
setClassUnion(name=".spadesPlottables",
              members=c(".spadesPlotObjects", ".spadesPlot"))


###########################################################################
# Classes
###########################################################################

################################################################################
#' Specify where to plot
#'
#' Switch to an existing plot device, or if not already open,
#' launch a new graphics device based on operating system used.
#'
#' For example, \code{dev(6)} switches the active plot device to device #6.
#' If it doesn't exist, it opens it. NOTE: if devices 1-5 don't exist
#' they will be opened too.
#'
#' @param x   The number of a plot device. If missing, will open a new
#'            non-RStudio plotting device
#'
#' @param ... Additional arguments passed to \code{\link{newPlot}}.
#'
#' @return Opens a new plot device on the screen.
#'
#' @export
#' @docType methods
#' @rdname dev
#' @author Eliot McIntire and Alex Chubaty
#'
dev <- function(x, ...) {
  if (missing(x)) {
    if(is.null(dev.list())) {
      x <- 2L
    } else {
      if(any(names(dev.list())=="RStudioGD")) {
        x <- min(max(dev.list())+1,
                 which(names(dev.list())=="RStudioGD")+3L)
        dev(x)
      } else {
        x <- max(dev.list())
        dev(x)
      }
    }
  }
  if(is.null(dev.list())) newPlot(...)
  while (dev.set(x)<x) newPlot(...)
}

################################################################################
#' Open a new plotting window
#'
#' @param noRStudioGD Logical Passed to dev.new. Default is TRUE to avoid using
#'                    RStudio graphics device, which is slow.
#' @param ...         Additional arguments.
#'
#' @note \code{\link{dev.new}} is supposed to be the correct way to open a new
#' window in a platform-generic way, however, this doesn't work in RStudio.
#'
#' @seealso \code{\link{dev}}.
#'
#' @author Eliot McIntire and Alex Chubaty
#'
#' @export
#' @docType methods
#' @rdname newPlot
newPlot <- function(noRStudioGD=TRUE, ...) {
  dev.new(noRStudioGD=TRUE, ...)
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
            #             if((sum(isSpatialObjects)!=0) & (sum(isSpatialObjects)!=length(isSpatialObjects))) {
            #               stop("All objects for Plot call must be either .spatialObjects or
            #                    none can be")
            #             }

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
#selectMethod("show", "gpar")
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

#             p$gpText <- if(is.list(p$gpText)) {if(length(p$gpText)!=n) {rep(p$gpText, length.out=n)} else {p$gpText}
#             } else {
#               if(length(p$gpText)==n) {as.list(p$gpText)} else {rep(list(p$gpText), length.out=n)}}
#             p$gpAxis <- if(is.list(p$gpAxis)) {if(length(p$gpAxis)!=n) {rep(p$gpAxis, length.out=n)} else {p$gpAxis}
#             } else {
#               if(length(p$gpAxis)==n) {as.list(p$gpAxis)} else {rep(list(p$gpAxis), length.out=n)}}
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
    if (abs(((extents[[extentInd]]@ymax - extents[[extentInd]]@ymin) /
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
#' fileList <- data.frame(files =
#'      dir(file.path(find.package("SpaDES",
#'                                 lib.loc=getOption("devtools.path"),
#'                                 quiet=FALSE),
#'                   "maps"),
#'         full.names=TRUE, pattern= "tif"),
#'      functions="rasterToMemory",
#'      packages="SpaDES",
#'      stringsAsFactors=FALSE)
#'
#' # Load files to memory (using rasterToMemory)
#' sim1 <- loadFiles(fileList=fileList)
#'
#' Plot(sim1$DEM)
#' caribouTraj <- makeLines(caribou1, caribou2)
#' Plot(caribouTraj, addTo="sim$DEM", length=0.1)
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
setGeneric("Plot", signature="...",
           function(..., new=FALSE, addTo=NULL, gp=gpar(), gpText=gpar(), gpAxis=gpar(),
                    axes=FALSE, speedup = 1,
                    size=5, cols=NULL, zoomExtent=NULL,
                    visualSqueeze=NULL, legend=TRUE, legendRange=NULL, legendText=NULL,
                    pch = 19, title=TRUE,
                    na.color="#FFFFFF00", zero.color=NULL, length=NULL) {
             standardGeneric("Plot")
 })

#' @rdname Plot
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

#' Convert grid.locator units
#'
#' Converts them to meaningful units. Used within \code{.clickCoord}
#'
#' @param grid.locator an object that was output by a call to grid.locator and mouse click(s)
#' @author Eliot McIntire
#' @rdname unittrim
#' @export
.unittrim <- function(grid.locator) {
  as.numeric(sub("^([0-9]+|[0-9]+[.][0-9])[0-9]*", "\\1", as.character(grid.locator)))
}


################################################################################
#' Mouse interactions with Plots
#'
#' These functions use \code{grid.locator}. The primary two user-level functions are
#' \code{clickValues} and \code{clickExtent}. These functions automatically select
#' the correct viewport (i.e., map) where the mouse clicks occured so the user
#' does not have to manually specify which map is being clicked on.
#' This works for \code{Raster*}, \code{SpatialPoints*}, and \code{SpatialPolygons*} objects.
#'
#' \code{clickValues} is equivalent to running \code{X[SpatialPoints(locator(n))]}, where
#' X is the raster being clicked on, in base graphics. This function determines which place in the
#' grid.layout was clicked and makes all appropriate calculations to determine the value
#' on the raster(s) at that or those location(s). It should be noted that when zooming in
#' to rasters, plotting of rasters will only allow for complete pixels to be plotted, even
#' if the extent is not perfectly in line with pixel edges. As a result, when values
#' returned by this function may be slightly off (<0.5 pixel width).
#'
#' \code{clickExtent} is for drawing an extent with two mouse clicks on a given Plotted map.
#'
#' \code{clickCoordinates} is the workhorse function that determines which plot has been
#' clicked on and passes this plot name and the clicked coordinates to \code{.clickCoord}.
#'
#' \code{.clickCoord} is intended for internal use and is called by other functions here.
#'
#' @param n The number of mouse clicks to do.
#'
#' @return \code{clickValues} returns the layer names and values at the clicked points.
#' \code{clickExtent} invisibly returns the extent object, and optionally plots it in a new device window.
#' \code{clickCoordinates} returns the xy coordinates in the units of the plot clicked on.
#'
#' @export
#' @docType methods
#' @author Eliot McIntire
#' @rdname spadesMouseClicks
clickValues <- function(n=1) {

  coords <- clickCoordinates(n=n)
  objLay <- strsplit(coords$map, "\\$")
  objNames <- sapply(objLay, function(x) x[1])
  layNames <- sapply(objLay, function(x) x[2])
  for (i in 1:n) {
    if(!is.na(layNames[i])) {
      coords$coords$value <- sapply(seq_len(n), function(i) {
        eval(parse(text=objNames[i]),
             envir=coords$envir[[i]])[[layNames[i]]][
               cellFromXY(eval(parse(text=objNames[i]),envir=coords$envir[[i]])[[layNames[i]]],
                          coords$coords[i,1:2])]
      })
    } else {
      coords$coords$value <- sapply(seq_len(n), function(i) {
        eval(parse(text=objNames[i]),
             envir=coords$envir[[i]])[
               cellFromXY(eval(parse(text=objNames[i]),envir=coords$envir[[i]]),
                          coords$coords[i,1:2])]
      })
    }
  }
  return(coords$coords)
}

#' @param devNum The device number for the new plot to be plotted on
#'
#' @param plot.it logical. If \code{TRUE} a new windows is made for the new extent. Default \code{TRUE}.
#'
#' @export
#' @docType methods
#' @author Eliot McIntire
#' @rdname spadesMouseClicks
clickExtent <- function(devNum=NULL, plot.it=TRUE) {

  corners <- clickCoordinates(2)
  zoom <- extent(c(sort(corners[[3]]$x), sort(corners[[3]]$y)))

  if(plot.it) {
    devActive <- dev.cur()
    if(is.null(devNum)) {
      newPlot()
    } else {
      dev(devNum)
    }

    objLay <- strsplit(corners$map, "\\$")
    objNames <- unique(sapply(objLay, function(x) x[1]))
    layNames <- unique(sapply(objLay, function(x) x[2]))
    if(!is.na(layNames)) {
      Plot(eval(parse(text=objNames), envir=corners$envir[[1]])[[layNames]], zoomExtent=zoom, new=TRUE)
    } else {
      Plot(get(objNames, envir=corners$envir[[1]]), zoomExtent=zoom, new=TRUE)
    }

    dev(devActive)
    return(invisible(zoom))
  } else {
    return(zoom)
  }
}

#' @export
#' @docType methods
#' @author Eliot McIntire
#' @rdname spadesMouseClicks
clickCoordinates <- function(n=1) {
  dc <- dev.cur()

  arr <- try(.getSpaDES(paste0("spadesPlot", dc)))
  if(is(arr, "try-error")) stop(paste("Plot does not already exist on current device.",
                                      "Try new=TRUE, clearPlot() or change device to",
                                      "one that has objects from a call to Plot()"))
  gl <- grid.layout(nrow=arr@arr@rows*3+2,
                    ncol=arr@arr@columns*3+2,
                    widths=arr@arr@layout$wdth,
                    heights=arr@arr@layout$ht)

  grepNullsW <- grep("null$", gl$widths)
  grepNpcsW <- grep("npc$", gl$widths)
  nulls <- as.numeric(unlist(strsplit(as.character(gl$widths)[grepNullsW], "null") ))
  npcs <- as.numeric(unlist(strsplit(as.character(gl$widths)[grepNpcsW], "npc") ))
  remaining <- 1 - sum(npcs)
  npcForNulls <- nulls*remaining/sum(nulls)
  widthNpcs <- c(npcs, npcForNulls)[order(c(grepNpcsW, grepNullsW))]

  grepNullsH <- grep("null$", gl$heights)
  grepNpcsH <- grep("npc$", gl$heights)
  nulls <- as.numeric(unlist(strsplit(as.character(gl$heights)[grepNullsH], "null") ))
  npcs <- as.numeric(unlist(strsplit(as.character(gl$heights)[grepNpcsH], "npc") ))
  remaining <- 1 - sum(npcs)
  npcForNulls <- nulls*remaining/sum(nulls)
  heightNpcs <- c(npcs, npcForNulls)[order(c(grepNpcsH, grepNullsH))]

  clickCoords <- data.frame(x=NA_real_, y=NA_real_, stringsAsFactors = FALSE)
  mapNames <- character(n)
  envs <- list()

  grobLoc <- list()

  for(i in 1:n) {
    seekViewport("top")
    gloc <- grid.locator(unit="npc")
    xInt <- findInterval(as.numeric(strsplit(as.character(gloc$x), "npc")[[1]]), c(0, cumsum(widthNpcs)))
    # for the y, grid package treats bottom left as origin, Plot treats top left
    #  as origin... so, require 1-
    yInt <- findInterval(as.numeric(strsplit(as.character(gloc$y), "npc")[[1]]), c(0, cumsum(heightNpcs)))
    if(!(xInt %in% grepNpcsW) & !(yInt %in% grepNpcsH)) {
      stop("No plot at those coordinates")
    }
    column <-  which(xInt==grepNpcsW)
    row <- which((yInt==grepNpcsH)[length(grepNpcsH):1])
    map <- column + (row-1)*arr@arr@columns

    maxLayX <- cumsum(widthNpcs)[xInt]
    minLayX <- cumsum(widthNpcs)[xInt-1]
    grobLoc$x <- unit((as.numeric(strsplit(as.character(gloc$x), "npc")[[1]])-minLayX)/(maxLayX-minLayX), "npc")

    maxLayY <- cumsum(heightNpcs)[yInt]
    minLayY <- cumsum(heightNpcs)[yInt-1]
    grobLoc$y <- unit((as.numeric(strsplit(as.character(gloc$y), "npc")[[1]])-minLayY)/(maxLayY-minLayY), "npc")

    clickCoords[i, ] <- .clickCoord(arr@spadesGrobList[[map]][[1]]@plotName, n=1, gl=grobLoc)
    mapNames[i] <- arr@spadesGrobList[[map]][[1]]@plotName
    envs[[i]] <- arr@spadesGrobList[[map]][[1]]@envir
  }
  return(list(map=mapNames, envir=envs, coords=clickCoords))
}

#' @param X The raster object whose values will be returned where mouse clicks occur.
#'
#' @param gl An object created by a call to \code{grid.locator}.
#'
#' @export
#' @author Eliot McIntire
#' @docType methods
#' @rdname spadesMouseClicks
.clickCoord <- function(X, n=1, gl=NULL) {
  pts<-data.frame(x=NA_real_, y=NA_real_, stringsAsFactors = FALSE)
  seekViewport(X)
  for(i in 1:n) {
    if(is.null(gl)) {
      gl <- grid.locator()
      pts[i, ] <- .unittrim(gl)
    } else {
      pts[i, ] <- c(convertX(gl$x, "native"), convertY(gl$y, "native"))
    }
  }
  return(pts)
}

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
#' @rdname identifyGrobToPlot
setGeneric(".identifyGrobToPlot", function(grobNamesi, toPlot, takeFromPlotObj) {
  standardGeneric(".identifyGrobToPlot")
})

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
  #} else { # Is this a new plot to be added or plotted
  #  if(nchar(grobNamesi@layerName)>0) {
  #    grobToPlot <- toPlot[[grobNamesi@objName]][[grobNamesi@layerName]]
  #  } else {
  #    grobToPlot <- toPlot[[grobNamesi@objName]]
  #  }
  #}
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

################################################################################
#' Clear plotting device
#'
#' Under some conditions, a device and its meta data needs to be cleared manually.
#' This can be done with either the \code{new=TRUE} argument within the call to \code{Plot}.
#' Sometimes, the metadata of a previous plot will prevent correct plotting of
#' a new \code{Plot} call. Use \code{clearPlot} to clear the
#' device and all the associated metadata manually.
#'
#' @param dev Numeric. Device number to clear.
#'
#' @param removeData Logical whether any data that was stored in the \code{.spadesEnv} should also
#' be removed, i.e., not just the plot window wiped.
#'
#' @export
#' @docType methods
#' @rdname Plot
#' @author Eliot McIntire
clearPlot <- function(dev=dev.cur(), removeData=TRUE) {
  suppressWarnings(try(rm(list=paste0("spadesPlot", dev), envir=.spadesEnv)))
  if(removeData) suppressWarnings(try(rm(list=ls(.spadesEnv[[paste0("dev",dev)]]),
                          envir=.spadesEnv[[paste0("dev",dev)]]), silent=TRUE))
  devActive <- dev.cur()
  if(devActive==1) return(invisible())
  dev(dev)
  grid.newpage()
  dev(devActive)
}
