### Allow gg and histogram S3 classes to be used with Plot, an S4 function
#' @import ggplot2
setOldClass("gg")

setOldClass("histogram")

################################################
#' The \code{spatialObjects} class
#'
#' This class contains the plotting arrangement information.
#'
#' @slot members SpatialPoints*, SpatialPolygons*, RasterLayer, RasterStack
#' @name spatialObjects-class
#' @rdname spatialObjects-class
#' @author Eliot McIntire
#' @exportClass spatialObjects
setClassUnion(name="spatialObjects", members=c("SpatialPoints", "SpatialPolygons",
                                               "RasterLayer", "RasterStack"))

################################################
#' The \code{spadesPlotObjects} class
#'
#' This class contains the plotting arrangement information.
#'
#' @slot members SpatialPoints*, SpatialPolygons*, RasterLayer, RasterStack
#' @name spadesPlotObjects-class
#' @rdname spadesPlotObjects-class
#' @author Eliot McIntire
#' @exportClass spadesPlotObjects
setClassUnion(name="spadesPlotObjects", members=c("spatialObjects", "gg", "histogram"))


##############################################################
#' Specify where to plot
#'
#' Switch to an existing plot device, or if not already open,
#' launch a new graphics device based on operating system used.
#'
#' For example, \code{dev(6)} switches the active plot device to device #6.
#' If it doesn't exist, it opens it. NOTE: if devices 1-5 don't exist
#' they will be opened too.
#'
#' @param x   The number of a plot device.
#'
#' @param ... Additional arguments passed to \code{\link{newPlot}}.
#'
#' @return Opens a new plot device on the screen.
#'
#' @export
#' @docType methods
#' @rdname dev-method
#'
# @examples
# needs examples
dev <- function(x, ...) {
  if(is.null(dev.list())) newPlot(...)
  while (dev.set(x)!=x) newPlot(...)
}

##############################################################
#' Open a new plotting window
#'
#' @param ... Additional arguments.
#'
#' @note \code{\link{dev.new}} is supposed to be the correct way to open a new
#' window in a platform-generic way, however, this doesn't work in RStudio.
#'
#' @seealso \code{\link{dev}}.
#'
#' @export
#' @docType methods
#' @rdname newPlot-method
#'
#' @param noRStudioGD logical Passed to dev.new. Default is TRUE to avoid using
#' RStudio graphics device, which is slow.
#'
# @examples
# needs examples
newPlot <- function(noRStudioGD=TRUE, ...) {
  dev.new(noRStudioGD=TRUE, ...)
}


##############################################################
#' Find the number of layers in a Spatial Object
#'
#' There are already methods for \code{Raster*} in the raster package.
#' Adding methods for \code{list}, \code{SpatialPolygons}, and \code{SpatialPoints}.
#' These latter classes return 1.
#'
#' @param x A \code{spadesPlotObjects} object or list of these.
#'
#' @export
#' @importFrom raster nlayers
#' @importFrom methods is
#' @rdname nlayers
setMethod("nlayers",
          signature="list",
          function(x) {
            y = sum(sapply(x, function(x) {
              if(is(x, "RasterStack")) {
                x<-nlayers(x)
              } else {
                x <- 1
              }
              return(x)
            }))
            return(y)
          })

#' @export
#' @rdname nlayers
setMethod("nlayers",
          signature="SpatialPolygons",
          definition=function(x) {
            return(1)
          })

#' @export
#' @rdname nlayers
setMethod("nlayers",
          signature="SpatialPoints",
          definition=function(x) {
            return(1)
          })

##############################################################
#' Extract the layer names of Spatial Objects
#'
#' There are methods for \code{Raster*}, \code{SpatialPoints*}, and \code{SpatialPolygons*},
#' though the latter return an empty character vector of length 1.
#'
#' @param object A \code{Raster*}, \code{SpatialPoints*}, or \code{SpatialPolygons*} object; or list of these.
#'
#' @name layerNames
#' @rdname layerNames
#' @export
setGeneric("layerNames", function(object) {
  standardGeneric("layerNames")
})

#' @rdname layerNames
#' @export
setMethod("layerNames",
          signature="list",
          definition=function(object) {
            unlist(lapply(object, layerNames))
          })


#' @export
#' @rdname layerNames
setMethod("layerNames",
          signature="SpatialPoints",
          definition=function(object) {
            return("")
          })


#' @export
#' @rdname layerNames
setMethod("layerNames",
          signature="SpatialPolygons",
          definition=function(object) {
            return("")
          })


#' @export
#' @rdname layerNames
setMethod("layerNames",
          signature="Raster",
          definition=function(object) {
            names(object)
          })




##############################################################
#' Assess whether a list of extents are all equal
#'
#' @param extents list of extents objects
#' @name equalExtent
#' @rdname equalExtent
#' @export
setGeneric("equalExtent", function(extents) {
  standardGeneric("equalExtent")
})

#' @rdname equalExtent
#' @export
setMethod("equalExtent",
          signature="list",
          definition=function(extents) {
            all(c(sapply(extents, function(x) x@xmin)==extents[[1]]@xmin,
                  sapply(extents, function(x) x@xmax)==extents[[1]]@xmax,
                  sapply(extents, function(x) x@ymin)==extents[[1]]@ymin,
                  sapply(extents, function(x) x@ymax)==extents[[1]]@ymax))
          })

###########################################################################
#' The \code{arrangement} class
#'
#' This class contains the plotting arrangement information.
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
#' @slot layout list of length 2, with width and height measurements for layout.
#'
#' @rdname arrangement-class
#' @exportClass arrangement
#'
#' @author Eliot McIntire
#'
setClass("arrangement",
         slots=list(rows="numeric", columns="numeric",
                    actual.ratio="numeric", ds.dimensionRatio="numeric",
                    ds="numeric", objects="list", isRaster="logical", names="character",
                    extents="list", isSpatialObjects="logical", layout="list"),
         prototype=list(rows=1, columns=1,
                        actual.ratio=1, ds.dimensionRatio=1,
                        ds=c(7, 7), objects=as.list(NULL), isRaster=NA,
                        names=as.character(NULL),
                        extents=as.list(NULL), isSpatialObjects=NA, layout=as.list(NULL)),
         validity=function(object) {
           # check for valid extents
           if (any(is.na(object@extents))) {
             stop("must supply a list of extents")
           }
         })

#' Determine optimal plotting arrangement of Spatial Objects
#'
#' This assesses the device geometry, the map geometry, and the number of spatial
#' objects to plot and builds an object that will be used by the Plot functions to plot
#' them efficiently
#'
#' @param extents A list of extents from spatial objects to plot
#' @rdname arrangeViewports
#' @export
#' @docType methods
setGeneric("arrangeViewports", function(extents){ #, name=NULL) {
  standardGeneric("arrangeViewports")
})

#' @rdname arrangeViewports
#' @export
setMethod("arrangeViewports",
          signature=c("list"),
          definition= function(extents) {
            dimx <- apply(sapply(extents, function(y) apply(bbox(y), 1, function(x) diff(range(x)))), 1, max)
            nPlots <- length(extents)
            names <- names(extents)

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

            out <- new("arrangement", rows=rows, columns=columns,
                       actual.ratio=actual.ratio, ds.dimensionRatio=ds.dimensionRatio,
                       ds=ds,
                       names=names, extents = extents)
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
#'   \code{devtools::install_github("s-u/fastshp")}
#'
#' NOTE: you may get errors relating to not having installed the software tools
#' required for building R packages on your system. For development purposes on
#' a Windows machine, you'll need to install Rtools from http://cran.r-project.org/bin/windows/Rtools/.
#'
#' @param grobToPlot \code{Raster*}, \code{SpatialPoints*}, \code{SpatialPolygons*} object
#'
#' @param col Currently only used for the legend of a \code{Raster*} object.
#'
#' @param size The size of the SpatialPoints
#'
#' @param minv The minimum value on a Raster*. Required because not all Rasters
#' have this defined internally
#'
#' @param maxv The maximum value on a Raster*. Required because not all Rasters
#' have this defined internally
#'
#' @param legend logical, whether a legend should be drawn. Default \code{TRUE}.
#'
#' @param legendText Vector of values to use for legend value labels. Defaults to \code{NULL} which results
#' in a pretty numeric representation. If \code{Raster*} has a Raster Attribute Table (rat, see raster
#' package), this will be used by default. Currently, only a single vector is accepted.
#'
#' @param draw logical. Whether the grob, after being created should be drawn. Default \code{TRUE}.
#'
#' @param gp grid parameters, usually the output of a call to \code{\link{gpar}}
#'
#' @param pch for \code{SpatialPoints}, as \code{par}
#'
#' @param real logical. Whether the data are real numbers (vs. integer or factor).
#'
#' @param speedup Numeric. The factor by which the number of vertices in \code{SpatialPolygons} will be subsampled.
#' The vertices are already subsampled by default to make plotting faster.
#'
#' @param ... additional arguments. Currently nothing.
#'
#' @name plotGrob
#' @rdname plotGrob
#' @export
#' @docType methods
setGeneric("plotGrob", function(grobToPlot, col=NULL, real=FALSE,
                                size=unit(5, "points"),
                                minv, maxv,
                                legend=TRUE, legendText=NULL,
                                draw=TRUE,
                                gp=gpar(), pch=19,
                                speedup=1, ...) {
  standardGeneric("plotGrob")
})


#' @rdname plotGrob
#' @export
setMethod("plotGrob",
          signature=c("matrix"),
          definition= function(grobToPlot, col, real, size, minv, maxv,
                               legend, legendText, draw,
                               gp, pch, ...) {

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
#             if(maxv<=maxNumCols ) {
#               if(minv>=0 & real) { # i.e., a proportion or real numbers between 0 and maxNumCols
#                 maxcol <- maxNumCols+1 # this corresponds to the maxNumCols in makeColorMatrix, with 1 extra for NAs
#               } else {
#                 maxcol<-length(col)#maxv
#               }
#             } else {
#               maxcol <- maxv - minv + max(1, -minv+1) + 1 # need one for the NA at the bottom,
#               #one for the zero at the bottom, if there, and one
#               #for when taking a difference between two numbers-- need to include
#               # both numbers
#             }

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
                                #                                 if(legend) { # if top or bottom entry of legend is white, make a box around it to see it
                                #
                                #                                   if(grepl("^#FFFFFF", col[1]) | grepl("^#FFFFFF", col[maxcol]))
                                #                                     rectGrob(x=1.04, y=0.5, height=0.5, width=0.03, gp=gpar(fill=NA))
                                #                                 },
                                if(legend) {
                                  txt <- if(is.null(legendText)){
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
                                    gp=gpar(cex=0.9),
                                    just="left", check.overlap=T,
                                    name="legendText")

                                }
                              ),
                              gp=gp,
                              cl="plotRast")
            if(draw) grid.draw(rastGrob)
            return(invisible(rastGrob))
          })

#' @rdname plotGrob
#' @export
setMethod("plotGrob",
          signature=c("SpatialPoints"),
          definition= function(grobToPlot, col, size,
                               legend, draw, gp=gpar(), pch, ...) {
            pntGrob <- gTree(grobToPlot=grobToPlot,
                             children=gList(
                               pointsGrob(x=grobToPlot$x, y=grobToPlot$y, pch=pch, size=size)
                             ),
                             gp=gp,
                             cl="plotPoint")
            if(draw) grid.draw(pntGrob)
            return(invisible(pntGrob))
          })

#' @rdname plotGrob
#' @export
setMethod("plotGrob",
          signature=c("SpatialPolygons"),
          definition= function(grobToPlot, col, size,
                               legend, draw, gp=gpar(), pch, ...) {


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
            idLength <- unlist(lapply(xyOrd.l, function(i) lapply(i, nrow)))
            xyOrd <- do.call(rbind, lapply(xyOrd.l, function(i) do.call(rbind, i)))

            if (requireNamespace("fastshp", quietly=TRUE)) {
              thinned <- fastshp::thin(xyOrd[, 1], xyOrd[, 2], tolerance=speedupScale*speedup)
              xyOrd <- xyOrd[thinned, ]
              idLength <- tapply(thinned, rep(1:length(idLength), idLength), sum)
            } else {
              message(paste("To speed up Polygons plotting using Plot please download fastshp",
                            "#install.packages(\"devtools\")",
                            "library(\"devtools\")",
                            "install_github(\"s-u/fastshp\")", sep="\n",
                            "You may also need to download and install Rtools",
                            "at http://cran.r-project.org/bin/windows/Rtools/"))
            }

            gp$fill[hole] <- "#FFFFFF00"
            polyGrob <- gTree(children=gList(
              polygonGrob(x=xyOrd[, 1], y=xyOrd[, 2], id.lengths=idLength,
                          gp=gp, default.units="native")
            ),
            gp=gp,
            cl="plotPoly")
            if(draw) grid.draw(polyGrob)
            return(invisible(polyGrob))
          })


##################
#' Make an optimal layout of plots
#'
#' Using the size of the current device, and number and dimension ratios of the plots,
#' this function will place them optimally in the plotting region.
#'
#' @param arr an object of class \code{arrangement}
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
#' @export
makeLayout <- function(arr, visualSqueeze, legend=TRUE, axes=TRUE, title=TRUE) {

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

  return(list(wdth=wdth, ht=ht))
}


##################
#' Make viewports
#'
#' Given a set of extents, and a layout for these extents, this function will output
#' a viewport tree to allow plotting.
#'
#' This function will either create a totally new set of viewports, or simply add
#' some nested viewports to an existing arrangement, i.e., is there still white
#' space availabe to plot.
#'
#' @param extents a list of extents objects.
#'
#' @param arr an object of class \code{arrangement}.
#'
#' @param newArr logical. Whether this function will create a completely new viewport.
#' Default \code{FALSE}.
#'
#' @export
makeViewports <- function(extents, arr, newArr = FALSE) {

  columns <- arr@columns
  rows <- arr@rows
  topVp <- viewport(layout=grid.layout(nrow=rows*3+2,
                                       ncol=columns*3+2,
                                       widths=arr@layout$wdth,
                                       heights=arr@layout$ht),
                    name="top")
  plotVps <- list()
  nam <- names(extents)

  for(extentInd in 1:length(extents)) {

    posInd <- match(nam[extentInd], arr@names)

    lpc = ceiling((posInd-1)%%columns+1)*3
    lpr = ceiling(posInd/columns)*3

    if(!arr@isSpatialObjects[posInd]) {
      lpc = c((lpc-1):(lpc+1))
      lpr = c((lpr):(lpr+1))
    }

    plotVps[[extentInd]] <- viewport(
      name=nam[extentInd],
      layout.pos.col = lpc,
      layout.pos.row = lpr,
      xscale=c(extents[[extentInd]]@xmin, extents[[extentInd]]@xmax),
      yscale=c(extents[[extentInd]]@ymin, extents[[extentInd]]@ymax))

  }

  if(newArr) {
    wholeVp <- vpTree(topVp, do.call(vpList, plotVps))
  } else {
    wholeVp <- do.call(vpList, plotVps)
  }
  return(wholeVp)
}


##############################################################
#' Plots arrows showing direction of agent movement
#'
#' @param from          Starting spatial coordinates (\code{SpatialPointsDataFrame}).
#'
#' @param to            Ending spatial coordinates (\code{SpatialPointsDataFrame}).
#'
#' @param addTo Optional character string. The name of a map layer on which to draw the arrows.
#'
#' @param title logical. Add title to plot. Defaults to \code{TRUE}.
#' Since this is also the viewport name, it is a good idea to plot it so the
#' viewport can be called for later plotting.
#'
#' @param axes logical Add axes to plot. Defaults to \code{TRUE}.
#'
#' @param ...           Additional plotting parameters passed to \code{\link{grid.polyline}}.
#' Currently does not appear to pass anything.
#'
#' @return Plots the vectors representing agent movement on the specified map.
#'
#' @import sp
#' @import grid
#' @export
#' @docType methods
#' @rdname drawArrows-method
#' @examples
#' # Make 2 objects
#' caribou1 <- SpatialPoints(cbind(x=runif(10, -50, 50), y=runif(10, -50, 50)))
#' caribou2 <- SpatialPoints(cbind(x=runif(10, -50, 50), y=runif(10, -50, 50)))
#'
#' drawArrows(caribou1, caribou2)
#'
#' # or  to a previous Plot
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
#' Plot(DEM)
#' drawArrows(caribou1, caribou2, addTo="DEM")
setGeneric("drawArrows", function(from, to, addTo, title=TRUE, axes=TRUE, ...) {
  standardGeneric("drawArrows")
})

#' @param length    The length of the arrows to draw (defaults to 0.1).
#'
#' @rdname drawArrows-method
#'
setMethod("drawArrows",
          signature=c("SpatialPoints", "SpatialPoints", "character"),
          definition=function(from, to, addTo, title, axes, ..., length=0.1) {
            seekViewport(addTo, recording=FALSE)
            grid.polyline(x=c(from$x, to$x), y=c(from$y, to$y),
                          default.units="native",
                          id=rep(1:length(from), 2),
                          arrow=arrow(length=unit(length, "inches"), ...))
            upViewport(0)
          })

#' @rdname drawArrows-method
#'
setMethod("drawArrows",
          signature=c("SpatialPoints", "SpatialPoints", "missing"),
          definition=function(from, to, addTo, title, axes, ..., length=0.1) {
            grid.newpage()
            extents <- list(extent(
              extendrange(c(min(min(from$x), min(to$x)), max(max(from$x, to$x)))),
              extendrange(c(min(min(from$y), min(to$y)), max(max(from$y, to$y))))))
            names(extents) <- .objectNames("drawArrows", argName = NULL)[1]
            arr <- arrangeViewports(extents)
            arr@layout <- makeLayout(arr=arr, visualSqueeze=0.75)
            arr@isSpatialObjects <- TRUE
            vps <- makeViewports(extents, arr=arr, newArr = TRUE)
            pushViewport(vps)
            seekViewport(names(extents), recording=FALSE)
            grid.polyline(x=c(from$x, to$x), y=c(from$y, to$y),
                          default.units="native",
                          id=rep(1:length(from), 2),
                          arrow=arrow(length=unit(length, "inches"), ...))
            if (title) grid.text(names(extents), 0.5, 1.05)
            if (axes) {grid.xaxis(); grid.yaxis()}

            upViewport(0)
          })


##############################################################
#' Extracts the object names
#'
#' This is primarily used from Plot.
#'
#' @param calledFrom character vector, length 1, indicating which function call is
#' desired. Defaults to \code{Plot}
#' @param argClass character vector, length 1, indicating which class is being
#' searched for among the arguments. Defaults to \code{spadesPlotObjects}
#' @param argName character vector, length 1, or \code{NULL}, indicating if the
#' arguments to select have a name, no name (empty string), or do not use name (NULL).
#'
#' @importFrom methods is
#' @export
#' @docType methods
#' @rdname objectNames
.objectNames <- function(calledFrom="Plot", argClass="spadesPlotObjects",
                         argName="") {


  scalls <- sys.calls()
  # First extract from the sys.calls only the function "calledFrom"
  frameCalledFrom<-which(sapply(scalls, function(x)
    grepl(x, pattern=paste0("^", calledFrom))[1]))
  callArgs <- as.list(scalls[frameCalledFrom][[1]])[-1]

  # Second, match argument names, via argName, if argName is not null and names exist
  callNamedArgs <- if(!is.null(argName)) {
    if(!is.null(names(callArgs))) {
      callArgs[names(callArgs)==argName]
    } else {
      callArgs
    }
  } else {
    callArgs
  }
  callNamedArgs <- callNamedArgs[sapply(callNamedArgs, function(x) x!="...")]


  # First run through call stack for simple, i.e., calls to Plot that are
  # just spadesPlotObjects to plot
  objs <- vector("list", length(callNamedArgs))
  first <- sapply(as.character(callNamedArgs), function(x)
    strsplit(split="[[:punct:]]", x)[[1]][1])
  firstSO <- sapply(first, function(y) is(get(y, sys.frame(frameCalledFrom-1)), argClass))
  if(any(firstSO)) { objs[firstSO] <- first[firstSO] }
  # cut short if all are dealt with
  if(all(!sapply(objs, is.null))) return(objs)

  # Many calls to Plot will use a simList object, which has an argument called
  #   sim. Search for this, and look up the sim object in the calling frame.
  #   Make it local here
  hasSim <- sapply(scalls, function(x)
    any(grepl(x, pattern=paste0("^sim$"))))
  if (any(hasSim)) {
    simEnv <- sys.frame(which(hasSim)-1)
    sim <- get("sim", envir=simEnv)
  }

  # Look for calls that use "get"; extract them and extract object names
  asChar <- lapply(callNamedArgs, function(x) as.character(x))
  isGet <- sapply(asChar, function(x) x[1]=="get")
  if(any(isGet)) {
    isGetTxt <- sapply(asChar[isGet], function(x) is(try(get(x[2], sys.frame(frameCalledFrom-1)),
                                                         silent=TRUE), argClass))
    if(any(isGetTxt)) {
      secondSO <- lapply(asChar[isGet][isGetTxt], function(x) x[2])
      thirdSO <- lapply(asChar[isGet][!isGetTxt], function(x) eval(parse(text=x[2]),
                                                                   envir=sys.frame(frameCalledFrom-1)))
      objs[isGet][isGetTxt] <- secondSO
      objs[isGet][!isGetTxt] <- thirdSO
    } else {
      thirdSO <- lapply(asChar[isGet], function(x) eval(parse(text=x[2]),
                                                        envir=sys.frame(frameCalledFrom-1)))
      objs[isGet] <- thirdSO
    }
  }
  # cut short if all are dealt with
  if(all(!sapply(objs, is.null))) return(objs)


  # Search for "get" nested within, most commonly because of a call to a layer
  #   within a stack e.g., get(simGlobals(sim)$.stackname)$Fires
  if(any(!isGet)) {
    isGetInner <- lapply(asChar[!isGet], function(x) grepl("get", x))
    if(any(sapply(isGetInner, any))) {
      innerGet <- asChar[!isGet][sapply(isGetInner, any)]
      insideGet <- lapply(1:length(innerGet), function(x)
        sub("\\)$", "", sub("get\\(", "", innerGet[[x]][isGetInner[[x]]])))
      fourthSO <- lapply(insideGet, function(w) {
        if(grepl(pattern=", ", w)) {
          insideGetSO <- sapply(strsplit(split="[, = ]", w)[[1]], function(y)
            is(try(get(eval(parse(text=y),
                            envir=sys.frame(frameCalledFrom-1))), silent=TRUE), argClass))
          fourthSO <- sapply(names(insideGetSO)[which(insideGetSO)], function(x)
            eval(parse(text=x), envir=sys.frame(frameCalledFrom-1)))
        } else {
          fourthSO <- eval(parse(text=w), envir=sys.frame(frameCalledFrom-1))
        }})
      objs[!isGet][sapply(isGetInner, any)] <- fourthSO
    }
  }

  # cut short if all are dealt with
  if(all(!sapply(objs, is.null))) return(objs)

  # Look for layer() which is used by shiny to indicate a plot
  isShinyLayer <- lapply(asChar, function(x) grepl("layer()", x))
  #   isShinyLayer <- lapply(asChar[!isGet][!sapply(isGetInner, any)],
  #                        function(x) grepl("layer()", x))
  if(any(sapply(isShinyLayer, any))) {
    whIsShinyLayer <- lapply(isShinyLayer[sapply(isShinyLayer, any)], which)
    shinyLayer <- asChar[sapply(isShinyLayer, any)]
    fifthSO <- lapply(1:length(shinyLayer), function(x)
      shinyLayer[[x]][whIsShinyLayer[[x]]+1])
    objs[sapply(isShinyLayer, any)] <- fifthSO
  }
  # cut short if all are dealt with
  if(all(!sapply(objs, is.null))) return(objs)

  isAdHocStack <- lapply(asChar, function(x) grepl("^stack", x))
  sixthSO <- rep("stack", sum(sapply(isAdHocStack, any)))
  #sixth[[x]][sapply(isAdHocStack, any)][!isAdHocStack[[x]]])
  objs[sapply(isAdHocStack, any)] <- sixthSO

  # cut short if all are dealt with
  if(all(!sapply(objs, is.null))) return(objs)

  #   isAdHocOther <- sapply(objs, function(x) length(x)==0)
  #   sixthSO <- rep("stack", sum(sapply(isAdHocStack, any)))
  #sixth[[x]][sapply(isAdHocStack, any)][!isAdHocStack[[x]]])

  warning("Please see documentation for Plot to try another way of calling Plot")

  return()
}


#####################
#' Plot: Fast, optimally arranged, multipanel plotting function with SpaDES
#'
#' The main plotting function accompanying \code{SpaDES}.
#' This can take objects of type \code{Raster*}, \code{SpatialPoints*},
#' \code{SpatialPolygons*}, and any combination of those.  It can
#' also handle \code{ggplot2} objects or base histogram objects via call to
#' \code{exHist <- hist(1:10, plot=F)}, but these non-spatial objects
#' cannot be mixed among types (i.e., can't mix and match spatial and
#' non-spatial objects, or base histogram and ggplot2 types). Customization of the
#' ggplot2 elements can be done as a normal ggplot2 plot, then added with
#' \code{Plot(ggplotObject)}
#'
#' If \code{new=TRUE}, a new plot will be generated.
#' When \code{new=FALSE}, any plot that already exists will be overplotted,
#' while plots that have not already been plotted will be added.
#' This function rearranges the plotting device to maximize the size of all the
#' plots, minimizing white space.
#' If using the RStudio IDE, it is recommended to make and use a new device
#' (using \code{\link{dev}}), because the built in device is not made for rapid redrawing.
#' The function is based on the grid package.
#'
#' Each panel in the multipanel plot must have a name.
#' This name is used to overplot, rearrange the plots, or overlay using
#' \code{addTo} when necessary.
#' If the \code{...} are named spadesPlotObjects, then \code{Plot} will use these names.
#' If not, then \code{Plot} will use the object name and the layer name (in the
#' case of \code{RasterLayer} or \code{RasterStack} objects).
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
#' Silently, one hidden object is made, \code{.spadesArr} in the \code{.spadesEnv}
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
#' @param ... \code{Raster*} object(s) and/or \code{SpatialPoints* objects}.
#' See details for naming.
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
#' to change plotting parameters (see \code{\link{grid}} package).
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
#' and upper bounds of a legend (i.e., 1:10 or c(1,10) will give same result)
#' that will override the data bounds contained within the \code{grobToPlot}.
#'
#' @param legendText Vector of values to use for legend value labels.
#' Defaults to \code{NULL}, which results in a pretty numeric representation.
#' If \code{Raster*} has a Raster Attribute Table (rat; see \code{\link{raster}}
#' package), this will be used by default. Currently, only a single vector is accepted.
#'
#' @param draw logical, whether to actually draw the plots.
#' Currently, there is no reason for this to be \code{FALSE}. Default is \code{TRUE}.
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
#' @rdname Plot-method
#' @docType methods
#' @export
#' @import grid
#' @importFrom methods is
#' @importFrom gridBase gridFIG
#' @import raster
#' @import RColorBrewer
#' @import rgdal
#' @import sp
#' @examples
#' \dontrun{
#' library(raster)
#' library(rgdal)
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
#'      .stackName="landscape",
#'      packages="SpaDES",
#'      stringsAsFactors=FALSE)
#'
#' # Load files to memory (using rasterToMemory) and stack them (because .stackName is provided above)
#' loadFiles(fileList=fileList)
#'
#' # extract a single one of these rasters
#' DEM <- landscape$DEM
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
#' Plot(landscape)
#'
#' # Can overplot, using addTo
#' Plot(caribou, addTo="landscape.forestAge", size=4, axes=FALSE)
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
#' Plot(SpP, addTo="landscape.forestCover", gp=gpar(lwd=2))
#'
#' }
setGeneric("Plot", signature="...",
           function(..., new=FALSE, addTo=NULL, gp=gpar(), axes="L", speedup = 1,
                    size=5, cols=NULL, zoomExtent=NULL,
                    visualSqueeze=0.75, legend=TRUE, legendRange=NULL, legendText=NULL,
                    draw = TRUE, pch = 19, title=TRUE,
                    na.color="#FFFFFF00", zero.color="#FFFFFF00") {
             standardGeneric("Plot")
           })


#' @rdname Plot-method
#' @export
setMethod("Plot",
          signature("spadesPlotObjects"),
          definition = function(..., new, addTo, gp, axes, speedup, size,
                                cols, zoomExtent, visualSqueeze,
                                legend, legendRange, legendText, draw, pch, title, na.color,
                                zero.color) {
            toPlot <- list(...)
            isSpatialObjects <- sapply(toPlot, function(x) is(x, "spatialObjects"))
            if((sum(isSpatialObjects)!=0) & (sum(isSpatialObjects)!=length(isSpatialObjects))) {
              stop("All objects for Plot call must be either spatialObjects or
                   none can be")
            }

            suppliedNames <- names(toPlot)

            # Section 1 # Determine object names that were passed and layer names of each
            names(toPlot) <- .objectNames()
            if(!is.null(suppliedNames)) names(toPlot)[!is.na(suppliedNames)] <- suppliedNames
            if(all(isSpatialObjects)) {

              isRaster <- sapply(toPlot, function(x) is(x, "Raster"))
              isStack <-  sapply(toPlot, function(x) is(x, "RasterStack"))
              isPolygon <- sapply(toPlot, function(x) is(x, "SpatialPolygons"))

              numLayers <- pmax(1, sapply(toPlot, nlayers))
              isRasterLong <- rep(isRaster, numLayers)
              isStackLong <- rep(isStack, numLayers)
              isSpatialObjects <- rep(isSpatialObjects, numLayers)
              objectNamesLong <- rep(names(toPlot), numLayers)

              # Full layer names, including object name. If layer name is same as object name
              #  omit it, and if layer name is "layer", omit it if within a RasterLayer
              lN <- rep(names(toPlot), numLayers)
              lN[isRasterLong] <- paste(objectNamesLong[isRasterLong],
                                        layerNames(toPlot[isRaster]), sep=".")
              useOnlyObjectName <- (layerNames(toPlot)=="layer") | (layerNames(toPlot)==objectNamesLong)
              if(any((!isStackLong) & isRasterLong & useOnlyObjectName)) {
                lN[(!isStackLong) & isRasterLong & useOnlyObjectName] <-
                  sapply(lapply(lN[(!isStackLong) & isRasterLong & useOnlyObjectName],
                                function(x) strsplit(x, "\\.")[[1]]), function(y)y[[1]]) }
              names(lN) <- rep(names(toPlot), numLayers)
            } else {
              lN <- names(toPlot)
            }


            #mapsToPlot <- lapply(toPlot, layerNames)
            #mapsToPlot[!isRaster] <- names(toPlot)[!isRaster]

            if(any(duplicated(lN))) stop(paste("Cannot plot two layers with same name slot. Check",
                                               "inside RasterStacks for objects"))

            # Section 2 - assess whether this is an "addTo" situation, i.e., plot one object over another
            if(is.null(addTo)) {
              addTo <- lN
            } else {
              if(length(addTo)!=length(lN)) stop("addTo must be same length as objects to plot")
              if(exists(paste0(".spadesArr", dev.cur()), envir=.spadesEnv)) {
                if(!any(addTo %in% get(paste0(".spadesArr", dev.cur()), envir=.spadesEnv)@names)) {
                  stop(paste("The addTo layer(s) --", addTo, "-- do(es) not exist", collapse=""))
                }
              }
              new <- FALSE
              legend <- FALSE
              title <- FALSE
              axes <- FALSE
            }


            # Section 3 # check whether .spadesArr exists, meaning that there is already a plot,
            #   If not, create a blank new one
            spadesArr <-.setOrGetSpadesArr1(new)
            currentNames <- spadesArr$currentNames
            arr <- spadesArr$arr
            new<-spadesArr$new

            # Section 4 # get extents from all SpatialPoints*, Rasters*, including Stacks
            extsToPlot <- .makeExtsToPlot(toPlot, zoomExtent, numLayers, lN)

            # Section 5 # Manage adding new plots to existing plots, assessing for overlaps
            # Do set operations to identify if there are "all new to plot", "only add in existing
            #   white space on device", "replot all existing smaller, rearranging, adding new"

            aR <- .assessRearrangement(extsToPlot, currentNames, addTo, new, lN, arr)
            extsToPlot <- aR$extsToPlot
            grobNames <- aR$grobNames
            addTo <- aR$addTo
            newArr <- aR$newArr

            # create full arr object, which will become .spadesArrx where x is dev.cur()
            #  i.e., the arrangement based on number and extents
            spadesArr <- .setOrGetSpadesArr2(newArr, extsToPlot, isSpatialObjects)
            arr <- spadesArr$arr
            newArr <- spadesArr$newArr
            #end create full arr object

            # Section 6 # Plotting scaling, pixels, axes, symbol sizes
            if(is.null(gp$cex)) {
              gp$cex <- cex <- max(0.6, min(1.2, sqrt(prod(arr@ds)/prod(arr@columns, arr@rows))*0.3))
            }
            if(axes==TRUE) { xaxis <- TRUE ; yaxis <- TRUE}
            if(axes==FALSE) { xaxis <- FALSE ; yaxis <- FALSE}

            # Section 7 # Optimal Layout and viewport making
            # Create optimal layout, given the objects to be plotted, whether legend and axes are to be
            #  plotted, and visualSqueeze
            arr@layout <- makeLayout(arr, visualSqueeze, legend, axes)
            # Create the viewports as per the optimal layout
            if(length(extsToPlot)>0) {
              vps <- makeViewports(extsToPlot, arr=arr, newArr=newArr)
              if(!new & !newArr)
                upViewport(1)
              pushViewport(vps, recording = FALSE)
              upViewport(2)
            }


            # Section 8 - the actual Plotting
            # Plot each element passed to Plot function, one at a time
            for(grobNamesi in grobNames) {

              whGrobNamesi <- match(grobNamesi, grobNames)

              whPlot <- match(addTo[whGrobNamesi], arr@names)
              if(axes=="L") {if(whPlot>(length(arr@names)-arr@columns)) { xaxis <- TRUE } else { xaxis <- FALSE}
                             if((whPlot-1)%%arr@columns==0) { yaxis <- TRUE } else { yaxis <- FALSE}}
              if(grobNamesi %in% currentNames) {
                if (!newArr) {
                  title <- FALSE
                  legend <- FALSE
                  xaxis <- FALSE
                  yaxis <- FALSE
                }
              }

              # Activate correct plot viewport (i.e., subplot)
              seek <- addTo[whGrobNamesi]
              a <- try(seekViewport(seek, recording=F))
              if(is(a, "try-error")) stop(paste("Plot does not already exist on current device.",
                                                "Try new=TRUE or change device to",
                                                "one that has a plot named", addTo[whGrobNamesi]))

              grobToPlot <- .identifyGrobToPlot(grobNamesi, toPlot, lN)
              newplot <- ifelse(!grobNamesi %in% lN, FALSE, TRUE)  # Is this a replot

              colour <- .colsFromList(cols=cols, lN, grobNamesi)

              if(is(grobToPlot, "Raster")) {
                # Rasters may be zoomed into and subsampled and have unique legend
                pR <- .prepareRaster(grobToPlot, zoomExtent, legendRange,
                                             newplot, arr, speedup)
                zMat <- makeColorMatrix(grobToPlot, pR$zoom, pR$maxpixels,
                                        pR$legendRange, na.color,
                                        zero.color=zero.color, cols=colour,
                                        skipSample=pR$skipSample)
              } else if (is(grobToPlot, "SpatialPoints")){ # it is a SpatialPoints object

                if(!is.null(zoomExtent)) {
                  grobToPlot <- crop(grobToPlot,zoomExtent)
                }

                len <- length(grobToPlot)
                if(len<(1e4/speedup)) {
                  z <- grobToPlot
                } else {
                  z <- sample(grobToPlot, 1e4/speedup)
                }
                zMat <- list(z=z, minz=0, maxz=0, cols=NULL, real=FALSE)
              } else if (is(grobToPlot, "SpatialPolygons")){ # it is a SpatialPolygons object
                if(!is.null(zoomExtent)) {
                  grobToPlot <- crop(grobToPlot,zoomExtent)
                  }
                z <- grobToPlot
                zMat <- list(z=z, minz=0, maxz=0, cols=NULL, real=FALSE)
              }

              if (is(grobToPlot, "gg")) {
                #grobToPlot <- grobToPlot + theme(plot.background=element_rect(fill="transparent",
                #                                                              colour = NA))
                print(grobToPlot, vp=seek)
                a <- try(seekViewport(seek, recording=F))
                if(is(a, "try-error")) stop(paste("Plot does not already exist on current device.",
                                                  "Try new=TRUE or change device to",
                                                  "one that has a plot named", addTo[whGrobNamesi]))
                if(title) grid.text(seek,
                                    name="title", y=1.08, vjust=0.5, gp = gp)

              } else if(is(grobToPlot, "histogram")) {
                # Because base plotting is not set up to overplot, must plot a white rectangle
                grid.rect(gp=gpar(fill="white", col="white"))
                par(fig=gridFIG())
                par(new=TRUE)
                plot(grobToPlot)
                if(title) grid.text(seek,
                                    name="title", y=1.08, vjust=0.5, gp = gp)

              } else {

                # Extract legend text if the raster is a factored raster
                if(is.factor(grobToPlot) & is.null(legendText)) {
                  legendTxt <- levels(grobToPlot)[[1]][,2]
                } else {
                  legendTxt <- legendText
                }

                # Actual plotting
                plotGrob(zMat$z, col = zMat$cols, size=unit(size, "points"),
                         real=zMat$real,
                         minv=zMat$minz,
                         maxv=zMat$maxz,
                         pch=pch, name = seek,
                         legend = legend, legendText=legendTxt,
                         gp = gp, draw = draw, speedup=speedup)
                if(title) grid.text(seek,
                                    name="title", y=1.08, vjust=0.5, gp = gp)

                if(xaxis) grid.xaxis(name="xaxis", gp = gp)
                if(yaxis) grid.yaxis(name="yaxis", gp = gp)
              }
            }

            assign(paste0(".spadesArr", dev.cur()), arr, envir=.spadesEnv)
          })




















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
#' @export
#' @docType methods
setGeneric("makeColorMatrix", function(grobToPlot, zoomExtent, maxpixels, legendRange,
                                       cols=NULL, na.color="#FFFFFF00",
                                       zero.color="#FFFFFF00", skipSample=TRUE) {
  standardGeneric("makeColorMatrix")
})


#' @rdname makeColorMatrix
#' @export
setMethod("makeColorMatrix",
          signature=c("Raster", "Extent", "numeric", "ANY"),
          definition= function(grobToPlot, zoomExtent, maxpixels, legendRange,
                               cols, na.color, zero.color, skipSample=TRUE) {
            zoom <- zoomExtent
            # It is 5x faster to access the min and max from the Raster than to calculate it,
            #  but it is also often wrong... it is only metadata on the raster, so it
            #  is possible that it is incorrect

            if(!skipSample) {
              colorTable <- getColors(grobToPlot)[[1]]
              if(!is(try(minValue(grobToPlot)),"try-error")) {
                minz <- minValue(grobToPlot)
                maxz <- maxValue(grobToPlot)
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
              minz <- min(z, na.rm=T)
            }
            #
            maxz <- max(z, na.rm=T)
            real <- any(na.omit(z) %% 1 != 0) # Test for real values or not

            # Deal with colors - This gets all combinations, real vs. integers,
            #  with zero, with no zero, with NA, with no NA, not enough numbers,
            #  too many numbers
            maxNumCols = 100

            nColors <- ifelse(real,maxNumCols+1, maxz-minz+1)
            if(is.null(cols)) {
              if(length(getColors(grobToPlot)[[1]])>0) {
                cols <- getColors(grobToPlot)[[1]]
                cols <- if(nColors>length(cols)) {colorRampPalette(cols)(nColors)
                } else if (nColors<length(cols)) {cols[minz:maxz]
                } else { cols }
              } else {
                cols <- rev(terrain.colors(nColors))
              }
            } else {
              cols <- if(nColors>length(cols)) {colorRampPalette(cols)(nColors)
              } else if (nColors<length(cols)) {cols[minz:maxz]
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


            if(any(!is.na(legendRange))){
              if((max(legendRange)-min(legendRange)+1)<length(cols)) {
                message(paste0("legendRange is not wide enough, using default"))
              } else {
                minz <- min(legendRange)
                maxz <- max(legendRange)
                cols <- colorRampPalette(cols)(maxz-minz+1)
              }
            }


            # here, the default color (transparent) for zero, if it is the minimum
            # value, can be overridden
            if(!is.null(zero.color)) {
              if(minz==0){
                cols[1] <- zero.color
              }
            }

            z <- z + 1 # for the NAs
            z[is.na(z)] <- 1

            cols<-c(na.color, cols) # make first index of colors be transparent
            if((minz>1) | (minz<0)) {
              z <- matrix(cols[z-minz+1], nrow=nrow(grobToPlot), ncol=ncol(grobToPlot), byrow=T)
            } else {
              z <- matrix(cols[z], nrow=nrow(grobToPlot), ncol=ncol(grobToPlot), byrow=T)
            }
            list(z=z, minz=minz, maxz=maxz, cols=cols, real=real)
          })

#' Convert grid.locator units
#'
#' Converts them to meaningful units. Used within \code{.clickCoord}
#'
#' @param grid.locator an object that was output by a call to grid.locator and mouse click(s)
#' @export
unittrim <- function(grid.locator) {
  as.numeric(sub("^([0-9]+|[0-9]+[.][0-9])[0-9]*", "\\1", as.character(grid.locator)))
}


##############################################################
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
#' @rdname spadesMouseClicks
#'
clickValues <- function(n=1) {

  coords <- clickCoordinates(n=n)

  objLay <- strsplit(coords[, 1], "\\.")
  objNames <- sapply(objLay, function(x) x[1])
  layNames <- sapply(objLay, function(x) x[2])
  for (i in 1:n) {
    if(!is.na(layNames[i])) {
      coords$value[i] <- get(objNames[i], envir=.GlobalEnv)[[layNames[i]]][cellFromXY(get(objNames[i], envir=.GlobalEnv)[[layNames[i]]], coords[i, 2:3])]
    } else {
      coords$value[i] <- get(objNames[i], envir=.GlobalEnv)[cellFromXY(get(objNames[i], envir=.GlobalEnv), coords[i, 2:3])]
    }
  }
  return(coords)
}

#' @param devNum The device number for the new plot to be plotted on
#'
#' @param plot.it logical. If \code{TRUE} a new windows is made for the new extent. Default \code{TRUE}.
#'
#' @export
#' @docType methods
#' @rdname spadesMouseClicks
clickExtent <- function(devNum=NULL, plot.it=TRUE) {
  corners <- clickCoordinates(2)
  zoom <- extent(c(sort(corners$x), sort(corners$y)))


  if(plot.it) {
    devActive <- dev.cur()
    if(is.null(devNum)) {
      newPlot()
    } else {
      dev(devNum)
    }
    objLay <- strsplit(corners[, 1], "\\.")
    objNames <- unique(sapply(objLay, function(x) x[1]))
    layNames <- unique(sapply(objLay, function(x) x[2]))
    if(!is.na(layNames)) {
      Plot(get(objNames, envir=.GlobalEnv)[[layNames]], zoomExtent=zoom, new=TRUE)
    } else {
      Plot(get(objNames, envir=.GlobalEnv), zoomExtent=zoom, new=TRUE)
    }

    dev(devActive)
    return(invisible(zoom))
  } else {
    return(zoom)
  }
}

#' @export
#' @docType methods
#' @rdname spadesMouseClicks
clickCoordinates <- function(n=1) {

  dc <- dev.cur()
  arr <- try(get(paste0(".spadesArr", dc), envir=.spadesEnv))
  if(is(arr, "try-error")) stop(paste("Plot does not already exist on current device.",
                                      "Try new=TRUE or change device to",
                                      "one that has objects from a call to Plot()"))
  gl <- grid.layout(nrow=arr@rows*3+2,
                    ncol=arr@columns*3+2,
                    widths=arr@layout$wdth,
                    heights=arr@layout$ht)

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
    map <- column + (row-1)*arr@columns

    maxLayX <- cumsum(widthNpcs)[xInt]
    minLayX <- cumsum(widthNpcs)[xInt-1]
    grobLoc$x <- unit((as.numeric(strsplit(as.character(gloc$x), "npc")[[1]])-minLayX)/(maxLayX-minLayX), "npc")

    maxLayY <- cumsum(heightNpcs)[yInt]
    minLayY <- cumsum(heightNpcs)[yInt-1]
    grobLoc$y <- unit((as.numeric(strsplit(as.character(gloc$y), "npc")[[1]])-minLayY)/(maxLayY-minLayY), "npc")

    clickCoords[i, ] <- .clickCoord(arr@names[map], n=1, gl=grobLoc)
    mapNames[i] <- arr@names[map]
  }
  return(data.frame(map=mapNames, clickCoords, stringsAsFactors = FALSE))
}


#' @param X The raster object whose values will be returned where mouse clicks occur.
#'
#' @param gl An object created by a call to \code{grid.locator}.
#'
#' @export
#' @docType methods
#' @rdname spadesMouseClicks
.clickCoord <- function(X, n=1, gl=NULL) {

  pts<-data.frame(x=NA_real_, y=NA_real_, stringsAsFactors = FALSE)
  seekViewport(X)
  for(i in 1:n) {
    if(is.null(gl)) {
      gl <- grid.locator()
      pts[i, ] <- unittrim(gl)
    } else {
      pts[i, ] <- c(convertX(gl$x, "native"), convertY(gl$y, "native"))
    }
  }
  return(pts)
}



.identifyGrobToPlot <- function(grobNamesi, toPlot, lN) {
  # get the object name associated with this grob
  objLayerName <- strsplit(grobNamesi, "\\.")[[1]]

  # Does it already exist on the plot device or not
  if(!grobNamesi %in% lN) { # Is this a replot
    if(length(objLayerName)==2 ) {# means it is in a raster
      grobToPlot <- get(objLayerName[1], envir=.GlobalEnv)[[objLayerName[2]]]
    } else {
      grobToPlot <- get(grobNamesi, envir=.GlobalEnv)
    }
  } else { # Is this a new plot to be added or plotted
    if(length(objLayerName)==2) {
      grobToPlot <- toPlot[[objLayerName[1]]][[objLayerName[2]]]
    } else {
      grobToPlot <- toPlot[[objLayerName[1]]]
    }
  }
}

.setOrGetSpadesArr1 <- function(new) {
  # Section 3 # check whether .spadesArr exists, meaning that there is already a plot

  if(!exists(paste0(".spadesArr", dev.cur()), envir=.spadesEnv)) {
    new<-TRUE
    arr <- new("arrangement"); arr@columns=0; arr@rows = 0
    if(new==FALSE) message("Nothing to add plots to; creating new plots")
    currentNames <- NULL
  } else {
    if(!new) {
      arr <- get(paste0(".spadesArr", dev.cur()), envir=.spadesEnv)
    } else {
      arr <- new("arrangement"); arr@columns=0; arr@rows = 0
      try(remove(list=paste0(".spadesArr", dev.cur()), envir=.spadesEnv))
    }
    currentNames <- arr@names
  }
  return(list(arr=arr, new=new, currentNames=currentNames))
}



##############################################################
#' Get dimensions of x and y axes
#'
#' For spatial objects, these are the extents. Used internally in Plot methods
#'
#' @param toPlot list of objects to plot
#'
#' @param zoomExtent an extent object
#'
#' @param numLayers numeric indicating number of layers in the list \code{toPlot}
#'
#' @param lN character vector of names of these layers
#'
#' @name makeExtsToPlot
#' @rdname internal-Plot
setGeneric(".makeExtsToPlot", function(toPlot=NULL, zoomExtent=NULL, numLayers=NULL, lN) {
  standardGeneric(".makeExtsToPlot")
})

#' @rdname internal-Plot
setMethod(".makeExtsToPlot",
          signature="list",
          definition <- function(toPlot, zoomExtent, numLayers, lN) {

            if(any(sapply(toPlot, function(x) any(is(x, "gg") | is(x, "histogram"))))) {

              extsToPlot <- lapply(1:length(toPlot), function(x) {
               # if(!is.null(toPlot[[x]]$coordinates$ratio)) {
                  extent(0,1,0,1)
               # }
              })
            } else {
              if(is.null(zoomExtent)) {
                extsToPlot <- rep(sapply(toPlot, extent), numLayers)
              } else {
                extsToPlot <- rep(list(zoomExtent), numLayers)
              }
            }
            names(extsToPlot)<-lN
            return(extsToPlot)
          }
)



.setOrGetSpadesArr2 <- function(newArr, extsToPlot, isSpatialObjects) {

  if(!newArr) {
    if(exists(paste0(".spadesArr", dev.cur()), envir=.spadesEnv)) {
      arr <- get(paste0(".spadesArr", dev.cur()), envir=.spadesEnv)
      arr@names <- append(arr@names, names(extsToPlot))
      arr@extents <- append(arr@extents, extsToPlot)
      arr@isSpatialObjects <- append(arr@isSpatialObjects, isSpatialObjects)
    } else {
      message("nothing to add to, creating new plot")
      newArr <- TRUE
    }
  }
  if (newArr) { # need a new arrangement
    if(exists(paste0(".spadesArr", dev.cur()), envir=.spadesEnv)) {
      isSpatialObjects <- append(get(paste0(".spadesArr", dev.cur()),
                                     envir=.spadesEnv)@isSpatialObjects,
                                 isSpatialObjects)
    }
    arr <- arrangeViewports(extsToPlot)
    arr@isSpatialObjects <- isSpatialObjects
    grid.newpage()
  }
  return(list(arr=arr, newArr=newArr))
}

.assessRearrangement <- function(extsToPlot, currentNames, addTo, new, lN, arr) {
  currentPlusToPlotN <- unique(c(currentNames, addTo))
  if(new==TRUE) { # "all new to plot"
    newArr <- TRUE
    grobNames <- lN
  } else { # new == FALSE, i.e., add in a modular way by keeping previous plots
    if(length(currentPlusToPlotN) > prod(arr@columns, arr@rows)) { #"replot all existing smaller, rearranging, adding new"
      newArr <- TRUE
      ind <- currentPlusToPlotN %in% lN + 1
      grobNames <- currentPlusToPlotN
      addTo <- grobNames
      extsUnmerged <- list(extCurrent=arr@extents[match(currentPlusToPlotN, currentNames)],
                           extlN=extsToPlot[match(currentPlusToPlotN, lN)])
      extsToPlot <- sapply(1:length(currentPlusToPlotN), function(x) extsUnmerged[[ind[x]]][x])
    } else {  # "only add in existing white space on device"
      newArr <- FALSE
      extsToPlot <- extsToPlot[!(addTo %in% currentNames)]
      if(sum(!(addTo %in% currentNames))!=0) {
        names(extsToPlot) <- addTo[!(addTo %in% currentNames)]
      } else {
        names(extsToPlot) <- NULL
      }
      grobNames <- lN
    }
  }
  return(list(grobNames=grobNames, newArr=newArr, extsToPlot=extsToPlot, addTo=addTo))
}

.colsFromList <- function(cols, lN, grobNamesi) {
  if(is.list(cols)) {
    if(length(cols)==length(lN)) {
      cols[[match(grobNamesi, lN)]] # use colours as supplied in the list
    } else {
      cols[[(match(grobNamesi, lN)-1) %% length(cols)+1]] #recycle colours
    }
  } else {
    cols
  }
  return(cols)
}

.prepareRaster <- function(grobToPlot, zoomExtent, legendRange,
              newplot, arr, speedup) {
  if(is.null(zoomExtent)) {
    zoom <- extent(grobToPlot)
    npixels <- ncell(grobToPlot)
  } else {
    zoom <- zoomExtent
    npixels <- ncell(crop(grobToPlot,zoom))
  }
  if(is.null(legendRange) | newplot==FALSE) legendRange <- NA


  maxpixels <- min(5e5,3e4/(arr@columns*arr@rows)*prod(arr@ds))/speedup
  #if(!is.null(npixels)) {
  maxpixels <- min(maxpixels, npixels)
  skipSample <- if(is.null(zoomExtent)) {maxpixels>=npixels} else {FALSE}
  #} else {
  #  skipSample <- TRUE
  #}

  return(list(maxpixels=maxpixels, skipSample=skipSample,
              legendRange=legendRange, zoom=zoom))

}
