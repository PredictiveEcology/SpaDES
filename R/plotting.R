if(getRversion() >= "3.1.0")  utils::globalVariables(paste0(".spadesArr",1:20))

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
#' @param ... Additional arguments passed to \code{\link{quartz}},
#'            \code{\link{windows}}, or \code{\link{x11}}.
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
#' Launch a new graphics device based on operating system used.
#' Mac OS: open device with \code{quartz()}.
#' Linux: open device with \code{x11()}.
#' Windows: open device with \code{windows()}.
#'
#' @param ... Additional arguments.
#'
#' @note \code{\link{dev.new}} is supposed to be the correct way to open a new
#' window in a platform-generic way, however, this doesn't work in RStudio.
#'
#' @seealso \code{\link{quartz}}, \code{\link{windows}}, \code{\link{x11}}.
#'
#' @export
#' @docType methods
#' @rdname newPlot-method
#'
# @examples
# needs examples
newPlot <- function(...) {
  if (Sys.info()[["sysname"]]=="Darwin") {
    quartz(...)
  } else if (Sys.info()[["sysname"]]=="Linux") {
    x11(...)
  } else if (Sys.info()[["sysname"]]=="Windows") {
    windows(...)
  } else {
    dev.new(...) # try dev.new() to see if it works
    warning("Which operating system are you using?")
  }
}

#' @export
setMethod("nlayers",
          signature="list",
          function(x) {
            y = sum(sapply(x, function(x) {
              if(is(x,"RasterStack")) {
                x=nlayers(x)
              } else {
                x = 1
              }
              return(x)
              }))
          return(y)
})

#' extract the layer names in a mixed set of layer objects
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
          signature="SpatialPointsNamed",
          definition=function(object) {
            name(object)
})

#' @export
#' @rdname layerNames
setMethod("layerNames",
          signature="SpatialPointsDataFrameNamed",
          definition=function(object) {
            name(object)
})

#' @export
#' @rdname layerNames
setMethod("layerNames",
          signature="Raster",
          definition=function(object) {
            names(object)
})

#' Assess whether a list of extents are all equal
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
            all(c(sapply(extents,function(x) x@xmin)==extents[[1]]@xmin,
            sapply(extents,function(x) x@xmax)==extents[[1]]@xmax,
            sapply(extents,function(x) x@ymin)==extents[[1]]@ymin,
            sapply(extents,function(x) x@ymax)==extents[[1]]@ymax))
})

#' determine which of the layers are provided within a stack
#' @name inRasterStack
#' @rdname inRasterStack
#' @export
setGeneric("inRasterStack", function(object) {
  standardGeneric("inRasterStack")
})

#' @name inRasterStack
#' @rdname inRasterStack
#' @export
setMethod("inRasterStack",
          signature="list",
          definition=function(object) {
            unlist(sapply(object, function(x) {
              if(is(x,"RasterStack")) {
                rep(TRUE,nlayers(x))
              } else {
                FALSE
              }}))
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
#' @slot stack  list with 2 elements: a character vector of stack names and
#' a character vector of the layer names in each of those
#'
#' @slot names  character vector. The names of the layers in the plot
#'
#' @slot extents list of class Extent objects. These are needed to calculate the
#' \code{ds.dimensionRatio}, which is used to scale the Raster* objects correctly
#'
#' @rdname arrangement-class
#' @exportClass arrangement
#'
#' @author Eliot McIntire
#'
setClass("arrangement",
         slots=list(rows="numeric", columns="numeric",
                    actual.ratio="numeric", ds.dimensionRatio="numeric",
                    ds="numeric", stack="list", names="character",
                    extents="list"),
         prototype=list(rows=1, columns=1,
                        actual.ratio=1, ds.dimensionRatio=1,
                        ds=c(7,7), stack=as.list(NULL), names=as.character(NULL),
                        extents=as.list(NULL)),
         validity=function(object) {
           # check for valid sim times and make default list
           if (any(is.na(object@extents))) {
             stop("must supply a list of extents")
           }
})

#' Determine optimal plotting arrangement of RasterStack
#'
#' Hidden function.
#'
#' This assesses the device geometry, the map geometry, and the number of rasters
#' to plot and builds an object that will be used by the Plot functions to plot
#' them efficiently
#'
#' @param toPlot Raster* object
#' @param axes passed from Plot
#' @rdname arrangeViewports
#' @export
#' @docType methods
setGeneric("arrangeViewports", function(extents, name=NULL) {
  standardGeneric("arrangeViewports")
})

#' @rdname arrangeViewports
#' @export
setMethod("arrangeViewports",
          signature=c("list"),
          definition= function(extents, name) {
    #rasters <- sapply(toPlot,function(x) is(x,"Raster"))
    #ext <- extent(toPlot[rasters][[1]])
    dimx <- apply(sapply(extents,function(y) apply(bbox(y),1,function(x) diff(range(x)))),1,max)
    if(is.null(name)) {
      nPlots <- length(extents)
      #vpnames <- paste("vp",names(extents),sep="")
      names <- names(extents)
    } else {
      nPlots <- length(name)
      #vpnames <- paste("vp",name,sep="")
      names <- name
    }

    if(dev.cur()==1) {
        dev.new(height=8, width=10)
    }

    ds <- dev.size()
    ds.ratio <- ds[1]/ds[2]

    #dimensionRatio <- dimx[2]/dimx[1]
    dimensionRatio <- dimx[1]/dimx[2]

    ds.dimensionRatio <- ds.ratio/dimensionRatio

    col.by.row <- data.frame(matrix(ncol=2, nrow=nPlots))

    col.by.row[,1] <- ceiling(nPlots/(1:nPlots))
    col.by.row[,2] <- ceiling(nPlots/col.by.row[,1])

    wh.best <- which.min(abs(apply(col.by.row,1,function(x) x[1]/x[2]) - ds.dimensionRatio))

    columns <- col.by.row[wh.best,1]
    rows <- col.by.row[wh.best,2]

    actual.ratio <- columns/rows

    out <- new("arrangement", rows=rows,columns=columns,
               actual.ratio=actual.ratio,ds.dimensionRatio=ds.dimensionRatio,
               ds=ds,#prettys=prettys,
               names=names, extents = extents)
#     out <- list(rows=rows,columns=columns,
#                 actual.ratio=actual.ratio,ds.dimensionRatio=ds.dimensionRatio,
#                 ds=ds,#prettys=prettys,
#                 names=names,ds.ratio=ds.ratio, extents = extents)
    return(out)
})

######################################################
#' Plot either a raster Grob or a points Grob
#'
#' @param grobToPlot Raster* or SpatialPoints* object
#' @name plotGrob
#' @rdname plotGrob
#' @export
#' @docType methods
setGeneric("plotGrob", function(grobToPlot, col=NULL, size=unit(5,"points"),
                                name="plot", minv, maxv,
                                legend=TRUE, addedNACol = addedNACol,
                                draw=TRUE,
                                gp=gpar(), vp=NULL, pch=19,
                                childrenvp=NULL, ...) {
  standardGeneric("plotGrob")
})


#' @rdname plotGrob
#' @export
setMethod("plotGrob",
          signature=c("matrix"),
          definition= function(grobToPlot, col, size, name, minv, maxv,
                               legend, addedNACol, draw,
                               gp, vp, pch,
                               childrenvp, ...) {

            pr <- pretty(range(minv,maxv))
            pr <- pr[pr<=maxv]
            pr <- pr[pr>=minv]

            if(maxv<=1) {
              if(maxv>0) {
                maxcol = maxv*47
              } else {
                maxcol=1
              }
            } else {
              maxcol = length(col) # need one for the NA at the bottom
            }
            rastGrob <- gTree(grobToPlot=grobToPlot, #title=title,
                              name=name,
                              pr=pr,col=col,
                              children=gList(
                                rasterGrob(as.raster(grobToPlot),#, col = col), #maxpixels=maxpixels,
                                           interpolate=FALSE,
                                           name="raster"),
                                if(legend) rasterGrob(as.raster(col[(maxcol):2]),
                                                      x=1.04,y=0.5,height=0.5,width=0.03,
                                                      interpolate=FALSE,
                                                      name="legend"),
                                if(legend) { # if top or bottom entry of legend is white, make a box around it to see it
                                  if(col[1]=="^#FFFFFF" | col[maxcol]=="^#FFFFFF")
                                    rectGrob(x=1.04,y=0.5,height=0.5,width=0.03)
                                },
                                if(legend) textGrob(pr, x=1.08,
                                                    if(maxv>1) {
                                                      y= ((pr-minv)/((maxv+1)-minv))/2+0.25+1/diff(range(minv,maxv))/4
                                                    } else {
                                                      y= ((pr-minv)/((maxv)-minv))/2+0.25
                                                    },
                                                    gp=gpar(cex=0.9),
                                                    just="left", check.overlap=T,
                                                    name="legendText")
                                #if(title) textGrob(names(grobToPlot), name="title", y=1.08, vjust=0.5)
                              ),
                              gp=gp,
                              #vp=vp,
                              cl="plotRast")
            if(draw) grid.draw(rastGrob)
            return(invisible(rastGrob))
          })

#' @rdname plotGrob
#' @export
setMethod("plotGrob",
          signature=c("SpatialPoints"),
          definition= function(grobToPlot, col, size,
                               legend, draw, #xaxis, yaxis, title,
                               gp=gpar(), vp=NULL, pch, #maxpixels,
                               childrenvp=NULL, ...) {
            pntGrob <- gTree(grobToPlot=grobToPlot, #title=title,
                             name=layerNames(grobToPlot),
                             childrenvp=childrenvp,
                             children=gList(
                               pointsGrob(x=grobToPlot$x, y=grobToPlot$y, pch=pch, size=size)
#                                if(xaxis) xaxisGrob(name="xaxis"),
#                                if(yaxis) yaxisGrob(name="yaxis"),
#                                if(title) textGrob(name(grobToPlot), name="title", y=1.08, vjust=0.5)
                             ),
                             gp=gp,
                             vp=vp,
                             cl="plotPoint")
            if(draw) grid.draw(pntGrob)
            return(invisible(pntGrob))
})



#' @export
makeLayout <- function(arr, visualSqueeze, legend=TRUE, axes=TRUE, title=TRUE) {
  columns <- arr@columns
  rows <- arr@rows

  # Reduce by 40% of remaining space if each of the following is not wanted
  if(legend==FALSE ) visualSqueeze <- visualSqueeze + 0.4*(1-visualSqueeze)
  if(axes==FALSE) visualSqueeze <- visualSqueeze + 0.4*(1-visualSqueeze)
  if(title==FALSE) visualSqueeze <- visualSqueeze + 0.4*(1-visualSqueeze)

  # calculate the visualSqueeze for the width (i.e., vS.w)
  vS.w = min(visualSqueeze/columns,
             visualSqueeze/columns*arr@actual.ratio/arr@ds.dimensionRatio)

  wdth <- unit.c(unit(1.5,"null"), unit(rep(c(vS.w,1.75),columns),
                                        rep(c("npc","null"),columns))[-columns*2],
                 unit(1.5,"null"))

  # calculate the visualSqueeze for the height (i.e., vS.h)
  vS.h = min(visualSqueeze/rows,
              visualSqueeze/rows*arr@ds.dimensionRatio/arr@actual.ratio)
  ht <- unit.c(unit(1,"null"), unit(rep(c(vS.h,1.75),rows),
                                    rep(c("npc","null"),rows))[-rows*2],
               unit(1,"null"))

  return(list(wdth=wdth,ht=ht))
}


#' @export
makeViewports <- function(extents, layout, arr, visualSqueeze, newArr = FALSE) {

  columns = arr@columns
  rows = arr@rows

  topVp <- viewport(layout=grid.layout(nrow=rows*2+1,
                                       ncol=columns*2+1,
                                       widths=layout$wdth,
                                       heights=layout$ht),
                    name="top")
  plotVps <- list()
  for(extentInd in 1:length(extents)) {
    nam = names(extents)[extentInd]
    posInd = match(nam, arr@names)

    plotVps[[extentInd]] <- viewport(
              name=nam,
              layout.pos.col = ceiling((posInd-1)%%columns+1)*2,
              layout.pos.row = ceiling(posInd/columns)*2,
              xscale=c(extents[[extentInd]]@xmin,extents[[extentInd]]@xmax),
              yscale=c(extents[[extentInd]]@ymin,extents[[extentInd]]@ymax))

  }

  if(newArr) {
    wholeVp <- vpTree(topVp, do.call(vpList, plotVps))
  } else {
    wholeVp <- do.call(vpList, plotVps)
  }
  return(wholeVp)
}


##############################################################
#' Plots arrows showing direction of agent movement.
#'
#' @param from          Starting spatial coordinates (\code{SpatialPointsDataFrame}).
#'
#' @param to            Ending spatial coordinates (\code{SpatialPointsDataFrame})..
#'
#' @param addTo Optional character string. The name of a map layer on which to draw the arrows.
#'
#' @param ...           Additional plotting parameters passed to grid.polyline. Currently
#' does not appear to pass anything.
#'
#' @return Plots the vectors representing agent movement on the specified map.
#'
#' @import sp
#' @export
#' @docType methods
#' @rdname drawArrows-method
#' @examples
#' # Make 2 objects
#' caribou1 <- SpatialPointsNamed(cbind(x=runif(10,-50,50),y=runif(10,-50,50)),name="caribou1")
#' caribou2 <- SpatialPointsNamed(cbind(x=runif(10,-50,50),y=runif(10,-50,50)),name="caribou2")
#'
#' drawArrows(caribou1, caribou2)
#' seekViewport("caribou1")
#' grid.text("caribou1",0.5,1.05)
#' grid.xaxis()
#' grid.yaxis()
#'
#' # or add to a previous Plot
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
setGeneric("drawArrows", function(from, to, addTo, ...) {
  standardGeneric("drawArrows")
})

#' @param length    The length of the arrows to draw (defaults to 0.1).
#'
#' @rdname drawArrows-method
#'
setMethod("drawArrows",
          signature=c("SpatialPoints","SpatialPoints","character"),
          definition=function(from, to, addTo, ..., length=0.1) {
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
               signature=c("SpatialPoints","SpatialPoints","missing"),
               definition=function(from, to, addTo, ..., length=0.1) {
                 grid.newpage()
                 extents <- list(extent(
                   extendrange(c(min(min(from$x),min(to$x)),max(max(from$x,to$x)))),
                   extendrange(c(min(min(from$y),min(to$y)),max(max(from$y,to$y))))))
                 names(extents) <- name(from)
                 arr <- arrangeViewports(extents,name=name(from))
                 lay <- makeLayout(arr=arr, visualSqueeze=0.75)
                 vps <- makeViewports(extents, arr=arr, layout=lay,
                                      visualSqueeze=0.75, newArr = TRUE)
                 pushViewport(vps)
                 seekViewport(name(from), recording=FALSE)
                 grid.polyline(x=c(from$x, to$x), y=c(from$y, to$y),
                               default.units="native",
                               id=rep(1:length(from), 2),
                               arrow=arrow(length=unit(length, "inches"), ...))
                 upViewport(0)
     })


#####################
#' Fast, optimally arranged, multipanel plotting function with spades
#'
#' The main plotting function accompanying spades. This can take objects of type Raster* or SpatialPoints*Named,
#' and any combination of those.
#'
#' If add=FALSE, then a new plot will be generated. When add=TRUE, then any plot that
#' already exists will be overplotted, while plots that have not already been plotted will be added. This function
#' rearrange the plotting device to maximize the size of all the plots, minimizing white space. If using RStudio,
#' it is recommended to makeand use a new device because the built in device is not made for rapid redrawing.
#' The function is based on the grid package.
#'
#' Silently, one hidden object is made, \code{.spadesArr}, which is used for arranging plots in the
#' device window, and identifying the objects to be replotted if rearranging is required,
#' subsequent to an add=T additional plot.
#'
#' \code{speedup} is not a precise number because it is faster to plot an un-resampled
#' raster if the new resampling is close to the original number of pixels. At the moment,
#' this is set to 1/3 of the original pixels. In other words, \code{speedup} may not do anything
#' if the factor for speeding up is not high enough (i.e., >3).
#'
#' \code{col} can be used to set the colors of Raster* objects, but it is preferable to use
#' setColors to give each layer its own color table. See examples.
#'
#' If plotting a single layer from a stack, any rearrangement plotting will fail because
#' Plot does not know where to look for the original file to replot. The layer is contained within
#' a stack, but the raster package unstacks the single layer when a single layer is called.
#' If this layer remained within a stacked object, then Plot would know where to look for it
#' for subsequent plotting. Recommendation: If single layers are to be plotted, extract them as
#' RasterLayerNamed objects to Plot them.
#'
#'
#' @param ... Raster* object(s) and or SpatialPoints*Named objects
#'
#' @param add Logical. If FALSE, then the previous plot is wiped and a new one made; if TRUE, then the ... plots
#' will be added to the current device, adding or rearranging the plot layout as necessary. Default is TRUE.
#'
#' @param addTo String vector, with same length as ...  This is for overplotting, when the overplot is not to occur on
#' the plot with the same name, such as plotting a SpatialPoints*Named object on a RasterLayer.
#'
#' @param gp A gpar object, created by gpar() function, to change plotting parameters (see grid package)
#'
#' @param axes Logical or "L", representing the left and bottom axes, overall plots
#'
#' @param speedup Numeric. The factor by which the number of pixels is divided by to plot rasters. See Details.
#'
#' @param size Numeric. The size, in points, for SpatialPoints symbols, if using a scalable symbol.
#'
#' @param col character vector. Hex codes for colors. See Details.
#'
#' @param zoomExtent extent object. Supplying a single extent object that is smaller than the
#' rasters will call a crop statement before plotting. Defaults to NULL. This occurs after any
#' downsampling of rasters, so it may produce very pixelated maps.
#'
#' @param visualSqueeze numeric. The proportion of the white space to be used for plots. Default is 0.75.
#'
#' @param legend logical. Whether legend should be drawn next to plots. Default is TRUE.
#'
#' @param draw logical, whether to actually draw the plots. Currently, there is no reason for this
#' to be FALSE. Default is TRUE
#'
#' @param pch see ?par
#'
#' @param title Logical. Whether the names of each plot should be written above plots
#'
#' @rdname Plot
#' @docType methods
#' @export
#' @import RColorBrewer
#' @import rgdal
#' @import raster
#' @import sp
#' @include named-objects.R
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
#'                            forestCover = RColorBrewer::brewer.pal(9,"Set1"),
#'                            forestAge = RColorBrewer::brewer.pal("Blues",n=8),
#'                            habitatQuality = RColorBrewer::brewer.pal(9,"Spectral"),
#'                            percentPine = RColorBrewer::brewer.pal("GnBu",n=8))
#'
#' #Make a new raster derived from a previous one; must give it a unique name
#' habitatQuality2 <- landscape$habitatQuality ^ 0.3
#' names(habitatQuality2) <- "habitatQuality2"
#'
#' # make a SpatialPointsNamed object
#' caribou <- SpatialPointsNamed(coords=cbind(x=runif(1e2,-50,50),y=runif(1e2,-50,50)),
#'                               name="caribou")
#'
#' #Plot all maps on a new plot windows - Do not use RStudio window
#' if(is.null(dev.list())) {
#'   dev(2)
#' } else {
#'   if(any(names(dev.list())=="RStudioGD")) {
#'     dev(which(names(dev.list())=="RStudioGD")+3)
#'   } else {
#'     dev(max(dev.list()))
#'   }
#' }
#'
#' Plot(landscape)
#'
#' # Can overplot, using addTo
#' Plot(caribou, addTo="forestAge", size=4, axes=FALSE)
#'
#' # can add a new plot to the plotting window
#' Plot(caribou, add=T)
#'
#' # can't add a two maps with same name
#' Plot(landscape, caribou, DEM)
#'
#' # can mix stacks, rasters, SpatialPoint*Named
#' Plot(landscape, habitatQuality2, caribou)
#'
#' # can mix stacks, rasters, SpatialPoint*Named
#' Plot(landscape, caribou)
#' Plot(habitatQuality2, add=T)
#' }
setGeneric("Plot", signature="...",
           function(..., add=TRUE, addTo=NULL, gp=gpar(), axes="L", speedup = 1,
                    size=5, cols, zoomExtent=NULL,
                    visualSqueeze=0.75, legend=TRUE, legendRange=NULL, draw = TRUE,
                    pch = 19, title=TRUE) {
             standardGeneric("Plot")
           })


#' @export
setMethod("Plot",
          signature("spatialObjects"),
          definition = function(..., add, addTo, gp, axes, speedup, size,
                                cols, zoomExtent, visualSqueeze,
                                legend, legendRange, draw, pch, title) {
            toPlot <- list(...)


            whStacks <- sapply(toPlot, function(x) is(x, "RasterStack"))
            if(any(whStacks)) {
              stacksToPlot <- lapply(toPlot[whStacks], layerNames)
              names(stacksToPlot) <- sapply(toPlot[whStacks], name)
            } else {
              stacksToPlot <- as.list(NULL)
            }

            if(add | !is.null(addTo)){
              if(exists(paste0(".spadesArr",dev.cur()), envir=.GlobalEnv)) {
                stacksInArr <- get(paste0(".spadesArr",dev.cur()))@stack
              } else {
                stacksInArr <- list(NULL)
              }
              #              if(!is.null(addTo)) add <- TRUE
            }

            lN <- layerNames(toPlot)
            if(any(duplicated(lN))) stop(paste("Cannot plot two layers with same name. Check",
                                               "inside RasterStacks"))
            if(is.null(addTo)) {
              addTo <- lN
            } else {
              if(length(addTo)!=length(lN)) stop("addTo must be same length as objects to plot")
              if(exists(paste0(".spadesArr",dev.cur()), envir=.GlobalEnv)) {
                if(!any(addTo %in% get(paste0(".spadesArr",dev.cur()))@names)) {
                  stop(paste("The addTo layer(s) --",addTo,"-- do(es) not exist",collapse=""))
                }
              }
              add <- TRUE
              legend <- FALSE
              title <- FALSE
              axes <- FALSE
            }


            # check whether .spadesArr exists, meaning that there is already a plot
            if(!exists(paste0(".spadesArr",dev.cur()),envir=.GlobalEnv)) {
              add=F
              arr = new("arrangement"); arr@columns=0; arr@rows = 0
              if(add==T) message("Nothing to add plots to; creating new plots")
              currentNames = NULL
            } else {

              if(add) {
                arr <- get(paste0(".spadesArr",dev.cur()))
              } else {
                arr = new("arrangement"); arr@columns=0; arr@rows = 0
              }
              currentNames <- arr@names
            }

            currentPlusToPlotN <- unique(c(currentNames, addTo))

            # if add == F, then new plots are only the ones in the function call, otherwise
            #  it needs to assess what is already there

            # get extents from all SpatialPoints*, Rasters*, including Stacks
            if(is.null(zoomExtent)) {
              extsToPlot <- rep(sapply(toPlot, extent),
                                sapply(toPlot, function(x) length(layerNames(x))))
            } else {
              extsToPlot <- rep(list(zoomExtent),
                                sapply(toPlot, function(x) length(layerNames(x))))
            }

            names(extsToPlot)<-lN

            if(add==F) {
              newArr = T
              vpNames <- addTo
              grobNames <- lN
              #if(exists(".grobs",envir=.GlobalEnv)) rm(.grobs, envir=.GlobalEnv)
              #grobs <- list()
            } else { # add == T
              if(length(currentPlusToPlotN) > prod(arr@columns, arr@rows)) {
                newArr = T
                vpNames = currentPlusToPlotN
                grobNames = vpNames
                addTo <- grobNames
                ind <- vpNames %in% lN + 1
                extsUnmerged <- list(extCurrent=arr@extents[match(vpNames,currentNames)],
                                     extlN=extsToPlot[match(vpNames, lN)])
                extsToPlot <- sapply(1:length(vpNames), function(x) extsUnmerged[[ind[x]]][x])
              } else {
                newArr = F
                if(sum(!(addTo %in% currentNames))!=0) {
                  vpNames <- addTo[!(addTo %in% currentNames)]
                } else {
                  vpNames <- NULL
                }
                extsToPlot <- extsToPlot[!(addTo %in% currentNames)]
                names(extsToPlot) <- vpNames
                grobNames <- lN
              }
            }

            # create get(paste0(".spadesArr",dev.cur())) object - i.e., the arrangement based on number and extents
            if(!newArr) {
              if(exists(paste0(".spadesArr",dev.cur()),envir=.GlobalEnv)) {
                arr <- get(paste0(".spadesArr",dev.cur()))
                arr@names = append(arr@names, names(extsToPlot))
                arr@extents = append(arr@extents, extsToPlot)
              } else {
                message("nothing to add to, creating new plot")
                arr <- arrangeViewports(extsToPlot)
                grid.newpage()
              }
            } else { # need a new arrangement
              arr <- arrangeViewports(extsToPlot)
              #rm(.spadesArr, envir=.GlobalEnv)
              grid.newpage()
            }
            #end create .spadesArr object

            if(is.null(gp$cex)) {
              gp$cex <- cex <- max(0.6,min(1,prod(arr@ds)/prod(arr@columns,arr@rows)*0.07))
            }

            lay <- makeLayout(arr, visualSqueeze, legend, axes)

            if(length(extsToPlot)>0) {
              vps <- makeViewports(extsToPlot, layout=lay, arr=arr, newArr=newArr)

              if(add & !newArr)
                upViewport(1)
              pushViewport(vps,recording = FALSE)
              upViewport(2)
            }

            npixels <- unlist(sapply(toPlot, function(x) if(is(x,"Raster")) ncell(x)))
            maxpixels <- 4e3/(arr@columns*arr@rows)*prod(arr@ds)/speedup
            if(!is.null(npixels)) {
              maxpixels <- min(maxpixels,max(npixels/3))
            }

            # because of stacks, have to find the right layer which may or may not be in a stack
            layerLengths <- lapply(toPlot, layerNames)
            if(axes==TRUE) { xaxis = TRUE ; yaxis = TRUE}
            if(axes==FALSE) { xaxis = FALSE ; yaxis = FALSE}

            for(grobNamesi in grobNames) {
              whGrobNamesi <- match(grobNamesi,grobNames)

              whPlot <- match(addTo[whGrobNamesi], arr@names)
              if(axes=="L") {if(whPlot>(length(arr@names)-arr@columns)) { xaxis = TRUE } else { xaxis = FALSE}
                             if((whPlot-1)%%arr@columns==0) { yaxis = TRUE } else { yaxis = FALSE}}
              if(grobNamesi %in% currentNames) {
                if (!newArr) {
                  title = FALSE
                  legend = FALSE
                  xaxis = FALSE
                  yaxis = FALSE
                }
              }



              a = try(seekViewport(addTo[whGrobNamesi],recording=F))
              if(is(a, "try-error")) stop(paste("Plot does not already exist on current device.",
                                                "Try add=F or change device to",
                                                "one that has a plot named",addTo[whGrobNamesi]))

              # Get the object from the environment
              if(!grobNamesi %in% lN) { # Is this an overplot
                if(length(stacksInArr)>0) {
                  # only take first one, if there are more than one. First one is most recent
                  isPrevLayerInStack <- na.omit(sapply(stacksInArr, function(x) {
                    match(arr@names[match(grobNamesi, grobNames)],x)}))[1]
                } else {
                  isPrevLayerInStack = NA
                }

                if(all(is.na(isPrevLayerInStack)) ) {# means it is in a stack
                  grobToPlot <- get(grobNamesi)
                } else {
                  grobToPlot <- get(names(isPrevLayerInStack))[[grobNamesi]]
                }
                if(is(grobToPlot, "Raster")) {
                  if(is.null(zoomExtent)) {
                    zoom <- extent(grobToPlot)
                  } else {
                    zoom <- zoomExtent
                  }
                  legendRange = NA
                  zMat <- makeColorMatrix(grobToPlot,zoom,maxpixels,legendRange)

                } else {
                  len <- length(grobToPlot)
                  if(len<(1e4/speedup)) {
                    z <- get(grobNamesi)
                  } else {
                    z <- sample(get(grobNamesi), 1e4/speedup)
                  }
                  zMat <- list(z=z,minz=0,maxz=0,cols=NULL)
                }

              } else { # Is this a new plot to be added or plotted

                toPlotInd <- which(!is.na(sapply(layerLengths,
                                                 function(x) match(grobNamesi,x))))
                if(is(toPlot[[toPlotInd]],"RasterStack")) {
                  grobToPlot = toPlot[[toPlotInd]][[grobNamesi]]
                } else {
                  grobToPlot = toPlot[[toPlotInd]]
                }

                if(is(grobToPlot, "Raster")) {
                  if(is.null(zoomExtent)) {
                    zoom <- extent(grobToPlot)
                  } else {
                    zoom <- zoomExtent
                  }
                  if(is.null(legendRange)) legendRange = NA
                  zMat <- makeColorMatrix(grobToPlot,zoom,maxpixels,legendRange)
                } else { # Not a Raster, i.e., a SpatialPoint* object
                  len <- length(grobToPlot)
                  if(len<(1e4/speedup)) {
                    z <- grobToPlot
                  } else {
                    z <- sample(grobToPlot, 1e4/speedup)
                  }
                  zMat <- list(z=z,minz=0,maxz=0,cols=NULL)
                }

              }
              plotGrob(zMat$z, col = zMat$cols, size=unit(size,"points"),
                       minv=zMat$minz,
                       maxv=zMat$maxz,
                       vp=NULL, pch=pch, name = layerNames(grobToPlot),
                       legend = legend,
                       gp = gp, draw = draw)
              if(title) grid.text(layerNames(grobToPlot),
                                  name="title", y=1.08, vjust=0.5, gp = gp)

              if(xaxis) grid.xaxis(name="xaxis", gp = gp)
              if(yaxis) grid.yaxis(name="yaxis", gp = gp)
            }

            if(add==FALSE) {
              arr@stack <- stacksToPlot
            } else {
              arr@stack <- append(stacksToPlot, stacksInArr)
              arr@stack <- arr@stack[!duplicated(arr@stack)]
            }
            assign(paste0(".spadesArr",dev.cur()), arr, envir=.GlobalEnv)
          })



#' Convert Raster to color matrix useable by raster function for plotting
#'
#' @param grobToPlot a SpatialObject
#' @param zoomExtent an extent object for zooming to. Defaults to whole extent of grobToPlot
#' @param maxpixels numeric. Number of cells to subsample the complete grobToPlot
#' @param legendRange numeric vector of length 2, representing the lower and upper bounds of
#' a legend that will override the data bounds contained within the grobToPlot
#' @rdname makeColorMatrix
#' @export
#' @docType methods
setGeneric("makeColorMatrix", function(grobToPlot, zoomExtent, maxpixels, legendRange) {
  standardGeneric("makeColorMatrix")
})


#' @rdname makeColorMatrix
#' @export
setMethod("makeColorMatrix",
          signature=c("Raster","Extent","numeric","ANY"),
          definition= function(grobToPlot, zoomExtent, maxpixels, legendRange) {

            if(sapply(getColors(grobToPlot),length)>0) {
              cols <- getColors(grobToPlot)[[1]]

              # If there is a legend that is too long for the number of values, this chops
              #  off the extraneous ones because the as.raster below will match min-max on
              #  both col and values, which is not the desired behaviour
              #   if(length(col)>(maxValue(grobToPlot)+1)) {
              #     col <- col[1:maxValue(grobToPlot)+1]
              #   }
            } else {
              cols=topo.colors(50)
            }
            zoom <- zoomExtent
            grobToPlot <- sampleRegular(x=grobToPlot, size=maxpixels,
                                        ext=zoom, asRaster=TRUE, useGDAL=TRUE)
            z <- getValues(grobToPlot)

            minz <- min(z, na.rm=T)
            maxz <- max(z, na.rm=T)

            #cols <- cols[minz:maxz - minz+1] # The actual colors may be fewer in the sampled raster
            # if data in raster are proportions, must treat colors differently
            if(maxz <= 1) {
              if(length(unique(z))>length(cols)) {
                cols <- colorRampPalette(cols)(50)

                z <- z*49
              }
            }

            # Single value rasters
            if((maxz-minz)==0) {
              legend <- FALSE
            }

            if ((maxz-minz+1)>length(cols)) { # if not enough colors, make more
              cols <- colorRampPalette(cols)(maxz-minz+1)
            }

            if(any(!is.na(legendRange))){
               if((diff(legendRange)+1)<length(cols)) {
                 message(paste0("legendRange is not wide enough, using default"))
               } else {
                cols <- colorRampPalette(cols)(diff(legendRange)+1)
                minz <- min(legendRange)
                maxz <- max(legendRange)
               }
            }


            # colors are indexed from 1, as with all objects in R, but there are generally
            #  zero values on the rasters, so shift by 1
            z <- z + 1
            z[is.na(z)] <- 1
#             if((length(cols)+1)==(maxz-minz+2))
#               cols=c("#FFFFFFFF",cols)
            z <- matrix(cols[z], nrow=nrow(grobToPlot), ncol=ncol(grobToPlot), byrow=T)
            list(z=z,minz=minz,maxz=maxz,cols=cols)
          })
