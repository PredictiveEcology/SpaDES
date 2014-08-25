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


#' @exportClass SpatialPointsDataFrameNamed
setClass("SpatialPointsDataFrameNamed",
         slots=list(name="character"),
         prototype=list(name=NA_character_),
         contains="SpatialPointsDataFrame",
         validity=function(object) {
           # check for valid sim times and make default list
           if (is.na(object@name)) {
             stop("name must be provided")
           }
         }
)

#' @exportClass SpatialPointsNamed
setClass("SpatialPointsNamed",
         slots=list(name="character"),
         prototype=list(name=NA_character_),
         contains="SpatialPoints",
         validity=function(object) {
           # check for valid sim times and make default list
           if (is.na(object@name)) {
             stop("name must be provided")
           }
         }
)

#' @exportClass NamedSpatialPoints
setClassUnion("NamedSpatialPoints", c("SpatialPointsNamed", "SpatialPointsDataFrameNamed"))

#' @export
setGeneric("SpatialPointsDataFrameNamed",
           signature=c("..."),
           function(..., name) {
             standardGeneric("SpatialPointsDataFrameNamed")
           })


#' @export
setMethod("SpatialPointsDataFrameNamed",
          signature="SpatialPointsDataFrame",
          definition= function(..., name) {
            new("SpatialPointsDataFrameNamed", ..., name=name)
          })



#' @export
setGeneric("SpatialPointsNamed",
           signature=c("..."),
           function(..., name) {
             standardGeneric("SpatialPointsNamed")
           })


#' @export
setMethod("SpatialPointsNamed",
          signature="SpatialPoints",
          definition= function(..., name) {
            new("SpatialPointsNamed", ..., name=name)
          })


#' @export
setMethod("show",
          signature="SpatialPointsNamed",
          definition=function(object) {
            out = list()
            out[[1]] = capture.output(print(object,
                                            quote=FALSE, row.names=FALSE))
            out[[2]] = capture.output(cat(paste("name        :",object@name)))

            ### print result
            cat(unlist(out), fill=FALSE, sep="\n")
          })

#' @export
setMethod("show",
          signature="SpatialPointsDataFrameNamed",
          definition=function(object) {
            out = list()
            out[[1]] = capture.output(print(object,
                                            quote=FALSE, row.names=FALSE))
            out[[2]] = capture.output(cat(paste("name        :",object@name)))

            ### print result
            cat(unlist(out), fill=FALSE, sep="\n")
          })


#' @export
setGeneric("name", function(object) {
  standardGeneric("name")
})

#' @export
#' @rdname name-accessor-methods
setMethod("name",
          signature="SpatialPointsNamed",
          definition=function(object) {
            return(object@name)
          })

#' @export
#' @rdname name-accessor-methods
setMethod("name",
          signature="SpatialPointsDataFrameNamed",
          definition=function(object) {
            return(object@name)
          })

#' set name of SpatialPoints and SpatialPointsDataFrames
#' @export
#' @name name<-
#' @rdname name-accessor-methods
setGeneric("name<-",
           function(object, value) {
             standardGeneric("name<-")
           })

#' @export
#' @name name<-
#' @rdname name-accessor-methods
setReplaceMethod("name",
                 signature="SpatialPointsNamed",
                 function(object, value) {
                   object@name <- value
                   validObject(object)
                   return(object)
                 })

#' @export
#' @name name<-
#' @rdname name-accessor-methods
setReplaceMethod("name",
                 signature="SpatialPoints",
                 function(object, value) {
                   new("SpatialPointsNamed", object, name=value)
                 })

#' @export
#' @name name<-
#' @rdname name-accessor-methods
setReplaceMethod("name",
                 signature="SpatialPointsDataFrameNamed",
                 function(object, value) {
                   object@name <- value
                   validObject(object)
                   return(object)
                 })

#' @export
#' @name name<-
#' @rdname name-accessor-methods
setReplaceMethod("name",
                 signature="SpatialPointsDataFrame",
                 function(object, value) {
                   new("SpatialPointsDataFrameNamed", object, name=value)
                 })


#' @export
setMethod("nlayers",
          signature="list",
          function(x) {
            sum(sapply(x,function(x) {
              if(is(x,"RasterStack")) {
                x=nlayers(x)
              } else {
                x = 1
              }
              return(x)
            }))}
            )

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
            as.character(sapply(object, function(x) {if(is(x,"Raster")) {names(x)} else {name(x)}}))
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
              }}))})


#' Makes a raster around a SpatialPoints object
#'
#' @name placeOnRaster
#' @rdname placeOnRaster
#' @export
setGeneric("placeOnRaster", function(obj, raster=NULL) {
  standardGeneric("placeOnRaster")
})

#' @rdname placeOnRaster
#' @export
setMethod("placeOnRaster",
          signature=c("NamedSpatialPoints"),
          definition= function(obj, raster) {
            if(is.null(raster)) {
              ext <- extent(bbox(obj))
              ext <- ext+diff(range(xmin(ext),xmax(ext)))*0.1
              rast <- raster(ext)
            } else {
              rast <- raster(raster)
            }
            names(rast) <- name(obj)
            rast <- setValues(rast, 0)
            return(rast)
          }
)


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
      vpnames <- paste("vp",names(extents),sep="")
      names <- names(extents)
    } else {
      nPlots <- length(name)
      vpnames <- paste("vp",name,sep="")
      names <- name
    }

    if(dev.cur()==1) {
        dev.new(height=8, width=10)
    }

    ds <- dev.size()
    ds.ratio <- ds[1]/ds[2]

    dimensionRatio <- dimx[2]/dimx[1]

    ds.dimensionRatio <- ds.ratio/dimensionRatio

    col.by.row <- data.frame(matrix(ncol=2, nrow=nPlots))

    col.by.row[,1] <- ceiling(nPlots/(1:nPlots))
    col.by.row[,2] <- ceiling(nPlots/col.by.row[,1])

    wh.best <- which.min(abs(apply(col.by.row,1,function(x) x[1]/x[2]) - ds.dimensionRatio))

    columns <- col.by.row[wh.best,1]
    rows <- col.by.row[wh.best,2]

    actual.ratio <- columns/rows

    cr <- expand.grid(columns=((1:columns/columns - 1/columns/2)-0.55)*0.9+0.55,rows=((rows:1/rows - 1/rows/2)-0.55)*0.9+0.55)
    out <- list(cr=cr,rows=rows,columns=columns,
                actual.ratio=actual.ratio,ds.dimensionRatio=ds.dimensionRatio,
                ds=ds,#prettys=prettys,
                names=names,ds.ratio=ds.ratio, extents = extents)
    return(out)
}
)





######################################################3
######################################################3
######################################################3
######################################################3
#' Plot either a raster Grob or a points Grob
#'
#' @param grobToPlot Raster* or SpatialPoints* object
#' @param add should grob be added to current plot
#' @name plotGrob
#' @rdname plotGrob
#' @export
#' @docType methods
setGeneric("plotGrob", function(grobToPlot, add=TRUE, col=NULL, size=unit(5,"points"),
                                legend=TRUE, draw=TRUE, xaxis=TRUE, yaxis=TRUE, title=TRUE,
                                gp=gpar(), vp=NULL, pch=19, maxpixels=1e6,
                                childrenvp=NULL, ...) {
  standardGeneric("plotGrob")
})

#' @rdname plotGrob
#' @export
setMethod("plotGrob",
          signature=c("Raster"),
          definition= function(grobToPlot, add, col, size,
                               legend, draw, xaxis, yaxis, title,
                               gp, vp, pch, maxpixels,
                               childrenvp, ...) {
            if(!add) { grid.newpage() }

            pr <- pretty(range(minValue(grobToPlot),maxValue(grobToPlot)))
            pr <- pr[pr<=maxValue(grobToPlot)]
            if(is.null(col)) col=rev(terrain.colors(40))

            rastGrob <- gTree(grobToPlot=grobToPlot, title=title, name=layerNames(grobToPlot),
                              pr=pr,col=col,
                              #childrenvp=childrenvp,
                              children=gList(
                                rasterGrob(as.raster(grobToPlot, maxpixels=maxpixels,col = col),
                                           interpolate=FALSE,
                                           name="raster"),
                                if(xaxis) xaxisGrob(name="xaxis"),
                                if(yaxis) yaxisGrob(name="yaxis"),
                                if(legend) rasterGrob(as.raster(col[length(col):1]),
                                                      x=1.04,y=0.5,height=0.5,width=0.03,
                                                      interpolate=FALSE,
                                                      name="legend"),
                                if(legend) textGrob(pr, x=1.08, y=pr/(2*maxValue(grobToPlot))+0.25,
                                                    gp=gpar(cex=max(0.5, 1-0.05)),
                                                    just="left",
                                                    name="legendText"),
                                if(title) textGrob(names(grobToPlot), name="title", y=1.08, vjust=0.5)
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
          definition= function(grobToPlot, add = FALSE, col, size,
                               legend, draw, xaxis, yaxis, title,
                               gp=gpar(), vp=NULL, pch, maxpixels,
                               childrenvp=NULL, ...) {
            if(!add) { grid.newpage() }
            pntGrob <- gTree(grobToPlot=grobToPlot, title=title, name=layerNames(grobToPlot),
                             childrenvp=childrenvp,
                             children=gList(
                               pointsGrob(x=grobToPlot$x, y=grobToPlot$y, pch=pch, size=size),
                               if(xaxis) xaxisGrob(name="xaxis"),
                               if(yaxis) yaxisGrob(name="yaxis"),
                               if(title) textGrob(name(grobToPlot), name="title", y=1.08, vjust=0.5)
                             ),
                             gp=gp,
                             vp=vp,
                             cl="plotPoint")
            if(draw) grid.draw(pntGrob)
            return(invisible(pntGrob))
          })



#' @title makeViewport
#' @name makeViewport
#' @export
#' @rdname makeViewport
setGeneric("makeViewport", function(obj,
                      layout.pos.col, layout.pos.row,
                      visualSqueeze=0.75) {
  standardGeneric("makeViewport")
})

#' @rdname makeViewport
setMethod("makeViewport",
          signature="Raster",
          definition=function(obj, layout.pos.col=1, layout.pos.row=1) {

  vp.obj <- viewport(name=paste("vp",names(obj),sep=""),
                     layout.pos.col = layout.pos.col,
                     layout.pos.row = layout.pos.row,
                     xscale=c(xmin(obj),xmax(obj)),
                     yscale=c(ymin(obj),ymax(obj)))
  return(vp.obj)
})

#' @export
makeLayout <- function(arr, visualSqueeze, cex) {
  columns <- arr$columns
  rows <- arr$rows


  # calculate the visualSqueeze for the width (i.e., vS.w)
  vS.w = min(visualSqueeze/columns,
             visualSqueeze/columns*arr$actual.ratio/arr$ds.dimensionRatio)
  wdth <- unit.c(unit(1.5,"null"), unit(rep(c(vS.w,1.75),columns),
                                        rep(c("npc","null"),columns))[-columns*2],
                 unit(1,"null"))

  # calculate the visualSqueeze for the height (i.e., vS.h)
  vS.h = min(visualSqueeze/rows,
             visualSqueeze/rows*arr$ds.dimensionRatio/arr$actual.ratio)
  ht <- unit.c(unit(1,"null"), unit(rep(c(vS.h,1.75),rows),
                                    rep(c("npc","null"),rows))[-rows*2],
               unit(1,"null"))

  return(list(wdth=wdth,ht=ht))
}


#' @export
makeViewports <- function(extents, layout, arr, visualSqueeze, newArr = FALSE) {

  columns = arr$columns
  rows = arr$rows

  topVp <- viewport(layout=grid.layout(nrow=rows*2+1,
                                       ncol=columns*2+1,
                                       widths=layout$wdth,
                                       heights=layout$ht),
                    name="top")
  plotVps <- list()

  for(extentInd in 1:length(extents)) {
    nam = names(extents)[extentInd]
    posInd = match(nam, arr$names)

    plotVps[[extentInd]] <- viewport(
              name=nam,
              layout.pos.col = ceiling((posInd-1)%%columns+1)*2,
              layout.pos.row = ceiling(posInd/columns)*2,
              xscale=c(extents[[extentInd]]@xmin,extents[[extentInd]]@xmax),
              yscale=c(extents[[extentInd]]@ymin,extents[[extentInd]]@ymax))

  }

  if(newArr)
    wholeVp <- vpTree(topVp, do.call(vpList, plotVps))
  else
    wholeVp <- do.call(vpList, plotVps)
  return(wholeVp)
}

#' @exportClass spatialObjects
setClassUnion("spatialObjects", c("SpatialPointsNamed","SpatialPointsDataFrameNamed",
                                  "RasterLayer", "RasterStack"))



##############################################################
#' Plots arrows showing direction of agent movement.
#'
#' @param from          Starting spatial coordinates (\code{SpatialPointsDataFrame}).
#'
#' @param to            Ending spatial coordinates (\code{SpatialPointsDataFrame})..
#'
#' @param on.which.to.plot The name of a map layer on which to draw the arrows.
#'
#' @param ...           Additional plotting parameters.
#'
#' @return Plots the vectors representing agent movement on the specified map.
#'
##' @import sp
#' @export
#' @docType methods
#' @rdname drawArrows-method
#'
setGeneric("drawArrows", function(from, to, on.which.to.plot=1, ...) {
  standardGeneric("drawArrows")
})

#' Plot arrows showing direction of mobileAgent movement
#'
#' @param length    The length of the arrows to draw (defaults to 0.1).
#'
#' @rdname drawArrows-method
#' @examples
#' hab <- raster(extent(0,1e2,0,1e2),res=1)
#' hab <- GaussMap(hab)
#' to <- SpatialPoints(cbind(x=runif(10)*100, y=runif(10)*100))
#' from <- SpatialPoints(cbind(x=rnorm(10,to$x,5), y=rnorm(10,to$y,5)))
#' Plot(hab)
#' Plot(to,pch=19,gp=gpar(cex=0.1));Plot(from,pch=19,gp=gpar(cex=0.1))
#' drawArrows(from, to)
#'
setMethod("drawArrows",
          signature=c("SpatialPoints","SpatialPoints"),
          definition=function(from, to, on.which.to.plot=1, ..., length=0.1) {
            vp.names <- grid.ls(grobs=FALSE, viewports=TRUE, recursive=TRUE, print=FALSE)$name
            vp.names <- vp.names[match(unique(vp.names[1:trunc(length(vp.names)/2)*2]),vp.names)]
            for (k in 1:length(on.which.to.plot)) {
              if(is.numeric(on.which.to.plot[k])) {
                vp.to.plot <- vp.names[on.which.to.plot[k]]
                seekViewport(vp.to.plot)
              } else {
                vp.to.plot <- on.which.to.plot[k]
                seekViewport(vp.to.plot)
              }

              grid.polyline(x=c(from$x, to$x), y=c(from$y, to$y),
                            default.units="native",
                            id=rep(1:length(from), 2),
                            #default.units="npc",
                            arrow=arrow(length=unit(length, "inches"), ...))#, #name=NULL,
              upViewport()
            }
            #           gp=gpar(),
            #            arrows(from$x, from$y, to$x, to$y, ..., length=length)
          })

######################################################################
#' A short selection of colour palettes that can be use
#'
#' Colour number 1 shows use of transparency
#' @export
#' @rdname cols
.cols = list(
 transparentGrey=c("#00000000",paste(RColorBrewer::brewer.pal(8,"Greys"),"66",sep="")[8:1]),
 grey = RColorBrewer::brewer.pal(9,"Greys"),
 spectral = RColorBrewer::brewer.pal(8,"Spectral"),
 terrain = terrain.colors(100),
 heat = heat.colors(10),
 topo = topo.colors(10),
 blueGreen = RColorBrewer::brewer.pal(9,"BuGn"),
 greens = RColorBrewer::brewer.pal(9,"Greens"),
 yellowBrown = RColorBrewer::brewer.pal(9, "YlOrBr"),
 discrete1 = RColorBrewer::brewer.pal(8,"BrBG")
)

#' @export
setGeneric("Plot2", signature="...",
           function(..., add=F, addTo=NULL, gp=gpar(), axes="L", speedup = 1,
                    size=5, cols=topo.colors(50), deletePrevious = add,
                    visualSqueeze=0.75, quick = FALSE, legend=!quick, draw = TRUE,
                    pch = 19, title=T) {
             standardGeneric("Plot2")
           })


#' @export
setMethod("Plot2",
          signature(c("spatialObjects")),
          definition = function(..., add, addTo, gp, axes, speedup, size,
                                cols, deletePrevious, visualSqueeze,
                                quick, legend, draw, pch, title) {
            toPlot <- list(...)
            # check whether .arr exists, meaning that there is already a plot
            if(!exists(".arr",envir=.GlobalEnv)) {
              add=F
              arr = list(); arr$columns=0; arr$row = 0
              if(add==T) message("Nothing to add plots to; creating new plots")
              currentNames = NULL
            } else {
              arr <- .arr
              currentNames <- arr$names
            }
            browser()

            lN <- layerNames(toPlot)

            if(is.null(addTo)) {
              addTo <- lN
              } else {
              add = TRUE
            }
            currentPlusToPlotN <- unique(c(currentNames, addTo))


            # if add == F, then new plots are only the ones in the function call, otherwise
            #  it needs to assess what is already there
            extsToPlot <- rep(sapply(toPlot, extent),
                            sapply(toPlot, function(x) length(layerNames(x))))
            names(extsToPlot)<-lN

            if(add==F) {
              newArr = T
              vpNames <- lN
              grobNames <- lN
              if(exists(".grobs",envir=.GlobalEnv)) rm(.grobs, envir=.GlobalEnv)
              grobs <- list()
            } else { # add == T
              if(length(currentPlusToPlotN) > prod(arr$column, arr$rows)) {
                grobs <- .grobs
                newArr = T
                vpNames = currentPlusToPlotN
                grobNames = vpNames
                addTo <- grobNames
                ind <- vpNames %in% lN + 1
                extsUnmerged <- list(extCurrent=arr$extents[match(vpNames,currentNames)],
                                     extlN=extsToPlot[match(vpNames, lN)])
                extsToPlot <- sapply(1:length(vpNames), function(x) extsUnmerged[[ind[x]]][x])
              } else {
                grobs <- .grobs
                newArr = F
                if(sum(!(lN %in% currentNames))!=0) {
                  vpNames <- !(lN %in% currentNames)
                } else {
                  vpNames <- NULL
                }
                extsToPlot <- extsToPlot[!names(extsToPlot) %in% currentNames]
                names(extsToPlot) <- vpNames
                grobNames <- lN
              }
            }

            # get extents from all SpatialPoints*, Rasters*, including Stacks
#             extsToPlot <- rep(sapply(toPlot, extent),
#                               sapply(toPlot, function(x) length(layerNames(x))))
#             names(extsToPlot)<-lN
#             ind <- vpNames %in% lN + 1
#             extsUnmerged <- list(extCurrent=arr$extents[match(vpNames,currentNames)],
#                                  extlN=extsToPlot[match(vpNames, lN)])
#             extsAll <- sapply(1:length(vpNames), function(x) extsUnmerged[[ind[x]]][x])
#             extsNew <- extsToPlot[!(lN %in% currentNames)]
#             extsToAdd <- list(extsNew,extsAll)[[newArr+1]]
#             extsNamesToAdd <- names(extsToAdd)

            # create .arr object - i.e., the arrangement based on number and extents
            if(!newArr) {
              if(exists(".arr",envir=.GlobalEnv)) {
                arr <- .arr
                #grobs <- .grobs
              } else {
                message("nothing to add to, creating new plot")
                arr <- arrangeViewports(extsToPlot)
                assign(".arr", arr, envir=.GlobalEnv)
                grid.newpage()
              }
            } else {
              arr <- arrangeViewports(extsToPlot)
              assign(".arr", arr, envir=.GlobalEnv)
              grid.newpage()
            }
            #end create .arr object
#browser()
            if(is.null(gp$cex)) {
              gp$cex <- cex <- max(0.5,min(1,prod(arr$ds)/prod(arr$columns,arr$rows)*0.1))
            }


            lay <- makeLayout(arr, visualSqueeze, cex)
            if(length(extsToPlot)>0) {
              vps <- makeViewports(extsToPlot, layout=lay, arr=arr, newArr=newArr)

              if(add & !newArr)
                upViewport(1)
              pushViewport(vps,recording = FALSE)
              upViewport(2)
            }

            npixels <- sapply(toPlot, ncell)
            maxpixels <- 1e3/(arr$columns*arr$rows)*prod(arr$ds)/speedup
            maxpixels <- c(maxpixels,npixels)[(npixels/2<maxpixels)+1]

            for(grobNamesi in grobNames) {
              whGrobNamesi <- match(grobNamesi,grobNames)
              if(addTo[whGrobNamesi] != grobNamesi) {
                title = FALSE
                legend = FALSE
              }
              whPlot <- match(addTo[whGrobNamesi], .arr$names)
              if(axes=="L") {if(arr$cr[whPlot,"rows"]==min(arr$cr[,"rows"])) { xaxis = TRUE } else { xaxis = FALSE}
                             if(arr$cr[whPlot,"columns"]==min(arr$cr[,"columns"])) { yaxis = TRUE } else { yaxis = FALSE}}
              if(axes==TRUE) { xaxis = TRUE ; yaxis = TRUE}
              if(axes==FALSE) { xaxis = FALSE ; yaxis = FALSE}

              #browser()

              #seekViewport(addTo[whPlot],recording=F)
              seekViewport(addTo[whGrobNamesi],recording=F)

              if(!grobNamesi %in% lN) {
                grid.draw(.grobs[[grobNamesi]])
                if(xaxis==TRUE) grid.xaxis(gp = gp)
                if(yaxis==TRUE) grid.xaxis(gp = gp)
              } else {
                # because of stacks, have to find the right layer which may or may not be in a stack
                layerLengths <- lapply(toPlot, layerNames)
                toPlotInd <- which(!is.na(sapply(layerLengths,
                                                 function(x) match(grobNamesi,x))))
                if(is(toPlot[[toPlotInd]],"RasterStack")) {
                  grobToPlot = toPlot[[toPlotInd]][[grobNamesi]]
                } else {
                  grobToPlot = toPlot[[toPlotInd]]
                }

                if (quick) {
                  plotGrob(grobToPlot, col = cols, size=unit(size,"points"),
                           add=TRUE, vp=NULL, pch=pch,
                           xaxis = xaxis, yaxis = yaxis, title=title,
                           maxpixels= maxpixels[toPlotInd],
                           legend = legend, gp = gp, draw = draw)
                } else {
                  grobs[[grobNamesi]] <- plotGrob(grobToPlot, col = cols, size=unit(size,"points"),
                                           add=TRUE, vp=NULL, pch=pch,
                                           xaxis = xaxis, yaxis = yaxis, title=title,
                                           maxpixels= maxpixels[toPlotInd],
                                           legend = legend, gp = gp, draw = draw)
                }
              }

            }
            assign(".grobs", grobs, envir=.GlobalEnv)
            return(invisible(grobs))
         })
