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

#' @exportClass RasterStackNamed
setClass("RasterStackNamed",
         slots=list(name="character"),
         prototype=list(name=NA_character_),
         contains="RasterStack",
         validity=function(object) {
           # check for valid sim times and make default list
           if (is.na(object@name)) {
             stop("name must be provided")
           }
         }
)

#' @export
setGeneric("RasterStackNamed",
           signature=c("..."),
           function(..., name) {
             standardGeneric("RasterStackNamed")
           })


#' @export
setMethod("RasterStackNamed",
          signature="RasterStack",
          definition= function(..., name) {
            new("RasterStackNamed", ..., name=name)
          })


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
                                            quote=FALSE))
            out[[2]] = capture.output(cat(paste("name        :",object@name)))

            ### print result
            cat(unlist(out), fill=FALSE, sep="\n")
          })

#' @export
setMethod("show",
          signature="RasterStackNamed",
          definition=function(object) {
  cat("class       :", class(object), "\n")
  if (rotated(object)) {
    cat("rotated     : TRUE\n")
  }
  mnr <- 15
  if (filename(object) != "") {
    cat("filename    :", filename(object), "\n")
  }
  nl <- nlayers(object)
  if (nl == 0) {
    cat("nlayers     :", nl, "\n")
  }
  else {
    cat("dimensions  : ", nrow(object), ", ", ncol(object),
        ", ", ncell(object), ", ", nl, "  (nrow, ncol, ncell, nlayers)\n",
        sep = "")
    cat("resolution  : ", xres(object), ", ", yres(object),
        "  (x, y)\n", sep = "")
    cat("extent      : ", object@extent@xmin, ", ", object@extent@xmax,
        ", ", object@extent@ymin, ", ", object@extent@ymax,
        "  (xmin, xmax, ymin, ymax)\n", sep = "")
    cat("name        :", name(object), "\n")
    cat("coord. ref. :", projection(object, TRUE), "\n")
    ln <- names(object)
    if (nl > mnr) {
      ln <- c(ln[1:mnr], "...")
    }
    n <- nchar(ln)
    if (nl > 5) {
      b <- n > 26
      if (any(b)) {
        ln[b] <- paste(substr(ln[b], 1, 9), "//", substr(ln[b],
                                                         nchar(ln[b]) - 9, nchar(ln[b])), sep = "")
      }
    }
    minv <- format(minValue(object))
    maxv <- format(maxValue(object))
    minv <- gsub("Inf", "?", minv)
    maxv <- gsub("-Inf", "?", maxv)
    if (nl > mnr) {
      minv <- c(minv[1:mnr], "...")
      maxv <- c(maxv[1:mnr], "...")
    }
    w <- pmax(nchar(ln), nchar(minv), nchar(maxv))
    m <- rbind(ln, minv, maxv)
    for (i in 1:ncol(m)) {
      m[, i] <- format(m[, i], width = w[i], justify = "right")
    }
    cat("names       :", paste(m[1, ], collapse = ", "),
        "\n")
    cat("min values  :", paste(m[2, ], collapse = ", "),
        "\n")
    cat("max values  :", paste(m[3, ], collapse = ", "),
        "\n")
  }
  z <- getZ(object)
  if (length(z) > 0) {
    name <- names(object@z)
    if (is.null(name))
      name <- "z-value"
    if (name == "")
      name <- "z-value"
    name <- paste(sprintf("%-12s", name), ":", sep = "")
    if (length(z) < mnr) {
      cat(name, paste(as.character(z), collapse = ", "),
          "\n")
    }
    else {
      z <- range(z)
      cat(name, paste(as.character(z), collapse = " - "),
          "(range)\n")
    }
  }
  cat("\n")
#             out = list()
#             out[[1]] = capture.output(show(object))
#             out[[2]] = capture.output(cat(paste("name        :",object@name)))
#
#             ### print result
#             cat(unlist(out), fill=FALSE, sep="\n")
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

#' @export
#' @rdname name-accessor-methods
setMethod("name",
          signature="RasterStackNamed",
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
#' @name name<-
#' @rdname name-accessor-methods
setReplaceMethod("name",
                 signature="RasterStackNamed",
                 function(object, value) {
                   object@name <- value
                   validObject(object)
                   return(object)
                 })

#' @export
#' @name name<-
#' @rdname name-accessor-methods
setReplaceMethod("name",
                 signature="RasterStack",
                 function(object, value) {
                   new("RasterStackNamed", object, name=value)
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
              }}))})



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

    out <- new("arrangement", rows=rows,columns=columns,
               actual.ratio=actual.ratio,ds.dimensionRatio=ds.dimensionRatio,
               ds=ds,#prettys=prettys,
               names=names, extents = extents)
#     out <- list(rows=rows,columns=columns,
#                 actual.ratio=actual.ratio,ds.dimensionRatio=ds.dimensionRatio,
#                 ds=ds,#prettys=prettys,
#                 names=names,ds.ratio=ds.ratio, extents = extents)
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
                                legend=TRUE, draw=TRUE, #xaxis=TRUE, yaxis=TRUE, title=TRUE,
                                gp=gpar(), vp=NULL, pch=19, maxpixels=1e6,
                                childrenvp=NULL, ...) {
  standardGeneric("plotGrob")
})

#' @rdname plotGrob
#' @export
setMethod("plotGrob",
          signature=c("Raster"),
          definition= function(grobToPlot, add, col, size,
                               legend, draw, #xaxis, yaxis, title,
                               gp, vp, pch, maxpixels,
                               childrenvp, ...) {
            if(!add) { grid.newpage() }

            pr <- pretty(range(minValue(grobToPlot),maxValue(grobToPlot)))
            pr <- pr[pr<=maxValue(grobToPlot)]
            if(is.null(col)) col=rev(terrain.colors(40))

            rastGrob <- gTree(grobToPlot=grobToPlot, #title=title,
                              name=layerNames(grobToPlot),
                              pr=pr,col=col,
                              #childrenvp=childrenvp,
                              children=gList(
                                rasterGrob(as.raster(grobToPlot, maxpixels=maxpixels,col = col),
                                           interpolate=FALSE,
                                           name="raster"),
#                                 if(xaxis) xaxisGrob(name="xaxis"),
#                                 if(yaxis) yaxisGrob(name="yaxis"),
                                if(legend) rasterGrob(as.raster(col[length(col):1]),
                                                      x=1.04,y=0.5,height=0.5,width=0.03,
                                                      interpolate=FALSE,
                                                      name="legend"),
                                if(legend) textGrob(pr, x=1.08, y=((pr-min(pr))/(max(pr)-min(pr)))/2+0.25,
                                                    gp=gpar(cex=max(0.6, 1-0.05)),
                                                    just="left",
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
          definition= function(grobToPlot, add = FALSE, col, size,
                               legend, draw, #xaxis, yaxis, title,
                               gp=gpar(), vp=NULL, pch, maxpixels,
                               childrenvp=NULL, ...) {
            if(!add) { grid.newpage() }
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
  columns <- arr@columns
  rows <- arr@rows


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

#####################
#' Fast, optimally arranged, multipanel plotting function with spades
#'
#' The main plotting function accompanying spades. This can take objects of type Raster* or SpatialPoints*Named,
#' and any combination of those. If add=F, then a new plot will be generated. When add=T, then any plot that
#' already exists will be overplotted, while plots that have not already been plotted will be added. This function
#' rearrange the plotting device to maximize the size of all the plots, minimizing white space. If using RStudio,
#' it is recommended to makeand use a new device because the built in device is not made for rapid redrawing.
#' The function is based on the grid package.
#'
#' Silently, two hidden objects are made, .arr and .grobs, which are used for later replotting (i.e., if add=T). Also,
#' the function invisibly returns the grobs (see grid package).
#'
#' @param ... Raster* object(s) and or SpatialPoints*Named objects
#' @param axes Logical. If FALSE, then the previous plot is wiped and a new one made; if TRUE, then the ... plots
#' will be added to the current device, adding or rearranging the plot layout as necessary.
#' @param addTo String vector, with same length as ...  This is for overplotting, when the overplot is not to occur on
#' the plot with the same name, such as plotting a SpatialPoints*Named object on a RasterLayer.
#' @param gp A gpar object, created by gpar() function, to change plotting parameters (see grid package)
#' @param axes Logical or "L", representing the left and bottom axes, overall plots
#' @rdname Plot
#' @export
#' @docType methods
#' @export
#' @examples
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
#' sim <- loadFiles(fileList=fileList)
#'
#' # make a stack of all these rasters
#' landscape <- stack(mget(unlist(simObjectsLoaded(sim))))
#'
#' # extract a single one of these rasters
#' DEM <- landscape$DEM
#' DEM1 <- landscape$DEM
#' names(DEM1) <- "DEM1"
#'
#' # make a SpatialPointsNamed object
#' caribou <- SpatialPoints(cbind(x=runif(1e2,-50,50),y=runif(1e2,-50,50)))
#'      name(caribou)<-"caribou"
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
#' Plot(caribou, addTo="forestAge",size=4, axes=F)
#'
#' # can add a new plot to the plotting window
#' Plot(caribou, add=T)
#'
#' # can't add a two maps with same name
#' Plot(landscape, caribou, DEM)
#'
#' # Can mix stacks, rasters, SpatialPoint*Named
#' Plot(landscape, DEM1, caribou)
#'
#' # Can mix stacks, rasters, SpatialPoint*Named
#' Plot(landscape, caribou)
#' Plot(DEM1, add=T)
#'
setGeneric("Plot", signature="...",
           function(..., add=F, addTo=NULL, gp=gpar(), axes="L", speedup = 1,
                    size=5, cols=topo.colors(50),
                    visualSqueeze=0.75, legend=TRUE, draw = TRUE,
                    pch = 19, title=T) {
             standardGeneric("Plot")
           })


#' @export
setMethod("Plot",
          signature(c("spatialObjects")),
          definition = function(..., add, addTo, gp, axes, speedup, size,
                                cols, visualSqueeze,
                                legend, draw, pch, title) {
            toPlot <- list(...)
            # check whether .arr exists, meaning that there is already a plot
            browser()

            if(!exists(".arr",envir=.GlobalEnv)) {
              add=F
              arr = new("arrangement"); arr@columns=0; arr@rows = 0
              if(add==T) message("Nothing to add plots to; creating new plots")
              currentNames = NULL
            } else {
              arr <- .arr
              currentNames <- arr@names
              currentStacks <- arr@stack
            }
            lN <- layerNames(toPlot)
            if(any(duplicated(lN))) stop(paste("Cannot plot two layers with same name. Check",
                                         "inside RasterStacks"))

            if(is.null(addTo)) {
              addTo <- lN
            } else {
              if(length(addTo)!=length(lN)) stop("addTo must be same length as objects to plot")
              add = TRUE
            }
            currentPlusToPlotN <- unique(c(currentNames, addTo))


            # if add == F, then new plots are only the ones in the function call, otherwise
            #  it needs to assess what is already there

            # get extents from all SpatialPoints*, Rasters*, including Stacks
            extsToPlot <- rep(sapply(toPlot, extent),
                            sapply(toPlot, function(x) length(layerNames(x))))
            names(extsToPlot)<-lN

            if(add==F) {
              newArr = T
              vpNames <- addTo
              grobNames <- lN
              if(exists(".grobs",envir=.GlobalEnv)) rm(.grobs, envir=.GlobalEnv)
              grobs <- list()
            } else { # add == T
              if(length(currentPlusToPlotN) > prod(arr@columns, arr@rows)) {
                grobs <- .grobs
                newArr = T
#                 if(sum(!(lN %in% currentPlusToPlotN))!=0) {
#                   vpNames <- lN[!(lN %in% currentPlusToPlotN)]
#                 } else {
#                   vpNames <- NULL
#                 }
                vpNames = currentPlusToPlotN
                grobNames = vpNames
                addTo <- grobNames
                ind <- vpNames %in% lN + 1
                extsUnmerged <- list(extCurrent=arr@extents[match(vpNames,currentNames)],
                                     extlN=extsToPlot[match(vpNames, lN)])
                extsToPlot <- sapply(1:length(vpNames), function(x) extsUnmerged[[ind[x]]][x])
              } else {
                grobs <- .grobs
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

            # create .arr object - i.e., the arrangement based on number and extents
            if(!newArr) {
              if(exists(".arr",envir=.GlobalEnv)) {
                arr <- .arr
                arr@names = append(arr@names, names(extsToPlot))
                arr@extents = append(arr@extents, extsToPlot)

                stacks <- which(sapply(toPlot, function(x) length(layerNames(x)))>1)
                if(length(stacks)>0) {
                  namesByStack <- lapply(toPlot[stacks], layerNames)
                  arrStackLen <- length(arr@stack)
                  arr@stack <- append(arr@stack, namesByStack)
                  names(arr@stack)[arrStackLen+1:length(namesByStack)] <- sapply(toPlot[stacks], name)
                }

                #grobs <- .grobs
              } else {
                message("nothing to add to, creating new plot")
                arr <- arrangeViewports(extsToPlot)
                stacks <- which(sapply(toPlot, function(x) length(layerNames(x)))>1)
                if(length(stacks)>0) {
                  arr@stack <- lapply(toPlot[stacks], layerNames)
                  names(arr@stack) <- sapply(toPlot[stacks], name)
                }
                assign(".arr", arr, envir=.GlobalEnv)
                grid.newpage()
              }
            } else { # need a new arrangement
              arr <- arrangeViewports(extsToPlot)
              stacks <- which(sapply(toPlot, function(x) length(layerNames(x)))>1)
              if(length(stacks)>0) {
                namesByStack <- lapply(toPlot[stacks], layerNames)
                arrStackLen <- length(arr@stack)
                arr@stack <- append(currentStacks, namesByStack)
                names(arr@stack)[arrStackLen+1:length(namesByStack)] <- sapply(toPlot[stacks], name)
              } else {
                arr@stack <- currentStacks
              }

              assign(".arr", arr, envir=.GlobalEnv)
              grid.newpage()
            }

            #end create .arr object

            if(is.null(gp$cex)) {
              gp$cex <- cex <- max(0.6,min(1,prod(arr@ds)/prod(arr@columns,arr@rows)*0.1))
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
            maxpixels <- 2e3/(arr@columns*arr@rows)*prod(arr@ds)/speedup
            maxpixels <- c(maxpixels,npixels)[(npixels/3<maxpixels)+1]


            # because of stacks, have to find the right layer which may or may not be in a stack
            layerLengths <- lapply(toPlot, layerNames)
            if(axes==TRUE) { xaxis = TRUE ; yaxis = TRUE}
            if(axes==FALSE) { xaxis = FALSE ; yaxis = FALSE}

            for(grobNamesi in grobNames) {
              whGrobNamesi <- match(grobNamesi,grobNames)
              if(addTo[whGrobNamesi] != grobNamesi) {
                title = FALSE
                legend = FALSE
              }
              whPlot <- match(addTo[whGrobNamesi], arr@names)
              if(axes=="L") {if(whPlot>(length(arr@names)-arr@columns)) { xaxis = TRUE } else { xaxis = FALSE}
                             if(whPlot%%arr@columns==1) { yaxis = TRUE } else { yaxis = FALSE}}

              seekViewport(addTo[whGrobNamesi],recording=F)

              if(!grobNamesi %in% lN) {
#                 lapply(arr@stack, function(x) x == grobNamesi)
                isPrevLayerInStack <- sapply(arr@stack, function(x) {
                    match(arr@names[match(grobNamesi, arr@names)],x)
                  }
                )
                if(!is.na(isPrevLayerInStack)) {# means it is in a stack
                  withinStacki <- match(grobNamesi,arr@stack[[names(isPrevLayerInStack)]])
                  grobToPlot <- get(names(isPrevLayerInStack))[[withinStacki]]
                } else {
                  grobToPlot <- get(grobNamesi)
                }

#                 inStack <- match(arr@stack, arr@names[match(grobNamesi, arr@names)])
                 plotGrob(grobToPlot, col = cols, size=unit(size,"points"),
                          add=TRUE, vp=NULL, pch=pch,
                          #xaxis = xaxis, yaxis = yaxis, title=title,
                          maxpixels= maxpixels[toPlotInd],
                          legend = legend, gp = gp, draw = draw)
                #grid.draw(.grobs[[grobNamesi]])
                if(title) grid.text(grobNamesi, name="title", y=1.08, vjust=0.5, gp = gp)

#                 if(xaxis==TRUE) grid.xaxis(gp = gp)
#                 if(yaxis==TRUE) grid.xaxis(gp = gp)
              } else {
                # because of stacks, have to find the right layer which may or may not be in a stack
#                layerLengths <- lapply(toPlot, layerNames)
#                toPlotInd <- lN[whGrobNamesi]
                toPlotInd <- which(!is.na(sapply(layerLengths,
                                                  function(x) match(grobNamesi,x))))
                if(is(toPlot[[toPlotInd]],"RasterStack")) {
                  grobToPlot = toPlot[[toPlotInd]][[grobNamesi]]
                } else {
                  grobToPlot = toPlot[[toPlotInd]]
                }

#                 if (quick) {
#                   plotGrob(grobToPlot, col = cols, size=unit(size,"points"),
#                            add=TRUE, vp=NULL, pch=pch,
#                            #xaxis = xaxis, yaxis = yaxis, title=title,
#                            maxpixels= maxpixels[toPlotInd],
#                            legend = legend, gp = gp, draw = draw)
#                 } else {
                  grobs[[grobNamesi]] <- plotGrob(grobToPlot, col = cols, size=unit(size,"points"),
                                           add=TRUE, vp=NULL, pch=pch,
                                           #xaxis = xaxis, yaxis = yaxis, title=title,
                                           maxpixels= maxpixels[toPlotInd],
                                           legend = legend, gp = gp, draw = draw)
                  if(title) grid.text(layerNames(grobToPlot), name="title", y=1.08, vjust=0.5, gp = gp)
#                }
              }
              if(xaxis) grid.xaxis(name="xaxis", gp = gp)
              if(yaxis) grid.yaxis(name="yaxis", gp = gp)
            }
            assign(".grobs", grobs, envir=.GlobalEnv)
            return(invisible(grobs))
         })
