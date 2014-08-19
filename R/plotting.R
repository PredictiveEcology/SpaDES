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
#' @rdname arrangePlots
#' @export
#' @docType methods
setGeneric("arrangePlots", function(toPlot, nam=NULL, which.to.plot="all", axes="L") {
  standardGeneric("arrangePlots")
})

#' @rdname arrangePlots
#' @export
setMethod("arrangePlots",
          signature=c("Raster"),
          definition= function(toPlot, nam=NULL, which.to.plot="all", axes="L") {
    ext = extent(toPlot)
    dimx = dim(toPlot)
    if(is.null(nam)) nam = names(toPlot)
    wh <- nam

    if(any(which.to.plot != "all")) {
      if (is.character(which.to.plot)) if (any(is.na(match(which.to.plot, nam)))) stop("Not a named map in rasterx")
      if (is.numeric(which.to.plot)) {
        wh <- wh[match(nam[which.to.plot], wh)]
      } else {
        wh <- wh[match(which.to.plot, wh)]
      }
    }

    if(dev.cur()==1) {
        dev.new(height=8, width=10)
    }

    ds <- dev.size()
    ds.ratio <- ds[1]/ds[2]

    dimensionRatio <- dimx[2]/dimx[1]

    ds.dimensionRatio <- ds.ratio/dimensionRatio

    col.by.row <- data.frame(matrix(ncol=2, nrow=length(wh)))

    col.by.row[,1] <- ceiling(length(wh)/(1:length(wh)))
    col.by.row[,2] <- ceiling(length(wh)/col.by.row[,1])


    wh.best <- which.min(abs(apply(col.by.row,1,function(x) x[1]/x[2]) - ds.dimensionRatio))

    columns <- col.by.row[wh.best,1]
    rows <- col.by.row[wh.best,2]

    actual.ratio <- columns/rows

    if (axes != "none" & axes != FALSE) {
        prettys <- list()
        prettys[["x"]] <- pretty(c(xmin(ext),xmax(ext)))
        prettys[["y"]] <- pretty(c(ymin(ext),ymax(ext)))
        prettys[["x"]] <- prettys[["x"]][which(prettys[["x"]]>=xmin(ext) & prettys[["x"]]<=xmax(ext))]
        prettys[["y"]] <- prettys[["y"]][which(prettys[["y"]]>=ymin(ext) & prettys[["y"]]<=ymax(ext))]
    }

    cr <- expand.grid(columns=((1:columns/columns - 1/columns/2)-0.55)*0.9+0.55,rows=((rows:1/rows - 1/rows/2)-0.55)*0.9+0.55)
    out <- list(cr=cr,rows=rows,columns=columns,actual.ratio=actual.ratio,ds.dimensionRatio=ds.dimensionRatio,
                ds=ds,prettys=prettys,wh=wh,ds.ratio=ds.ratio)
    return(out)
}
)

#' @rdname arrangePlots
setMethod("arrangePlots",
          signature=c("SpatialPoints"),
          definition= function(toPlot, nam=deparse(substitute(toPlot)), axes="L") {
            arrangePlots(out)
          }
)

placeOnRaster <- function(obj, raster) {
  #ext <- extent(bbox(obj))
  #ext <- ext+diff(range(xmin(ext),xmax(ext)))*0.05
  rast <- raster(extent(raster))
  nam <- deparse(substitute(obj))
  names(rast) <- nam
  return(rast)
}

#' @rdname arrangePlots
setMethod("arrangePlots",
          signature=c("list"),
          definition=function(toPlot, nam=deparse(substitute(toPlot)), axes="L") {
            rasters <- sapply(toPlot,function(x) is(x,"Raster"))
            spatialPoints <- sapply(toPlot,function(x) is(x,"SpatialPoints"))

            #can't
            obj <- append(stack(toPlot[rasters]),lapply(toPlot[spatialPoints],placeOnRaster))

            ext = extent(bbox(toPlot))+diff(range(xmin(ext),xmax(ext)))*0.05
            out <- raster(ext)
            names(out)<-nam
            arrangePlots(out)
          }
)

##############################################################
#' plot arrows showing direction of mobileAgent movement
#'
#' Plots arrows showing direction of mobileAgent movement.
#'
#' @param agent         A \code{mobileAgent} object.
#'
#' @param ...           Additional plotting parameters.
#'
#' @return Returns the modified \code{SimList} object.
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
#'
#' @export
#' @rdname cols
.cols = list(
 transparent.red=c("#00000000",paste(RColorBrewer::brewer.pal(8,"Greys"),"66",sep="")[8:1]),
 grey = RColorBrewer::brewer.pal(9,"Greys"),
 spectral = RColorBrewer::brewer.pal(8,"Spectral"),
 terrain = rev(terrain.colors(100)),
 heat = heat.colors(10),
 topo = topo.colors(10)
)


##########
rastVp <- function(toPlot){
  vp.toPlot <- viewport(name=paste("vp",names(toPlot),sep=""),
                      xscale=c(xmin(toPlot),xmax(toPlot)),
                      yscale=c(ymin(toPlot),ymax(toPlot)))
  return(vp.toPlot)
}


######################################################3
######################################################3
######################################################3
######################################################3

#' @export
plotRast <- function(toPlot, add = FALSE, col=NULL,
                     legend=TRUE, draw=TRUE, xaxis=TRUE, yaxis=TRUE, title=TRUE,
                     gp=gpar(), vp=NULL, maxpixels=1e6,
                     childrenvp=NULL, ...) {
  if(!add) { grid.newpage() }

  pr <- pretty(range(minValue(toPlot),maxValue(toPlot)))
  pr <- pr[pr<=maxValue(toPlot)]
  if(is.null(col)) col=rev(terrain.colors(40))

  rastGrob <- gTree(toPlot=toPlot, name=names(toPlot),
                    pr=pr,col=col,
                    childrenvp=childrenvp,
                    children=gList(
                      rasterGrob(as.raster(toPlot, maxpixels=maxpixels,col = col),
                                 interpolate=FALSE,
                                 name="raster"),
                      if(xaxis) xaxisGrob(name="xaxis"),
                      if(yaxis) yaxisGrob(name="yaxis"),
                      if(legend) rasterGrob(as.raster(col[length(col):1]),
                                            x=1.04,y=0.5,height=0.5,width=0.03,
                                            interpolate=FALSE,
                                            name="legend"),
                      if(legend) textGrob(pr, x=1.08, y=pr/(2*maxValue(toPlot))+0.25,
                                          gp=gpar(cex=max(0.5, 1-0.05)),
                                          just="left",
                                          name="legendText"),
                      if(title) textGrob(names(toPlot), name="title", y=1.08, vjust=0.5)
                    ),
                    gp=gp,
                    vp=vp,
                    cl="toPlot")
  if(draw) grid.draw(rastGrob)
  return(invisible(rastGrob))
}

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

makeLayout <- function(visualSqueeze, columns, rows, arr, currentcex) {
  # calculate the visualSqueeze for the width (i.e., vS.w)
  vS.w = min(visualSqueeze/columns, visualSqueeze/columns*arr$actual.ratio/arr$ds.dimensionRatio)
  # check to see if the windows is too small and all axes and legends can't fit
  tooTight <- (((arr$ds[1] - arr$ds[1]*columns*vS.w)/(2*columns))/max(0.3,currentcex/2))
  #adjust if visualSqueeze is set to default value; otherwise, take user's provided value
  if(visualSqueeze==0.75) #if(tooTight<1.5)
    vS.w = vS.w*(tooTight^0.09)
  # make vector of unit objects to make layout
  wdth <- unit.c(unit(1.5,"null"), unit(rep(c(vS.w,1),columns),
                                        rep(c("npc","null"),columns))[-columns*2],
                 unit(1,"null"))

  vS.h = min(visualSqueeze/rows, visualSqueeze/rows*arr$ds.dimensionRatio/arr$actual.ratio)
  tooTight <- (((arr$ds[1] - arr$ds[1]*columns*vS.w)/(2*columns))/max(0.3,currentcex/2))
  if(visualSqueeze==0.75) #if(tooTight<1.5)
    vS.h = vS.h*(tooTight^0.09)
  bottopMargin <- max(max(0.2,currentcex/3),(arr$ds[2] - arr$ds[2]*rows*vS.h)/(2*rows))
  ht <- unit.c(unit(bottopMargin,"inches"), unit(rep(c(vS.h,2),rows),
                                                 rep(c("npc","null"),rows))[-rows*2], unit(bottopMargin,"inches"))
  ht <- unit.c(unit(1,"null"), unit(rep(c(vS.h,1),rows),
                                    rep(c("npc","null"),rows))[-rows*2],
               unit(1,"null"))

  return(list(wdth=wdth,ht=ht))
}


#' @export
makeViewports <- function(obj, arr, add, visualSqueeze, needRearrange = FALSE) {

  columns = arr$columns
  rows = arr$rows

  # need to know current plot cex to scale the dimensions of the plot
  #  The following line is used within the main plot function and is repeated here
  currentcex = max(0.4,min(1,prod(arr$ds)/prod(columns,rows)*0.2))

  #make a layout that fits nicely with the open device
  lay <- makeLayout(visualSqueeze, columns, rows, arr, currentcex)

  topVp <-
    viewport(layout=grid.layout(nrow=rows*2+1, ncol=columns*2+1,
                                widths=lay$wdth,
                                heights=lay$ht),
             name="top")
  plotVps <- list()

  if((.arr$columns != arr$columns) | (.arr$rows != arr$rows) | add==FALSE | needRearrange) {
    for(i in 1:length(arr$wh)) {

      #use layout just made
      plotVps[[i]] <- viewport(
                name=paste("vp",arr$wh[i],sep=""),
                layout.pos.col = ceiling((i-1)%%columns+1)*2,
                layout.pos.row = ceiling(i/columns)*2,
                xscale=c(xmin(obj),xmax(obj)),
                yscale=c(ymin(obj),ymax(obj)))

#         plotVps[[i]] <- makeViewport(obj[[i]], #visualSqueeze = 0.6,
#                                     layout.pos.row=ceiling(i/columns)*2,
#                                     layout.pos.col=ceiling((i-1)%%columns+1)*2)
    }
    wholeVp <- vpTree(topVp, do.call(vpList, plotVps))

  } else {
    currentViewports <- paste("vp",sapply(grobs,function(x) x$name),sep="")
    toAdd <- which(is.na(match(paste("vp",names(obj),sep=""), currentViewports)))
    nameToAdd <- paste("vp",names(obj),sep="")[toAdd]
    for(i in toAdd) {
      i2 <- match(i,toAdd)
      i3 <- i2 + length(currentViewports)
      plotVps[[i2]] <- makeViewport(obj[[i]],
                                 layout.pos.row=ceiling(i3/columns)*2,
                                 layout.pos.col=ceiling((i3-1)%%columns+1)*2)
    }
    wholeVp <- do.call(vpList, plotVps)
  }
  return(wholeVp)
}

#' @export
setGeneric("Plot", signature="...", function(..., add=F, gp=gpar(), axes="L", speedup = 1,
                                              cols=topo.colors(50), deletePrevious = add,
                                              visualSqueeze=0.75, quick = FALSE, legend=!quick, draw = TRUE) {
  standardGeneric("Plot")
})

setClassUnion("spatialObjects", c("Raster","SpatialPoints"))

#' @export
setMethod("Plot",
          signature("Raster"),
#          signature("map"),
          definition = function(..., add, gp, axes, speedup,
                                cols, deletePrevious, visualSqueeze,
                                quick, legend, draw) {
            if(all(sapply(list(...),function(x) is(x,"Raster")))) {
                obj <- stack(list(...))
            } else {
              stop("Plot is currently only implemented for objects of class Raster")
            }



            nam <- names(obj)
            if(exists(".grobs",envir=.GlobalEnv)) grobs = .grobs
#            return(obj)
#          })

#             if(is.null(cols)) {
#               rastCols <- getColors(obj)
#               namesColMaps <- names(rastCols)
#
#               cols <- lapply(1:length(rastCols), function(x) {
#                 if(length(rastCols[[x]])==0)
#                   col1 <- topo.colors(min(50,round(maxValue(obj[[x]])-minValue(obj[[x]])))+1)
#                 else
#                   col1 <- rastCols[[x]]
#               })
#             }

            # recycle colours, if not enough provided
            if(is.character(cols)) { # if it is a single vector of colours
              cols = lapply(1:length(nam), function(x) return(cols))
            } else if(length(cols) < length(nam)) {
              cols = lapply(1:length(nam), function(x) {
                cols[[((x-1)%%length(cols))+1]]
              })
            }


            needRearrange = FALSE
            if(add==F) {
              grid.newpage()
              grobs = list()
              arr <- arrangePlots(obj)
              assign(".arr", arr, envir=.GlobalEnv)
              toAdd <- nam

            } else {
              currentGrobs <- unique(grid.ls(grob=T,recursive=F,print=F)$name)
              alreadyPlotted <- match(nam, currentGrobs)
              if(any(is.na(alreadyPlotted))){
                toAdd <- nam[which(is.na(alreadyPlotted))]
              } else {
                toAdd <- NULL
              }

              # check to see if new addition can fit without rearranging
              if(length(c(currentGrobs, toAdd))>prod(.arr$columns,.arr$rows)){
                toAdd = c(currentGrobs, toAdd)
                arr <- arrangePlots(obj,
                                       nam=toAdd)
                needRearrange <- TRUE
                grid.newpage()
              } else {
                arr = .arr
                arr$wh <- c(arr$wh,toAdd)
              }
              assign(".arr", arr, envir=.GlobalEnv)

            }

            # Assess optimal number of pixels to plot, optimized for speed, maintaining visibility
            npixels <- ncell(obj)
            maxpixels <- 1e3/(arr$columns*arr$rows)*prod(arr$ds)/speedup
            maxpixels <- c(maxpixels,npixels)[(npixels/2<maxpixels)+1]


            if (!is.null(toAdd)) {
              vps <- makeViewports(obj, arr, add=add, visualSqueeze=visualSqueeze,
                                   needRearrange=needRearrange)
              if(add & !needRearrange)
                upViewport(1)
              pushViewport(vps,recording = FALSE)
              upViewport(2)
            }

            if(is.null(gp$cex)) {
              gp$cex = max(0.4,min(1,prod(arr$ds)/prod(arr$columns,arr$rows)*0.2))
            }

            if (needRearrange){
              for(i in 1:length(toAdd)) {
                if(axes=="L") {if(arr$cr[i,"rows"]==min(arr$cr[,"rows"])) { xaxis = TRUE } else { xaxis = FALSE}
                               if(arr$cr[i,"columns"]==min(arr$cr[,"columns"])) { yaxis = TRUE } else { yaxis = FALSE}}
                if(axes==TRUE) { xaxis = TRUE ; yaxis = TRUE}
                if(axes==FALSE) { xaxis = FALSE ; yaxis = FALSE}
                title=TRUE

                seekViewport(paste("vp",toAdd[i],sep=""),recording=F)#(!quick | !add))

                if(all(i != match(names(obj),toAdd))) {
                  #if (!xaxis) grid.remove("xaxis")
                  #if (!yaxis) grid.remove("yaxis")
                  grid.draw(grobs[[i]])
                } else {
                  i2 = match(toAdd,names(obj))[i]
                  grobs[[i]] <- plotRast(obj[[i2]], col = cols[[i2]], add=TRUE, vp=NULL,
                              xaxis = xaxis, yaxis = yaxis, title=title,
                              maxpixels= maxpixels,
                              legend = legend, gp = gp, draw = draw)
                }

              }
            } else {
              for(i in match(names(obj),arr$wh)) {
                i2 <- match(arr$wh[i],names(obj))
                if(any(nam[[i2]]==toAdd)) {
                  if(axes=="L") {if(arr$cr[i,"rows"]==min(arr$cr[,"rows"])) { xaxis = TRUE } else { xaxis = FALSE}
                                 if(arr$cr[i,"columns"]==min(arr$cr[,"columns"])) { yaxis = TRUE } else { yaxis = FALSE}}
                  if(axes==TRUE) { xaxis = TRUE ; yaxis = TRUE}
                  if(axes==FALSE) { xaxis = FALSE ; yaxis = FALSE}
                  title=TRUE
                } else {
                  xaxis=FALSE; yaxis=FALSE; legend = FALSE; title=FALSE
                }


                #if(deletePrevious & !quick) {grid.remove(nam[[i2]])}
  #              if(deletePrevious) {grid.remove(gPath(nam[[i2]],"legend"))}
                seekViewport(paste("vp",names(obj)[i2],sep=""),recording=F)#names(obj)[i2] %in% toAdd)#(!quick | !add))
                grobs[[i]] <- plotRast(obj[[i2]], col = cols[[i2]], add=TRUE, vp=NULL,
                              xaxis = xaxis, yaxis = yaxis, title=title,
                              maxpixels= maxpixels,
                              legend = legend, gp = gp, draw = draw)
              }
            }
          assign(".grobs",grobs,envir=.GlobalEnv)
          #assign(".grobs", grobs, pos="package:SpaDES")
          return(invisible(grobs))
          }
)

#' @export
setMethod("Plot",
          #signature("Raster"),
          signature("map"),
          definition = function(..., add, gp, axes, speedup,
                                cols, deletePrevious, visualSqueeze,
                                quick, legend, draw) {

            if(all(sapply(list(...),function(x) is(x,"Raster")))) {
              obj <- stack(list(...))
            } else {
              stop("Plot is currently only implemented for objects of class Raster")
            }



            nam <- names(obj)
            if(exists(".grobs",envir=.GlobalEnv)) grobs = .grobs
            #            return(obj)
            #          })

            #             if(is.null(cols)) {
            #               rastCols <- getColors(obj)
            #               namesColMaps <- names(rastCols)
            #
            #               cols <- lapply(1:length(rastCols), function(x) {
            #                 if(length(rastCols[[x]])==0)
            #                   col1 <- topo.colors(min(50,round(maxValue(obj[[x]])-minValue(obj[[x]])))+1)
            #                 else
            #                   col1 <- rastCols[[x]]
            #               })
            #             }

            # recycle colours, if not enough provided
            if(is.character(cols)) { # if it is a single vector of colours
              cols = lapply(1:length(nam), function(x) return(cols))
            } else if(length(cols) < length(nam)) {
              cols = lapply(1:length(nam), function(x) {
                cols[[((x-1)%%length(cols))+1]]
              })
            }


            needRearrange = FALSE
            if(add==F) {
              grid.newpage()
              grobs = list()
              arr <- arrangePlots(obj)
              assign(".arr", arr, envir=.GlobalEnv)
              toAdd <- nam

            } else {
              currentGrobs <- unique(grid.ls(grob=T,recursive=F,print=F)$name)
              alreadyPlotted <- match(nam, currentGrobs)
              if(any(is.na(alreadyPlotted))){
                toAdd <- nam[which(is.na(alreadyPlotted))]
              } else {
                toAdd <- NULL
              }

              # check to see if new addition can fit without rearranging
              if(length(c(currentGrobs, toAdd))>prod(.arr$columns,.arr$rows)){
                toAdd = c(currentGrobs, toAdd)
                arr <- arrangePlots(obj,
                                    nam=toAdd)
                needRearrange <- TRUE
                grid.newpage()
              } else {
                arr = .arr
                arr$wh <- c(arr$wh,toAdd)
              }
              assign(".arr", arr, envir=.GlobalEnv)

            }

            # Assess optimal number of pixels to plot, optimized for speed, maintaining visibility
            npixels <- ncell(obj)
            maxpixels <- 1e3/(arr$columns*arr$rows)*prod(arr$ds)/speedup
            maxpixels <- c(maxpixels,npixels)[(npixels/2<maxpixels)+1]


            if (!is.null(toAdd)) {
              vps <- makeViewports(obj, arr, add=add, visualSqueeze=visualSqueeze,
                                   needRearrange=needRearrange)
              if(add & !needRearrange)
                upViewport(1)
              pushViewport(vps,recording = FALSE)
              upViewport(2)
            }

            if(is.null(gp$cex)) {
              gp$cex = max(0.4,min(1,prod(arr$ds)/prod(arr$columns,arr$rows)*0.2))
            }

            if (needRearrange){
              for(i in 1:length(toAdd)) {
                if(axes=="L") {if(arr$cr[i,"rows"]==min(arr$cr[,"rows"])) { xaxis = TRUE } else { xaxis = FALSE}
                               if(arr$cr[i,"columns"]==min(arr$cr[,"columns"])) { yaxis = TRUE } else { yaxis = FALSE}}
                if(axes==TRUE) { xaxis = TRUE ; yaxis = TRUE}
                if(axes==FALSE) { xaxis = FALSE ; yaxis = FALSE}
                title=TRUE

                seekViewport(paste("vp",toAdd[i],sep=""),recording=F)#(!quick | !add))

                if(all(i != match(names(obj),toAdd))) {
                  #if (!xaxis) grid.remove("xaxis")
                  #if (!yaxis) grid.remove("yaxis")
                  grid.draw(grobs[[i]])
                } else {
                  i2 = match(toAdd,names(obj))[i]
                  grobs[[i]] <- plotRast(obj[[i2]], col = cols[[i2]], add=TRUE, vp=NULL,
                                         xaxis = xaxis, yaxis = yaxis, title=title,
                                         maxpixels= maxpixels,
                                         legend = legend, gp = gp, draw = draw)
                }

              }
            } else {
              for(i in match(names(obj),arr$wh)) {
                i2 <- match(arr$wh[i],names(obj))
                if(any(nam[[i2]]==toAdd)) {
                  if(axes=="L") {if(arr$cr[i,"rows"]==min(arr$cr[,"rows"])) { xaxis = TRUE } else { xaxis = FALSE}
                                 if(arr$cr[i,"columns"]==min(arr$cr[,"columns"])) { yaxis = TRUE } else { yaxis = FALSE}}
                  if(axes==TRUE) { xaxis = TRUE ; yaxis = TRUE}
                  if(axes==FALSE) { xaxis = FALSE ; yaxis = FALSE}
                  title=TRUE
                } else {
                  xaxis=FALSE; yaxis=FALSE; legend = FALSE; title=FALSE
                }


                #if(deletePrevious & !quick) {grid.remove(nam[[i2]])}
                #              if(deletePrevious) {grid.remove(gPath(nam[[i2]],"legend"))}
                seekViewport(paste("vp",names(obj)[i2],sep=""),recording=F)#names(obj)[i2] %in% toAdd)#(!quick | !add))
                grobs[[i]] <- plotRast(obj[[i2]], col = cols[[i2]], add=TRUE, vp=NULL,
                                       xaxis = xaxis, yaxis = yaxis, title=title,
                                       maxpixels= maxpixels,
                                       legend = legend, gp = gp, draw = draw)
              }
            }
            assign(".grobs",grobs,envir=.GlobalEnv)
            #assign(".grobs", grobs, pos="package:SpaDES")
            return(invisible(grobs))
          }
)

grid.points(x1/max(1,ds.map.ratio/actual.ratio),y1/max(1,actual.ratio/ds.map.ratio),
            name=deparse(substitute(x)) ,...)

