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


#' @export
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

#' @export
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



# #' @export
# setGeneric("name",
#            signature=c("..."),
#            function(..., name) {
#              standardGeneric("name")
#            })
#
# #' @export
# setMethod("name",
#           signature="SpatialPoints",
#           definition= function(..., name) {
#             new("SpatialPointsNamed", ..., name=name)
#           })
#
# #' @export
# setMethod("name",
#           signature="SpatialPointsDataFrame",
#           definition= function(..., name) {
#             new("SpatialPointsDataFrameNamed", ..., name=name)
#           })

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

#' set list of simulation modules
#' @export
#' @rdname name-accessor-methods
setGeneric("name<-",
           function(object, value) {
             standardGeneric("name<-")
           })

#' @export
#' @rdname name-accessor-methods
setReplaceMethod("name",
                 signature="SpatialPointsNamed",
                 function(object, value) {
                   object@name <- value
#                   validObject(object)
                   return(object)
                 })

#' @export
#' @rdname name-accessor-methods
setReplaceMethod("name",
                 signature="SpatialPoints",
                 function(object, value) {
                   new("SpatialPointsNamed", object, name=value)
                 })

#' @export
#' @rdname name-accessor-methods
setReplaceMethod("name",
                 signature="SpatialPointsDataFrameNamed",
                 function(object, value) {
                   object@name <- value
                   validObject(object)
                   return(object)
                 })

#' @export
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
            sum(sapply(toPlot,function(x) {
              if(is(x,"RasterStack")) {
                x=nlayers(x)
              } else {
                x = 1
              }
              return(x)
            }))}
            )

#' @export
setGeneric("layerNames", function(object) {
  standardGeneric("layerNames")
})

#' @rdname name-accessor-methods
#' @export
setMethod("layerNames",
          signature="list",
          definition=function(object) {
            unlist(sapply(toPlot, function(x) {if(is(x,"Raster")) {names(x)} else {name(x)}}))
          })


#################################################
#' @export
setGeneric("placeOnRaster", function(obj, raster=NULL) {
  standardGeneric("placeOnRaster")
})

#' @rdname arrangePlots
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
#' @rdname arrangePlots
#' @export
#' @docType methods
setGeneric("arrangePlots", function(toPlot) {
  standardGeneric("arrangePlots")
})

#' @rdname arrangePlots
#' @export
setMethod("arrangePlots",
          signature=c("list"),
          definition= function(toPlot) {
    rasters <- sapply(toPlot,function(x) is(x,"RasterLayer"))
    ext <- extent(toPlot[rasters][[1]])
    dimx <- dim(toPlot[rasters][[1]])
    nPlots <- nlayers(toPlot)

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

#     if (axes != "none" & axes != FALSE) {
#         prettys <- list()
#         prettys[["x"]] <- pretty(c(xmin(ext),xmax(ext)))
#         prettys[["y"]] <- pretty(c(ymin(ext),ymax(ext)))
#         prettys[["x"]] <- prettys[["x"]][which(prettys[["x"]]>=xmin(ext) & prettys[["x"]]<=xmax(ext))]
#         prettys[["y"]] <- prettys[["y"]][which(prettys[["y"]]>=ymin(ext) & prettys[["y"]]<=ymax(ext))]
#     }

    cr <- expand.grid(columns=((1:columns/columns - 1/columns/2)-0.55)*0.9+0.55,rows=((rows:1/rows - 1/rows/2)-0.55)*0.9+0.55)
    out <- list(cr=cr,rows=rows,columns=columns,
                actual.ratio=actual.ratio,ds.dimensionRatio=ds.dimensionRatio,
                ds=ds,#prettys=prettys,
                names=layerNames(toPlot),ds.ratio=ds.ratio)
    return(out)
}
)

#' @export
#' @rdname arrangePlots
# setMethod("arrangePlots",
#           signature=c("SpatialPoints"),
#           definition= function(toPlot, nam=deparse(substitute(toPlot)), axes="L") {
#             placeOnRaster(toPlot)
#             arrangePlots(out)
#           }
# )



#' @export
# #' @rdname arrangePlots
# setMethod("arrangePlots",
#           signature=c("list"),
#           definition=function(toPlot, nam, axes="L") {
#             if(any(nchar(names(toPlot))==0)) stop("list must be a named list")
#             rasters <- sapply(toPlot,function(x) is(x,"Raster"))
#             spatialPoints <- sapply(toPlot,function(x) is(x,"SpatialPoints"))
#
#             if(all(spatialPoints)) {
#               rast <- placeOnRaster(toPlot[[1]])
#               obj <- stack(lapply(toPlot, function(x){
#                 placeOnRaster(x, rast)
#               }))
#             } else {
#               obj <- stack(append(toPlot[rasters],
#                                   lapply(toPlot[spatialPoints],
#                                          function(x) placeOnRaster(x,toPlot[rasters][[1]]))))
#             }
#             arrangePlots(obj)
#           }
# )




######################################################3
######################################################3
######################################################3
######################################################3

#' @export
plotPoints <- function(toPlot, add = TRUE, size=unit(5,"points"),
                       draw=TRUE, xaxis=TRUE, yaxis=TRUE, title=TRUE,
                       gp=gpar(), vp=NULL, pch=19,
                       childrenvp=NULL, ...) {
  if(!add) { grid.newpage() }
  pntGrob <- gTree(toPlot=toPlot[[1]], name=names(toPlot),
                    childrenvp=childrenvp,
                    children=gList(
                      pointsGrob(x=toPlot[[1]]$x, y=toPlot[[1]]$y, pch=pch, size=size),
                      if(xaxis) xaxisGrob(name="xaxis"),
                      if(yaxis) yaxisGrob(name="yaxis"),
                      if(title) textGrob(names(toPlot), name="title", y=1.08, vjust=0.5)
                    ),
                    gp=gp,
                    vp=vp,
                    cl="plotPoint")
  if(draw) grid.draw(pntGrob)
  return(invisible(pntGrob))
}

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
                    cl="plotRast")
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
makeViewports <- function(toPlot, arr, cex, add, visualSqueeze, needRearrange = FALSE) {

  columns = arr$columns
  rows = arr$rows
  listClass <- sapply(toPlot, function(x) is(x)[1])

  #make a layout that fits nicely with the open device
  lay <- makeLayout(arr, visualSqueeze, cex)

  topVp <-
    viewport(layout=grid.layout(nrow=rows*2+1, ncol=columns*2+1,
                                widths=lay$wdth,
                                heights=lay$ht),
             name="top")
  plotVps <- list()

  # Test that
  if(needRearrange | add==FALSE) {
  #if((.arr$columns != arr$columns) | (.arr$rows != arr$rows) | add==FALSE | needRearrange) {
    for(toPloti in toPlot) {
      if(listClass[toPloti]=="SpatialPointNamed") nam = name(toPlot[[toPloti]])
#        for(j in name()))
        #use layout just made
        plotVps[[toPloti]] <- viewport(
                  name=paste("vp",arr$names[toPloti],sep=""),
                  layout.pos.col = ceiling((toPloti-1)%%columns+1)*2,
                  layout.pos.row = ceiling(toPloti/columns)*2,
                  xscale=c(extent(toPlot[[toPloti]])@xmin,extent(toPlot[[toPloti]])@xmax),
                  yscale=c(extent(toPlot[[toPloti]])@ymin,extent(toPlot[[toPloti]])@ymax))

  #         plotVps[[toPloti]] <- makeViewport(obj[[toPloti]], #visualSqueeze = 0.6,
  #                                     layout.pos.row=ceiling(toPloti/columns)*2,
  #                                     layout.pos.col=ceiling((toPloti-1)%%columns+1)*2)
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
setGeneric("Plot", signature="...",
           function(..., add=F, addTo=NULL, gp=gpar(), axes="L", speedup = 1,
                                             size=5, cols=topo.colors(50), deletePrevious = add,
                                             visualSqueeze=0.75, quick = FALSE, legend=!quick, draw = TRUE,
                                             pch = 19) {
  standardGeneric("Plot")
})

#' @export
setClassUnion("spatialObjects", c("SpatialPointsNamed","SpatialPointsDataFrameNamed",
                                  "RasterLayer", "RasterStack"))


#grid.points(x1/max(1,ds.map.ratio/actual.ratio),y1/max(1,actual.ratio/ds.map.ratio),
#            name=deparse(substitute(x)) ,...)



#' @export
setMethod("Plot",
          signature(c("spatialObjects")),
          definition = function(..., add, addTo, gp, axes, speedup, size,
                                cols, deletePrevious, visualSqueeze,
                                quick, legend, draw) {
            toPlot <- list(...)

            # which are rasters and which are not
            listClass <- sapply(toPlot, function(x) is(x)[1])
            rasters <- sapply(toPlot, function(x) is(x,"Raster"))
            nlayers(toPlot)
            layerNames(toPlot)

#             #stack them, converting any spatialPoints to empty Rasters
#             if(any(rasters)) {
#               obj <- stack(lapply(toPlot, function(x) {
#                 if(is(x,"SpatialPoints")) x = placeOnRaster(x, toPlot[rasters][[1]])
#                 return(x)
#               }))
#             } else {
#               obj <- stack(lapply(toPlot, function(x) {
#                   placeOnRaster(x, toPlot[[1]])
#                 }))
#             }

            if(!is.null(addTo)) add = TRUE
            if(add) addTo = nam

            #nam <- names(obj)

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
            existsDotGrobs = exists(".grobs",envir=.GlobalEnv)
            if((add & !existsDotGrobs)) {
              message("Nothing to add plots to, creating new plot")
              add=F
            }
            if(any(!(addTo %in% getNames()))) {
              message("addTo does not exist, plotting own plots")
              addTo=NULL
            }

            #
            if(add==F) {
              grid.newpage()
              grobs = list()
              arr <- arrangePlots(toPlot)
              assign(".arr", arr, envir=.GlobalEnv)
              needVP <- nam

            } else {
              currentGrobs <- unique(getNames())
              alreadyPlotted <- match(nam, currentGrobs)
              if(any(is.na(alreadyPlotted))){
                needVP <- nam[which(is.na(alreadyPlotted))]
              } else {
                needVP <- NULL
              }

              # check to see if new addition can fit without rearranging
              if(length(c(currentGrobs, needVP))>prod(.arr$columns,.arr$rows)){
                needVP = c(currentGrobs, needVP)
                arr <- arrangePlots(obj,
                                    nam=needVP)
                needRearrange <- TRUE
                if(exists(".grobs",envir=.GlobalEnv)) {
                  grobs = .grobs
                } else {
                  stop("Cannot add to plot; there are no recorded plots")
                }
                grid.newpage()
              } else {
                arr = .arr
                arr$names <- c(arr$names,needVP)
              }
              assign(".arr", arr, envir=.GlobalEnv)
            }

            # Assess optimal number of pixels to plot, optimized for speed, maintaining visibility
            npixels <- sapply(toPlot[rasters], ncell)
            maxpixels <- 1e3/(arr$columns*arr$rows)*prod(arr$ds)/speedup
            maxpixels <- c(maxpixels,npixels)[(npixels/2<maxpixels)+1]
            if(is.null(gp$cex)) {
              gp$cex <- cex <- max(0.4,min(1,prod(arr$ds)/prod(arr$columns,arr$rows)*0.1))
            }

            if (!is.null(needVP)) {
              vps <- makeViewports(obj, arr, cex=cex, add=add, visualSqueeze=visualSqueeze,
                                   needRearrange=needRearrange)
              if(add & !needRearrange)
                upViewport(1)
              pushViewport(vps,recording = FALSE)
              upViewport(2)
            }


            if (needRearrange){
              for(i in 1:length(needVP)) {
                i2 = match(needVP,nam)[i]

                if(axes=="L") {if(arr$cr[i,"rows"]==min(arr$cr[,"rows"])) { xaxis = TRUE } else { xaxis = FALSE}
                               if(arr$cr[i,"columns"]==min(arr$cr[,"columns"])) { yaxis = TRUE } else { yaxis = FALSE}}
                if(axes==TRUE) { xaxis = TRUE ; yaxis = TRUE}
                if(axes==FALSE) { xaxis = FALSE ; yaxis = FALSE}
                title=TRUE

                if(is.null(addTo)) {
                  seekViewport(paste("vp",needVP[i],sep=""),recording=F)#(!quick | !add))
                } else {
                  seekViewport(paste("vp",addTo[i2],sep=""),recording=F)#(!quick | !add))
                }

                if(all(i != match(nam,needVP))) {
                  grid.draw(grobs[[i]])
                } else {
                  if(rasters[i2]) {
                    grobs[[i]] <- plotRast(obj[[i2]], col = cols[[i2]], add=TRUE, vp=NULL,
                                         xaxis = xaxis, yaxis = yaxis, title=title,
                                         maxpixels= maxpixels,
                                         legend = legend, gp = gp, draw = draw)
                  } else {
                    grobs[[i]] <- plotPoints(toPlotSpatialPoints[i2], pch=pch, size=unit(size,"points"),
                                           add=TRUE, vp=NULL,
                                           xaxis = xaxis, yaxis = yaxis, title=title,
                                           gp = gp, draw = draw)
                  }
                }

              }
            } else {
              for(i in match(nam,arr$names)) {
                i2 <- match(arr$names[i],nam)

                if(is.null(addTo)) {
                  seekViewport(paste("vp",needVP[i],sep=""),recording=F)#(!quick | !add))
                } else {
                  seekViewport(paste("vp",addTo[i2],sep=""),recording=F)#(!quick | !add))
                }

                if(nam[[i2]] %in% needVP) {
                  if(axes=="L") {if(arr$cr[i,"rows"]==min(arr$cr[,"rows"])) { xaxis = TRUE } else { xaxis = FALSE}
                                 if(arr$cr[i,"columns"]==min(arr$cr[,"columns"])) { yaxis = TRUE } else { yaxis = FALSE}}
                  if(axes==TRUE) { xaxis = TRUE ; yaxis = TRUE}
                  if(axes==FALSE) { xaxis = FALSE ; yaxis = FALSE}
                  title=TRUE
                } else {
                  xaxis=FALSE; yaxis=FALSE; legend = FALSE; title=FALSE
                }


                if(rasters[i2]) {
                  grobs[[i]] <- plotRast(obj[[i2]], col = cols[[i2]], add=TRUE, vp=NULL,
                                       xaxis = xaxis, yaxis = yaxis, title=title,
                                       maxpixels= maxpixels,
                                       legend = legend, gp = gp, draw = draw)
                } else {

                  grobs[[i]] <- plotPoints(toPlotSpatialPoints[i2], pch=pch,size=unit(size,"points"),
                                           add=TRUE, vp=NULL,
                                           xaxis = xaxis, yaxis = yaxis, title=title,
                                           gp = gp, draw = draw)
                }

              }
            }
            assign(".grobs",grobs,envir=.GlobalEnv)
            #assign(".grobs", grobs, pos="package:SpaDES")
            return(invisible(grobs))
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

