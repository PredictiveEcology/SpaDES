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
#' @rdname arrangeSimPlots
#' @export
#' @docType methods
setGeneric("arrangeSimPlots", function(toPlot, nam=NULL, which.to.plot="all", axes="L") {
  standardGeneric("arrangeSimPlots")
})

#' @rdname arrangeSimPlots
#' @export
setMethod("arrangeSimPlots",
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

##############################################################
#' Plotting methods
#'
#' Plotting for RasterStack, Agents and other classes used in simulations.
#'
#' For class \code{RasterStack}, this method plots many rasters. This method automatically plots individual
#' rasters in with an arrangement on the plotting window that is optimal for the size and shape of
#' the window. \code{Speedup} will make downsample the number of pixels, allowing for greater plotting
#' speed, at a cost of more fuzzy plots.
#'
#' @param x rasterStack, rasterLayer, or list of named rasters (not implemented yet), SpatialPoints* object.
#'
#' @param on.which.to.plot when add=TRUE, numeric or character string identifying on which raster in existing plot window to plot. Used on \code{RasterLayer} and \code{pointAgent} and \code{mobileAgent}. Defaults to 1.
#'
#' @param which.to.plot Numeric or character vector identifying which rasters in \code{rasterStack} to plot.
#'
#' @param col a colour palette vector, possibly from RColorBrewer. Defaults to topo.colors(40)
#'
#' @param visualSqueeze numeric index (from 0 to 1) that indicates how tightly the rasters should be plotted next
#' to each other. Default is 0.75, which allows for legends.
#'
#' @param ... Additional plotting functions passed to grid.raster (if rasterStack) or grid.points (if pointAgent)
#'
#' @param add Logical indicating whether to plot new maps (\code{FALSE}) or update exising maps (\code{TRUE}).
#' Default is \code{FALSE} for rasters and \code{TRUE} for agents.
#'
#' @param speedup Scalar indicating how much faster than normal to make plots (see Details). Defaults to 1.
#'
#' @param axes String either "all", "L", or "none" (see Details). Default is "L".
#'
#' @return Creates a plot within the active plotting device.
#'
#' @seealso \code{\link{grid.raster}}
#'
#' @import grid raster sp
#' @export
#' @docType methods
#' @rdname simPlot
#'
# @examples
# needs examples
setGeneric("simPlot", function(x, on.which.to.plot=1, which.to.plot="all",
                               col=rev(terrain.colors(255)), visualSqueeze=0.75, ..., add=FALSE, speedup=1,
                               axes="L", add.legend=TRUE) {
  standardGeneric("simPlot")
})

#' @aliases simPlot
#' @export
#' @rdname simPlot
setMethod("simPlot",
          signature="RasterStack",
          definition=function(x, on.which.to.plot, which.to.plot="all", col, visualSqueeze, ...,
                              add, speedup, axes, add.legend=TRUE) {
            nam <- names(x)
            ext <- extent(x)
            #              ext.ratio <- diff(c(xmin(ext),xmax(ext)))/diff(c(ymin(ext),ymax(ext)))
            dimx <- dim(x)

            if(!is.list(col)) col <- as.list(data.frame(matrix(rep(col,dimx[3]), ncol=dimx[3]), stringsAsFactors=FALSE))

            if (add==FALSE) {
              arr <- arrangeSimPlots(x, which.to.plot, axes,...)

              vp <- list()
              grid.newpage()

              with(arr, {
                for (w in wh) {
                  if (is.numeric(w)) w <- nam[w]
                  ma <- match(w,nam)
                  if(is.numeric(wh)) i <- match(ma,wh) else i <- match(nam[ma],wh)

                  vp[[i]] <- viewport(x=cr[i,"columns"], y=cr[i,"rows"],
                                      width=min(1/columns*visualSqueeze,1/columns*visualSqueeze/(ds.map.ratio/actual.ratio)),
                                      height=min(1/rows*visualSqueeze,1/rows*visualSqueeze/(actual.ratio/ds.map.ratio)),
                                      just="centre",
                                      name=w,
                                      xscale=c(xmin(ext),xmax(ext)),
                                      yscale= c(ymin(ext), ymax(ext)))
                  pushViewport(vp[[i]])
                  if (axes != "none" & axes != FALSE) {
                    if (axes == "L") {
                      if (cr$columns[i]==min(cr$columns)) {
                        grid.yaxis(gp=gpar(cex=0.5))
                        #grid.yaxis(gp=gpar(cex=0.5), at=prettys[["y"]]/max(1,actual.ratio/ds.map.ratio),
                        #           label=prettys[["y"]],name=paste(w,"yaxis",sep=""))
                      }
                      if (cr$rows[i] == min(cr$rows)) {
                        #grid.xaxis(gp=gpar(cex=0.5), at=prettys[["x"]]/max(1,ds.map.ratio/actual.ratio), label=prettys[["x"]])
                        grid.xaxis(gp=gpar(cex=0.5))
                        #                                   grid.xaxis(gp=gpar(cex=0.5), at=prettys[["x"]]/min(1,ds.map.ratio/actual.ratio),
                        #                                              label=prettys[["x"]],name=paste(w,"xaxis",sep=""))
                      }
                    } else {
                      grid.xaxis(gp=gpar(cex=0.5))#, at=prettys[["x"]]/max(1,ds.map.ratio/actual.ratio),
                      #           label=prettys[["x"]],name=paste(w,"xaxis",sep=""))
                      grid.yaxis(gp=gpar(cex=0.5))#, at=prettys[["y"]]/max(1,actual.ratio/ds.map.ratio),
                      #           label=prettys[["y"]],name=paste(w,"yaxis",sep=""))
                    }
                  }
                  grid.text(names(x)[ma], y=1.08, vjust=0.5, gp=gpar(cex=1-0.015*length(wh)),
                            name=paste(w,"title",sep=""))
                  grid.raster(as.raster(x[[w]], maxpixels=1e3/(columns*rows)*prod(ds)/speedup,
                                        col=col[[ma]] ),
                              interpolate=FALSE, name=w,...)
                  if (add.legend){
                    #upViewport()
                    grid.raster(as.raster(col[[i]][length(col[[i]]):1] ),
                                x=1.04,y=0.5,height=0.5,width=0.03,
                                interpolate=TRUE)
                    pr <- pretty(range(minValue(x[[w]]),maxValue(x[[w]])))
                    pr <- pr[pr<maxValue(x[[w]])]
                    grid.text(pr, x=1.08, y=pr/(2*maxValue(x[[w]]))+0.25,
                              gp=gpar(cex=max(0.5, 1-0.05*length(wh))),
                              just="left", ...)
                  }

                  upViewport()
                }
              })

            } else if (add==TRUE){
              vp.names= grid.ls(grobs=FALSE, viewports=TRUE, recursive=TRUE, print=FALSE)$name
              vp.names= vp.names[match(unique(vp.names[1:trunc(length(vp.names)/2)*2]),vp.names)]
              #                       #                  vp.names= vp.names[(1:trunc(length(vp.names)/2))*2]
              for (i in which.to.plot) {

                if (is.numeric(i)) i <- nam[i]#match(nam,vp.names)
                seekViewport(i)
                grid.remove(i)
                grid.raster(as.raster(x[[i]],maxpixels=1e3/(length(vp.names))*prod(dev.size())/speedup,
                                      col=col[[ma]] ),
                            interpolate=FALSE,name=names(x[[i]]),...)
                upViewport()
              }
            } else {
              stop("Error: Logical `add` should be TRUE or FALSE.")
            }
          })


#' @aliases simPlot
#' @export
#' @rdname simPlot
setMethod("simPlot",
          signature="RasterLayer",
          definition=function(x, on.which.to.plot, which.to.plot, col, visualSqueeze, delete.previous=TRUE, ...,
                              add, speedup, axes, add.legend=TRUE) {
            ext <- extent(x)
            if (add==TRUE) {
              wh=which(names(x)==grid.ls(grobs=FALSE, viewports=TRUE, recursive=TRUE, print=FALSE)$name)
              if(length(wh)>1) {
                nam <- paste(names(x), length(wh)+1, sep="")
              } else {
                nam <- names(x)
              }
            } else {
              nam <- names(x)
            }
            dimx <- dim(x)

            if(!is.list(col)) col <- as.list(data.frame(matrix(rep(col,dimx[3]), ncol=dimx[3]), stringsAsFactors=FALSE))

            if (add==FALSE) {
              arr <- arrangeSimPlots(x,which.to.plot=1,axes=axes,...)
              with (arr, {
                grid.newpage()
                vp <- viewport(x=cr[1,"columns"], y=cr[1,"rows"],
                               width=1/columns*visualSqueeze/(ds.map.ratio/actual.ratio),
                               height=1/rows*visualSqueeze, just="centre", name=nam,
                               xscale=c(xmin(ext), xmax(ext)), yscale=c(ymin(ext), ymax(ext)))

                #                   vp <- viewport(width=0.8, height=0.8,
                #                                       just=c(0.5, 0.5),
                #                                       name=nam,
                #                                       xscale=c(xmin(ext),xmax(ext)),yscale= c(ymin(ext),ymax(ext)))
                pushViewport(vp)
                if (axes != "none" & axes != FALSE) {
                  if (axes == "L") {
                    grid.yaxis()#gp=gpar(cex=0.5), at=prettys[["y"]]/max(1,actual.ratio/ds.map.ratio),
                    #                                              label=prettys[["y"]],name=paste(w,"yaxis",sep=""))
                  }
                  #                                  if (cr$rows[i] == min(cr$rows)) {
                  #grid.xaxis(gp=gpar(cex=0.5), at=prettys[["x"]]/max(1,ds.map.ratio/actual.ratio), label=prettys[["x"]])
                  grid.xaxis()#gp=gpar(cex=0.5), at=prettys[["x"]]/max(1,ds.map.ratio/actual.ratio),
                  #                                                label=prettys[["x"]],name=paste(w,"xaxis",sep=""))
                  #                                }
                } else {
                  grid.xaxis()#gp=gpar(cex=0.5), at=prettys[["x"]]/max(1,ds.map.ratio/actual.ratio),
                  #                                             label=prettys[["x"]],name=paste(w,"xaxis",sep=""))
                  grid.yaxis()#gp=gpar(cex=0.5), at=prettys[["y"]]/max(1,actual.ratio/ds.map.ratio),
                  #                                            label=prettys[["y"]],name=paste(w,"yaxis",sep=""))
                }

                grid.text(y=1.08, vjust=0.5, gp=gpar(cex=1-0.015), label=nam)
                grid.raster(as.raster(x, maxpixels=1e3*prod(dev.size())/speedup,
                                      col=col[[1]]), interpolate=FALSE, name=nam, ...)
                #upViewport()
              })
              if (add.legend){
                #upViewport()
                grid.raster(as.raster(col[[1]][length(col[[1]]):1] ),
                            x=1.04,y=0.5,height=0.5,width=0.03,
                            interpolate=TRUE)
                pr <- pretty(range(minValue(x),maxValue(x)))
                pr <- pr[pr<maxValue(x)]
                grid.text(pr, x=1.08, y=pr/(2*maxValue(x))+0.25, just="left", ...)

              }


            } else if (add==TRUE){
              vp.names= grid.ls(grobs=FALSE, viewports=TRUE, recursive=FALSE, flatten=TRUE, print=FALSE)$name
              vp.names= vp.names[match(unique(vp.names[1:trunc(length(vp.names)/2)*2]),vp.names)]

              if (is.numeric(on.which.to.plot)) {
                i <- vp.names[on.which.to.plot]
              } else {
                i <- on.which.to.plot
              }
              seekViewport(i)

              if (delete.previous) grid.remove(i)
              grid.raster(as.raster(x,maxpixels=1e3/(length(vp.names))*prod(dev.size())/speedup,
                                    col=col[[1]]),interpolate=FALSE,
                          name=nam,...)
              upViewport(0)

            } else {
              stop("Error: Logical `add` should be TRUE or FALSE.")
            }
          })


#' @param ext an extent object to describe the size of the map that is being plotted on
#'
#' @param delete.previous should the immediately previously simPlotted object be removed before adding current simPlot call
#'
#' @param max.agents is the maximum number of agents to plot. \code{speedup} reduces the number plotted from this max.agents
#'
#' @aliases simPlot
#' @export
#' @rdname simPlot
setMethod("simPlot",
          signature="SpatialPoints",
          definition=function(x, on.which.to.plot=1, which.to.plot, visualSqueeze=0.75, delete.previous=TRUE,
                              max.agents=1e4, ..., add=TRUE, speedup, axes, add.legend ) {
            #ext <- extent(x)
            len <- length(x)
            if (len>max.agents) {
              sam <- sample.int(len,size=max.agents,replace=FALSE)
              len <- max.agents
            } else {
              sam=1:len
            }
            if(length(len)==1) speed.keep=1:len else speed.keep=sam
            if(speedup != 1) {
              speed.keep <- sample(sam,len/speedup,replace=FALSE)
            }

            x1 <- coordinates(x)[speed.keep,"x"]
            y1 <- coordinates(x)[speed.keep,"y"]
            rangex <- range(x1)
            rangey <- range(y1)

            if (add==FALSE) {
              arr <- arrangeSimPlots(raster(extent(c(rangex,rangey))),1,axes="L")

              grid.newpage()
              with(arr, {
                vp <- viewport(xscale=rangex, yscale=rangey,
                               width=visualSqueeze,height=visualSqueeze,
                               name=paste(deparse(substitute(x))))
                pushViewport(vp)
                grid.points(x1/max(1,ds.map.ratio/actual.ratio),y1/max(1,actual.ratio/ds.map.ratio),
                            name=deparse(substitute(x)) ,...)
                ats <- list()
                prettys <- list()
                ats[["x"]] <- rangex/max(1,ds.ratio/actual.ratio)
                ats[["y"]] <- rangey/max(1,actual.ratio/ds.ratio)
                prettys[["x"]] <- pretty(ats[["x"]])
                prettys[["y"]] <- pretty(ats[["y"]])

                grid.yaxis(gp=gpar(cex=0.5), at=prettys[["y"]]/max(1,actual.ratio/ds.map.ratio),
                           label=prettys[["y"]],name=paste(deparse(substitute(x)),"yaxis",sep=""))
                grid.xaxis(gp=gpar(cex=0.5), at=prettys[["x"]]/max(1,ds.map.ratio/actual.ratio),
                           label=prettys[["x"]],name=paste(deparse(substitute(x)),"xaxis",sep=""))

                #                     grid.xaxis(gp=gpar(cex=0.5), at=seq(ats[["x"]][1], ats[["x"]][2], length.out=length(prettys[["x"]])), label=prettys[["x"]])
                #                     grid.yaxis(gp=gpar(cex=0.5), at=seq(ats[["y"]][1], ats[["y"]][2], length.out=length(prettys[["y"]])), label=prettys[["y"]])
                upViewport(0)
              })
              #                grid.yaxis(gp=gpar(cex=0.5), at=pretty(rangey/max(1, actual.ratio/ds.ratio)), label=pretty(rangey))
            } else { #add=TRUE
              vp.names <- grid.ls(grobs=FALSE, viewports=TRUE, recursive=TRUE, print=FALSE)$name
              vp.names <- vp.names[match(unique(vp.names[1:trunc(length(vp.names)/2)*2]),vp.names)]

              for (k in 1:length(on.which.to.plot)) {
                upViewport(0)
                if(is.numeric(on.which.to.plot[k])) {
                  vp.to.plot <- vp.names[on.which.to.plot[k]]
                  seekViewport(vp.to.plot)
                } else {
                  vp.to.plot <- on.which.to.plot[k]
                  seekViewport(vp.to.plot)
                }

                grid.points(x1,y1, name=deparse(substitute(x)), ...)
              }
            }
          })
