###################################################################
###
###     Methods for the ABM simulation: "observer" module:
###
###     All plotting and graphics (via event calls);
###         - stats to file or to screen (toggle)
###     
###################################################################


# Notes to self (Eliot)... 
#DONE 1. fix when rasters are not square... need equivalent to eqscplot
# 2. use arrange.simplot for pointAgent
# 3. allow plotting of legends



##############################################################
#' Open a new plotting window
#'
#' Launch a new graphics device based on operating system used.
#' Mac OS: open device with \code{quartz()}.
#' Linux: open device with \code{x11()}.
#' Windows: open device with \code{windows()}.
#' 
#' @return Opens a new plot device on the screen.
#' 
#' @export
#' @docType methods
#' @rdname newPlot
#'
# @examples
# needs examples
newPlot = function() {
    if (Sys.info()[["sysname"]]=="Darwin") {
        quartz()
    } else if (Sys.info()[["sysname"]]=="Linux") {
        x11()
    } else if (Sys.info()[["sysname"]]=="Windows") {
        windows()
    } else {
        x11() # try x11() to see if it works
        print("Which operating system are you using?")
    }
}


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
#' @param x rasterStack object.
#' 
#' @param ... Additional plotting functions passed to grid.raster (if rasterStack) or grid.points (if pointAgent)
#'
#' @return Creates a plot within the active plotting device.
#' 
#' #@seealso \code{\link{grid.raster}}
#' 
#' @import grid raster
#' @export
#' @docType methods
#' @rdname simplot
#'
# @examples
# needs examples
setGeneric("simplot", function(x, ...) {
           standardGeneric("simplot")
})



#' @param which.to.plot Numeric or character vector identifying which rasters in \code{rasterStack} to plot.
#' 
#' @param speedup Scalar indicating how much faster than normal to make plots (see Details).
#' 
#' @param axes String either "all", "L", or "none" (see Details). Default is "L".
#' 
#' @param add Logical indicating whether to plot new maps (\code{FALSE}) or update exising maps (\code{TRUE}).
#' Default is \code{FALSE}.
#' 
#' @aliases simplot,RasterStack
#' @rdname simplot
setMethod("simplot",
          signature = "RasterStack",
          definition = function(x, which.to.plot="all", speedup=10, axes="L", add=FALSE, ...) {
              nam = names(x)
              ext = extent(x)
#              ext.ratio = diff(c(xmin(ext),xmax(ext)))/diff(c(ymin(ext),ymax(ext)))
              dimx = dim(x)
              
              if (add==FALSE) {
                 arr = arrange.simplots(ext,dimx,nam,which.to.plot,axes,...)    
                 
                 vp = list()
                 grid.newpage()
                 
                 with(arr, {
                     for (w in wh) {
                      if (is.numeric(w)) w = nam[w]
                      ma = match(w,nam)
                      if(is.numeric(wh)) i = match(ma,wh) else i = match(nam[ma],wh)
                      
                      vp[[i]] <- viewport(x=cr[i,"cols"], y=cr[i,"rows"], w=1/cols*0.8, h=1/rows*0.8,
                                          just = c(0.5, 0.5),
                                          name = w,
                                          xscale = c(xmin(ext),xmax(ext)),yscale= c(ymin(ext),ymax(ext)))
                      pushViewport(vp[[i]])
                      if (axes != "none" & axes != FALSE) {
                          if (axes == "L") {
                              if (cr$cols[i]==min(cr$cols)) {
                                  #grid.yaxis(gp=gpar(cex=0.5), at=prettys[["y"]])#/max(1,actual.ratio/ds.map.ratio), label=prettys[["y"]])
                                  grid.yaxis(gp=gpar(cex=0.5), at=prettys[["y"]]/max(1,actual.ratio/ds.map.ratio), 
                                             label=prettys[["y"]],name=paste(w,"yaxis",sep=""))
                              }
                              if (cr$rows[i] == min(cr$rows)) {
                                  #grid.xaxis(gp=gpar(cex=0.5), at=prettys[["x"]]/max(1,ds.map.ratio/actual.ratio), label=prettys[["x"]])
                                  grid.xaxis(gp=gpar(cex=0.5), at=prettys[["x"]]/max(1,ds.map.ratio/actual.ratio), 
                                             label=prettys[["x"]],name=paste(w,"xaxis",sep=""))
                              }
                          } else {
                              grid.xaxis(gp=gpar(cex=0.5), at=prettys[["x"]]/max(1,ds.map.ratio/actual.ratio), 
                                         label=prettys[["x"]],name=paste(w,"xaxis",sep=""))
                              grid.yaxis(gp=gpar(cex=0.5), at=prettys[["y"]]/max(1,actual.ratio/ds.map.ratio), 
                                         label=prettys[["y"]],name=paste(w,"yaxis",sep=""))
                          }
                      }
                      grid.text(names(x)[ma], y=1.08, vjust=0.5, gp=gpar(cex=1-0.015*length(wh)),
                                name = paste(w,"title",sep=""))
                      grid.raster(as.raster(x[[w]],maxpixels=1e4/(cols*rows)*prod(ds)/speedup),
                                  name=w,...)
                      upViewport()
                    }
                 })
              } else if (add==TRUE){
                    vp.names= grid.ls(grobs=F, viewports=TRUE, recursive=TRUE, print=FALSE)$name
                    vp.names= vp.names[match(unique(vp.names[1:trunc(length(vp.names)/2)*2]),vp.names)]
#                       #                  vp.names= vp.names[(1:trunc(length(vp.names)/2))*2]
                    for (i in which.to.plot) {
    
                      if (is.numeric(i)) i = nam[i]#match(nam,vp.names)
                      seekViewport(i)
                      grid.remove(i)
                      grid.raster(as.raster(x[[i]],maxpixels=1e4/(length(vp.names))*prod(dev.size())/speedup),
                                  name=names(x[[i]]),...)
                      upViewport()
                  }
              } else {
                  stop("Error: Logical `add` should be TRUE or FALSE.")
              }
})


#' @param on.which.to.plot when add = TRUE, which map to plot on
#' @aliases simplot,pointAgent
#' @import graphics
#' @rdname simplot
setMethod("simplot",
          signature = "pointAgent",
          definition = function(x, on.which.to.plot=1, ext,map.names = NULL, speedup=1, delete.previous = T,
                                axes="L", max.agents = 1e4, add=TRUE,... ) {
              len = length(x)
              if (len>max.agents) {
                  sam = sample.int(len,size=max.agents,replace=F) 
                  len = max.agents
              } else {
                  sam=1:len
              }
              if(length(len)==1) speed.keep=1:len else speed.keep=sam
              if(speedup != 1) {
                  speed.keep = sample(sam,len/speedup,replace=F)
              } 
              
              x1 = coordinates(x)[speed.keep,"x"]
              y1 = coordinates(x)[speed.keep,"y"]
              rangex = range(x1)
              rangey = range(y1)

#              if(!exists("gp1")){if (exists("cex")) {gp1 = gpar(cex = cex);rm(cex)} else {gp1=gpar()}}

              if (add==FALSE) {
                arr = arrange.simplots(extent(c(rangex,rangey)),1,deparse(substitute(x)),1,axes="L")    

                grid.newpage()
                with(arr, {
                    vp = viewport(xscale = rangex,yscale= rangey,w=0.8,h=0.8,
                                  name=paste(deparse(substitute(x))))
                    pushViewport(vp)
                    grid.points(x1/max(1,ds.map.ratio/actual.ratio),y1/max(1,actual.ratio/ds.map.ratio),
                                name=deparse(substitute(x)) ,...)  
                    ats = list()
                    prettys = list()
                    ats[["x"]] = rangex/max(1,ds.ratio/actual.ratio)
                    ats[["y"]] = rangey/max(1,actual.ratio/ds.ratio)
                    prettys[["x"]] = pretty(ats[["x"]])
                    prettys[["y"]] = pretty(ats[["y"]])

                    grid.yaxis(gp=gpar(cex=0.5), at=prettys[["y"]]/max(1,actual.ratio/ds.map.ratio), 
                               label=prettys[["y"]],name=paste(deparse(substitute(caribou)),"yaxis",sep=""))
                    grid.xaxis(gp=gpar(cex=0.5), at=prettys[["x"]]/max(1,ds.map.ratio/actual.ratio), 
                               label=prettys[["x"]],name=paste(deparse(substitute(caribou)),"xaxis",sep=""))
                    
#                     grid.xaxis(gp=gpar(cex=0.5),at = seq(ats[["x"]][1],ats[["x"]][2],length.out=length(prettys[["x"]])),label = prettys[["x"]])
#                     grid.yaxis(gp=gpar(cex=0.5),at = seq(ats[["y"]][1],ats[["y"]][2],length.out=length(prettys[["y"]])),label = prettys[["y"]])
                    upViewport()
                })
#                grid.yaxis(gp=gpar(cex=0.5),at = pretty(rangey/max(1,actual.ratio/ds.ratio)),label = pretty(rangey))
              } else { #add=T
                  if(is.null(map.names)) {
                      vp.names= grid.ls(grobs=FALSE, viewports=TRUE, recursive=TRUE, print=FALSE)$name
                      vp.names= vp.names[match(unique(vp.names[1:trunc(length(vp.names)/2)*2]),vp.names)]
#                      vp.names= vp.names[1:trunc(length(vp.names)/2)*2]
                  } else {
                      vp.names = map.names
                  }
                  arr = arrange.simplots(ext,length(vp.names),
                                         deparse(substitute(x)),1:length(vp.names),axes="L")    
                  for (k in 1:length(on.which.to.plot)) {
                    if(is.numeric(on.which.to.plot[k])) {
                      vp.to.plot=vp.names[on.which.to.plot[k]]
                      seekViewport(vp.to.plot)
                    } else {
                        vp.to.plot = on.which.to.plot[k]
                      seekViewport(vp.to.plot)
                    }
                    
                    with(arr,{
                        if (delete.previous) {grob.names = grid.ls(grobs=TRUE, recursive=TRUE, print=FALSE)$name
                           if(any(grob.names==deparse(substitute(x))))
                              grid.remove(deparse(substitute(x)))
                        }                    
                        grid.points(x1/max(1,ds.map.ratio/actual.ratio),y1/max(1,actual.ratio/ds.ratio),
                                    name = deparse(substitute(x)), ...)  
#                    grid.points(x1,y1,gp=gpar(cex=0.4),pch=19,...)  
                    })
                    upViewport()
                }
              }

})


#' @param ... additional plotting functions passed to plot or points
#' @param on.which.to.plot when add = T, which map to plot on
#' @import raster
#' @rdname simplot
setMethod("simplot",
          signature = "raster",
          definition = function(x, on.which.to.plot=1, speedup=100, axes="L", add=FALSE, ...) {
              if (add==FALSE) {
                  plot(x, type="p", ...)
              } else {
                  plot(x, ...)
              }
})

#' Determine optimal plotting arrangement of RasterStack
#'
#' Hidden function.
#' 
#' This assesses the device geometry, the map geometry, and the number of rasters
#' to plot and builds an object that will be used by the simPlot functions to plot
#' them efficiently 
#' 
#' @param ext extent object
#' @param dimx dimension of rasterStack
#' @param nam names in rasterStack
#' @param which.to.plot vector of numbers or names in rasterStack to plot
#' @param axes passed from simplot 
#' @rdname arrange.simplots
#' @importMethodsFrom Hmisc llist
arrange.simplots = function(ext,dimx,nam,which.to.plot,axes="L") {
    
    if (length(which.to.plot)==1) {
        if(which.to.plot=="all")
            wh = 1:dimx[3]
        else
            wh = which.to.plot
    } else {
        wh = which.to.plot
    }
    if (is.character(wh)) if (any(is.na(match(wh, nam)))) stop("Not a named map in rasterx")
    
    if(dev.cur()==1) {
        dev.new(height=8, width=10)
    }
    
    ds = dev.size()
    ds.ratio = ds[1]/ds[2]
    
    map.ratio = (xmax(ext)-xmin(ext))/(ymax(ext)-ymin(ext))
    
    ds.map.ratio = ds.ratio/map.ratio
    
    col.by.row = data.frame(matrix(ncol=2, nrow=length(wh)))
    
    col.by.row[,1] = ceiling(length(wh)/(1:length(wh)))
    col.by.row[,2] = ceiling(length(wh)/col.by.row[,1])
    
    
    wh.best = which.min(abs(apply(col.by.row,1,function(x) x[1]/x[2]) - ds.map.ratio))
    
    cols = col.by.row[wh.best,1]
    rows = col.by.row[wh.best,2]
    
    actual.ratio = cols/rows
    
    if (axes != "none" & axes != FALSE) {
        prettys = list()
        prettys[["x"]] = pretty(c(xmin(ext),xmax(ext)))
        prettys[["y"]] = pretty(c(ymin(ext),ymax(ext)))
        prettys[["x"]] = prettys[["x"]][which(prettys[["x"]]>=xmin(ext) & prettys[["x"]]<=xmax(ext))]
        prettys[["y"]] = prettys[["y"]][which(prettys[["y"]]>=ymin(ext) & prettys[["y"]]<=ymax(ext))]
    }
    
    cr = expand.grid(cols=((1:cols/cols - 1/cols/2)-0.55)*0.9+0.55,rows=((1:rows/rows - 1/rows/2)-0.55)*0.9+0.55)
    out = llist(cr,rows,cols,actual.ratio,ds.map.ratio,ds,prettys,wh,ds.ratio)
    return(out)
}
