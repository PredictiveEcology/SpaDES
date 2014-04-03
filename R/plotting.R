###################################################################
###
###     Methods for the ABM simulation: "observer" module:
###
###     All plotting and graphics (via event calls);
###         - stats to file or to screen (toggle)
###     
###################################################################


# Notes to self... 
# 1. fix when rasters are not square... need equivalent to eqscplot
# 2. raster and points are not sized exactly the same... still



##############################################################
#' Ploting methods
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
              
              if (length(which.to.plot)==1) {
                  if(which.to.plot=="all")
                      wh = 1:dim(x)[3]
                  else
                      wh = which.to.plot
              } else {
                  wh = which.to.plot
              }
              if (is.character(wh)) if (any(is.na(match(wh, names(x))))) stop("Not a named map in rasterx")
              
              ext = extent(x)
              if(dev.cur()==1) {
                  dev.new(height=8, width=10)
              }
              
              ds = dev.size()
              ds.ratio = ds[1]/ds[2]
              
              if (add==FALSE) {
                  col.by.row = data.frame(matrix(ncol=2, nrow=length(wh)))
                  
                  col.by.row[,1] = ceiling(length(wh)/(1:length(wh)))
                  col.by.row[,2] = ceiling(length(wh)/col.by.row[,1])
                  
                  wh.best = which.min(abs(apply(col.by.row,1,function(x) x[1]/x[2]) - ds.ratio))
                  
                  cols = col.by.row[wh.best,1]
                  rows = col.by.row[wh.best,2]
                  
                  actual.ratio = cols/rows
                  
#                   row.col = min(rows/cols*ds[1]/ds[2] , 1)
#                   col.row = min(cols/rows*ds[2]/ds[1] , 1)
                  
                  
                  #            if (add == FALSE) {
                  vp = list()
                  ats = list()
                  grid.newpage()
                  if (axes != "none" & axes != FALSE) {
                      ats[["x"]] = signif(seq(xmin(ext),xmax(ext),length.out=5),4)
                      ats[["y"]] = signif(seq(ymin(ext),ymax(ext),length.out=5),4)
                      
                  }
                  
                  cr = expand.grid(cols=((1:cols/cols - 1/cols/2)-0.55)*0.9+0.55,rows=((1:rows/rows - 1/rows/2)-0.55)*0.9+0.55)
                  
                  for (w in wh) {
                      if (is.numeric(w)) w = nam[w]
                      ma = match(w,nam)
                      if(is.numeric(wh)) i = match(ma,wh) else i = match(nam[ma],wh)
                      
                      
                      vp[[i]] <- viewport(x=cr[i,"cols"], y=cr[i,"rows"], w=1/cols*0.8, h=1/rows*0.8,
                                          just = c(0.5, 0.5),
                                          name = w,
                                          xscale = c(xmin(ext),xmax(ext)),yscale= c(ymin(ext),ymax(ext)))
                      pushViewport(vp[[i]])
                      grid.raster(as.raster(x[[w]],maxpixels=1e4/(cols*rows)*prod(ds)/speedup))#,...)
                      if (axes != "none" & axes != FALSE) {
                          if (axes == "L") {
                              if (cr$cols[i]==min(cr$cols)) {
                                  grid.yaxis(gp=gpar(cex=0.5), at=ats[["y"]]/max(1,actual.ratio/ds.ratio), label=ats[["y"]])
                              }
                              if (cr$rows[i] == min(cr$rows)) {
                                  grid.xaxis(gp=gpar(cex=0.5), at=ats[["x"]]/max(1,ds.ratio/actual.ratio), label=ats[["x"]])
                              }
                          } else {
                              grid.xaxis(gp=gpar(cex=0.5), at=ats[["x"]], label=ats[["x"]])
                              grid.yaxis(gp=gpar(cex=0.5), at=ats[["y"]], label=ats[["y"]])
                          }
                      }
                      grid.text(names(x)[ma], y=1.05, vjust=0.5, gp=gpar(cex=1-0.015*length(wh)))
                      upViewport()
                  }
              } else if (add==TRUE){
                  for (i in wh) {
                      vp.names= grid.ls(grobs=FALSE, viewports=TRUE, recursive=TRUE, print=FALSE)$name
                      vp.names= vp.names[match(unique(vp.names[1:trunc(length(vp.names)/2)*2]),vp.names)]
#                       #                  vp.names= vp.names[(1:trunc(length(vp.names)/2))*2]
# 
#                       vp.names= vp.names[1:30*2]
#                      vp.names = sapply(current.vpTree()$children, function(x) x$name)
                      if (is.numeric(i)) i = nam[i]#match(nam,vp.names)
                      seekViewport(i)
                      grid.raster(as.raster(x[[i]],maxpixels=1e4/(length(vp.names))*prod(ds)/speedup),...)
                      upViewport()
                  }
              } else {
                  stop("Error: Logical `add` should be TRUE or FALSE.")
              }
})


<<<<<<< HEAD
#' @param on.which.to.plot when add = T, which map to plot on
#' @aliases simplot,pointAgent
=======
#' @param on.which.to.plot when add = TRUE, which map to plot on
#' @import graphics
>>>>>>> 68f14f6b15a7e235b3bf1558c4564169d2e61084
#' @rdname simplot
setMethod("simplot",
          signature = "pointAgent",
          definition = function(x, on.which.to.plot=1, map.names=NULL,speedup=1, 
                                axes="L", max.agents = 1e4, add=TRUE,pch=19, cex=0.2, ... ) {
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

              if(!exists("gp1")){if (exists("cex")) {gp1 = gpar(cex = cex);rm(cex)} else {gp1=gpar()}}

              ds = dev.size()
              ds.ratio = ds[1]/ds[2]
              
              if(is.null(map.names)) {
                  vp.names= grid.ls(grobs=F,viewports = T,recursive=T,print=F)$name
                  vp.names= vp.names[match(unique(vp.names[1:trunc(length(vp.names)/2)*2]),vp.names)]
#                  vp.names= vp.names[(1:trunc(length(vp.names)/2))*2]
              } else {
                  vp.names = map.names
              }
              col.by.row = data.frame(matrix(ncol = 2, nrow = length(vp.names)))
              
              col.by.row[,1] = ceiling(length(vp.names)/(1:length(vp.names)))
              col.by.row[,2] = ceiling(length(vp.names)/col.by.row[,1])
              
              wh.best = which.min(abs(apply(col.by.row,1,function(x) x[1]/x[2]) - ds.ratio))
              
              cols = col.by.row[wh.best,1]
              rows = col.by.row[wh.best,2]
              
              actual.ratio = cols/rows
              
              if (add==F) {
                grid.newpage()
                vp = viewport(xscale = rangex,yscale= rangey,w=0.8,h=0.8)
                pushViewport(vp)
                grid.points(x1/max(1,ds.ratio/actual.ratio),y1/max(1,actual.ratio/ds.ratio),gp=gp1,...)  
                ats = list()
                prettys = list()
                ats[["x"]] = rangex/max(1,ds.ratio/actual.ratio)
                ats[["y"]] = rangey/max(1,actual.ratio/ds.ratio)
                prettys[["x"]] = pretty(ats[["x"]])
                prettys[["y"]] = pretty(ats[["y"]])
                
                grid.xaxis(gp=gpar(cex=0.5),at = seq(ats[["x"]][1],ats[["x"]][2],length.out=length(prettys[["x"]])),label = prettys[["x"]])
                grid.yaxis(gp=gpar(cex=0.5),at = seq(ats[["y"]][1],ats[["y"]][2],length.out=length(prettys[["y"]])),label = prettys[["y"]])
                upViewport()
#                grid.yaxis(gp=gpar(cex=0.5),at = pretty(rangey/max(1,actual.ratio/ds.ratio)),label = pretty(rangey))
              } else { #add=T
                for (i in 1:length(on.which.to.plot)) {
                    if(is.numeric(on.which.to.plot[i])) {
                      seekViewport(vp.names[on.which.to.plot[i]])
                    } else {
                      seekViewport(on.which.to.plot[i])
                    }
                    grid.points(x1/max(1,ds.ratio/actual.ratio),y1/max(1,actual.ratio/ds.ratio),gp=gpar(cex=0.4),pch=19)  
#                    grid.points(x1,y1,gp = gpar(col = "green"))#,...)  
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
