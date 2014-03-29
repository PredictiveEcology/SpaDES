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
#' @return Creates a plot within the active plotting device.
#' 
#' #@seealso \code{\link{grid.raster}}
#' 
#' @import grid
#' @import raster
#' @export
#' @docType methods
#' @rdname simplot
#'
# @examples
# needs examples
setGeneric("simplot", function(x,...) {
           standardGeneric("simplot")
})

# which functions are used from what packages
# needs grid package for 10 functions

#' @param which.to.plot numeric or character vector identifying which rasters in \code{rasterStack} to plot.
#' 
#' @param speedup scalar indicating how much faster than normal to make plots (see Details).
#' 
#' @param axes string either "all", "L", or "none" (see Details). Default is "L".
#' 
#' @param add logical indicating whether to plot new maps (F) or update exising maps (T). Default is F.
#' 
#' @param ... additional plotting functions passed to grid.raster
#' @rdname simplot
setMethod("simplot",
          signature = "RasterStack",
          definition = function(x, which.to.plot = "all", speedup = 100, axes = "L", add = F,... ) {
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
              
              if (add == F) {
                  col.by.row = data.frame(matrix(ncol = 2, nrow = length(wh)))
                  
                  col.by.row[,1] = ceiling(length(wh)/(1:length(wh)))
                  col.by.row[,2] = ceiling(length(wh)/col.by.row[,1])
                  
                  wh.best = which.min(abs(apply(col.by.row,1,function(x) x[1]/x[2]) - ds.ratio))
                  
                  cols = col.by.row[wh.best,1]
                  rows = col.by.row[wh.best,2]
                  
                  row.col = min(rows/cols*ds[1]/ds[2] , 1)
                  col.row = min(cols/rows*ds[2]/ds[1] , 1)
                  
                  
                  #            if (add == F) {
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
                      
                      
                      vp[[i]] <- viewport(x = cr[i,"cols"], y = cr[i,"rows"], w = 1/cols*0.8, h = 1/rows*0.8,
                                          just = c(0.5, 0.5),
                                          name = w,
                                          xscale = c(xmin(ext),xmax(ext)),yscale= c(ymin(ext),ymax(ext)))
                      pushViewport(vp[[i]])
                      grid.raster(as.raster(x[[w]],maxpixels=1e4/(cols*rows)*prod(ds)/speedup))#,...)
                      if (axes != "none" & axes != FALSE) {
                          if (axes == "L") {
                              if (cr$cols[i]==min(cr$cols)) {
                                  grid.yaxis(gp=gpar(cex=0.5),at = ats[["y"]], label = ats[["y"]])
                              }
                              if (cr$rows[i] == min(cr$rows)) {
                                  grid.xaxis(gp=gpar(cex=0.5),at = ats[["x"]], label = ats[["x"]])
                              }
                          } else {
                              grid.xaxis(gp=gpar(cex=0.5),at = ats[["x"]], label = ats[["x"]])
                              grid.yaxis(gp=gpar(cex=0.5),at = ats[["y"]], label = ats[["y"]])
                          }
                      }
                      grid.text(names(x)[ma], y = 1.05, vjust = 0.5, gp = gpar(cex=1-0.015*length(wh)))
                      upViewport()
                  }
              } else if (add == T){
                  for (i in wh) {
                      vp.names = sapply(current.vpTree()$children, function(x) x$name)
                      if (is.numeric(i)) i = nam[i]#match(nam,vp.names)
                      seekViewport(i)
                      grid.raster(as.raster(x[[i]],maxpixels=1e4/(length(vp.names))*prod(ds)/speedup))
                      upViewport()
                  }
              }
          })


#' @param ... additional plotting functions passed to plot or points
#' @param on.which.to.plot when add = T, which map to plot on
#' @rdname simplot
setMethod("simplot",
          signature = "mobileAgent",
          definition = function(x, on.which.to.plot = 1, speedup = 100, axes = "L", add = F, ...) {
              if (add==F) {
                plot(x,type = "p", ...)
              } else {
                points(x,...)  
              }
          })

###