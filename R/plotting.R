# This creates a rasterList class, very general, no checking... and plot function
setClass("rasterList", slots=c(maps="list"))

setMethod("plot",
          signature = "rasterList",
          definition = function(x, y, which.to.plot = "all", speedup = 100, axes = "L", ...) {
              map.list = x
              if (length(which.to.plot)==1) {if(which.to.plot=="all") wh = 1:length(map.list) else wh = which.to.plot
              } else {
                  wh = which.to.plot
              }
              
              ext = extent(map.list[[1]])
              if(dev.cur()==1) {
                  dev.new(height=8, width=10)
              }
              
              ds = dev.size()
              ds.ratio = ds[1]/ds[2]
              
              col.by.row = data.frame(matrix(ncol = 2, nrow = length(wh)))
              
              col.by.row[,1] = ceiling(length(wh)/(1:length(wh)))
              col.by.row[,2] = ceiling(length(wh)/col.by.row[,1])
              
              wh.best = which.min(abs(apply(col.by.row,1,function(x) x[1]/x[2]) - ds.ratio))
              
              cols = col.by.row[wh.best,1]
              rows = col.by.row[wh.best,2]
              
              row.col = min(rows/cols*ds[1]/ds[2] , 1)
              col.row = min(cols/rows*ds[2]/ds[1] , 1)
              vp = list()
              
              grid.newpage()
              if (axes != "none" & axes != FALSE) {
                  atx = seq(xmin(ext),xmax(ext),length.out=5)
                  aty = seq(ymin(ext),ymax(ext),length.out=5)
              }
              cr = expand.grid(cols=((1:cols/cols - 1/cols/2)-0.55)*0.9+0.55,rows=((1:rows/rows - 1/rows/2)-0.55)*0.9+0.55)
              
              for (w in wh) {
                  i = match(w,wh)
                  
                  
                  vp[[i]] <- viewport(x = cr[i,"cols"], y = cr[i,"rows"], w = 1/cols*0.8, h = 1/rows*0.8,
                                      just = c(0.5, 0.5),
                                      name = names(map.list)[w],
                                      xscale = c(xmin(ext),xmax(ext))/col.row,yscale= c(ymin(ext),ymax(ext))/row.col)
                  pushViewport(vp[[i]])
                  grid.raster(as.raster(map.list[[w]],maxpixels=1e4/(cols*rows)*prod(ds)/speedup))
                  if (axes != "none" & axes != FALSE) {
                      if (axes == "L") {
                          if (cr$cols[i]==min(cr$cols)) {
                              grid.yaxis(gp=gpar(cex=0.5),at = aty, label = aty)
                          }
                          if (cr$rows[i] == min(cr$rows)) {
                              grid.xaxis(gp=gpar(cex=0.5),at = atx, label = atx)
                          }
                      } else {
                          grid.xaxis(gp=gpar(cex=0.5),at = atx, label = atx)
                          grid.yaxis(gp=gpar(cex=0.5),at = aty, label = aty)
                      }
                  }
                  grid.text(names(map.list)[w], y = 1.05, vjust = 0.5, gp = gpar(cex=1-0.025*length(wh)))
                  upViewport()
              }
})

###