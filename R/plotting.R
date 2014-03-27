# This creates a rasterList class, very general, no checking... and plot function
setClass("rasterList", slots=c(maps="list"))

# which functions are used from what packages
# needs grid package for 10 functions
setMethod("plot",
          signature = "rasterList",
          definition = function(x, y, which.to.plot = "all", speedup = 100, axes = "L", add = F, ...) {
              map.list = x
              nam = names(map.list)
              
              if (length(which.to.plot)==1) {
                  if(which.to.plot=="all")
                      wh = 1:length(map.list)
                  else
                      wh = which.to.plot
              } else {
                  wh = which.to.plot
              }
              if (is.character(wh)) if (any(is.na(match(wh, names(map.list))))) stop("Not a named map in rasterList")
              
              ext = extent(map.list[[1]])
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
                                          #                                    xscale = c(xmin(ext),xmax(ext))/col.row,yscale= c(ymin(ext),ymax(ext))/row.col)
                                          xscale = c(xmin(ext),xmax(ext)),yscale= c(ymin(ext),ymax(ext)))
                      pushViewport(vp[[i]])
                      grid.raster(as.raster(map.list[[w]],maxpixels=1e4/(cols*rows)*prod(ds)/speedup))
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
                      grid.text(names(map.list)[ma], y = 1.05, vjust = 0.5, gp = gpar(cex=1-0.015*length(wh)))
                      upViewport()
                  }
              } else if (add == T){
                  for (i in wh) {
                      vp.names = sapply(current.vpTree()$children, function(x) x$name)
                      if (is.numeric(i)) i = vp.names[i]
                      seekViewport(i)
                      grid.raster(as.raster(map.list[[i]],maxpixels=1e4/(length(vp.names))*prod(ds)/speedup))
                      upViewport()
                  }
              }
          })


###