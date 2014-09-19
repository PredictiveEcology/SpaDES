## ----set_colors, eval=TRUE, echo=TRUE, fig.height=2----------------------
# can change color palette
library(RColorBrewer)
setColors(landscape, n=50) <-
              list(DEM=topo.colors(50),
                   forestCover=brewer.pal(9, "Set1"),
                   forestAge=brewer.pal("Blues", n=8),
                   habitatQuality=brewer.pal(9, "Spectral"),
                   percentPine=brewer.pal("GnBu", n=8))
Plot(landscape[[2:3]])

## ----new_layers, eval=TRUE, echo=TRUE, fig.height=2----------------------
#Make a new raster derived from a previous one; must give it a unique name
habitatQuality2 <- ((landscape$forestAge) / 100 + 1) ^ 6
#setColors(habitatQuality2) <- heat.colors(50)
Plot(landscape[[1:3]], add=FALSE)
Plot(habitatQuality2, add=TRUE)

## ----naming, eval=TRUE, echo=TRUE, fig.height=2--------------------------
name(habitatQuality2) <- "habitatQuality2"
print(habitatQuality2)
Plot(landscape[[1:3]], add=FALSE)
Plot(habitatQuality2, add=TRUE)

## ----mixing_layer_types, eval=TRUE, echo=TRUE, fig.height=2--------------
Plot(landscape, caribou, habitatQuality2, add=FALSE)

## ----visualSqueeze, eval=TRUE, echo=TRUE, fig.height=2, dpi=900----------
# x axis gets cut off in pdf and html
Plot(DEM, add=FALSE)
Plot(DEM, visualSqueeze=0.6, add=FALSE)

## ----simple_add, eval=TRUE, echo=TRUE, fig.height=3----------------------
Plot(landscape, add=FALSE)
# can add a new plot to the plotting window
Plot(caribou, add=TRUE, axes=FALSE)

## ----add_with_rearrangement, eval=TRUE, echo=TRUE, fig.height=2----------
Plot(landscape[[1:4]], add=FALSE)
# can add a new plot to the plotting window
Plot(caribou, add=TRUE, axes=FALSE)

## ----add_with_same_name, eval=TRUE, echo=TRUE, fig.height=2--------------
Plot(landscape[[1:3]], add=FALSE)
landscape$forestAge = (landscape$forestAge +10 %% 100)
landscape$forestCover = (landscape$forestCover +10 %% 30)
# can add a new plot to the plotting window
Plot(landscape[[2:3]], add=TRUE)

## ----speedup, eval=TRUE, echo=TRUE, fig.height=2-------------------------
system.time(Plot(landscape, caribou, habitatQuality2, add=FALSE))
system.time(Plot(landscape, caribou, habitatQuality2, speedup=200, add=FALSE))
# can add a new plot to the plotting window

## ----add, eval=TRUE, echo=TRUE, fig.height=2-----------------------------
Plot(landscape, add=FALSE)
Plot(caribou, addTo="forestAge", size=2, axes=F)

## ----, echo=TRUE, eval=FALSE, fig.height=2-------------------------------
#  Plot(landscape, caribou, DEM, add=FALSE)

## ----, echo=TRUE, eval=FALSE---------------------------------------------
#  # simple:
#  dev(4)
#  
#  # better:
#  #Plot all maps on a new plot windows - Do not use RStudio window
#  if(is.null(dev.list())) {
#     dev(2)
#  } else {
#   if(any(names(dev.list())=="RStudioGD")) {
#     dev(which(names(dev.list())=="RStudioGD")+3)
#   } else {
#     dev(max(dev.list()))
#   }
#  }

