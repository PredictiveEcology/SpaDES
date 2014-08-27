\docType{methods}
\name{Plot}
\alias{Plot}
\title{Fast, optimally arranged, multipanel plotting function with spades}
\usage{
Plot(..., add = F, addTo = NULL, gp = gpar(), axes = "L", speedup = 1,
  size = 5, cols, visualSqueeze = 0.75, legend = TRUE, draw = TRUE,
  pch = 19, title = T)
}
\arguments{
  \item{...}{Raster* object(s) and or SpatialPoints*Named
  objects}

  \item{axes}{Logical. If FALSE, then the previous plot is
  wiped and a new one made; if TRUE, then the ... plots
  will be added to the current device, adding or
  rearranging the plot layout as necessary.}

  \item{addTo}{String vector, with same length as ...  This
  is for overplotting, when the overplot is not to occur on
  the plot with the same name, such as plotting a
  SpatialPoints*Named object on a RasterLayer.}

  \item{gp}{A gpar object, created by gpar() function, to
  change plotting parameters (see grid package)}

  \item{axes}{Logical or "L", representing the left and
  bottom axes, overall plots}

  \item{speedup}{Numeric. The factor by which the number of
  pixels is divided by to plot rasters. See Details.}

  \item{size}{Numeric. The size, in points, for
  SpatialPoints symbols, if using a scalable symbol.}

  \item{col}{character vector. Hex codes for colors. See
  Details.}

  \item{visualSqueeze}{numeric. The proportion of the white
  space to be used for plots. Default is 0.75.}

  \item{legend}{logical. Whether legend should be drawn
  next to plots. Default is TRUE.}

  \item{draw}{logical, whether to actually draw the plots.
  Currently, there is no reason for this to be FALSE.
  Default is TRUE}

  \item{pch}{see ?par}

  \item{title}{Logical. Whether the names of each plot
  should be written above plots}
}
\description{
The main plotting function accompanying spades. This can
take objects of type Raster* or SpatialPoints*Named, and
any combination of those.
}
\details{
If add=F, then a new plot will be generated. When add=T,
then any plot that already exists will be overplotted,
while plots that have not already been plotted will be
added. This function rearrange the plotting device to
maximize the size of all the plots, minimizing white space.
If using RStudio, it is recommended to makeand use a new
device because the built in device is not made for rapid
redrawing. The function is based on the grid package.

Silently, one hidden object is made, .arr, which is used
for arranging plots in the device window, and identifying
the objects to be replotted if rearranging is required,
subsequent to an add=T additional plot.

\code{speedup} is not a precise number because it is faster
to plot an un-resampled raster if the new resampling is
close to the original number of pixels. At the moment, this
is set to 1/3 of the original pixels. In other words,
\code{speedup} may not do anything if the factor for
speeding up is not high enough (i.e., >3).

\code{col} can be used to set the colors of Raster*
objects, but it is preferable to use setColors to give each
layer its own color table. See examples.
}
\examples{
#  Make list of maps from package database to load, and what functions to use to load them
fileList <-
   data.frame(files =
     dir(file.path(
                   find.package("SpaDES",
                                lib.loc=getOption("devtools.path"),
                                quiet=FALSE),
                  "maps"),
        full.names=TRUE, pattern= "tif"),
     functions="rasterToMemory",
     packages="SpaDES",
     stringsAsFactors=FALSE)

# Load files to memory (using rasterToMemory)
sim <- loadFiles(fileList=fileList)

# make a stack of all these rasters
landscape <- stack(mget(unlist(simObjectsLoaded(sim))))
name(landscape) <- "landscape"

# extract a single one of these rasters
DEM <- landscape$DEM

# can change color palette
setColors(landscape, n = 50)<-list(DEM=topo.colors(50),
                           forestCover = brewer.pal(9,"Set1"),
                           forestAge = brewer.pal("Blues",n=8),
                           habitatQuality = brewer.pal(9,"Spectral"),
                           percentPine = brewer.pal("GnBu",n=8))

DEM1 <- landscape$DEM
names(DEM1) <- "DEM1"

# make a SpatialPointsNamed object
caribou <- SpatialPoints(cbind(x=runif(1e2,-50,50),y=runif(1e2,-50,50)))
     name(caribou)<-"caribou"

#Plot all maps on a new plot windows - Do not use RStudio window
if(is.null(dev.list())) {
  dev(2)
} else {
  if(any(names(dev.list())=="RStudioGD")) {
    dev(which(names(dev.list())=="RStudioGD")+3)
  } else {
    dev(max(dev.list()))
  }
}

Plot(landscape)

# Can overplot, using addTo
Plot(caribou, addTo="forestAge",size=4, axes=F)

# can add a new plot to the plotting window
Plot(caribou, add=T)

# can't add a two maps with same name
Plot(landscape, caribou, DEM)

# can mix stacks, rasters, SpatialPoint*Named
Plot(landscape, DEM1, caribou)

# can mix stacks, rasters, SpatialPoint*Named
Plot(landscape, caribou)
Plot(DEM1, add=T)
}

