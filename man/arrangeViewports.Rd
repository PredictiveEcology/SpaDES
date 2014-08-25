\docType{methods}
\name{arrangeViewports}
\alias{Plot}
\alias{arrangeViewports}
\alias{arrangeViewports,list-method}
\title{Determine optimal plotting arrangement of RasterStack}
\usage{
arrangeViewports(extents, name = NULL)

\S4method{arrangeViewports}{list}(extents, name)

Plot(..., add = F, addTo = NULL, gp = gpar(), axes = "L", speedup = 1,
  size = 5, cols = topo.colors(50), deletePrevious = add,
  visualSqueeze = 0.75, quick = FALSE, legend = !quick, draw = TRUE,
  pch = 19, title = T)
}
\arguments{
  \item{toPlot}{Raster* object}

  \item{axes}{passed from Plot}

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
}
\description{
Hidden function.

The main plotting function accompanying spades. This can
take objects of type Raster* or SpatialPoints*Named, and
any combination of those. If add=F, then a new plot will be
generated. When add=T, then any plot that already exists
will be overplotted, while plots that have not already been
plotted will be added. This function rearrange the plotting
device to maximize the size of all the plots, minimizing
white space. If using RStudio, it is recommended to makeand
use a new device because the built in device is not made
for rapid redrawing. The function is based on the grid
package.
}
\details{
This assesses the device geometry, the map geometry, and
the number of rasters to plot and builds an object that
will be used by the Plot functions to plot them efficiently

Silently, two hidden objects are made, .arr and .grobs,
which are used for later replotting (i.e., if add=T). Also,
the function invisibly returns the grobs (see grid
package).
}

