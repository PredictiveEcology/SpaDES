\docType{methods}
\name{plotGrob}
\alias{plotGrob}
\alias{plotGrob,Raster-method}
\alias{plotGrob,SpatialPoints-method}
\title{Plot either a raster Grob or a points Grob}
\usage{
plotGrob(grobToPlot, add = TRUE, col = NULL, size = unit(5, "points"),
  legend = TRUE, draw = TRUE, gp = gpar(), vp = NULL, pch = 19,
  maxpixels = 1e+06, childrenvp = NULL, ...)

\S4method{plotGrob}{Raster}(grobToPlot, add, col, size, legend, draw, gp, vp,
  pch, maxpixels, childrenvp, ...)

\S4method{plotGrob}{SpatialPoints}(grobToPlot, add = FALSE, col, size, legend,
  draw, gp = gpar(), vp = NULL, pch, maxpixels, childrenvp = NULL, ...)
}
\arguments{
  \item{grobToPlot}{Raster* or SpatialPoints* object}

  \item{add}{should grob be added to current plot}
}
\description{
Plot either a raster Grob or a points Grob
}

