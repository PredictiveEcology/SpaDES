\docType{methods}
\name{plotGrob}
\alias{plotGrob}
\alias{plotGrob,Raster-method}
\alias{plotGrob,SpatialPoints-method}
\title{Plot either a raster Grob or a points Grob}
\usage{
plotGrob(grobToPlot, col = NULL, size = unit(5, "points"), legend = TRUE,
  draw = TRUE, gp = gpar(), vp = NULL, pch = 19, maxpixels = 1e+06,
  childrenvp = NULL, ...)

\S4method{plotGrob}{Raster}(grobToPlot, col, size, legend, draw, gp, vp, pch,
  maxpixels, childrenvp, ...)

\S4method{plotGrob}{SpatialPoints}(grobToPlot, col, size, legend, draw,
  gp = gpar(), vp = NULL, pch, maxpixels, childrenvp = NULL, ...)
}
\arguments{
  \item{grobToPlot}{Raster* or SpatialPoints* object}
}
\description{
Plot either a raster Grob or a points Grob
}

