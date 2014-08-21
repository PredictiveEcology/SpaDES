\docType{methods}
\name{placeOnRaster,NamedSpatialPoints-method}
\alias{arrangePlots}
\alias{arrangePlots,list-method}
\alias{placeOnRaster,NamedSpatialPoints-method}
\title{Determine optimal plotting arrangement of RasterStack}
\usage{
\S4method{placeOnRaster}{NamedSpatialPoints}(obj, raster)

arrangePlots(toPlot, name = NULL)

\S4method{arrangePlots}{list}(toPlot, name)
}
\arguments{
  \item{toPlot}{Raster* object}

  \item{axes}{passed from Plot}
}
\description{
Hidden function.
}
\details{
This assesses the device geometry, the map geometry, and
the number of rasters to plot and builds an object that
will be used by the Plot functions to plot them efficiently
}

