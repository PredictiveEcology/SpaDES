\docType{methods}
\name{arrangeViewports}
\alias{arrangeViewports}
\alias{arrangeViewports,list-method}
\title{Determine optimal plotting arrangement of RasterStack}
\usage{
arrangeViewports(toPlot, name = NULL)

\S4method{arrangeViewports}{list}(toPlot, name)
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

