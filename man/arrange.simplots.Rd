\name{arrange.simplots}
\alias{arrange.simplots}
\title{Determine optimal plotting arrangement of RasterStack}
\usage{
arrange.simplots(ext, dimx, nam, which.to.plot, axes, ...)
}
\arguments{
  \item{ext}{extent object}

  \item{dimx}{dimension of rasterStack}

  \item{nam}{names in rasterStack}

  \item{which.to.plot}{vector of numbers or names in
  rasterStack to plot}

  \item{axes}{passed from simplot}
}
\description{
Hidden function.
}
\details{
This assesses the device geometry, the map geometry, and
the number of rasters to plot and builds an object that
will be used by the simPlot functions to plot them
efficiently
}

