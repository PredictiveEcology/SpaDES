\docType{methods}
\name{simplot}
\alias{simplot}
\alias{simplot,RasterLayer}
\alias{simplot,RasterLayer-method}
\alias{simplot,RasterStack}
\alias{simplot,RasterStack-method}
\alias{simplot,pointAgent}
\alias{simplot,pointAgent-method}
\title{Plotting methods}
\usage{
simplot(x, ...)

\S4method{simplot}{RasterStack}(x, which.to.plot = "all", speedup = 10,
  axes = "L", add = FALSE, ...)

\S4method{simplot}{RasterLayer}(x, speedup = 1, axes = "L", add = FALSE,
  on.which.to.plot = NULL, ...)

\S4method{simplot}{pointAgent}(x, on.which.to.plot = 1, ext,
  map.names = NULL, speedup = 1, delete.previous = T, axes = "L",
  max.agents = 10000, add = TRUE, ...)
}
\arguments{
  \item{x}{rasterStack object.}

  \item{...}{Additional plotting functions passed to
  grid.raster (if rasterStack) or grid.points (if
  pointAgent)}

  \item{which.to.plot}{Numeric or character vector
  identifying which rasters in \code{rasterStack} to plot.}

  \item{speedup}{Scalar indicating how much faster than
  normal to make plots (see Details).}

  \item{axes}{String either "all", "L", or "none" (see
  Details). Default is "L".}

  \item{add}{Logical indicating whether to plot new maps
  (\code{FALSE}) or update exising maps (\code{TRUE}).
  Default is \code{FALSE}.}

  \item{on.which.to.plot}{Numeric or character vector
  identifying on which raster in existing plot window to
  plot.}

  \item{speedup}{Scalar indicating how much faster than
  normal to make plots (see Details).}

  \item{axes}{String either "all", "L", or "none" (see
  Details). Default is "L".}

  \item{add}{Logical indicating whether to plot new maps
  (\code{FALSE}) or update exising maps (\code{TRUE}).
  Default is \code{FALSE}.}

  \item{on.which.to.plot}{when add = TRUE, which map to
  plot on}
}
\value{
Creates a plot within the active plotting device.
}
\description{
Plotting for RasterStack, Agents and other classes used in
simulations.
}
\details{
For class \code{RasterStack}, this method plots many
rasters. This method automatically plots individual rasters
in with an arrangement on the plotting window that is
optimal for the size and shape of the window.
\code{Speedup} will make downsample the number of pixels,
allowing for greater plotting speed, at a cost of more
fuzzy plots.
}
\seealso{
\code{\link{grid.raster}}
}

